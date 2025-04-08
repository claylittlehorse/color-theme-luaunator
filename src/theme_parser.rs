use serde::Deserialize;
use std::collections::HashMap;
use std::fs;
use std::thread::Scope;

macro_rules! extract_value {
    ($json:expr, $key:expr, $method:ident) => {
        $json.get($key).and_then(|value| value.$method())
    };
}

macro_rules! extract_string {
    ($json:expr, $key:expr) => {
        $json.get($key).and_then(|value| value.as_str())
    };
}

#[derive(Deserialize, Debug, Clone)]
pub struct ThemeEntry {
    pub label: String,
    pub path: String,
}

#[repr(u8)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum FontStyle {
    None = 0b0,
    Italic = 0b1,
    Bold = 0b10,
    Underline = 0b100,
    Strikethrough = 0b1000,
    NotSet = 0b11111111,
}

fn parse_font_style(font_style: Option<&str>) -> u8 {
    match font_style {
        Some(font_style) => {
            let mut font_mask = FontStyle::None as u8;
            for style in font_style.split_whitespace() {
                match style {
                    "bold" => font_mask |= FontStyle::Bold as u8,
                    "italic" => font_mask |= FontStyle::Italic as u8,
                    "underline" => font_mask |= FontStyle::Underline as u8,
                    "strikethrough" => font_mask |= FontStyle::Strikethrough as u8,
                    _ => {}
                }
            }

            font_mask
        }
        None => FontStyle::NotSet as u8,
    }
}

#[derive(Debug)]
struct ScopeStack<'a> {
    scope_name: &'a str,
    parent: Option<Box<ScopeStack<'a>>>,
}

impl<'a> ScopeStack<'a> {
    fn new(parent: Option<Box<ScopeStack<'a>>>, scope_name: &'a str) -> ScopeStack<'a> {
        ScopeStack { parent, scope_name }
    }

    fn push(self, scope_path: &'a str) -> ScopeStack<'a> {
        ScopeStack::new(Some(Box::new(self)), scope_path)
    }

    fn from_scope_path(scope_path: &'a str) -> Option<ScopeStack<'a>> {
        let segments: Vec<&str> = scope_path.split_whitespace().collect();

        let mut final_scope: Option<Box<ScopeStack<'a>>> = None;

        for segment in segments.into_iter() {
            final_scope = Some(Box::new(ScopeStack::new(final_scope, segment)));
        }

        final_scope.map(|boxed_scope| *boxed_scope)
    }
}

struct ParsedThemeRule<'a> {
    scope: &'a str,
    parent_scopes: Option<Vec<&'a str>>,
    index: usize,
    font_style: u8,
    foreground: Option<&'a str>,
    background: Option<&'a str>,
}

fn process_scope_string<'a>(scope_str: &'a str) -> impl Iterator<Item = &'a str> {
    scope_str
        .trim()
        .trim_matches(',')
        .trim()
        .split(',')
        .map(|s| s.trim())
        .filter(|s| !s.is_empty())
}

fn process_scopes<'a>(scope: &'a str) -> (Option<&'a str>, Option<Vec<&'a str>>) {
    let segments: Vec<&'a str> = scope.split_whitespace().collect();

    if segments.is_empty() {
        return (None, None);
    }

    let scope = segments.last().unwrap();
    let parent_scopes = if segments.len() > 1 {
        Some(
            segments[..segments.len() - 1]
                .iter()
                .rev()
                .cloned()
                .collect(),
        )
    } else {
        None
    };

    return (Some(*scope), parent_scopes);
}

#[derive(Debug, Clone)]
pub struct ThemeTrieRule<'a> {
    pub scope: &'a str,
    scope_depth: usize,
    pub font_style: u8,
    pub foreground: Option<&'a str>,
    pub background: Option<&'a str>,
    parent_scopes: Option<Vec<&'a str>>,
}

impl<'a> ThemeTrieRule<'a> {
    fn accept_overwrite(
        &mut self,
        scope_depth: usize,
        font_style: u8,
        foreground: Option<&'a str>,
        background: Option<&'a str>,
    ) {
        if self.scope_depth > scope_depth {
            return;
        }
        self.scope_depth = scope_depth;
        if font_style != FontStyle::NotSet as u8 {
            self.font_style = font_style;
        }
        self.foreground = foreground.or(self.foreground);
        self.background = background.or(self.background);
    }
}

#[derive(Debug)]
struct ThemeTrieNode<'a> {
    main_rule: ThemeTrieRule<'a>,
    rules_with_parent_scopes: Vec<ThemeTrieRule<'a>>,
    children: HashMap<String, ThemeTrieNode<'a>>,
}

impl<'a> ThemeTrieNode<'a> {
    fn new(main_rule: ThemeTrieRule<'a>, rules_with_parent_scopes: Vec<ThemeTrieRule<'a>>) -> Self {
        ThemeTrieNode {
            main_rule,
            rules_with_parent_scopes,
            children: HashMap::new(),
        }
    }

    fn insert(
        &mut self,
        depth: usize,
        scope: &'a str,
        unclipped_scope: &'a str,
        parent_scopes: Option<Vec<&'a str>>,
        font_style: u8,
        foreground: Option<&'a str>,
        background: Option<&'a str>,
    ) {
        if scope.is_empty() {
            self.insert_here(
                unclipped_scope,
                depth,
                parent_scopes,
                font_style,
                foreground,
                background,
            );
            return;
        }

        let (head, tail) = if let Some(dot_index) = scope.find('.') {
            (&scope[..dot_index], &scope[dot_index + 1..])
        } else {
            (scope, "")
        };

        let child = self.children.entry(head.to_string()).or_insert_with(|| {
            ThemeTrieNode::new(
                self.main_rule.clone(),
                self.rules_with_parent_scopes.clone(),
            )
        });

        child.insert(
            depth + 1,
            tail,
            unclipped_scope,
            parent_scopes,
            font_style,
            foreground,
            background,
        );
    }

    fn insert_here(
        &mut self,
        scope: &'a str,
        scope_depth: usize,
        parent_scopes: Option<Vec<&'a str>>,
        font_style: u8,
        foreground: Option<&'a str>,
        background: Option<&'a str>,
    ) {
        if parent_scopes.is_none() {
            // Merge into the main rule
            self.main_rule
                .accept_overwrite(scope_depth, font_style, foreground, background);
            self.main_rule.scope = scope;
            return;
        }

        let parent_scopes = parent_scopes.unwrap();
        for rule in &mut self.rules_with_parent_scopes {
            if rule.parent_scopes.as_ref() == Some(&parent_scopes) {
                rule.accept_overwrite(scope_depth, font_style, foreground, background);
                return;
            }
        }

        let inherited_font_style = if font_style == FontStyle::NotSet as u8 {
            self.main_rule.font_style
        } else {
            font_style
        };

        let inherited_foreground = foreground.or(self.main_rule.foreground);
        let inherited_background = background.or(self.main_rule.background);

        // Add the new rule
        self.rules_with_parent_scopes.push(ThemeTrieRule {
            scope,
            scope_depth,
            font_style: inherited_font_style,
            foreground: inherited_foreground,
            background: inherited_background,
            parent_scopes: Some(parent_scopes),
        });
    }

    fn match_scope(&self, scope: &str) -> Vec<ThemeTrieRule<'a>> {
        println!("Matching scope: {}", scope);
        if !scope.is_empty() {
            let (head, tail) = if let Some(dot_index) = scope.find('.') {
                (&scope[..dot_index], &scope[dot_index + 1..])
            } else {
                (scope, "")
            };

            println!("Head: [{}] Tail: [{}]", head, tail);

            if let Some(child) = self.children.get(head) {
                return child.match_scope(tail);
            }
        };

        println!("Found match!");

        let mut rules = self.rules_with_parent_scopes.clone();
        rules.push(self.main_rule.clone());

        println!("All rules here: {:#?}", rules);

        rules.sort_by(|a, b| ThemeTrieNode::compare_rules(a, b));
        rules
    }

    fn compare_rules(a: &ThemeTrieRule<'a>, b: &ThemeTrieRule<'a>) -> std::cmp::Ordering {
        if a.scope_depth != b.scope_depth {
            return b.scope_depth.cmp(&a.scope_depth);
        }

        let mut a_parent_index = 0;
        let mut b_parent_index = 0;

        while a_parent_index < a.parent_scopes.as_ref().unwrap_or(&Vec::new()).len()
            && b_parent_index < b.parent_scopes.as_ref().unwrap_or(&Vec::new()).len()
        {
            let a_parent = &a.parent_scopes.as_ref().unwrap()[a_parent_index];
            let b_parent = &b.parent_scopes.as_ref().unwrap()[b_parent_index];

            let parent_scope_length_diff = b_parent.len().cmp(&a_parent.len());
            if parent_scope_length_diff != std::cmp::Ordering::Equal {
                return parent_scope_length_diff;
            }

            a_parent_index += 1;
            b_parent_index += 1;
        }

        b.parent_scopes
            .as_ref()
            .unwrap_or(&Vec::new())
            .len()
            .cmp(&a.parent_scopes.as_ref().unwrap_or(&Vec::new()).len())
    }
}

fn scope_path_matches_parent_scopes(
    scope: Option<&ScopeStack>,
    parent_scopes_to_match: &Vec<&str>,
) -> bool {
    if parent_scopes_to_match.is_empty() {
        return true;
    }

    let mut scope: Option<&ScopeStack> = scope;

    let mut index = 0;

    while index < parent_scopes_to_match.len() {
        let mut scope_pattern = parent_scopes_to_match[index];
        let mut scope_must_match = false;

        if scope_pattern == ">" {
            if index == parent_scopes_to_match.len() - 1 {
                return false;
            }
            index += 1;
            scope_pattern = parent_scopes_to_match[index];
            scope_must_match = true;
        }

        while let Some(ref scope_result) = scope {
            if scope_result.scope_name == scope_pattern
                || (scope_result.scope_name.starts_with(scope_pattern)
                    && scope_result.scope_name[scope_pattern.len()..].starts_with('.'))
            {
                break;
            }

            if scope_must_match {
                return false;
            }

            scope = scope_result.parent.as_deref()
        }

        index += 1;
    }

    true
}

#[derive(Debug)]
pub struct ThemeTrie<'a> {
    root: ThemeTrieNode<'a>,
    default_foreground: &'a str,
    default_background: &'a str,
}

impl<'a> ThemeTrie<'a> {
    fn match_scope(&self, scope_path: &'a str) -> Option<ThemeTrieRule<'a>> {
        let scope = ScopeStack::from_scope_path(scope_path);

        if let None = scope {
            return None;
        }

        let matching_rules = self.root.match_scope(scope.as_ref().unwrap().scope_name);

        // println!("Matching rules: {:#?}", matching_rules);

        let matching_rule = matching_rules.into_iter().find(|rule| {
            if let Some(parent_scopes) = &rule.parent_scopes {
                scope_path_matches_parent_scopes(scope.as_ref(), parent_scopes)
            } else {
                true
            }
        });

        matching_rule
    }
}

#[derive(Debug)]
pub struct SemanticTokenScope {
    pub token_type: Option<String>,
    pub modifiers: i16,
    pub language: Option<String>,
}

#[repr(i16)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum SemanticTokenModifier {
    Declaration = 0b1,
    Definition = 0b10,
    Readonly = 0b100,
    Static = 0b1000,
    Deprecated = 0b10000,
    Abstract = 0b100000,
    Async = 0b1000000,
    Modification = 0b10000000,
    Documentation = 0b100000000,
    DefaultLibrary = 0b1000000000,
}

pub struct SemanticThemeSelector<'a> {
    pub rules: Vec<(SemanticTokenScope, u8, Option<&'a str>)>,
}

impl<'a> SemanticThemeSelector<'a> {
    fn new() -> Self {
        SemanticThemeSelector { rules: Vec::new() }
    }

    fn add_rule(&mut self, scope: SemanticTokenScope, font_style: u8, foreground: Option<&'a str>) {
        self.rules.push((scope, font_style, foreground));
    }

    fn select_theme_rule(&self, token_scope: &SemanticTokenScope) -> Option<(u8, Option<&'a str>)> {
        let mut highest_score = 0;
        let mut selected_theme: Option<(u8, Option<&'a str>)> = None;

        for (scope, font_style, foreground) in &self.rules {
            if let Some(scope_language) = &scope.language {
                if Some(scope_language) != token_scope.language.as_ref() {
                    continue;
                }
            }

            let mut specificity = 0;

            if let Some(scope_token_type) = &scope.token_type {
                if scope_token_type == token_scope.token_type.as_ref().unwrap_or(&String::new()) {
                    specificity += 1;
                }
            }

            specificity += (scope.modifiers & token_scope.modifiers).count_ones() as u8;

            if specificity >= highest_score {
                highest_score = specificity;
                selected_theme = Some((*font_style, *foreground));
            }
        }

        selected_theme
    }
}

use std::rc::Rc;

pub struct ParsedTheme {
    pub label: String,
    pub theme_json: &'static serde_json::Value, // Shared ownership of theme_json
    pub editor_background: Option<&'static str>, // Store as an owned String
    pub source_style: Option<ThemeTrieRule<'static>>,
    pub theme_trie: ThemeTrie<'static>,
    pub semantic_theme_selector: Option<SemanticThemeSelector<'static>>,
}

fn is_valid_hex_color(hex: &str) -> bool {
    let hex = hex.trim();

    if hex.len() == 7 && hex.starts_with('#') {
        // #rrggbb
        return hex[1..].chars().all(|c| c.is_ascii_hexdigit());
    }

    if hex.len() == 9 && hex.starts_with('#') {
        // #rrggbbaa
        return hex[1..].chars().all(|c| c.is_ascii_hexdigit());
    }

    if hex.len() == 4 && hex.starts_with('#') {
        // #rgb
        return hex[1..].chars().all(|c| c.is_ascii_hexdigit());
    }

    if hex.len() == 5 && hex.starts_with('#') {
        // #rgba
        return hex[1..].chars().all(|c| c.is_ascii_hexdigit());
    }

    false
}

fn from_font_style_booleans(
    bold: Option<bool>,
    italic: Option<bool>,
    underline: Option<bool>,
    strikethrough: Option<bool>,
    foreground: Option<usize>,
    background: Option<usize>,
) -> (u8, Option<usize>, Option<usize>) {
    let mut font_style_flags = 0;

    if bold.unwrap_or(false) {
        font_style_flags |= FontStyle::Bold as u8;
    }
    if italic.unwrap_or(false) {
        font_style_flags |= FontStyle::Italic as u8;
    }
    if underline.unwrap_or(false) {
        font_style_flags |= FontStyle::Underline as u8;
    }
    if strikethrough.unwrap_or(false) {
        font_style_flags |= FontStyle::Strikethrough as u8;
    }

    let font_style = if font_style_flags == 0 {
        FontStyle::NotSet as u8
    } else {
        font_style_flags
    };

    (font_style, foreground, background)
}

fn parse_semantic_token_scope(scope: &str) -> SemanticTokenScope {
    let mut token_type: Option<String> = None;
    let mut modifiers: i16 = 0;

    let mut parts = scope.split(':');
    let main_part = parts.next().unwrap();
    let language = parts.next().map(|s| s.to_string());

    let mut main_parts = main_part.split('.');
    if let Some(first_part) = main_parts.next() {
        if first_part != "*" {
            token_type = Some(first_part.to_string());
        }
    }

    for modifier in main_parts {
        match modifier {
            "declaration" => modifiers |= SemanticTokenModifier::Declaration as i16,
            "definition" => modifiers |= SemanticTokenModifier::Definition as i16,
            "readonly" => modifiers |= SemanticTokenModifier::Readonly as i16,
            "static" => modifiers |= SemanticTokenModifier::Static as i16,
            "deprecated" => modifiers |= SemanticTokenModifier::Deprecated as i16,
            "abstract" => modifiers |= SemanticTokenModifier::Abstract as i16,
            "async" => modifiers |= SemanticTokenModifier::Async as i16,
            "modification" => modifiers |= SemanticTokenModifier::Modification as i16,
            "documentation" => modifiers |= SemanticTokenModifier::Documentation as i16,
            "defaultLibrary" => modifiers |= SemanticTokenModifier::DefaultLibrary as i16,
            _ => {}
        }
    }

    SemanticTokenScope {
        token_type,
        modifiers,
        language,
    }
}

pub fn parse_theme(theme_json: &'static serde_json::Value, label: &str) -> Option<ParsedTheme> {
    let editor_background = theme_json
        .get("colors")
        .and_then(|value| extract_string!(value, "editor.background"))
        .or_else(|| {
            theme_json
                .get("colors")
                .and_then(|value| extract_string!(value, "background"))
        });
    let editor_foreground = theme_json
        .get("colors")
        .and_then(|value| extract_string!(value, "editor.foreground"))
        .or_else(|| {
            theme_json
                .get("colors")
                .and_then(|value| extract_string!(value, "foreground"))
        });

    let mut parsed_theme_rules: Vec<ParsedThemeRule> = Vec::new();
    if let Some(token_colors_array) = extract_value!(&theme_json, "tokenColors", as_array) {
        for token_color in token_colors_array {
            if let Some(settings) = token_color.get("settings") {
                let font_style = parse_font_style(extract_string!(settings, "fontStyle"));
                let foreground = extract_string!(settings, "foreground")
                    .filter(|color| is_valid_hex_color(color));
                let background = extract_string!(settings, "background")
                    .filter(|color| is_valid_hex_color(color));

                if let Some(scope_value) = token_color.get("scope") {
                    let mut scopes = Vec::new();

                    if let Some(scope_str) = scope_value.as_str() {
                        scopes.extend(process_scope_string(scope_str));
                    } else if let Some(scope_array) = scope_value.as_array() {
                        for scope_item in scope_array {
                            if let Some(scope_str) = scope_item.as_str() {
                                scopes.extend(process_scope_string(scope_str));
                            }
                        }
                    }

                    for scope in &scopes {
                        let segments: Vec<&str> = scope.split_whitespace().collect();

                        if segments.is_empty() {
                            continue;
                        }

                        let (scope, parent_scopes) = process_scopes(scope);

                        if let None = scope {
                            continue;
                        }

                        parsed_theme_rules.push(ParsedThemeRule {
                            scope: scope.unwrap(),
                            parent_scopes,
                            index: parsed_theme_rules.len(),
                            font_style,
                            foreground,
                            background,
                        })
                    }
                }
            }
        }
    }

    parsed_theme_rules.sort_by(|a, b| {
        let scope_cmp = a.scope.cmp(&b.scope);
        if scope_cmp != std::cmp::Ordering::Equal {
            return scope_cmp;
        }
        let parent_scopes_cmp = a.parent_scopes.cmp(&b.parent_scopes);
        if parent_scopes_cmp != std::cmp::Ordering::Equal {
            return parent_scopes_cmp;
        }
        a.index.cmp(&b.index)
    });

    // Determine defaults
    let mut default_font_style = FontStyle::None as u8;
    let mut default_foreground = editor_foreground.unwrap_or("#ffffff");
    let mut default_background = editor_background.unwrap_or("#000000");

    let mut test_index = 0 as usize;

    while test_index <= parsed_theme_rules.len() && parsed_theme_rules[test_index].scope.is_empty()
    {
        let incoming_defaults = &parsed_theme_rules[test_index];
        test_index += 1;

        if incoming_defaults.font_style != FontStyle::NotSet as u8 {
            println!(
                "New default font style for [{}]: {}",
                label, incoming_defaults.font_style
            );
            default_font_style = incoming_defaults.font_style;
        }
        if let Some(foreground) = incoming_defaults.foreground {
            default_foreground = foreground;
        }
        if let Some(background) = incoming_defaults.background {
            default_background = background;
        }
    }

    let mut root = ThemeTrieNode::new(
        ThemeTrieRule {
            scope: "",
            scope_depth: 0,
            parent_scopes: None,
            font_style: default_font_style,
            foreground: Some(default_foreground),
            background: Some(default_background),
        },
        Vec::new(),
    );

    for i in test_index..parsed_theme_rules.len() {
        let rule = &parsed_theme_rules[i];
        root.insert(
            0,
            rule.scope,
            rule.scope,
            rule.parent_scopes.clone(),
            rule.font_style,
            rule.foreground,
            rule.background,
        );
    }

    let theme_trie = ThemeTrie {
        root,
        default_foreground,
        default_background,
    };

    let semantic_token_colors = &theme_json
        .get("semanticTokenColors")
        .and_then(|value| value.as_object());

    let semantic_theme_selector = if semantic_token_colors.is_some()
        && theme_json
            .get("semanticHighlighting")
            .and_then(|value| value.as_bool())
            .unwrap_or(false)
    {
        let mut semantic_theme_selector = SemanticThemeSelector::new();

        for (scope_pattern, settings) in semantic_token_colors.unwrap() {
            let (font_style, foreground) = match settings.as_str() {
                Some(color) => (None, Some(color)),
                None => match settings.as_object() {
                    Some(settings) => (
                        extract_string!(settings, "fontStyle")
                            .and_then(|fs| Some(parse_font_style(Some(fs)))),
                        extract_string!(settings, "foreground"),
                    ),
                    None => continue,
                },
            };

            let token_scope = parse_semantic_token_scope(scope_pattern);

            semantic_theme_selector.add_rule(token_scope, font_style.unwrap_or(0), foreground);
        }

        Some(semantic_theme_selector)
    } else {
        None
    };

    let source_style = Some(ThemeTrieRule {
        scope: "",
        scope_depth: 0,
        font_style: 0,
        foreground: Some(default_foreground),
        background: Some(default_background),
        parent_scopes: None,
    });

    Some(ParsedTheme {
        label: label.to_string(),
        theme_json,
        editor_background,
        source_style,
        theme_trie,
        semantic_theme_selector,
    })
}
