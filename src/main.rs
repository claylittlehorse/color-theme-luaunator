use serde::Deserialize;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Deserialize, Debug)]
struct Location {
    path: String,
}

#[derive(Deserialize, Debug)]
struct Extension {
    location: Location,
}

#[derive(Deserialize, Debug)]
struct Package {
    contributes: Contributes,
}

#[derive(Deserialize, Debug)]
struct Contributes {
    themes: Option<Vec<ThemeEntry>>,
}

#[derive(Deserialize, Debug, Clone)]
struct ThemeEntry {
    label: String,
    path: String,
}

fn normalize_path(raw_path: &str) -> PathBuf {
    #[cfg(target_os = "windows")]
    {
        let path = raw_path.replace("/", "\\"); // Replace forward slashes with backslashes
        match path.strip_prefix("\\") {
            Some(path) => PathBuf::from(path),
            None => PathBuf::from(path),
        }
    }

    // On macOS/Linux, use the raw path as-is
    #[cfg(any(target_os = "macos", target_os = "linux"))]
    {
        if raw_path.starts_with("~") {
            // Expand `~` to the user's home directory
            let expanded = shellexpand::tilde(raw_path);
            PathBuf::from(expanded.into_owned())
        } else {
            PathBuf::from(raw_path)
        }
    }
}

fn get_stdin_filepath_with_reprompt_backup() -> String {
    let mut input = String::new();
    if let Err(e) = std::io::stdin().read_line(&mut input) {
        eprintln!(
            "Failed to read input: '{}'. Please provide the path to your .vscode/extensions/extensions.json file:",
            e
        );
        return get_stdin_filepath_with_reprompt_backup();
    };
    input
}

fn read_extensions_json_with_filepath_prompt_backup(file_path: &PathBuf) -> Vec<Extension> {
    match fs::read_to_string(file_path) {
        Ok(json_content) => {
            serde_json::from_str(&json_content).expect("Failed to parse extensions.json")
        }
        Err(e) => {
            eprintln!(
                "Failed to read extensions.json: '{}'. Please provide the path to your .vscode/extensions/extensions.json file:",
                e
            );
            let input = get_stdin_filepath_with_reprompt_backup();
            let input_path = PathBuf::from(input.trim());
            read_extensions_json_with_filepath_prompt_backup(&input_path)
        }
    }
}

fn read_theme_entries_from_json(extension_path: &Path) -> Option<Vec<ThemeEntry>> {
    let package_json_path = extension_path.join("package.json");
    match fs::read_to_string(&package_json_path) {
        Ok(content) => {
            let package: Package =
                serde_json::from_str(&content).expect("Failed to parse extension package.json");
            package.contributes.themes
        }
        Err(e) => {
            eprintln!(
                "Failed to read package.json in {}: '{}'",
                extension_path.display(),
                e
            );
            None
        }
    }
}

enum InputResult {
    Filter(Option<String>),
    ThemeIndex(usize),
}

fn prompt_theme_select_input_with_reprompt_backup(themes_length: usize) -> InputResult {
    let mut input = String::new();
    if let Err(e) = std::io::stdin().read_line(&mut input) {
        eprintln!("Error reading input: '{}'. Try again:", e);
        return prompt_theme_select_input_with_reprompt_backup(themes_length);
    }

    let input = input.trim();

    // Ensure "filter" is followed by a space
    if let Some(new_filter) = input.strip_prefix("filter ") {
        let new_filter = new_filter.trim().to_lowercase(); // Trim and make case-insensitive
        let new_filter = if new_filter.is_empty() {
            None
        } else {
            Some(new_filter)
        };

        InputResult::Filter(new_filter)
    } else if let Ok(index) = input.parse::<usize>() {
        if index > 0 && index <= themes_length {
            InputResult::ThemeIndex(index - 1)
        } else {
            println!(
                "Number out of range. Input number between 1 - {}",
                themes_length
            );
            prompt_theme_select_input_with_reprompt_backup(themes_length)
        }
    } else {
        println!(
            "Invalid input. Input [1 - {}] OR `filter [theme name]`.",
            themes_length
        );
        prompt_theme_select_input_with_reprompt_backup(themes_length)
    }
}

fn select_theme(theme_entries: Vec<ThemeEntry>, filter: Option<String>) -> ThemeEntry {
    let filtered_theme_entries: Vec<ThemeEntry> = if let Some(filter_str) = &filter {
        theme_entries
            .iter()
            .filter(|theme| {
                theme
                    .label
                    .to_lowercase()
                    .contains(&filter_str.to_lowercase())
            })
            .cloned()
            .collect()
    } else {
        theme_entries.iter().cloned().collect()
    };

    if filtered_theme_entries.is_empty() {
        println!("No themes match provided filter string. Press enter to continue...");
        let mut input = String::new();
        let _ = std::io::stdin().read_line(&mut input);
        return select_theme(theme_entries, None);
    } else {
        for (theme_index, theme) in filtered_theme_entries.iter().enumerate() {
            println!("{}: {}", theme_index + 1, theme.label);
        }
        println!("== ----- ==");
        println!(
            "Input number [1 - {}] OR `filter [theme name]`",
            filtered_theme_entries.len()
        )
    }

    match prompt_theme_select_input_with_reprompt_backup(filtered_theme_entries.len()) {
        InputResult::Filter(filter_string) => select_theme(theme_entries, filter_string),
        InputResult::ThemeIndex(theme_index) => filtered_theme_entries[theme_index].clone(),
    }
}

fn hex_to_rgba(hex: &str) -> Option<(u8, u8, u8, Option<u8>)> {
    let hex = hex.trim_start_matches('#');
    match hex.len() {
        6 => {
            let r = u8::from_str_radix(&hex[0..2], 16).ok()?;
            let g = u8::from_str_radix(&hex[2..4], 16).ok()?;
            let b = u8::from_str_radix(&hex[4..6], 16).ok()?;
            Some((r, g, b, None)) // No alpha channel
        }
        8 => {
            let r = u8::from_str_radix(&hex[0..2], 16).ok()?;
            let g = u8::from_str_radix(&hex[2..4], 16).ok()?;
            let b = u8::from_str_radix(&hex[4..6], 16).ok()?;
            let a = u8::from_str_radix(&hex[6..8], 16).ok()?;
            Some((r, g, b, Some(a))) // Alpha channel present
        }
        _ => None, // Invalid hex length
    }
}

fn hex_to_rgb_string(hex: &str) -> String {
    if let Some((r, g, b, _)) = hex_to_rgba(hex) {
        format!("{};{};{}", r, g, b) // Ignore alpha for terminal colors
    } else {
        "255;255;255".to_string() // Default to white if invalid hex
    }
}

fn hex_to_hsb(hex: &str) -> Option<(f32, f32, f32)> {
    // Use `hex_to_rgba` to handle both 6-digit and 8-digit hex codes
    let (r, g, b, _) = hex_to_rgba(hex)?; // Ignore the alpha channel

    let r = r as f32 / 255.0;
    let g = g as f32 / 255.0;
    let b = b as f32 / 255.0;

    let max = r.max(g).max(b);
    let min = r.min(g).min(b);
    let delta = max - min;

    // Calculate Hue
    let h = if delta == 0.0 {
        0.0
    } else if max == r {
        60.0 * (((g - b) / delta) % 6.0)
    } else if max == g {
        60.0 * (((b - r) / delta) + 2.0)
    } else {
        60.0 * (((r - g) / delta) + 4.0)
    };

    let h = if h < 0.0 { h + 360.0 } else { h };
    let s = if max == 0.0 { 0.0 } else { delta / max };
    let b = max;

    Some((h, s * 100.0, b * 100.0))
}

fn label_color_by_hue_sat(hue: f32, sat: f32) -> &'static str {
    // Return "Neutral" if saturation is too low
    if sat < 30.0 {
        return "Neutral";
    }

    // Round the hue to the nearest 15-degree increment
    let rounded_hue = ((hue / 15.0).round() * 15.0) % 360.0;

    match rounded_hue as u32 {
        0 => "Red",
        15 => "Red-Orange",
        30 => "Orange",
        45 => "Yellow-Orange",
        60 => "Yellow",
        75 => "Yellow-Lime",
        90 => "Lime",
        105 => "Green-Lime",
        120 => "Green",
        135 => "Spring-Green",
        150 => "Cyan-Green",
        165 => "Cyan-Blue",
        180 => "Cyan",
        195 => "Azure-Cyan",
        210 => "Azure",
        225 => "Blue-Azure",
        240 => "Blue",
        255 => "Violet-Blue",
        270 => "Violet",
        285 => "Magenta-Violet",
        300 => "Magenta",
        315 => "Rose-Magenta",
        330 => "Rose",
        345 => "Red-Rose",
        _ => "Unknown", // Fallback case (shouldn't happen with valid hue values)
    }
}

fn get_hue_by_label(label: &str) -> f32 {
    match label {
        "Red" => 0.0,
        "Red-Orange" => 15.0,
        "Orange" => 30.0,
        "Yellow-Orange" => 45.0,
        "Yellow" => 60.0,
        "Yellow-Lime" => 75.0,
        "Lime" => 90.0,
        "Green-Lime" => 105.0,
        "Green" => 120.0,
        "Spring-Green" => 135.0,
        "Cyan-Green" => 150.0,
        "Cyan-Blue" => 165.0,
        "Cyan" => 180.0,
        "Azure-Cyan" => 195.0,
        "Azure" => 210.0,
        "Blue-Azure" => 225.0,
        "Blue" => 240.0,
        "Violet-Blue" => 255.0,
        "Violet" => 270.0,
        "Magenta-Violet" => 285.0,
        "Magenta" => 300.0,
        "Rose-Magenta" => 315.0,
        "Rose" => 330.0,
        "Red-Rose" => 345.0,
        "Neutral" => 360.0,
        "Misc" => 361.0,
        _ => 362.0,
    }
}

fn main() {
    let file_path = if cfg!(target_os = "windows") {
        PathBuf::from(format!(
            "{}\\.vscode\\extensions\\extensions.json",
            std::env::var("USERPROFILE").unwrap()
        ))
    } else if cfg!(target_os = "macos") || cfg!(target_os = "linux") {
        normalize_path("~/.vscode/extensions/extensions.json")
    } else {
        panic!("Unsupported platform");
    };

    let extensions = read_extensions_json_with_filepath_prompt_backup(&file_path);

    println!(
        "Found '.vscode/extensions/extensions.json'. Parsing themes from extensions' package.json."
    );

    let mut all_theme_entries: Vec<ThemeEntry> = Vec::new();

    for extension in extensions {
        let extension_path = normalize_path(&extension.location.path);
        let theme_entries = read_theme_entries_from_json(&extension_path);

        if let Some(theme_entries) = theme_entries {
            for mut theme_entry in theme_entries {
                let absolute_path = extension_path.join(normalize_path(&theme_entry.path));
                theme_entry.path = absolute_path.to_string_lossy().to_string();

                all_theme_entries.push(theme_entry);
            }
        }
    }

    let theme_entry = select_theme(all_theme_entries, None);

    match fs::read_to_string(&theme_entry.path) {
        Err(e) => {
            eprintln!("Failed to read `{}`: {}", theme_entry.path, e);
            std::process::exit(0);
        }
        Ok(theme_json) => {
            let theme: Theme = json5::from_str(&theme_json).expect("Failed to parse JSON");

            let background_color = theme.colors.get("editor.background");

            // HashMap to track unique settings and their counts

            // Iterate over token colors and count unique settings

            // Print the sorted categories and their settings
            for (category, style_settings) in sorted_categories {
                for (index, style_setting) in style_settings.iter().enumerate() {
                    let color_name = if style_settings.len() > 1 {
                        format!("{}{}", category, index + 1)
                    } else {
                        category.to_string()
                    };

                    let mut style_codes = Vec::new();
                    let mut style_names = Vec::new();

                    for style in &style_setting.font_style {
                        match style.as_str() {
                            "bold" => {
                                style_codes.push("1");
                                style_names.push("bold");
                            }
                            "italic" => {
                                style_codes.push("3");
                                style_names.push("italic");
                            }
                            "underline" => {
                                style_codes.push("4");
                                style_names.push("underline");
                            }
                            "strikethrough" => {
                                style_codes.push("9");
                                style_names.push("strikethrough");
                            }
                            _ => {}
                        }
                    }

                    let mut is_any_ansi = false;

                    let (style_ansi, style_text) = if !style_codes.is_empty() {
                        is_any_ansi = true;
                        (
                            style_codes.join(";") + ";",
                            format!(" {}", style_names.join(" ")),
                        )
                    } else {
                        (String::new(), String::new())
                    };

                    let (foreground_ansi, foreground_text, alpha_text) =
                        if let Some(foreground) = &style_setting.foreground {
                            is_any_ansi = true;
                            (
                                format!("38;2;{}", hex_to_rgb_string(&foreground)),
                                foreground.clone(),
                                if let Some((_, _, _, Some(alpha))) = hex_to_rgba(foreground) {
                                    format!(" alpha: {}", alpha)
                                } else {
                                    String::new()
                                },
                            )
                        } else {
                            (String::new(), String::new(), String::new())
                        };

                    let background_text = if let Some(background) = &style_setting.background {
                        format!(" background: {}", background)
                    } else {
                        String::new()
                    };

                    let background_ansi = if let Some(bg) = &style_setting.background {
                        let bga = format!(
                            "{}48;2;{}",
                            if is_any_ansi { ";" } else { "" },
                            hex_to_rgb_string(&bg)
                        );
                        is_any_ansi = true;
                        bga
                    } else if let Some(bg) = background_color {
                        let bga = format!(
                            "{}48;2;{}",
                            if is_any_ansi { ";" } else { "" },
                            hex_to_rgb_string(&bg)
                        );
                        is_any_ansi = true;
                        bga
                    } else {
                        String::new()
                    };

                    let (ansi_format_prefix, ansi_format_suffix) = if is_any_ansi {
                        (
                            format!("\x1b[{}{}{}m", style_ansi, foreground_ansi, background_ansi),
                            "\x1b[0m".to_string(),
                        )
                    } else {
                        (String::new(), String::new())
                    };

                    print!(
                        "{}{} ({}{}{}{}){} ",
                        ansi_format_prefix,
                        color_name,
                        foreground_text,
                        background_text,
                        style_text,
                        alpha_text,
                        ansi_format_suffix
                    )
                }

                println!();
            }
        }
    }
}

#[derive(PartialEq, Eq, Hash)]
struct StyleSettings {
    foreground: Option<usize>,
    background: Option<usize>,
    font_style: u8,
}

struct ParsedTheme {
    theme_json: serde_json::Value,
    background_color_idx: Option<usize>,
    source_style: Option<StyleSettings>,
    theme_trie: ThemeTrie,
    semantic_theme_trie: Option<ThemeTrie>,
}

macro_rules! extract_value {
    ($json:expr, $key:expr, $method:ident) => {
        $json.get($key).and_then(|value| value.$method())
    };
}

macro_rules! extract_string {
    ($json:expr, $key:expr) => {
        $json
            .get($key)
            .and_then(|value| value.as_str())
            .map(|s| s.to_string())
    };
}

fn parse_theme(theme_entry: &ThemeEntry) -> Option<ParsedTheme> {
    match fs::read_to_string(&theme_entry.path) {
        Err(e) => {
            eprintln!(
                "Failed to load `{}` for theme `{}`: {}",
                theme_entry.path, theme_entry.label, e
            );
            None
        }
        Ok(theme_json) => {
            // Parse with json5 in case someone put comments in their theme. (I'm someone)
            let theme_json: serde_json::Value =
                json5::from_str(&theme_json).expect("Failed to parse JSON");

            let mut color_map = Vec::new();
            let mut color_hash = HashMap::new();

            let background_color_idx = theme_json.get("colors").and_then(|value| {
                extract_string!(value, "editor.background")
                    .map(|color| get_or_insert_color(&color, &mut color_map, &mut color_hash))
            });

            let token_colors_array = extract_value!(&theme_json, "token_colors", as_array);

            let mut theme_trie = ThemeTrie::new();
            if let Some(token_colors_array) = token_colors_array {
                for token_color in token_colors_array {
                    if let Some(settings) = token_color.get("settings") {
                        let font_style =
                            extract_string!(settings, "fontStyle").unwrap_or("".to_string());

                        let foreground_index =
                            extract_string!(settings, "foreground").map(|color| {
                                get_or_insert_color(&color, &mut color_map, &mut color_hash)
                            });
                        let background_index =
                            extract_string!(settings, "background").map(|color| {
                                get_or_insert_color(&color, &mut color_map, &mut color_hash)
                            });

                        if let Some(scope_value) = token_color.get("scope") {
                            let scope_patterns = parse_scope_patterns(scope_value);

                            for scope_pattern in scope_patterns {
                                let segments: Vec<&str> =
                                    scope_pattern.split_whitespace().collect();

                                if let Some((scope, parent_scopes)) =
                                    extract_scope_and_parents(&segments)
                                {
                                    theme_trie.insert(
                                        scope,
                                        parent_scopes,
                                        &font_style,
                                        foreground_index,
                                        background_index,
                                    );
                                }
                            }
                        }
                    }
                }
            } else {
                return None;
            }

            let semantic_token_colors = theme_json
                .get("semanticTokenColors")
                .and_then(|value| value.as_object());

            let semantic_theme_trie = if semantic_token_colors.is_some()
                && theme_json
                    .get("semanticHighlighting")
                    .and_then(|value| value.as_bool())
                    .unwrap_or(false)
            {
                let mut semantic_theme_trie = ThemeTrie::new();

                for (scope_pattern, settings) in semantic_token_colors.unwrap() {
                    let font_style =
                        extract_string!(settings, "fontStyle").unwrap_or("".to_string());

                    let foreground_index = extract_string!(settings, "foreground")
                        .map(|color| get_or_insert_color(&color, &mut color_map, &mut color_hash));
                    let background_index = extract_string!(settings, "background")
                        .map(|color| get_or_insert_color(&color, &mut color_map, &mut color_hash));

                    let segments: Vec<&str> = scope_pattern.split_whitespace().collect();

                    if let Some((scope, parent_scopes)) = extract_scope_and_parents(&segments) {
                        semantic_theme_trie.insert(
                            scope,
                            parent_scopes,
                            &font_style,
                            foreground_index,
                            background_index,
                        );
                    }
                }

                Some(semantic_theme_trie)
            } else {
                None
            };

            theme_trie.match_scope("source");

            Some(ParsedTheme {
                theme_json,
                background_color_idx,
                source_style: None,
                theme_trie,
                semantic_theme_trie,
            })
        }
    }
}

fn parse_scope_patterns(scope_value: &serde_json::Value) -> Vec<String> {
    let mut scope_patterns = Vec::new();

    if let Some(scope_str) = scope_value.as_str() {
        scope_patterns.extend(
            scope_str
                .split(',')
                .map(|s| s.trim().to_string())
                .filter(|s| !s.is_empty()),
        );
    } else if let Some(scope_array) = scope_value.as_array() {
        for scope_item in scope_array {
            if let Some(scope_str) = scope_item.as_str() {
                scope_patterns.extend(
                    scope_str
                        .split(',')
                        .map(|s| s.trim().to_string())
                        .filter(|s| !s.is_empty()),
                );
            }
        }
    }

    scope_patterns
}

fn extract_scope_and_parents<'a>(segments: &'a [&'a str]) -> Option<(&'a str, Vec<String>)> {
    if segments.is_empty() {
        return None;
    }

    let scope = segments.last().unwrap();
    let parent_scopes = if segments.len() > 1 {
        segments[..segments.len() - 1]
            .iter()
            .rev()
            .map(|s| s.to_string())
            .collect()
    } else {
        Vec::new()
    };

    Some((scope, parent_scopes))
}

#[derive(Debug, Clone)]
struct ThemeTrieRule {
    scope_depth: usize,
    font_style: u8,
    foreground: Option<usize>,
    background: Option<usize>,
    parent_scopes: Vec<String>,
}

fn scope_path_matches_parent_scopes(scope_path: &[&str], parent_scopes: &[String]) -> bool {
    if parent_scopes.is_empty() {
        return true;
    }

    let mut scope_path_index = scope_path.len();
    let mut parent_index = parent_scopes.len();

    while parent_index > 0 {
        parent_index -= 1;
        let mut parent_scope = &parent_scopes[parent_index];
        let mut strict_match = false;

        if parent_scope == ">" {
            if parent_index == 0 {
                return false;
            }
            parent_index -= 1;
            parent_scope = &parent_scopes[parent_index];
            strict_match = true;
        }

        while scope_path_index > 0 {
            scope_path_index -= 1;
            if scope_path[scope_path_index] == parent_scope {
                break;
            }
            if strict_match {
                return false;
            }
        }

        if scope_path_index == 0 {
            return false;
        }
    }

    true
}

#[derive(Debug)]
struct ThemeTrieNode {
    main_rule: ThemeTrieRule,
    rules_with_parent_scopes: Vec<ThemeTrieRule>,
    children: HashMap<String, ThemeTrieNode>,
}

impl ThemeTrieNode {
    fn new() -> Self {
        ThemeTrieNode {
            main_rule: ThemeTrieRule {
                scope_depth: 0,
                font_style: 0,
                foreground: None,
                background: None,
                parent_scopes: Vec::new(),
            },
            rules_with_parent_scopes: Vec::new(),
            children: HashMap::new(),
        }
    }

    fn insert(
        &mut self,
        scope: &str,
        parent_scopes: Vec<String>,
        font_style: &str,
        foreground_index: Option<usize>,
        background_index: Option<usize>,
        depth: usize,
    ) {
        if scope.is_empty() {
            self.insert_here(
                depth,
                parent_scopes,
                font_style,
                foreground_index,
                background_index,
            );
            return;
        }

        let (head, tail) = if let Some(dot_index) = scope.find('.') {
            (&scope[..dot_index], &scope[dot_index + 1..])
        } else {
            (scope, "")
        };

        let child = self
            .children
            .entry(head.to_string())
            .or_insert_with(ThemeTrieNode::new);

        child.insert(
            tail,
            parent_scopes,
            font_style,
            foreground_index,
            background_index,
            depth + 1,
        );
    }

    fn insert_here(
        &mut self,
        scope_depth: usize,
        parent_scopes: Vec<String>,
        font_style: &str,
        foreground_index: Option<usize>,
        background_index: Option<usize>,
    ) {
        let font_style_flags = parse_font_style(font_style);

        if parent_scopes.is_empty() {
            self.main_rule = ThemeTrieRule {
                scope_depth,
                font_style: font_style_flags,
                foreground: foreground_index,
                background: background_index,
                parent_scopes: Vec::new(),
            };
        } else {
            for rule in &mut self.rules_with_parent_scopes {
                if rule.parent_scopes == parent_scopes {
                    rule.scope_depth = scope_depth;
                    rule.font_style |= font_style_flags;
                    if foreground_index.is_some() {
                        rule.foreground = foreground_index;
                    }
                    if background_index.is_some() {
                        rule.background = background_index;
                    }
                    return;
                }
            }

            self.rules_with_parent_scopes.push(ThemeTrieRule {
                scope_depth,
                font_style: font_style_flags,
                foreground: foreground_index,
                background: background_index,
                parent_scopes,
            });
        }
    }

    fn match_scope(&self, scope: &str) -> Vec<ThemeTrieRule> {
        if !scope.is_empty() {
            if let Some(dot_index) = scope.find('.') {
                let head = &scope[..dot_index];
                let tail = &scope[dot_index + 1..];

                if let Some(child) = self.children.get(head) {
                    return child.match_scope(tail);
                }
            } else {
                if let Some(child) = self.children.get(scope) {
                    return child.match_scope("");
                }
            }
        }

        let mut rules = self.rules_with_parent_scopes.clone();
        rules.push(self.main_rule.clone());

        rules.sort_by(|a, b| ThemeTrieNode::compare_rules(a, b));
        rules
    }

    fn compare_rules(a: &ThemeTrieRule, b: &ThemeTrieRule) -> std::cmp::Ordering {
        if a.scope_depth != b.scope_depth {
            return b.scope_depth.cmp(&a.scope_depth);
        }

        let mut a_parent_index = 0;
        let mut b_parent_index = 0;

        while a_parent_index < a.parent_scopes.len() && b_parent_index < b.parent_scopes.len() {
            // Child combinators don't affect specificity.
            if a.parent_scopes[a_parent_index] == ">" {
                a_parent_index += 1;
                continue;
            }
            if b.parent_scopes[b_parent_index] == ">" {
                b_parent_index += 1;
                continue;
            }

            let parent_scope_length_diff = b.parent_scopes[b_parent_index]
                .len()
                .cmp(&a.parent_scopes[a_parent_index].len());
            if parent_scope_length_diff != std::cmp::Ordering::Equal {
                return parent_scope_length_diff;
            }

            a_parent_index += 1;
            b_parent_index += 1;
        }

        b.parent_scopes.len().cmp(&a.parent_scopes.len())
    }
}

fn parse_font_style(font_style: &str) -> u8 {
    let mut flags = 0;
    for style in font_style.split_whitespace() {
        match style {
            "bold" => flags |= 0b00000001,
            "italic" => flags |= 0b00000010,
            "underline" => flags |= 0b00000100,
            "strikethrough" => flags |= 0b00001000,
            _ => {}
        }
    }
    flags
}

fn get_or_insert_color(
    color: &str,
    color_map: &mut Vec<String>,
    color_hash: &mut HashMap<String, usize>,
) -> usize {
    if let Some(&index) = color_hash.get(color) {
        index
    } else {
        let index = color_map.len();
        color_map.push(color.to_string());
        color_hash.insert(color.to_string(), index);
        index
    }
}

#[derive(Debug)]
struct ThemeTrie {
    root: ThemeTrieNode,
}

impl ThemeTrie {
    fn new() -> Self {
        ThemeTrie {
            root: ThemeTrieNode::new(),
        }
    }

    fn insert(
        &mut self,
        scope: &str,
        parent_scopes: Vec<String>,
        font_style: &str,
        foreground_index: Option<usize>,
        background_index: Option<usize>,
    ) {
        self.root.insert(
            scope,
            parent_scopes,
            font_style,
            foreground_index,
            background_index,
            0,
        );
    }

    fn match_scope(&self, scope_path: &str) -> Option<ThemeTrieRule> {
        let segments: Vec<&str> = scope_path.split('.').collect();
        let final_scope = segments.last().unwrap_or(&"");

        let matching_rules = self.root.match_scope(final_scope);

        let matching_rule = matching_rules.into_iter().find(|rule| {
            let reversed_parent_scopes: Vec<String> =
                rule.parent_scopes.iter().rev().cloned().collect();
            scope_path_matches_parent_scopes(&segments, &reversed_parent_scopes)
        });

        matching_rule
    }
}

// let mut semantic_theme_trie = if ectract_value!(settings, "semanticHighlighting", as)
//     let mut settings_occurences: HashMap<Settings, usize> = HashMap::new();

//     let mut style_hash: HashMap<&'static str, Vec<StyleSetting>> = HashMap::new();
//     for (settings, setting_info) in &settings_hash {
//         let label = match &settings.foreground {
//             Some(foreground) => match hex_to_hsb(foreground) {
//                 Some((hue, sat, _)) => label_color_by_hue_sat(hue, sat),
//                 None => "Misc",
//             },
//             None => "Misc",
//         };

//         let entry = style_hash.entry(label).or_insert_with(Vec::new);

//         entry.push(StyleSetting {
//             occurences: setting_info.occurences,
//             foreground: settings.foreground.clone(),
//             background: settings.background.clone(),
//             font_style: settings.font_style.clone(),
//         });
//     }

//     for (_, style_settings) in style_hash.iter_mut() {
//         style_settings.sort_by(|a, b| b.occurences.cmp(&a.occurences));
//     }

//     // Sort the categories by their hue
//     let mut sorted_categories: Vec<(&str, &Vec<StyleSetting>)> = style_hash
//         .iter()
//         .map(|(&key, value)| (key, value))
//         .collect();

//     sorted_categories.sort_by(|(category_a, _), (category_b, _)| {
//         let hue_a = get_hue_by_label(&category_a);
//         let hue_b = get_hue_by_label(&category_b);
//         hue_a
//             .partial_cmp(&hue_b)
//             .unwrap_or(std::cmp::Ordering::Equal)
//     });

//     None
// } else {
//     None
// }

// [function definition]
// meta.function
// >> punctuation.definition.parameters.begin
// >> punctuation.definition.parameters.end
// >> meta.parameter
// >> >> variable.parameter.function.varargs
// >> >> variable.parameter.function
// >> >> punctuation.separator.arguments.luau
// >> variable.language.metamethod
// >> entity.name.function

// [local declaration]
// storage.modifier.local
// >> variable.other.constant.luau (if LOUD_SNAKE_CASE)
// >> variable.other.readwrite

// [for loop]
// >> (for) keyword.control
// >> (in) keyword.control
// >> (=) keyword.operator.assignment

// [shebang]
// (#!) punctuation.definition.comment.luau
// comment.line.shebang

// [string_escape]
// constant.character.escape

// [number]
// constant.numeric.hex
// constant.numeric.binary
// constant.numeric.decimal

// [string]
// string.quoted.double
// string.quoted.single
// string.other.multiline
// string.interpolated

// [interpolated string expression]
// meta.template.expression
// punctuation.definition.interpolated-string-expression.begin
// punctuation.definition.interpolated-string-expression.end
// >> meta.embedded.line.luau

// [standard library]
// support.function (assert, error, type, next ...)
// constant.language (_G, _VERSION)
// support.function (bit32.etc, coroutine.etc, debug.etc, table.etc)
// support.function (delay, DebuggerManager, spawn, tick, wait, etc)
// constant.language (game, plugin, script, workspace, Enum.(etc))

// [keyword]
// keyword.control.luau (break, do, else, for, if, elseif, return, then, repeat, while, until, end, in, continue)
// storage.modifier.local (local)
// keyword.control.lua (function)
// variable.language.self (self)
// keyword.operator.logical, keyword.operator.wordlike (and, or, not)
// variable.language.metamethod
// keyword.other.unit (...)

// [identifier]
// entity.name.function (function calls followed by strings and tables)
// variable.other.property
// variable.other.constant
// variaible.other.readwrite

// [operator]
// keyword.operator.comparison
// keyword.operator.assignment
// keyword.operator.arithmetic
// keyword.operator.other

// [parentheses]
// punctuation.arguments.begin
// punctuation.arguments.end
// punctuation.separator.arguments

// [table]
// punctuation.table.begin
// punctuation.table.end
// punctuation.separator.fields

// [language constant]
// constant.language.boolean.false
// constant.language.boolean.true
// constant.language.nil

// [comment]
// keyword.operator.other (```luau) (```)
// comment
// comment.line.double-dash.documentation
// comment.line.double-dash

// [doc comment tags]
// storage.type.class.luadoc
// variable.parameter

// [generics declaration]
// entity.name.type (generic)
// keyword.operator.assignment (=)

// [type alias declaration]
// storage.modifier.visibility (export)
// storage.type (type keyword)

// [type cast]
// keyword.operator.typecast

// [type literal]
// keyword.operator.type (? &)
// keyword.operator.type.function (->)
// constant.language.boolean.false
// constant.language.boolean.true
// support.type.primitive (nil, string, number, boolean, thread, userdata, symbol, any)
// punctuation.arguments.begin.typeof
// punctuation.arguments.end.typeof
// punctuation.definition.typeparameters.begin
// punctuation.definition.typeparameters.end
// punctuation.seperator.fields.type
