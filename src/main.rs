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

#[derive(Deserialize, Debug)]
struct Theme {
    colors: HashMap<String, String>, // Map for "colors" key
    tokenColors: Vec<TokenColor>,    // List of token colors
}

#[derive(Deserialize, Debug)]
struct TokenColor {
    settings: RawSettings,
}

#[derive(Deserialize, Debug)]
struct RawSettings {
    foreground: Option<String>,
    background: Option<String>,
    fontStyle: Option<String>, // Optional field
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct Settings {
    foreground: Option<String>,
    background: Option<String>,
    font_style: Vec<String>,
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

fn read_extensions_json(file_path: &PathBuf) -> Vec<Extension> {
    match fs::read_to_string(file_path) {
        Ok(json_content) => {
            serde_json::from_str(&json_content).expect("Failed to parse extensions.json")
        }
        Err(e) => {
            eprintln!(
                "Failed to read extensions.json: '{}'. Please provide the correct path to your .vscode/extensions/extensions.json file:",
                e
            );
            let input = get_stdin_filepath_with_reprompt_backup();
            let input_path = PathBuf::from(input.trim());
            read_extensions_json(&input_path)
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

struct StyleSetting {
    // color_name: &'static str,
    count: usize,
    foreground: Option<String>,
    background: Option<String>,
    font_style: Vec<String>,
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

    let extensions = read_extensions_json(&file_path);

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
            let mut settings_count: HashMap<Settings, usize> = HashMap::new();

            // Iterate over token colors and count unique settings
            for token_color in theme.tokenColors {
                if let None = token_color.settings.foreground {
                    continue;
                }

                let mut font_style: Vec<String> = token_color
                    .settings
                    .fontStyle
                    .as_deref()
                    .unwrap_or("")
                    .split_whitespace()
                    .map(str::to_string)
                    .collect();

                font_style.sort();

                if font_style.len() == 1 && font_style[0] == "normal" {
                    font_style.clear();
                }

                let settings = Settings {
                    background: token_color.settings.background.clone(),
                    foreground: token_color.settings.foreground.clone(),
                    font_style,
                };

                *settings_count.entry(settings).or_insert(0) += 1;
            }

            let mut style_hash: HashMap<&'static str, Vec<StyleSetting>> = HashMap::new();

            for (settings, count) in &settings_count {
                let label = if let Some(foreground) = &settings.foreground {
                    if let Some((hue, sat, _)) = hex_to_hsb(foreground) {
                        label_color_by_hue_sat(hue, sat)
                    } else {
                        "BMisc"
                    }
                } else {
                    "AMisc"
                };

                let entry = style_hash.entry(label).or_insert_with(Vec::new);

                entry.push(StyleSetting {
                    count: *count,
                    foreground: settings.foreground.clone(),
                    background: settings.background.clone(),
                    font_style: settings.font_style.clone(),
                });
            }

            for (_, style_settings) in style_hash.iter_mut() {
                style_settings.sort_by(|a, b| b.count.cmp(&a.count));
            }

            // Sort the categories by their hue
            let mut sorted_categories: Vec<(&str, &Vec<StyleSetting>)> = style_hash
                .iter()
                .map(|(&key, value)| (key, value))
                .collect();

            sorted_categories.sort_by(|(category_a, _), (category_b, _)| {
                let hue_a = get_hue_by_label(&category_a);
                let hue_b = get_hue_by_label(&category_b);
                hue_a
                    .partial_cmp(&hue_b)
                    .unwrap_or(std::cmp::Ordering::Equal)
            });

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
