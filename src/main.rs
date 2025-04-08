use serde::Deserialize;
use std::fs;
use std::path::{Path, PathBuf};
use std::rc::Rc;

mod theme_parser;
use theme_parser::{FontStyle, ParsedTheme, ThemeEntry, ThemeTrieRule, parse_theme};

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

fn select_theme(parsed_themes: &Vec<ParsedTheme>, filter: Option<String>) -> usize {
    let mut filtered_themes: Vec<(&str, &Option<ThemeTrieRule>, usize)> = Vec::new();

    for (index, parsed_theme) in parsed_themes.iter().enumerate() {
        if let Some(filter_str) = &filter {
            if parsed_theme
                .label
                .to_lowercase()
                .contains(&filter_str.to_lowercase())
            {
                filtered_themes.push((&parsed_theme.label, &parsed_theme.source_style, index));
            }
        } else {
            filtered_themes.push((&parsed_theme.label, &parsed_theme.source_style, index));
        }
    }

    if filtered_themes.is_empty() {
        println!("No themes match the provided filter string. Press enter to continue...");
        let mut input = String::new();
        let _ = std::io::stdin().read_line(&mut input);
        return select_theme(parsed_themes, None);
    } else {
        for (theme_index, (label, source_style, _)) in filtered_themes.iter().enumerate() {
            let styled_label = if let Some(source_style) = source_style {
                format_styled_text(
                    source_style.font_style,
                    source_style.foreground,
                    source_style.background,
                    &format!("{} {}", label, source_style.scope),
                )
            } else {
                label.to_string() // No styling if `source_style` is None
            };

            println!("{}: {}", theme_index + 1, styled_label);
        }

        println!("== ----- ==");
        println!(
            "Input number [1 - {}] OR `filter [theme name]`",
            filtered_themes.len()
        )
    }

    match prompt_theme_select_input_with_reprompt_backup(filtered_themes.len()) {
        InputResult::Filter(filter_string) => select_theme(parsed_themes, filter_string),
        InputResult::ThemeIndex(theme_index) => filtered_themes[theme_index].2,
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

    let mut parsed_themes: Vec<ParsedTheme> = Vec::new();
    let mut theme_files: Vec<&String> = Vec::new();

    for extension in extensions {
        let extension_path = normalize_path(&extension.location.path);
        let theme_entries = read_theme_entries_from_json(&extension_path);

        if let Some(theme_entries) = theme_entries {
            for theme_entry in theme_entries {
                let absolute_path = extension_path.join(normalize_path(&theme_entry.path));
                let theme_content = match fs::read_to_string(&absolute_path) {
                    Err(e) => {
                        eprint!(
                            "Failed to load `{}` for theme `{}`: {}",
                            theme_entry.path, theme_entry.label, e
                        );
                        continue;
                    }
                    Ok(content) => content,
                };

                let theme_content = Box::leak(Box::new(theme_content));
                let theme_json = match serde_json::from_str(&theme_content) {
                    Ok(json) => Box::leak(Box::new(json)),
                    Err(_) => match json5::from_str(&theme_content) {
                        Ok(json) => Box::leak(Box::new(json)),
                        Err(e) => {
                            eprintln!(
                                "Failed to parse `{}` for theme `{}` as JSON or JSON5: {}",
                                theme_entry.path, theme_entry.label, e
                            );
                            continue;
                        }
                    },
                };

                if let Some(parsed_theme) = parse_theme(theme_json, &theme_entry.label) {
                    parsed_themes.push(parsed_theme);
                    theme_files.push(theme_content);
                }
            }
        }
    }

    let selected_theme_idx = select_theme(&parsed_themes, None);

    // println!("Selected theme: {}", selected_theme_idx + 1)

    // match fs::read_to_string(&theme_entry.path) {
    //     Err(e) => {
    //         eprintln!("Failed to read `{}`: {}", theme_entry.path, e);
    //         std::process::exit(0);
    //     }
    //     Ok(theme_json) => {
    //         let theme: Theme = json5::from_str(&theme_json).expect("Failed to parse JSON");

    //         let background_color = theme.colors.get("editor.background");

    //         // HashMap to track unique settings and their counts

    //         // Iterate over token colors and count unique settings

    //         // Print the sorted categories and their settings
    //         for (category, style_settings) in sorted_categories {
    //             for (index, style_setting) in style_settings.iter().enumerate() {

    //                 let mut is_any_ansi = false;

    //                 let (style_ansi, style_text) = if !style_codes.is_empty() {
    //                     is_any_ansi = true;
    //                     (
    //                         style_codes.join(";") + ";",
    //                         format!(" {}", style_names.join(" ")),
    //                     )
    //                 } else {
    //                     (String::new(), String::new())
    //                 };

    //                 let (foreground_ansi, foreground_text, alpha_text) =
    //                     if let Some(foreground) = &style_setting.foreground {
    //                         is_any_ansi = true;
    //                         (
    //                             format!("38;2;{}", hex_to_rgb_string(&foreground)),
    //                             foreground.clone(),
    //                             if let Some((_, _, _, Some(alpha))) = hex_to_rgba(foreground) {
    //                                 format!(" alpha: {}", alpha)
    //                             } else {
    //                                 String::new()
    //                             },
    //                         )
    //                     } else {
    //                         (String::new(), String::new(), String::new())
    //                     };

    //                 let background_text = if let Some(background) = &style_setting.background {
    //                     format!(" background: {}", background)
    //                 } else {
    //                     String::new()
    //                 };

    //                 let background_ansi = if let Some(bg) = &style_setting.background {
    //                     let bga = format!(
    //                         "{}48;2;{}",
    //                         if is_any_ansi { ";" } else { "" },
    //                         hex_to_rgb_string(&bg)
    //                     );
    //                     is_any_ansi = true;
    //                     bga
    //                 } else if let Some(bg) = background_color {
    //                     let bga = format!(
    //                         "{}48;2;{}",
    //                         if is_any_ansi { ";" } else { "" },
    //                         hex_to_rgb_string(&bg)
    //                     );
    //                     is_any_ansi = true;
    //                     bga
    //                 } else {
    //                     String::new()
    //                 };

    //                 let (ansi_format_prefix, ansi_format_suffix) = if is_any_ansi {
    //                     (
    //                         format!("\x1b[{}{}{}m", style_ansi, foreground_ansi, background_ansi),
    //                         "\x1b[0m".to_string(),
    //                     )
    //                 } else {
    //                     (String::new(), String::new())
    //                 };

    //                 print!(
    //                     "{}{} ({}{}{}{}){} ",
    //                     ansi_format_prefix,
    //                     color_name,
    //                     foreground_text,
    //                     background_text,
    //                     style_text,
    //                     alpha_text,
    //                     ansi_format_suffix
    //                 )
    //             }

    //             println!();
    //         }
    //     }
    // }
}

fn format_styled_text(
    font_style: u8,
    foreground: Option<&str>,
    background: Option<&str>,
    text: &str,
) -> String {
    let mut style_codes: Vec<String> = Vec::new();

    // Handle font styles using FontStyle enums
    if font_style & FontStyle::Bold as u8 != 0 {
        style_codes.push("1".to_string());
    }
    if font_style & FontStyle::Italic as u8 != 0 {
        style_codes.push("3".to_string());
    }
    if font_style & FontStyle::Underline as u8 != 0 {
        style_codes.push("4".to_string());
    }
    if font_style & FontStyle::Strikethrough as u8 != 0 {
        style_codes.push("9".to_string());
    }

    // Handle foreground color
    if let Some(foreground) = foreground {
        let formatted_foreground = format!("38;2;{}", hex_to_rgb_string(foreground));
        style_codes.push(formatted_foreground);
    }

    // Handle background color
    if let Some(background) = background {
        let background_code = format!("48;2;{}", hex_to_rgb_string(background));
        style_codes.push(background_code);
    }

    // Build the ANSI style prefix and suffix
    let (style_prefix, style_suffix) = if !style_codes.is_empty() {
        (format!("\x1b[{}m", style_codes.join(";")), "\x1b[0m")
    } else {
        (String::new(), "")
    };

    // Return the formatted string
    format!("{}{}{}", style_prefix, text, style_suffix)
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
