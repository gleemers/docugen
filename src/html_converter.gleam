import gleam/string

// Define HTML template with title
pub fn html_template(content: String, styles: String, title: String) -> String {
  "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"UTF-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
  <title>" <> title <> "</title>
  <style>
" <> styles <> "
  </style>
</head>
<body>
  <header>
    <h1 class=\"document-title\">" <> title <> "</h1>
  </header>
  <main>
" <> content <> "
  </main>
  <footer>
    <p>Generated with <a style=\"text-decoration: none; color: inherit;\" href=\"https://github.com/gleemers/docugen\">DocuGen</a></p>
  </footer>
</body>
</html>"
}

// Convert markdown line to HTML
pub fn convert_line(line: String) -> String {
  let trimmed = string.trim(line)

  case trimmed {
    // Headers
    "###### " <> rest -> "<h6>" <> rest <> "</h6>"
    "##### " <> rest -> "<h5>" <> rest <> "</h5>"
    "#### " <> rest -> "<h4>" <> rest <> "</h4>"
    "### " <> rest -> "<h3>" <> rest <> "</h3>"
    "## " <> rest -> "<h2>" <> rest <> "</h2>"
    "# " <> rest -> "<h1>" <> rest <> "</h1>"

    // Lists
    "* " <> rest -> "<ul><li>" <> rest <> "</li></ul>"
    "- " <> rest -> "<ul><li>" <> rest <> "</li></ul>"
    "1. " <> rest -> "<ol><li>" <> rest <> "</li></ol>"

    // Code blocks - handle language specification
    "```" -> "</div>"
    "```" <> lang -> {
      case string.trim(lang) {
        "" -> "<div class=\"code-block\">"
        "js" | "javascript" -> "<div class=\"code-block language-javascript\">"
        "ts" | "typescript" -> "<div class=\"code-block language-typescript\">"
        "py" | "python" -> "<div class=\"code-block language-python\">"
        "rb" | "ruby" -> "<div class=\"code-block language-ruby\">"
        "go" -> "<div class=\"code-block language-go\">"
        "rs" | "rust" -> "<div class=\"code-block language-rust\">"
        "gleam" -> "<div class=\"code-block language-gleam\">"
        "erlang" -> "<div class=\"code-block language-erlang\">"
        "elixir" -> "<div class=\"code-block language-elixir\">"
        "java" -> "<div class=\"code-block language-java\">"
        "c" | "cpp" | "c++" -> "<div class=\"code-block language-cpp\">"
        "cs" | "csharp" -> "<div class=\"code-block language-csharp\">"
        "php" -> "<div class=\"code-block language-php\">"
        "html" -> "<div class=\"code-block language-html\">"
        "css" -> "<div class=\"code-block language-css\">"
        "sh" | "bash" | "shell" -> "<div class=\"code-block language-bash\">"
        language -> "<div class=\"code-block language-" <> language <> "\">"
      }
    }

    // Horizontal rule
    "---" -> "<hr>"
    "***" -> "<hr>"

    // Empty line
    "" -> ""

    // Default paragraph
    _ -> {
      // Check if this is a code line (indented with spaces or tabs)
      case string.starts_with(line, "    ") || string.starts_with(line, "\t") {
        True -> "<pre><code>" <> line <> "</code></pre>"
        False -> "<p>" <> line <> "</p>"
      }
    }
  }
}

// Find the index of the style section
fn find_style_section(
  lines: List(String),
  current_index: Int,
) -> Result(Int, Nil) {
  case lines {
    [] -> Error(Nil)
    [first, ..rest] -> {
      case string.starts_with(first, "## Styles") {
        True -> Ok(current_index)
        False -> find_style_section(rest, current_index + 1)
      }
    }
  }
}
