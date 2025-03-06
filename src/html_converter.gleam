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

pub type State {
  Normal
  InCodeBlock
}

pub fn convert_line(line: String, state: State) -> #(String, State) {
  case state {
    InCodeBlock -> {
      case string.trim(line) {
        "```" -> #("</div>", Normal)
        _ -> #(line, InCodeBlock)
      }
    }
    Normal -> {
      case string.trim(line) {
        // Headers
        "###### " <> rest -> #("<h6>" <> rest <> "</h6>", Normal)
        "##### " <> rest -> #("<h5>" <> rest <> "</h5>", Normal)
        "#### " <> rest -> #("<h4>" <> rest <> "</h4>", Normal)
        "### " <> rest -> #("<h3>" <> rest <> "</h3>", Normal)
        "## " <> rest -> #("<h2>" <> rest <> "</h2>", Normal)
        "# " <> rest -> #("<h1>" <> rest <> "</h1>", Normal)

        // Lists
        "* " <> rest -> #("<ul><li>" <> rest <> "</li></ul>", Normal)
        "- " <> rest -> #("<ul><li>" <> rest <> "</li></ul>", Normal)
        "1. " <> rest -> #("<ol><li>" <> rest <> "</li></ol>", Normal)

        // Code blocks - handle language specification
        "```" -> #("<div class=\"code-block\">", InCodeBlock)
        "```" <> lang -> {
          case string.trim(lang) {
            "" -> #("<div class=\"code-block\">", InCodeBlock)
            "gleam" -> #(
              "<div class=\"code-block language-gleam\">",
              InCodeBlock,
            )
            "erl" | "erlang" -> #(
              "<div class=\"code-block language-erlang\">",
              InCodeBlock,
            )
            "ex" | "exs" | "elixir" -> #(
              "<div class=\"code-block language-elixir\">",
              InCodeBlock,
            )
            "js" | "javascript" -> #(
              "<div class=\"code-block language-javascript\">",
              InCodeBlock,
            )
            "ts" | "typescript" -> #(
              "<div class=\"code-block language-typescript\">",
              InCodeBlock,
            )
            "py" | "python" -> #(
              "<div class=\"code-block language-python\">",
              InCodeBlock,
            )
            "rb" | "ruby" -> #(
              "<div class=\"code-block language-ruby\">",
              InCodeBlock,
            )
            "go" -> #("<div class=\"code-block language-go\">", InCodeBlock)
            "rs" | "rust" -> #(
              "<div class=\"code-block language-rust\">",
              InCodeBlock,
            )
            "java" -> #("<div class=\"code-block language-java\">", InCodeBlock)
            "c" | "cpp" | "c++" -> #(
              "<div class=\"code-block language-cpp\">",
              InCodeBlock,
            )
            "cs" | "csharp" -> #(
              "<div class=\"code-block language-csharp\">",
              InCodeBlock,
            )
            "php" -> #("<div class=\"code-block language-php\">", InCodeBlock)
            "html" -> #("<div class=\"code-block language-html\">", InCodeBlock)
            "css" -> #("<div class=\"code-block language-css\">", InCodeBlock)
            "sh" | "bash" | "shell" -> #(
              "<div class=\"code-block language-bash\">",
              InCodeBlock,
            )
            language -> #(
              "<div class=\"code-block language-" <> language <> "\">",
              InCodeBlock,
            )
          }
        }

        // Horizontal rule
        "---" -> #("<hr>", Normal)
        "***" -> #("<hr>", Normal)

        // Empty line
        "" -> #("", Normal)

        // Default paragraph
        _ -> {
          // Check if this is a code line (indented with spaces or tabs)
          case
            string.starts_with(line, "    ") || string.starts_with(line, "\t")
          {
            True -> #("<pre><code>" <> line <> "</code></pre>", Normal)
            False -> #("<p>" <> line <> "</p>", Normal)
          }
        }
      }
    }
  }
}
