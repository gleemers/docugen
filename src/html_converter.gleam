import gleam/string

pub type State {
  Normal
  InCodeBlock
}

/// Construct HTML template
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

pub fn convert_line(line: String, state: State) -> #(String, State) {
  let processed_line = {
    line
    |> string.replace("<", "&lt;")
    |> string.replace(">", "&gt;")
  }

  let trimmed = processed_line |> string.trim()

  case state {
    InCodeBlock -> {
      case trimmed {
        "```" -> #("</div>", Normal)
        _ -> #(trimmed, InCodeBlock)
      }
    }
    Normal -> {
      case trimmed {
        "###### " <> rest -> #("<h6>" <> rest <> "</h6>", Normal)
        "##### " <> rest -> #("<h5>" <> rest <> "</h5>", Normal)
        "#### " <> rest -> #("<h4>" <> rest <> "</h4>", Normal)
        "### " <> rest -> #("<h3>" <> rest <> "</h3>", Normal)
        "## " <> rest -> #("<h2>" <> rest <> "</h2>", Normal)
        "# " <> rest -> #("<h1>" <> rest <> "</h1>", Normal)

        "* " <> rest -> #("<ul><li>" <> rest <> "</li></ul>", Normal)
        "- " <> rest -> #("<ul><li>" <> rest <> "</li></ul>", Normal)
        "1. " <> rest -> #("<ol><li>" <> rest <> "</li></ol>", Normal)

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

        "---" -> #("<hr>", Normal)
        "***" -> #("<hr>", Normal)

        "" -> #("", Normal)

        _ -> {
          case
            string.starts_with(line, "    ") || string.starts_with(line, "\t")
          {
            True -> #(
              "<pre><code>" <> processed_line <> "</code></pre>",
              Normal,
            )
            False -> #("<p>" <> processed_line <> "</p>", Normal)
          }
        }
      }
    }
  }
}

