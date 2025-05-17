import erl_wrapper
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import html_converter

pub fn main() {
  let argv = erl_wrapper.get_argv()

  case argv {
    "" -> {
      // wrapper prints usage if no argv
      Nil
    }
    _ -> {
      let args = string.split(argv, " ")

      let filename = case list.first(args) {
        Ok(filename) -> filename
        Error(_) -> {
          io.println("Error: No filename provided, using README.md")
          "README.md"
        }
      }

      let output_filename = case args {
        [_, output_name, ..] -> output_name
        _ -> default_output_filename(filename)
      }

      let title = extract_title(filename)
      let file_contents = erl_wrapper.readfile(filename)

      case file_contents {
        "___DOCUGEN__::FILE_EMPTY" -> {
          io.println("Error: File '" <> filename <> "' is empty")
          Nil
        }
        "___DOCUGEN__::FILE_NOT_FOUND" -> {
          io.println("Error: File '" <> filename <> "' does not exist")
          Nil
        }
        "___DOCUGEN__::FILE_ERROR" -> {
          io.println("Error: Could not read file '" <> filename <> "'")
          Nil
        }
        _ -> {
          let #(html_lines, _) =
            string.split(file_contents, "\n")
            |> list.fold(#([], html_converter.Normal), fn(acc, line) {
              let #(lines, state) = acc
              let #(converted, new_state) =
                html_converter.convert_line(line, state)
              #([converted, ..lines], new_state)
            })

          let html_content =
            html_lines
            |> list.reverse()
            |> string.join("\n")

          let processed_html = process_html_content(html_content)
          let html_document =
            html_converter.html_template(
              processed_html,
              erl_wrapper.readfile("priv/styles.css"),
              title,
            )

          case erl_wrapper.write_to_file(output_filename, html_document) {
            Ok(_) ->
              io.println(
                "Successfully converted "
                <> filename
                <> " to "
                <> output_filename,
              )
            Error(_) -> io.println("Error writing to " <> output_filename)
          }
        }
      }
    }
  }
}

fn default_output_filename(filename: String) -> String {
  case string.ends_with(filename, ".md") {
    True -> string.replace(filename, ".md", ".html")
    False -> filename <> ".html"
  }
}

fn extract_title(filename: String) -> String {
  let filename = case string.contains(filename, "/") {
    True -> {
      let parts = string.split(filename, "/")
      list.last(parts) |> result.unwrap(filename)
    }
    False -> filename
  }

  let base_name = case string.contains(filename, ".") {
    True -> {
      let parts = string.split(filename, ".")
      list.first(parts) |> result.unwrap("")
    }
    False -> filename
  }

  let title = string.replace(base_name, "_", " ")
  let title = string.replace(title, "-", " ")

  string.capitalise(title)
}

fn process_html_content(content: String) -> String {
  let content = case
    string.contains(content, "<div class=\"code-block\">")
    && !string.contains(content, "</div>")
  {
    True -> content <> "\n</div>"
    False -> content
  }

  let content = string.replace(content, "<p></p>\n<p></p>", "<p></p>")
  let content = string.replace(content, "</ul>\n<ul>", "")
  let content = string.replace(content, "</ol>\n<ol>", "")

  content
}
