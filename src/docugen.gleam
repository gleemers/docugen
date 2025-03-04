import erl_wrapper
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import html_converter
import styles

pub fn main() {
  // Get command line arguments
  let argv = erl_wrapper.get_argv()

  // Check if argv is empty
  case argv {
    "" -> {
      // The Erlang wrapper already printed usage and will exit
      Nil
    }
    _ -> {
      // Process the arguments
      let args = string.split(argv, " ")

      // Get input filename
      let filename = case list.first(args) {
        Ok(filename) -> filename
        Error(_) -> {
          io.println("Error: No filename provided, using README.md")
          "README.md"
        }
      }

      // Get output filename (if provided)
      let output_filename = case args {
        [_, output_name, ..] -> output_name
        _ -> default_output_filename(filename)
      }

      // Extract title from filename
      let title = extract_title(filename)

      // Read the file contents
      let file_contents = erl_wrapper.readfile(filename)

      // Check for special error messages
      case file_contents {
        "FILE_EMPTY" -> {
          io.println("Error: File '" <> filename <> "' is empty")
          Nil
        }
        "FILE_NOT_FOUND" -> {
          io.println("Error: File '" <> filename <> "' does not exist")
          Nil
        }
        "FILE_ERROR" -> {
          io.println("Error: Could not read file '" <> filename <> "'")
          Nil
        }
        _ -> {
          // Convert each line to HTML
          let html_lines =
            string.split(file_contents, "\n")
            |> list.map(html_converter.convert_line)

          // Join the HTML lines
          let html_content = string.join(html_lines, "\n")

          // Process the HTML content to fix any issues
          let processed_html = process_html_content(html_content)

          // Create the final HTML document
          let html_document =
            html_converter.html_template(
              processed_html,
              styles.get_styles(),
              title,
            )

          // Write the HTML to a file
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

// Helper function to generate default output filename
fn default_output_filename(filename: String) -> String {
  case string.ends_with(filename, ".md") {
    True -> string.replace(filename, ".md", ".html")
    False -> filename <> ".html"
  }
}

// Extract a title from the filename
fn extract_title(filename: String) -> String {
  // Extract just the filename without the path
  let filename = case string.contains(filename, "/") {
    True -> {
      let parts = string.split(filename, "/")
      list.last(parts) |> result.unwrap(filename)
    }
    False -> filename
  }

  // Remove file extension if present
  let base_name = case string.contains(filename, ".") {
    True -> {
      let parts = string.split(filename, ".")
      list.first(parts) |> result.unwrap("")
    }
    False -> filename
  }

  // Replace underscores and hyphens with spaces
  let title = string.replace(base_name, "_", " ")
  let title = string.replace(title, "-", " ")

  // Capitalize the title
  string.capitalise(title)
}

// Process HTML content to fix any issues
fn process_html_content(content: String) -> String {
  // Ensure all code blocks are properly closed
  let content = case
    string.contains(content, "<div class=\"code-block\">")
    && !string.contains(content, "</div>")
  {
    True -> content <> "\n</div>"
    False -> content
  }

  // Remove consecutive empty paragraphs
  let content = string.replace(content, "<p></p>\n<p></p>", "<p></p>")

  // Fix list items
  let content = string.replace(content, "</ul>\n<ul>", "")
  let content = string.replace(content, "</ol>\n<ol>", "")

  content
}
