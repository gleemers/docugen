pub fn get_styles() -> String {
  "body {
      background-color: rgb(28, 24, 31);
      color: #ababab;
      font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      line-height: 1.6;
      max-width: 800px;
      margin: 0 auto;
      padding: 20px;
    }
    
    header {
      margin-bottom: 2em;
      border-bottom: 1px solid #444;
      padding-bottom: 1em;
    }
    
    .document-title {
      color: #ffffff;
      font-size: 2.5em;
      margin: 0;
      padding: 0;
      text-align: center;
    }
    
    main {
      min-height: 70vh;
    }
    
    footer {
      margin-top: 3em;
      padding-top: 1em;
      border-top: 1px solid #444;
      text-align: center;
      font-size: 0.9em;
      color: #777;
    }
    
    h1, h2, h3, h4, h5, h6 {
      color: #e0e0e0;
      margin-top: 1.5em;
      margin-bottom: 0.5em;
    }
    
    h1 {
      font-size: 2.2em;
      padding-bottom: 0.3em;
    }
    
    h2 {
      font-size: 1.8em;
    }
    
    p {
      margin: 1em 0;
    }
    
    a {
      color: #6ba4ff;
      text-decoration: none;
    }
    
    a:hover {
      text-decoration: underline;
    }
    
    code {
      font-family: 'Courier New', Courier, monospace;
      background-color: rgba(255, 255, 255, 0.1);
      padding: 2px 4px;
      border-radius: 3px;
    }
    
    pre {
      background-color: rgba(0, 0, 0, 0.3);
      padding: 16px;
      border-radius: 5px;
      overflow-x: auto;
      margin: 1em 0;
    }
    
    pre code {
      background-color: transparent;
      padding: 0;
      border-radius: 0;
      display: block;
    }
    
    .code-block {
      background-color: rgba(0, 0, 0, 0.3);
      padding: 16px;
      border-radius: 5px;
      overflow-x: auto;
      margin: 1em 0;
      font-family: 'Courier New', Courier, monospace;
      white-space: pre;
      line-height: 1.3;
    }
    
    .code-block::before {
      content: attr(class);
      display: block;
      background-color: rgba(0, 0, 0, 0.5);
      padding: 4px 8px;
      margin: -16px -16px 16px -16px;
      border-radius: 5px 5px 0 0;
      font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      font-size: 0.8em;
      color: #888;
    }
    
    .language-javascript::before, .language-js::before {
      content: \"JavaScript\";
      color: #f7df1e;
    }
    
    .language-typescript::before, .language-ts::before {
      content: \"TypeScript\";
      color: #3178c6;
    }
    
    .language-python::before, .language-py::before {
      content: \"Python\";
      color: #3776ab;
    }
    
    .language-ruby::before {
      content: \"Ruby\";
      color: #cc342d;
    }
    
    .language-go::before {
      content: \"Go\";
      color: #00add8;
    }
    
    .language-rust::before {
      content: \"Rust\";
      color: #dea584;
    }
    
    .language-gleam::before {
      content: \"Gleam\";
      color: #ffaff3;
    }

    .language-erlang::before {
      content: \"Erlang\";
      color:rgb(209, 62, 62);
    }
    
    .language-elixir::before {
      content: \"Elixir\";
      color:rgba(148, 111, 207, 0.8);
    }
    
    .language-java::before {
      content: \"Java\";
      color: #b07219;
    }
    
    .language-cpp::before {
      content: \"C++\";
      color: #f34b7d;
    }
    
    .language-csharp::before {
      content: \"C#\";
      color: #178600;
    }
    
    .language-php::before {
      content: \"PHP\";
      color: #4F5D95;
    }
    
    .language-html::before {
      content: \"HTML\";
      color: #e34c26;
    }
    
    .language-css::before {
      content: \"CSS\";
      color: #563d7c;
    }
    
    .language-bash::before {
      content: \"Bash\";
      color: #89e051;
    }
    
    blockquote {
      border-left: 4px solid #555;
      padding-left: 16px;
      margin-left: 0;
      color: #999;
    }
    
    ul, ol {
      padding-left: 2em;
      margin: 1em 0;
    }
    
    li {
      margin: 0.5em 0;
    }"
}

