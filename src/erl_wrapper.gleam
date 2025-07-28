//// Providers Erlang functions

/// ERL: Read file
@external(erlang, "docugen_erlang", "readfile")
pub fn readfile(filename: String) -> String

/// ERL: Get program arguments
@external(erlang, "docugen_erlang", "get_argv")
pub fn get_argv() -> String

/// ERL: Write to file
@external(erlang, "docugen_erlang", "write_file")
pub fn write_to_file(filename: String, content: String) -> Result(Nil, String)
