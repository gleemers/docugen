-module(docugen_erlang).
-export([readfile/1, get_argv/0, write_file/2]).

% Read file contents
-spec readfile(binary()) -> binary().
readfile(FileName) ->
    case file:read_file(FileName) of
        {ok, <<>>} -> 
            % File exists but is empty
            <<"FILE_EMPTY">>;
        {ok, Binary} -> 
            Binary;
        {error, enoent} -> 
            % File does not exist
            <<"FILE_NOT_FOUND">>;
        {error, _Reason} -> 
            % Other error
            <<"FILE_ERROR">>
    end.

% Write content to a file
-spec write_file(binary(), binary()) -> {ok, nil} | {error, binary()}.
write_file(FileName, Content) ->
    case file:write_file(FileName, Content) of
        ok -> {ok, nil};
        {error, Reason} -> {error, list_to_binary(file:format_error(Reason))}
    end.

% Get program arguments
-spec get_argv() -> binary().
get_argv() ->
    Args = init:get_plain_arguments(),
    case Args of
        [] ->
            print_usage(),
            <<>>;
        _ ->
            list_to_binary(string:join(Args, " "))
    end.

% Print usage information
print_usage() ->
    io:format("Usage: docugen <input_filename.md> [output_filename.html]~n").
