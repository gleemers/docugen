-module(docugen_erlang).
-export([readfile/1, get_argv/0, write_file/2]).

-spec readfile(binary()) -> binary().
readfile(FileName) ->
    case file:read_file(FileName) of
        {ok, <<>>} -> 
            % exists but empty
            <<"___DOCUGEN__::FILE_EMPTY">>;
        {ok, Binary} -> 
            Binary;
        {error, enoent} -> 
            % nonexistent
            <<"___DOCUGEN__::FILE_NOT_FOUND">>;
        {error, _Reason} -> 
            % ????
            <<"___DOCUGEN__::FILE_ERROR">>
    end.

-spec write_file(binary(), binary()) -> {ok, nil} | {error, binary()}.
write_file(FileName, Content) ->
    case file:write_file(FileName, Content) of
        ok -> {ok, nil};
        {error, Reason} -> {error, list_to_binary(file:format_error(Reason))}
    end.

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

print_usage() ->
    io:format("Usage: docugen <input_filename.md> [output_filename.html]~n").
