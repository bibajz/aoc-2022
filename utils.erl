-module(utils).

-export([read_lines/1]).

read_lines(FileName) ->
    case file:read_file(FileName) of
        {ok, Text} -> {ok, lists:droplast(string:split(binary_to_list(Text), "\n", all))};
        {error, Reason} -> {error, Reason}
    end.
