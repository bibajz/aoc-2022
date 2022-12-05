-module(utils).

-export([read_lines/1, transpose/1]).

read_lines(FileName) ->
    case file:read_file(FileName) of
        {ok, Text} -> {ok, lists:droplast(string:split(binary_to_list(Text), "\n", all))};
        {error, Reason} -> {error, Reason}
    end.

transpose_inner(LoL, Agg, N) ->
    _Len = length(lists:nth(1, LoL)),
    case N > _Len of
        true -> lists:reverse(Agg);
        false -> transpose_inner(LoL, [lists:map(fun(L) -> lists:nth(N, L) end, LoL) | Agg], N + 1)
    end.

transpose(ListOfLists) -> transpose_inner(ListOfLists, [], 1).
