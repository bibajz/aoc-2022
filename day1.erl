-module(day1).

-export([read_lines/1, partition_by/2, solution_pt1/0, solution_pt2/0]).

read_lines(FileName) ->
    case file:read_file(FileName) of
        {ok, Text} -> {ok, lists:droplast(string:split(Text, "\n", all))};
        {error, Reason} -> {error, Reason}
    end.

partition_by_inner([H | T], AggList, List, Func) ->
    case Func(H) of
        true -> partition_by_inner(T, [lists:reverse(List) | AggList], [], Func);
        false -> partition_by_inner(T, AggList, [H | List], Func)
    end;
partition_by_inner([], AggList, List, _) ->
    lists:reverse([List | AggList]).

partition_by(Func, List) -> partition_by_inner(List, [], [], Func).

solution_pt1() ->
    {ok, Lines} = read_lines("input/day1.txt"),
    lists:max(
        lists:map(
            fun(L) -> lists:sum(L) end,
            lists:map(
                fun(L) ->
                    lists:map(
                        fun(S) ->
                            {I, _} = string:to_integer(S),
                            I
                        end,
                        L
                    )
                end,
                partition_by(fun(X) -> X == <<>> end, Lines)
            )
        )
    ).

solution_pt2() ->
    {ok, Lines} = read_lines("input/day1.txt"),
    [H1, H2, H3 | _] =
        lists:reverse(
            lists:sort(
                lists:map(
                    fun(L) -> lists:sum(L) end,
                    lists:map(
                        fun(L) ->
                            lists:map(
                                fun(S) ->
                                    {I, _} = string:to_integer(S),
                                    I
                                end,
                                L
                            )
                        end,
                        partition_by(fun(X) -> X == <<>> end, Lines)
                    )
                )
            )
        ),
    H1 + H2 + H3.
