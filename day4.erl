-module(day4).

-import(utils, [read_lines/1]).

-export([solution_pt1/0, solution_pt2/0]).

seq_from_range(Range) ->
    [{L, _}, {R, _}] = lists:map(
        fun(S) -> string:to_integer(S) end, lists:sublist(string:split(Range, "-", all), 2)
    ),
    lists:seq(L, R).

are_overlapping(List1, List2) ->
    {S1, S2} = {sets:from_list(List1), sets:from_list(List2)},
    sets:is_subset(S1, S2) or sets:is_subset(S2, S1) or false.

have_common_element(List1, List2) ->
    {S1, S2} = {sets:from_list(List1), sets:from_list(List2)},
    not sets:is_empty(sets:intersection([S1, S2])).

bool_to_int(Bool) ->
    case Bool of
        true -> 1;
        false -> 0
    end.

solution_pt1() ->
    {ok, Lines} = read_lines("input/day4.txt"),
    lists:sum(
        lists:map(
            fun(B) -> bool_to_int(B) end,
            lists:map(
                fun({LSeq, RSeq}) -> are_overlapping(LSeq, RSeq) end,
                lists:map(
                    fun({LString, RString}) ->
                        {seq_from_range(LString), seq_from_range(RString)}
                    end,
                    lists:map(
                        fun(L) ->
                            [LString, RString | _] = string:split(L, ",", all),
                            {LString, RString}
                        end,
                        Lines
                    )
                )
            )
        )
    ).

solution_pt2() ->
    {ok, Lines} = read_lines("input/day4.txt"),
    lists:sum(
        lists:map(
            fun(B) -> bool_to_int(B) end,
            lists:map(
                fun({LSeq, RSeq}) -> have_common_element(LSeq, RSeq) end,
                lists:map(
                    fun({LString, RString}) ->
                        {seq_from_range(LString), seq_from_range(RString)}
                    end,
                    lists:map(
                        fun(L) ->
                            [LString, RString | _] = string:split(L, ",", all),
                            {LString, RString}
                        end,
                        Lines
                    )
                )
            )
        )
    ).
