-module(day3).

-import(utils, [read_lines/1]).

-export([solution_pt1/0, solution_pt2/0]).

split_on_len(List, Len) ->
    {lists:sublist(List, 1, Len), lists:sublist(List, 1 + Len, Len)}.

split_in_half(List) -> split_on_len(List, round(length(List) / 2)).

first_in_both_inner([H | T], Set) ->
    case sets:is_element(H, Set) of
        true -> {ok, H};
        false -> first_in_both_inner(T, Set)
    end;
first_in_both_inner([], _) ->
    {error, "Disjoint!"}.

% python codegen for the win :D
priority_map(Char) ->
    case Char of
        $a -> 1;
        $b -> 2;
        $c -> 3;
        $d -> 4;
        $e -> 5;
        $f -> 6;
        $g -> 7;
        $h -> 8;
        $i -> 9;
        $j -> 10;
        $k -> 11;
        $l -> 12;
        $m -> 13;
        $n -> 14;
        $o -> 15;
        $p -> 16;
        $q -> 17;
        $r -> 18;
        $s -> 19;
        $t -> 20;
        $u -> 21;
        $v -> 22;
        $w -> 23;
        $x -> 24;
        $y -> 25;
        $z -> 26;
        $A -> 27;
        $B -> 28;
        $C -> 29;
        $D -> 30;
        $E -> 31;
        $F -> 32;
        $G -> 33;
        $H -> 34;
        $I -> 35;
        $J -> 36;
        $K -> 37;
        $L -> 38;
        $M -> 39;
        $N -> 40;
        $O -> 41;
        $P -> 42;
        $Q -> 43;
        $R -> 44;
        $S -> 45;
        $T -> 46;
        $U -> 47;
        $V -> 48;
        $W -> 49;
        $X -> 50;
        $Y -> 51;
        $Z -> 52
    end.

first_in_both(List1, List2) -> first_in_both_inner(List1, sets:from_list(List2)).

solution_pt1() ->
    {ok, Lines} = read_lines("input/day3.txt"),
    lists:sum(
        lists:map(
            fun(C) -> priority_map(C) end,
            lists:map(
                fun({L1, L2}) ->
                    {ok, C} = first_in_both(L1, L2),
                    C
                end,
                lists:map(fun(L) -> split_in_half(L) end, Lines)
            )
        )
    ).

partition_by_len_inner(List, Len, AggList) ->
    Prefix = lists:sublist(List, Len),
    case length(Prefix) < Len of
        true ->
            case length(Prefix) == 0 of
                true -> lists:reverse(AggList);
                false -> lists:reverse([Prefix | AggList])
            end;
        false ->
            partition_by_len_inner(lists:nthtail(Len, List), Len, [Prefix | AggList])
    end.

partition_by_len(List, Len) -> partition_by_len_inner(List, Len, []).

in_all_three(L1, L2, L3) ->
    [C | _] = sets:to_list(
        sets:intersection([sets:from_list(L1), sets:from_list(L2), sets:from_list(L3)])
    ),
    C.

solution_pt2() ->
    {ok, Lines} = read_lines("input/day3.txt"),
    lists:sum(
        lists:map(
            fun(C) -> priority_map(C) end,
            lists:map(
                fun([H1, H2, H3 | _]) -> in_all_three(H1, H2, H3) end,
                partition_by_len(Lines, 3)
            )
        )
    ).
