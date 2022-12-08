-module(day8).

-import(utils, [read_lines/1, partition_by_len/2]).

-export([solution_pt1/0, solution_pt2/0]).

product_inner(AggList, _, []) -> lists:reverse(AggList);
product_inner(AggList, H1, [H2 | T2]) -> product_inner([{H1, H2} | AggList], H1, T2).

product([], _) -> [];
product([H1 | T1], List2) -> product_inner([], H1, List2) ++ product(T1, List2).

all_(List) -> lists:all(fun(X) -> X == true end, List).

solution_pt1() ->
    {ok, Lines} = read_lines("input/day8.txt"),
    Grid = lists:map(
        fun(Line) ->
            lists:map(
                fun(X) ->
                    {Int, _} = string:to_integer(X),
                    Int
                end,
                partition_by_len(1, Line)
            )
        end,
        Lines
    ),
    {Height, Width} = {length(Grid), length(lists:nth(1, Grid))},

    Indexed = lists:enumerate(lists:map(fun lists:enumerate/1, Grid)),
    Flattened = lists:foldl(
        fun({Ind1, Row}, Acc) -> Acc ++ lists:map(fun({Ind2, X}) -> {{Ind1, Ind2}, X} end, Row) end,
        [],
        Indexed
    ),

    InnerIndices = product(lists:seq(2, Height - 1), lists:seq(2, Width - 1)),

    VisibleIndexes = lists:foldl(
        fun({Direction, {H, W}}, Acc) ->
            case
                all_(
                    lists:map(
                        fun({_, X}) -> lists:nth(W, lists:nth(H, Grid)) - X > 0 end,
                        lists:filter(
                            fun({{Ind1, Ind2}, _}) ->
                                case Direction of
                                    up -> (Ind1 < H) and (Ind2 == W);
                                    right -> (Ind1 == H) and (Ind2 > W);
                                    down -> (Ind1 > H) and (Ind2 == W);
                                    left -> (Ind1 == H) and (Ind2 < W)
                                end
                            end,
                            Flattened
                        )
                    )
                )
            of
                true -> [{H, W}] ++ Acc;
                false -> Acc
            end
        end,
        [],
        product([up, right, down, left], InnerIndices)
    ),

    length(lists:uniq(VisibleIndexes)) + (2 * Height) + (2 * Width) - 4.

solution_pt2() ->
    {ok, Lines} = read_lines("input/day8.txt"),
    Grid = lists:map(
        fun(Line) ->
            lists:map(
                fun(X) ->
                    {Int, _} = string:to_integer(X),
                    Int
                end,
                partition_by_len(1, Line)
            )
        end,
        Lines
    ),
    {Height, Width} = {length(Grid), length(lists:nth(1, Grid))},

    Indexed = lists:enumerate(lists:map(fun lists:enumerate/1, Grid)),
    Flattened = lists:foldl(
        fun({Ind1, Row}, Acc) -> Acc ++ lists:map(fun({Ind2, X}) -> {{Ind1, Ind2}, X} end, Row) end,
        [],
        Indexed
    ),

    InnerIndices = product(lists:seq(2, Height - 1), lists:seq(2, Width - 1)),

    VisibleLength = lists:map(
        fun({Direction, {H, W}}) ->
            {H, W, Direction,
                case Direction of
                    up ->
                        L = lists:takewhile(
                            fun({_, X}) -> X < lists:nth(W, lists:nth(H, Grid)) end,
                            lists:reverse(
                                lists:filter(
                                    fun({{Ind1, Ind2}, _}) -> (Ind1 < H) and (Ind2 == W) end,
                                    Flattened
                                )
                            )
                        ),
                        case length(L) == H - 1 of
                            true -> length(L);
                            false -> length(L) + 1
                        end;
                    right ->
                        L = lists:takewhile(
                            fun({_, X}) -> X < lists:nth(W, lists:nth(H, Grid)) end,
                            lists:filter(
                                fun({{Ind1, Ind2}, _}) -> (Ind1 == H) and (Ind2 > W) end,
                                Flattened
                            )
                        ),
                        case length(L) == Width - W of
                            true -> length(L);
                            false -> length(L) + 1
                        end;
                    down ->
                        L = lists:takewhile(
                            fun({_, X}) -> X < lists:nth(W, lists:nth(H, Grid)) end,
                            lists:filter(
                                fun({{Ind1, Ind2}, _}) -> (Ind1 > H) and (Ind2 == W) end,
                                Flattened
                            )
                        ),
                        case length(L) == Height - H of
                            true -> length(L);
                            false -> length(L) + 1
                        end;
                    left ->
                        L = lists:takewhile(
                            fun({_, X}) -> X < lists:nth(W, lists:nth(H, Grid)) end,
                            lists:reverse(
                                lists:filter(
                                    fun({{Ind1, Ind2}, _}) -> (Ind1 == H) and (Ind2 < W) end,
                                    Flattened
                                )
                            )
                        ),
                        case length(L) == W - 1 of
                            true -> length(L);
                            false -> length(L) + 1
                        end
                end}
        end,

        product([up, right, down, left], InnerIndices)
    ),
    lists:max(
        lists:map(
            fun({_, L}) -> lists:foldl(fun({_, _, _, X}, Acc) -> X * Acc end, 1, L) end,
            maps:to_list(
                maps:groups_from_list(
                    fun({H, W, _, _}) -> {H, W} end, VisibleLength
                )
            )
        )
    ).
