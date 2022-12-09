-module(day9).

-import(utils, [read_lines/1]).

-export([solution_pt1/0, solution_pt2/0]).

l_inf({X1, Y1}, {X2, Y2}) -> lists:max([abs(X1 - X2), abs(Y1 - Y2)]).
l_1({X1, Y1}, {X2, Y2}) -> abs(X1 - X2) + abs(Y1 - Y2).

parse_line(Line) ->
    [L, R | _] = string:split(Line, " ", all),
    {Int, _} = string:to_integer(R),
    {
        case L of
            "U" -> up;
            "R" -> right;
            "D" -> down;
            "L" -> left
        end,
        Int
    }.

move_head1(Direction, {X, Y}) ->
    case Direction of
        up -> {X, Y + 1};
        right -> {X + 1, Y};
        down -> {X, Y - 1};
        left -> {X - 1, Y}
    end.

move_tail(HeadPos, TailPos) ->
    case l_inf(HeadPos, TailPos) =< 1 of
        true ->
            TailPos;
        false ->
            {{X_H, Y_H}, {X_T, Y_T}} = {HeadPos, TailPos},
            case l_1(HeadPos, TailPos) == 4 of
                true ->
                    {round((X_H + X_T) / 2), round((Y_H + Y_T) / 2)};
                false ->
                    case X_H - X_T of
                        2 ->
                            {X_T + 1, Y_H};
                        -2 ->
                            {X_T - 1, Y_H};
                        _ ->
                            case Y_H - Y_T of
                                2 -> {X_H, Y_T + 1};
                                -2 -> {X_H, Y_T - 1}
                            end
                    end
            end
    end.

move_rope(Direction, HeadPos, TailPos) ->
    NewHeadPos = move_head1(Direction, HeadPos),
    {NewHeadPos, move_tail(NewHeadPos, TailPos)}.

move_by({Direction, 1}, {HeadPos, TailPos}) ->
    [move_rope(Direction, HeadPos, TailPos)];
move_by({Direction, Magitude}, {HeadPos, TailPos}) ->
    NewPos = move_rope(Direction, HeadPos, TailPos),
    [NewPos] ++ move_by({Direction, Magitude - 1}, NewPos).

solution_pt1() ->
    {ok, Lines} = read_lines("input/day9.txt"),
    Instructions = lists:map(fun parse_line/1, Lines),

    Positions = lists:foldl(
        fun(X, Acc) -> Acc ++ move_by(X, lists:last(Acc)) end,
        [{{0, 0}, {0, 0}}],
        Instructions
    ),

    TailPositions = lists:map(fun({_, T}) -> T end, Positions),

    length(lists:uniq(TailPositions)).

move_tails([], _, AggList) ->
    lists:reverse(AggList);
move_tails([H | T], PrevTail, AggList) ->
    NewTail = move_tail(PrevTail, H),
    move_tails(T, NewTail, [NewTail | AggList]).

move_rope_pt2(Direction, [H | T]) ->
    NewH = move_head1(Direction, H),
    [NewH | move_tails(T, NewH, [])].

move_by_pt2({Direction, 1}, PosList) ->
    [move_rope_pt2(Direction, PosList)];
move_by_pt2({Direction, Magitude}, PosList) ->
    NewPos = move_rope_pt2(Direction, PosList),
    [NewPos] ++ move_by_pt2({Direction, Magitude - 1}, NewPos).

solution_pt2() ->
    {ok, Lines} = read_lines("input/day9.txt"),
    Instructions = lists:map(fun parse_line/1, Lines),

    InitPositions = lists:map(fun(_) -> {0, 0} end, lists:seq(1, 10)),

    Positions = lists:foldl(
        fun(X, Acc) -> Acc ++ move_by_pt2(X, lists:last(Acc)) end,
        [InitPositions],
        Instructions
    ),
    TailPositions = lists:map(fun(L) -> lists:last(L) end, Positions),

    length(lists:uniq(TailPositions)).
