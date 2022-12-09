-module(day9).

-import(utils, [read_lines/1, partition_by_len/2]).

-export([solution_pt1/0]).

l_inf({X1, Y1}, {X2, Y2}) -> lists:max([abs(X1 - X2), abs(Y1 - Y2)]).

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
