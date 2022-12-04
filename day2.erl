-module(day2).

-export([solution_pt1/0, solution_pt2/0]).

read_lines(FileName) ->
    case file:read_file(FileName) of
        {ok, Text} -> {ok, lists:droplast(string:split(Text, "\n", all))};
        {error, Reason} -> {error, Reason}
    end.

turn({Move1, Move2}) ->
    case Move1 of
        <<"A">> ->
            case Move2 of
                <<"X">> -> {draw, Move2};
                <<"Y">> -> {win, Move2};
                <<"Z">> -> {loss, Move2}
            end;
        <<"B">> ->
            case Move2 of
                <<"X">> -> {loss, Move2};
                <<"Y">> -> {draw, Move2};
                <<"Z">> -> {win, Move2}
            end;
        <<"C">> ->
            case Move2 of
                <<"X">> -> {win, Move2};
                <<"Y">> -> {loss, Move2};
                <<"Z">> -> {draw, Move2}
            end
    end.

turn_pt2({Move1, Move2}) ->
    case Move2 of
        <<"X">> ->
            {loss,
                case Move1 of
                    <<"A">> -> <<"C">>;
                    <<"B">> -> <<"A">>;
                    <<"C">> -> <<"B">>
                end};
        <<"Y">> ->
            {draw,
                case Move1 of
                    <<"A">> -> <<"A">>;
                    <<"B">> -> <<"B">>;
                    <<"C">> -> <<"C">>
                end};
        <<"Z">> ->
            {win,
                case Move1 of
                    <<"A">> -> <<"B">>;
                    <<"B">> -> <<"C">>;
                    <<"C">> -> <<"A">>
                end}
    end.

score_move_pt1(Move) ->
    case Move of
        <<"X">> -> 1;
        <<"Y">> -> 2;
        <<"Z">> -> 3
    end.

score_move_pt2(Move) ->
    case Move of
        <<"A">> -> 1;
        <<"B">> -> 2;
        <<"C">> -> 3
    end.

score_result(Result) ->
    case Result of
        win -> 6;
        draw -> 3;
        loss -> 0
    end.

score_turn_pt1({Result, Move}) ->
    score_move_pt1(Move) + score_result(Result).

score_turn_pt2({Result, Move}) ->
    score_move_pt2(Move) + score_result(Result).

parse_line(Line) ->
    [L, R | _] = string:split(Line, " "),
    {L, R}.

solution_pt1() ->
    {ok, Lines} = read_lines("input/day2.txt"),
    lists:sum(
        lists:map(
            fun(S) -> score_turn_pt1(S) end,
            lists:map(
                fun(S) -> turn(S) end,
                lists:map(fun(S) -> parse_line(S) end, Lines)
            )
        )
    ).

solution_pt2() ->
    {ok, Lines} = read_lines("input/day2.txt"),
    lists:sum(
        lists:map(
            fun(S) -> score_turn_pt2(S) end,
            lists:map(
                fun(S) -> turn_pt2(S) end,
                lists:map(fun(S) -> parse_line(S) end, Lines)
            )
        )
    ).
