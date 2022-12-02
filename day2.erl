-module(day2).

-export([solution_pt1/0, parse_line/1]).

read_lines(FileName) ->
  case file:read_file(FileName) of
    {ok, Text} -> {ok, lists:droplast(string:split(Text, "\n", all))};
    {error, Reason} -> {error, Reason}
  end.

turn({Move1, Move2}) ->
  case Move1 of
    <<"A">> -> case Move2 of
      <<"X">> -> {draw, Move2};
      <<"Y">> -> {win, Move2};
      <<"Z">> -> {loss, Move2}
    end;
    <<"B">> -> case Move2 of
      <<"X">> -> {loss, Move2};
      <<"Y">> -> {draw, Move2};
      <<"Z">> -> {win, Move2}
    end;
    <<"C">> -> case Move2 of
      <<"X">> -> {win, Move2};
      <<"Y">> -> {loss, Move2};
      <<"Z">> -> {draw, Move2}
    end
  end.

score_move(Move) ->
  case Move of
        <<"X">> -> 1;
        <<"Y">> -> 2;
        <<"Z">> -> 3
  end.

score_result(Result) ->
  case Result of
        win -> 6;
        draw -> 3;
        loss -> 0
  end.

score_turn({Result, Move}) ->
  score_move(Move) + score_result(Result).

parse_line(Line) ->
  [L,R|_] = string:split(Line, " "),
  {L, R}.

solution_pt1() ->
  {ok, Lines} = read_lines("input/day2.txt"),
  lists:sum(
    lists:map(fun(S) -> score_turn(S) end,
      lists:map(fun(S) -> turn(S) end,
      lists:map(fun(S) -> parse_line(S) end, Lines)))).

