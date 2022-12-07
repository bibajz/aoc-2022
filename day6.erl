-module(day6).

-import(utils, [read_lines/1]).

-export([solution_pt1/0, solution_pt2/0]).

rolling_window_inner(_, AggList, SubList, []) -> lists:reverse([SubList | AggList]);
rolling_window_inner(N, AggList, SubList, [H|T]) ->
  case length(SubList) < N of
      true -> rolling_window_inner(N, AggList, SubList ++ [H], T);
      false -> [_|Rest] = SubList, rolling_window_inner(N, [SubList | AggList], Rest ++ [H], T)
  end.
rolling_window(N, List) -> rolling_window_inner(N, [], [], List).

solution_pt1() ->
  {ok, [Line|_]} = read_lines("input/day6.txt"),
  [{Pos, Signal}|_] = lists:filter(fun({_, X}) -> length(lists:uniq(X)) == length(X) end, lists:enumerate(rolling_window(4, Line))),
  {Pos + 3, Signal}.


solution_pt2() ->
  {ok, [Line|_]} = read_lines("input/day6.txt"),
  [{Pos, Signal}|_] = lists:filter(fun({_, X}) -> length(lists:uniq(X)) == length(X) end, lists:enumerate(rolling_window(14, Line))),
  {Pos + 13, Signal}.
