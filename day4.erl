-module(day4).

-export([read_lines/1, seq_from_range/1, are_overlapping/2, solution_pt1/0]).

read_lines(FileName) ->
  case file:read_file(FileName) of
    {ok, Text} -> {ok, lists:droplast(string:split(binary_to_list(Text), "\n", all))};
    {error, Reason} -> {error, Reason}
  end.

seq_from_range(Range) ->
  [{L, _}, {R, _}] = lists:map(fun(S) -> string:to_integer(S) end, lists:sublist(string:split(Range, "-", all), 2)),
  lists:seq(L, R).


are_overlapping(List1, List2) ->
  {S1, S2} = {sets:from_list(List1), sets:from_list(List2)},
  sets:is_subset(S1, S2) or sets:is_subset(S2, S1) or false.

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
    lists:map(fun({LSeq, RSeq}) -> are_overlapping(LSeq, RSeq) end,
    lists:map(fun({LString, RString}) -> {seq_from_range(LString), seq_from_range(RString)} end,
      lists:map(fun(L) -> [LString,RString|_] = string:split(L, ",", all), {LString, RString} end, Lines))))).
