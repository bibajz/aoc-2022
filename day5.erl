-module(day5).

-import(utils, [read_lines/1, transpose/1]).

-export([partition_by_len/2, solution_pt1/0, solution_pt2/0, parse_instruction/1]).

partition_by_len_inner(Len, List, AggList) ->
    Prefix = lists:sublist(List, Len),
    case length(Prefix) < Len of
        true ->
            case length(Prefix) == 0 of
                true -> lists:reverse(AggList);
                false -> lists:reverse([Prefix | AggList])
            end;
        false ->
            partition_by_len_inner(Len, lists:nthtail(Len, List), [Prefix | AggList])
    end.

partition_by_len(Len, List) -> partition_by_len_inner(Len, List, []).


construct_stacks(ListOfStrings) ->
  lists:map(fun(LoL) -> lists:filter(fun(L) -> L /= [] end, LoL) end,
      transpose(
        lists:map(fun(L) -> lists:map(fun string:strip/1, L) end,
          lists:map(fun(S) -> partition_by_len(4, S) end, ListOfStrings)))).

move_n(0, From, To) -> {From, To};
move_n(N, From, To) ->
  [H|T] = From,
  move_n(N-1, T, [H|To]).

parse_instruction(Line) ->
  Regex = "move (?<Number>\\d+) from (?<From>\\d+) to (?<To>\\d+)$",
  {ok, MP} = re:compile(Regex),
  {match, [_,{Pos1, Len1},{Pos2, Len2},{Pos3,Len3}]} = re:run(Line, MP),
  {I1, _} = string:to_integer(string:slice(Line, Pos1, Len1)),
  {I2, _} = string:to_integer(string:slice(Line, Pos2, Len2)),
  {I3, _} = string:to_integer(string:slice(Line, Pos3, Len3)),
  {I1, I2, I3}.


move_stacks({N, From, To}, Stacks) ->
    {NewFrom, NewTo} = move_n(N, lists:nth(From, Stacks), lists:nth(To, Stacks)),
    lists:map(
      fun({I, X}) -> case I == From of
          true -> NewFrom;
          false -> case I == To of
              true -> NewTo;
              false -> X
              end
          end
      end, lists:enumerate(Stacks)).

solution_pt1() ->
    {ok, Lines} = read_lines("input/day5.txt"),
    ContainerLines = lists:droplast(lists:takewhile(fun(L) -> L /= "" end, Lines)),
    Stacks = construct_stacks(ContainerLines),

    Instructions = lists:map(fun parse_instruction/1, lists:nthtail(1, lists:dropwhile(fun(L) -> L /= "" end, Lines))),

    lists:concat(lists:map(fun([H|_]) -> H end, lists:foldl(fun(X, Acc) -> move_stacks(X, Acc) end, Stacks, Instructions))).


move_n_pt2(N, From, To) ->
  {FirstN, Tail} = {lists:sublist(From, N), lists:nthtail(N, From)},
  {Tail, FirstN ++ To}.


move_stacks_pt2({N, From, To}, Stacks) ->
    {NewFrom, NewTo} = move_n_pt2(N, lists:nth(From, Stacks), lists:nth(To, Stacks)),
    lists:map(
      fun({I, X}) -> case I == From of
          true -> NewFrom;
          false -> case I == To of
              true -> NewTo;
              false -> X
              end
          end
      end, lists:enumerate(Stacks)).


solution_pt2() ->
    {ok, Lines} = read_lines("input/day5.txt"),
    ContainerLines = lists:droplast(lists:takewhile(fun(L) -> L /= "" end, Lines)),
    Stacks = construct_stacks(ContainerLines),

    Instructions = lists:map(fun parse_instruction/1, lists:nthtail(1, lists:dropwhile(fun(L) -> L /= "" end, Lines))),

    lists:concat(lists:map(fun([H|_]) -> H end, lists:foldl(fun(X, Acc) -> move_stacks_pt2(X, Acc) end, Stacks, Instructions))).
