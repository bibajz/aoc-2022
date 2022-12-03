-module(day3).

-export([read_lines/1, solution_pt1/0]).

len([_|T]) -> 1 + len(T);
len([]) -> 0.

read_lines(FileName) ->
  case file:read_file(FileName) of
    {ok, Text} -> {ok, lists:droplast(string:split(binary_to_list(Text), "\n", all))};
    {error, Reason} -> {error, Reason}
  end.


split_on_len(List, Len) ->
  {lists:sublist(List, 1, Len), lists:sublist(List, 1+Len, Len)}.

split_in_half(List) -> split_on_len(List, round(len(List)/2)).


first_in_both_inner([H|T], Set) ->
  case sets:is_element(H, Set) of 
    true -> {ok, H};
    false -> first_in_both_inner(T, Set)
  end;
first_in_both_inner([], _) -> {error, "Disjoint!"}.

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
    lists:map(fun({L1, L2}) -> {ok, C} = first_in_both(L1, L2), C end,
      lists:map(fun(L) -> split_in_half(L) end, Lines)))).
