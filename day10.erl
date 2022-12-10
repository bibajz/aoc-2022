-module(day10).

-import(utils, [read_lines/1]).

-export([solution_pt1/0]).


parse_line(Line) ->
    Split = string:split(Line, " ", all),
    case length(Split) of
        1 -> {noop, {}};
        2 -> [_,Num|_] = Split,
             {Int, _} = string:to_integer(Num),
             {add, Int}
    end.


liftToList(MaybeList) ->
    case is_list(MaybeList) of
        true -> MaybeList;
        false -> [MaybeList]
    end.

apply_noop({Cycle, Value}) -> {Cycle + 1, Value}.
apply_fastadd(ToAdd, {Cycle, Value}) -> {Cycle + 1, Value + ToAdd}.
apply_add(ToAdd, {Cycle, Value}) -> New = apply_noop({Cycle, Value}), [New, apply_fastadd(ToAdd, New)].


apply_instruction({Instruction, Payload}, State) ->
    case Instruction of
        noop -> liftToList(apply_noop(State));
        add ->  apply_add(Payload, State)
    end.

solution_pt1() ->
    {ok, Lines} = read_lines("input/day10.txt"),
    Instructions = lists:map(fun parse_line/1, Lines),
    
    InitState = [{1, 1}],

    StateEvolution = lists:foldl(
      fun(X, Acc) -> Acc ++ apply_instruction(X, lists:last(Acc)) end,
      InitState,
      Instructions
    ),

    OfInterest = sets:from_list([20, 60, 100, 140, 180, 220]),

    Periods = lists:filter(fun({I, _}) -> sets:is_element(I, OfInterest) end, StateEvolution),

    lists:foldl(
        fun({Cycle, Val}, Acc) -> Acc + Cycle * Val end,
        0,
        Periods
    ).

