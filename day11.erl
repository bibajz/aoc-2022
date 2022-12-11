-module(day11).

-import(utils, [read_lines/1, partition_by_len/2]).

-export([to_eval/1, solution_pt1/0]).

floor_div(Num, Denom) -> round(math:floor(Num / Denom)).

to_int(String) ->
    {Int, _} = string:to_integer(String),
    Int.

to_eval(Expr) ->
    ErlExpr = lists:concat(
        lists:join(" ", lists:map(fun string:titlecase/1, string:split(Expr ++ ".", " ", all)))
    ),
    {ok, Tokens, _} = erl_scan:string(ErlExpr),
    {ok, Ast} = erl_parse:parse_exprs(Tokens),
    fun(Old) ->
        B = erl_eval:add_binding('Old', Old, erl_eval:new_bindings()),
        {value, Result, _} = erl_eval:exprs(Ast, B),
        Result
    end.

monkey_number(Line) ->
    % Monkey 5:
    [_, B] = string:split(Line, " ", all),
    to_int(B).

monkey_items(Line) ->
    % Starting items: 1, 2, 3, 4
    NumList = lists:nthtail(length("  Starting items: "), Line),
    lists:map(fun to_int/1, lists:map(fun string:strip/1, string:split(NumList, ",", all))).

monkey_operation(Line) ->
    % Operation: new = 2 * old
    [_, B] = string:split(Line, "=", all),
    to_eval(string:strip(B)).

monkey_test(Line) ->
    % Test: divisible by 123
    A = string:split(Line, " ", all),
    to_int(lists:last(A)).

monkey_throw_to(Line) ->
    % If true/false: throw to monkey 123
    A = string:split(Line, " ", all),
    to_int(lists:last(A)).

parse_monkey_state([A, B, C, D, E, F]) ->
    {
        monkey_number(A),
        monkey_items(B),
        monkey_operation(C),
        monkey_test(D),
        monkey_throw_to(E),
        monkey_throw_to(F)
    }.

floor_div3(Num) -> floor_div(Num, 3).
to_div_test(Div) -> fun(X) -> X rem Div == 0 end.

split_on_inner(_, [], Truthy, Falsy) ->
    {lists:reverse(Truthy), lists:reverse(Falsy)};
split_on_inner(Pred, [H|T], Truthy, Falsy) ->
    case Pred(H) of
        true -> split_on_inner(Pred, T, [H|Truthy], Falsy);
        false -> split_on_inner(Pred, T, Truthy, [H|Falsy])
    end.
split_on(Pred, List) -> split_on_inner(Pred, List, [], []).

throw_items(M1, M2, M3) ->
    {Ix1, Items1, Fn1, Div1, TT1, TF1} = M1,
    {Ix2, Items2, Fn2, Div2, TT2, TF2} = M2,
    {Ix3, Items3, Fn3, Div3, TT3, TF3} = M3,

    NewItems = lists:map(fun floor_div3/1, lists:map(fun(X) -> Fn1(X) end, Items1)),
    {ToTrueItems, ToFalseItems} = split_on(fun(X) -> (to_div_test(Div1))(X) end, NewItems),
    {
        {Ix1, [], Fn1, Div1, TT1, TF1},
        {Ix2, Items2 ++ ToTrueItems, Fn2, Div2, TT2, TF2},
        {Ix3, Items3 ++ ToFalseItems, Fn3, Div3, TT3, TF3}
    }.



advance_state(N, MonkeyState) ->
    Current = maps:get(N, MonkeyState),
    {_, _, _, _, ToTrueIx, ToFalseIx} = Current,
    {ToTrue, ToFalse} = {maps:get(ToTrueIx, MonkeyState), maps:get(ToFalseIx, MonkeyState)},
    {New, NewToTrue, NewToFalse} = throw_items(Current, ToTrue, ToFalse),
    maps:merge(MonkeyState, #{N => New, ToTrueIx => NewToTrue, ToFalseIx => NewToFalse}).

turns([], MonkeyState) -> [];
turns([H | T], MonkeyState) ->
    NewState = advance_state(H, MonkeyState),
    [NewState] ++ turns(T, NewState).

solution_pt1() ->
    {ok, Lines} = read_lines("input/day11.txt"),
    Init = maps:from_list(
        lists:map(
            fun(X) ->
                {Ix, _, _, _, _, _} = X,
                {Ix, X}
            end,
            lists:map(
                fun parse_monkey_state/1,
                partition_by_len(6, lists:filter(fun(L) -> L /= "" end, Lines))
            )
        )
    ),

    ThrowEvolution = lists:foldl(
        fun(_, Acc) -> Acc ++ turns(lists:seq(0, maps:size(Init) - 1), lists:last(Acc)) end,
        [Init],
        lists:seq(1, 20)
    ),

    ByMonkeys = maps:groups_from_list(
        fun({I, _}) -> I rem maps:size(Init) end,
        fun({I, X}) ->
            {_, Items, _, _, _, _} = maps:get(I rem maps:size(Init), X),
            length(Items)
        end,
        lists:enumerate(0, lists:droplast(ThrowEvolution))
    ),

    NumInspectionByMonkey =  maps:map(fun(K, V) -> lists:sum(V) end, ByMonkeys),

    [N1, N2|_] = lists:reverse(lists:sort(maps:values(NumInspectionByMonkey))),
    N1 * N2.
