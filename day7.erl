-module(day7).

-import(utils, [read_lines/1]).

-export([solution_pt1/0, solution_pt2/0]).

collect_until_inner(_, [], AggList, SubList) ->
    lists:reverse([lists:reverse(SubList) | AggList]);
collect_until_inner(Pred, [H | T], AggList, SubList) ->
    case Pred(H) of
        true ->
            case SubList of
                [] -> collect_until_inner(Pred, T, AggList, [H]);
                _ -> collect_until_inner(Pred, T, [lists:reverse(SubList) | AggList], [H])
            end;
        false ->
            collect_until_inner(Pred, T, AggList, [H | SubList])
    end.

collect_until(Pred, List) -> collect_until_inner(Pred, List, [], []).

parse_ls_line(Line) ->
    [DirOrSize, Name | _] = string:split(Line, " ", all),
    case string:to_integer(DirOrSize) of
        {error, _} -> {Name, dir};
        {Int, _} -> {Name, Int}
    end.

ls_output_to_map(Lines) ->
    maps:from_list(lists:map(fun parse_ls_line/1, Lines)).

parse_lines(Lines) ->
    [H | T] = Lines,
    case T of
        [] -> {cd, lists:last(string:split(H, " ", all))};
        _ -> {ls, ls_output_to_map(T)}
    end.

recursive_get([Key], MapOfMaps) ->
    maps:get(Key, MapOfMaps);
recursive_get([H | T], MapOfMaps) ->
    recursive_get(T, maps:get(H, MapOfMaps)).

recursive_update([Key], Value, MapOfMaps) ->
    maps:update(Key, Value, MapOfMaps);
recursive_update([H | T], Value, MapOfMaps) ->
    maps:update(H, recursive_update(T, Value, maps:get(H, MapOfMaps)), MapOfMaps).

update_dir_tree(Pointer, DirContent, DirTree) ->
    case recursive_get(Pointer, DirTree) of
        dir -> recursive_update(Pointer, DirContent, DirTree);
        _ -> DirTree
    end.

df(Map) ->
    maps:fold(
        fun(_, V, AccIn) ->
            case is_map(V) of
                true -> AccIn + df(V);
                false -> AccIn + V
            end
        end,
        0,
        Map
    ).

all_dirnames_inner(MapOfMaps, [], []) ->
    DirPointers = lists:map(
        fun({K, _}) -> [K] end, lists:filter(fun({_, V}) -> is_map(V) end, maps:to_list(MapOfMaps))
    ),
    all_dirnames_inner(MapOfMaps, DirPointers, DirPointers);
all_dirnames_inner(_, AggList, []) ->
    AggList;
all_dirnames_inner(MapOfMaps, AggList, [H | T]) ->
    DirPointers = lists:map(
        fun({K, _}) -> H ++ [K] end,
        lists:filter(fun({_, V}) -> is_map(V) end, maps:to_list(recursive_get(H, MapOfMaps)))
    ),
    all_dirnames_inner(MapOfMaps, AggList ++ DirPointers, T ++ DirPointers).

all_dirnames(Map) -> all_dirnames_inner(Map, [], []).

apply_instruction({cd, "/"}, {_, DirTree}) ->
    {["/"], DirTree};
apply_instruction({cd, ".."}, {Pointer, DirTree}) ->
    {lists:droplast(Pointer), DirTree};
apply_instruction({cd, S}, {Pointer, DirTree}) ->
    {Pointer ++ [S], DirTree};
apply_instruction({ls, DirContent}, {Pointer, DirTree}) ->
    {Pointer, update_dir_tree(Pointer, DirContent, DirTree)}.

solution_pt1() ->
    {ok, Lines} = read_lines("input/day7.txt"),
    Instructions = lists:map(
        fun parse_lines/1,
        collect_until(
            fun(L) ->
                [H | _] = L,
                H == $$
            end,
            Lines
        )
    ),
    {_, DirTree} = lists:foldl(fun apply_instruction/2, {[], #{"/" => dir}}, Instructions),
    lists:sum(
        lists:filter(
            fun(X) -> X =< 100000 end,
            lists:map(fun(D) -> df(recursive_get(D, DirTree)) end, all_dirnames(DirTree))
        )
    ).

solution_pt2() ->
    {ok, Lines} = read_lines("input/day7.txt"),
    Instructions = lists:map(
        fun parse_lines/1,
        collect_until(
            fun(L) ->
                [H | _] = L,
                H == $$
            end,
            Lines
        )
    ),
    {_, DirTree} = lists:foldl(fun apply_instruction/2, {[], #{"/" => dir}}, Instructions),

    TotalSize = df(DirTree),
    NeededToSave = TotalSize - 40000000,
    


    lists:min(
        lists:filter(
            fun(X) -> X >= NeededToSave end,
            lists:map(fun(D) -> df(recursive_get(D, DirTree)) end, all_dirnames(DirTree))
        )
    ).
