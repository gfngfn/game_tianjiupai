-module('SesterlStdlib.List').
-export(['map'/2, 'filter'/2, 'filter_map'/2, 'for_each'/2, 'foldl'/3, 'foldl_effect'/3, 'foldr'/3, 'all'/2, 'any'/2, 'append'/2, 'reverse'/1, 'length'/1]).

    map(F, Xs) ->
        lists:map(F, Xs).
  

    filter(P, Xs) ->
        lists:filter(P, Xs).
  

    filter_map(F, Xs) ->
        lists:filtermap(
            fun(X) ->
                case F(X) of
                    error   -> false;
                    {ok, Y} -> {true, Y}
                end
            end,
            Xs).
  

    for_each(F, Xs) ->
        lists:foreach(F, Xs).
  

    foldl(F, I, Xs) ->
        lists:foldl(fun(X, AccIn) -> F(AccIn, X) end, I, Xs).
  

    foldl_effect(F, I, Xs) ->
        lists:foldl(fun(X, AccIn) -> F(AccIn, X) end, I, Xs).
  

    foldr(F, I, Xs) ->
        lists:foldr(F, I, Xs).
  

    all(P, Xs) ->
        lists:all(P, Xs).
  

    any(P, Xs) ->
        lists:any(P, Xs).
  

    append(List1, List2) ->
        lists:append(List1, List2).
  

    reverse(List) ->
        lists:reverse(List).
  

    length(List) ->
        erlang:length(List).
  
