-module(pollution).
-export[(createMonitor/0), (addStation/3), (addValueInside/4), (addValue/5), (removeValueInside/3), (removeValue/4), (getOneValueInside/3),
(getOneValue/4), (getStationMean/3), (getMean/2),(getMapSize/2), (getDailyStationSum/3), (getDailyMean/3), (getDailyStationLen/3)
,(getMaxOfTypeStation/3),(maxx/2), (getMostFrequentTypeStation/2)].

maxx(ninf, N) -> N;
maxx(N, ninf) -> N;
maxx(N, S) -> erlang:max(N, S).

createMonitor()->#{}.

getMapSize(Pred, Map)->
    maps:fold(Pred, 0, Map).

getMean(_, 0) ->0;
getMean(Sum, Len)->Sum/Len.

addStation(Name, Coords, M)->
    case M of
        #{Coords := _} -> M;
        #{Name := _} -> M;
        _ -> M#{Coords => Name, Name => #{}}
    end.

addValueInside({A, B}, T, Val, M)->
    case maps:is_key({A, B}, M) of
        false -> M;
        _->
            case maps:get(maps:get({A, B}, M), M) of
                #{T := _} -> M;
                _ -> M#{maps:get({A, B}, M) := maps:put(T, Val, maps:get(maps:get({A, B}, M), M))}
            end
    end;

addValueInside(Name, T,Val,  M) ->
    case maps:is_key(Name, M) of
        false -> M;
        _ -> 
            case maps:get(Name, M) of
                #{T := _} -> M;
                _ -> M#{Name := maps:put(T, Val, maps:get(Name, M))}
            end
    end.

addValue(Key, Type, Date, Val, M)->
    addValueInside(Key, {Type, Date}, Val, M).

removeValueInside({A, B}, T, M)->
    M#{maps:get({A, B}, M) := maps:remove(T,maps:get(maps:get({A, B},M),M))};

removeValueInside(Name, T, M)->
    M#{Name := maps:remove(T, maps:get(Name, M))}.

removeValue(Key, Type, Date, M)->
    removeValueInside(Key, {Type, Date}, M).

getOneValueInside(Name,T, M)->
    maps:get(T, maps:get(Name, M)).

getOneValue(Name, Type, Date, M)->
    getOneValueInside(Name, {Type, Date}, M).

getStationMean(Name, Type, M)->
    getMean(
        getMapSize(
            fun(K, V, Acc)->
                case K of
                    {Type, _} -> Acc + V;
                    _ -> Acc
                end
            end, 
        maps:get(Name, M)), 
        getMapSize(
            fun(K, _, Acc)->
                case K of
                    {Type, _} -> Acc + 1;
                    _ -> Acc
                end
            end, 
        maps:get(Name, M))
    ).

getDailyStationSum(Type, Day, Map)->
    getMapSize(
        fun(K, V, Acc)->
            case K of
                {Type, {Day, _}} -> Acc + V;
                _ -> Acc
            end
        end, 
    Map).

getDailyStationLen(Type, Day, Map)->
    getMapSize(
        fun(K, _, Acc)->
            case K of
                {Type, {Day, _}} -> Acc + 1;
                _ -> Acc
            end
        end, 
    Map).

getDailyMean(Type, Day, M)->
    getMean(
        getMapSize(
            fun(_, V, Acc)->
                case V of
                    #{} -> Acc + getDailyStationSum(Type, Day, V);
                    _-> Acc
                end
            end, 
        M),
        getMapSize(
            fun(_, V, Acc)-> 
                case V of
                    #{} -> Acc + getDailyStationLen(Type, Day, V);
                    _ -> Acc
                end
            end, 
        M)
    ).

getMaxOfTypeStation(Name, Type, M)->
    maps:fold(
            fun(K, V, Acc)->
                case K of
                    {Type, _} -> maxx(V, Acc);
                    _ -> Acc
                end
            end, 
        ninf, maps:get(Name, M)
    ).

getMostFrequentTypeStation(Name, M)->
    TypeCounts = maps:fold(
                        fun({Type, _}, _, Acc)->
                            case Acc of
                                #{Type := _} -> Acc#{Type := maps:get(Type, Acc) + 1};
                                _ -> Acc#{Type => 1}
                            end
                        end,
                    #{}, maps:get(Name, M)),
    maps:fold(
            fun(K, V, {Type, Count}) ->
                case Type of
                    undefined -> {K, V};
                    _ ->
                        if
                            V > Count -> {K, V};
                            true -> {Type, Count}
                        end
                end
        end, {undefined, undefined}, TypeCounts).