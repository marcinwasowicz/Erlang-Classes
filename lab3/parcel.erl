-module(parcel).
-export[(generateRandomList/2), (findParcel/2),(calculate/2),(calculateSend/3), (processLoop/2), (cutList/5), 
(paralelize/4), (getCutLength/2), (printTime/1), (measureTime/4)].

cutList([], Acc1, Acc2, _, _) -> Acc1 ++ [Acc2];
cutList(List, Acc1, Acc2, Max, Max) -> cutList(List, Acc1 ++ [Acc2], [], 0, Max);
cutList([H | T], Acc1, Acc2, Num, Max) -> cutList(T, Acc1, Acc2 ++ [H], Num + 1, Max).

getCutLength(Len, Num) -> Len div Num + erlang:min(1, Len rem Num).

generateRandomList(Size, Max)->
    [{rand:uniform(Max), rand:uniform(Max)} || _ <- lists:seq(1, Size)].

findParcel({X, Y}, [{F, S} | T]) ->
    lists:foldl(fun({A, B}, {Ac, Bc})->
        if
            ((X-A)*(X-A) + (Y-B)*(Y-B)) < ((Ac-X)*(Ac-X) + (Bc-Y)*(Bc-Y)) -> {A, B};    
            true -> {Ac, Bc}     
        end 
    end, {F, S}, T).

calculate(People, Parcels) ->
    [{X, findParcel(X, Parcels)} || X <- People].

calculateSend(People, Parcels, Pid) ->
    Pid ! calculate(People, Parcels).

processLoop(0, L) -> L;
processLoop(N, L) ->
    receive
        [X | Y] -> processLoop(N-1, L ++ [X | Y])
    end.

paralelize(People, Parcels, Len, Proc) ->
    [spawn(parcel, calculateSend, [X, Parcels, self()]) || X <- cutList(People, [], [], 0, getCutLength(Len, Proc))],
    processLoop(Proc, []).

printTime({Time, _})->
    io:format("Calculations took ~p~n", [Time]).

measureTime(People, Parcels, Len, Proc)->
    printTime(timer:tc(parcel, paralelize, [People, Parcels, Len, Proc])).

