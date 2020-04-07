-module(exercises).
-export([power/2, contains/2, duplicate/1, sumFloats/1, sumFloatsAccum/2]).

power(X, Y) -> 
    case Y of
        0->1;
        _ -> X*power(X, Y-1)    
    end.


contains([], _Val) -> false;
contains([Head | Tail], Head) -> true;
contains([_Head | Tail], Val) -> contains(Tail, Val).


duplicate([]) -> [];
duplicate([Head | Tail]) -> [Head, Head] ++ duplicate(Tail).

sumFloats(List) -> sumFloatsAccum(List, 0.0).

sumFloatsAccum([], Acc) -> Acc;
sumFloatsAccum([Head | Tail], Acc) -> 
    if
        is_float(Head) -> sumFloatsAccum(Tail, Acc + Head);
        true -> sumFloatsAccum(Tail, Acc)
    end.