-module(server).
-export[(start/0), (stop/0), (init/0), (call/2)].
-export[(addStation/2), (addValue/4), (removeValue/3), (getOneValue/3), (getStationMean/2),(getDailyMean/2),(getMaxOfTypeStation/2),
(getMostFrequentTypeStation/1), (printMonitor/0)].

start()->
    register(pollutionServer, spawn(server, init, [])).

init() ->
    loop(pollution:createMonitor()).

loop(Monitor)->
    receive
        {request, Pid, addStation, [Name, Coords]} ->
            Pid ! {reply, ok},
            loop(pollution:addStation(Name, Coords, Monitor));
        {request, Pid, addValue, [Key, Type, Date, Val]} ->
            Pid ! {reply, ok},
            loop(pollution:addValue(Key, Type, Date, Val, Monitor));
        {request, Pid, removeValue, [Key, Type, Date]} ->
            Pid ! {reply, ok},
            loop(pollution:removeValue(Key, Type, Date, Monitor));
        {request, Pid, getOneValue, [Name, Type, Date]}->
            Pid ! {reply, pollution:getOneValue(Name, Type, Date, Monitor)},
            loop(Monitor);
        {request, Pid, getStationMean, [Name, Type]} -> 
            Pid ! {reply, pollution:getStationMean(Name, Type, Monitor)},
            loop(Monitor);
        {request, Pid, getDailyMean, [Type, Day]} ->
            Pid ! {reply, pollution:getDailyMean(Type, Day, Monitor)},
            loop(Monitor);
        {request, Pid, getMaxOfTypeStation, [Name, Type]} ->
            Pid ! {reply, pollution:getMaxOfTypeStation(Name, Type, Monitor)},
            loop(Monitor);
        {request, Pid, getMostFrequentTypeStation, [Name]} ->
            Pid ! {reply, pollution:getMostFrequentTypeStation(Name, Monitor)},
            loop(Monitor);
        {request, Pid, printMonitor, []} ->
            Pid ! {reply, Monitor},
            loop(Monitor);
        {request, Pid, stop, []} ->
            Pid ! {reply, ok}
        end.

call(Query, ArgsList) ->
    pollutionServer ! {request, self(), Query, ArgsList},
    receive
        {reply, Ans} -> Ans
    end.

addStation(Name, Coords) ->
    call(addStation, [Name, Coords]).

addValue(Key, Type, Date, Val) ->
    call(addValue, [Key, Type, Date, Val]).

removeValue(Key, Type, Date) ->
    call(removeValue, [Key, Type, Date]).

getOneValue(Name, Type, Date) ->
    call(getOneValue, [Name, Type, Date]).

getStationMean(Name, Type) -> 
    call(getStationMean, [Name, Type]).

getDailyMean(Type, Day) ->
    call(getDailyMean, [Type, Day]).

getMaxOfTypeStation(Name, Type) ->
    call(getMaxOfTypeStation, [Name, Type]).

getMostFrequentTypeStation(Name) -> 
    call(getMostFrequentTypeStation, [Name]).

printMonitor() ->
    call(printMonitor, []).

stop() ->
    call(stop, []).