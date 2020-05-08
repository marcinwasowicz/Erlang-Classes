-module(pollution_app_server).
-behaviour(gen_server).

-export[(start_link/1), (init/1),(handle_call/3),(handle_cast/2), (terminate/2)].

-export([(stop/0),(addStation/2), (addValue/4), (removeValue/3),(getOneValue/3), (getDailyMean/2), (getStationMean/2)]).
-export([(getMaxOfTypeStation/2), (getMostFrequentTypeStation/1), (raportData/0)]).

start_link(InitialValue)->
    gen_server:start_link({local, pollution_server},?MODULE, InitialValue, []).

init(InitialValue)->
    io:format("============= Server Correctly Initialized ============\n"),
    {ok, InitialValue}.


handle_cast({addStation, Name, Coords}, LoopData)->
    {noreply, pollution:addStation(Name, Coords, LoopData)};

handle_cast({addValue, Key, Type, Date, Val}, LoopData)->
    {noreply, pollution:addValue(Key, Type, Date, Val, LoopData)};

handle_cast({removeValue, Key, Type, Date}, LoopData)->
    {noreply, pollution:removeValue(Key, Type, Date, LoopData)};

handle_cast(stop, LoopData)->
    {stop, normal, LoopData}.


handle_call({getOneValue, Name, Type, Date}, _From, LoopData)->
    {reply, pollution:getOneValue(Name, Type, Date, LoopData), LoopData};

handle_call({getStationMean, Name, Type}, _From, LoopData)->
    {reply, pollution:getStationMean(Name, Type, LoopData), LoopData};

handle_call({getDailyMean, Type, Day}, _From, LoopData)->
    {reply, pollution:getDailyMean(Type, Day, LoopData), LoopData};

handle_call({getMaxOfTypeStation, Name, Type}, _From, LoopData)->
    {reply, pollution:getMaxOfTypeStation(Name, Type,LoopData), LoopData};

handle_call({raportData}, _From, LoopData)->
    {reply, LoopData, LoopData};

handle_call({getMostFrequentTypeStation, Name}, _From, LoopData)->
    {reply, pollution:getMostFrequentTypeStation(Name, LoopData), LoopData}.


terminate(Reason, LoopData)->
    io:format("=========== Server Terminated with value ~p ===========\n", [LoopData]),
    Reason.


addStation(Name, Coords)->
    gen_server:cast(pollution_server, {addStation, Name, Coords}).

addValue(Key, Type, Date, Val)->
    gen_server:cast(pollution_server, {addValue, Key, Type, Date, Val}).

removeValue(Key, Type, Date)->
    gen_server:cast(pollution_server, {removeValue, Key, Type, Date}).

getOneValue(Name, Type, Date)->
    gen_server:call(pollution_server, {getOneValue, Name, Type, Date}).

getStationMean(Name, Type)->
    gen_server:call(pollution_server, {getStationMean, Name, Type}).

getDailyMean(Type, Day)->
    gen_server:call(pollution_server, {getDailyMean, Type, Day}).

getMaxOfTypeStation(Name, Type)->
    gen_server:call(pollution_server, {getMaxOfTypeStation, Name, Type}).

getMostFrequentTypeStation(Name)->
    gen_server:call(pollution_server, {getMostFrequentTypeStation, Name}).

raportData()->
    gen_server:call(pollution_server, {raportData}).


stop()->
    gen_server:cast(pollution_server, stop).