-module(serverTests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

startTest() ->
    server:start(),
    ?assert(lists:member(pollutionServer, registered())).

dataManipulationTests() ->
    ?assertEqual(ok,server:addStation("Station1", {12,34})),
    ?assertEqual(ok,server:addStation("Station2", {13,56})),
    ?assertEqual(ok,server:addStation("Station3", {15,68})),
    ?assertEqual(ok,server:addValue("Station1", "Ozone", {{2020,4,13}, {18, 10, 10}}, 59)),
    ?assertEqual(ok,server:addValue("Station2", "Ozone", {{2020,4,13}, {18, 10, 10}}, 68)),
    ?assertEqual(ok,server:addValue("Station3", "Ozone", {{2020,4,13}, {18, 10, 10}}, 68)),
    ?assertEqual(ok,server:addValue("Station3", "Ozone", {{2020,4,13}, {18, 50, 10}}, 99)),
    ?assertEqual(ok,server:addValue("Station1", "Temperature", {{2020,4,13}, {18, 10, 10}}, 15)),
    ?assertEqual(ok,server:addValue("Station2", "Temperature", {{2020,4,13}, {18, 10, 10}}, 19)),
    ?assertEqual(ok,server:addValue("Station3", "Temperature", {{2020,4,13}, {18, 10, 10}}, 18)),
    ?assertEqual(ok,server:addValue("Station3", "Temperature", {{2020,4,13}, {18, 10, 10}}, 36)),
    ?assertEqual(ok,server:removeValue("Station2", "Temperature",{{2020,4,13}, {18, 10, 10}})).

dataRetrievalTests() ->
    ?assertEqual(99, server:getOneValue("Station3", "Ozone", {{2020,4,13}, {18,50,10}})),
    ?assertEqual(83.5, server:getStationMean("Station3", "Ozone")),
    ?assertEqual(16.5, server:getDailyMean("Temperature", {2020,4,13})),
    ?assertEqual(73.5,server:getDailyMean("Ozone", {2020,4,13})),
    ?assertEqual(99, server:getMaxOfTypeStation("Station3", "Ozone")),
    ?assertEqual({"Ozone", 2}, server:getMostFrequentTypeStation("Station3")).

stopTest() ->
    server:stop(),
    timer:sleep(50),
    ?assert(not lists:member(pollutionServer, registered())).

runTests() ->
    startTest(),
    dataManipulationTests(),
    dataRetrievalTests(),
    stopTest().