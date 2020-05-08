-module(supervisor_and_server_tests).
-include_lib("eunit/include/eunit.hrl").
-export[(run_test/0), (dataManipulation/0), (dataRetrieval/0)].

dataManipulation() ->
    ?assertEqual(ok,pollution_app_server:addStation("Station1", {12,34})),
    ?assertEqual(ok,pollution_app_server:addStation("Station2", {13,56})),
    ?assertEqual(ok,pollution_app_server:addStation("Station3", {15,68})),
    ?assertEqual(ok,pollution_app_server:addValue("Station1", "Ozone", {{2020,4,13}, {18, 10, 10}}, 59)),
    ?assertEqual(ok,pollution_app_server:addValue("Station2", "Ozone", {{2020,4,13}, {18, 10, 10}}, 68)),
    ?assertEqual(ok,pollution_app_server:addValue("Station3", "Ozone", {{2020,4,13}, {18, 10, 10}}, 68)),
    ?assertEqual(ok,pollution_app_server:addValue("Station3", "Ozone", {{2020,4,13}, {18, 50, 10}}, 99)),
    ?assertEqual(ok,pollution_app_server:addValue("Station1", "Temperature", {{2020,4,13}, {18, 10, 10}}, 15)),
    ?assertEqual(ok,pollution_app_server:addValue("Station2", "Temperature", {{2020,4,13}, {18, 10, 10}}, 19)),
    ?assertEqual(ok,pollution_app_server:addValue("Station3", "Temperature", {{2020,4,13}, {18, 10, 10}}, 18)),
    ?assertEqual(ok,pollution_app_server:addValue("Station3", "Temperature", {{2020,4,13}, {18, 10, 10}}, 36)),
    ?assertEqual(ok,pollution_app_server:removeValue("Station2", "Temperature",{{2020,4,13}, {18, 10, 10}})).

dataRetrieval() ->
    ?assertEqual(99, pollution_app_server:getOneValue("Station3", "Ozone", {{2020,4,13}, {18,50,10}})),
    ?assertEqual(83.5, pollution_app_server:getStationMean("Station3", "Ozone")),
    ?assertEqual(16.5, pollution_app_server:getDailyMean("Temperature", {2020,4,13})),
    ?assertEqual(73.5,pollution_app_server:getDailyMean("Ozone", {2020,4,13})),
    ?assertEqual(99, pollution_app_server:getMaxOfTypeStation("Station3", "Ozone")),
    ?assertEqual({"Ozone", 2}, pollution_app_server:getMostFrequentTypeStation("Station3")).

run_test()->
    pollution_app_supervisor:start_link(#{}),
    ?assert(lists:member(pollution_server, registered())),
    ?assert(lists:member(pollution_supervisor, registered())),
    dataManipulation(),
    dataRetrieval(),
    pollution_app_server:stop(),
    ?assert(lists:member(pollution_server, registered())),
    ?assert(lists:member(pollution_supervisor, registered())).