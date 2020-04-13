-module(pollutionUnitTests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

setUpStation()->
    S = pollution:createMonitor(),
    S1 = pollution:addStation("Station1", {12,34}, S),
    S2 = pollution:addStation("Station2", {13,56}, S1),
    S3 = pollution:addStation("Station3", {15,68}, S2),
    S4 = pollution:addValue("Station1", "Ozone", {{2020,4,13}, {18, 10, 10}}, 59, S3),
    S5 = pollution:addValue("Station2", "Ozone", {{2020,4,13}, {18, 10, 10}}, 68, S4),
    S6 = pollution:addValue("Station3", "Ozone", {{2020,4,13}, {18, 10, 10}}, 68, S5),
    S7 = pollution:addValue("Station3", "Ozone", {{2020,4,13}, {18, 50, 10}}, 99, S6),
    S8 = pollution:addValue("Station1", "Temperature", {{2020,4,13}, {18, 10, 10}}, 15, S7),
    S9 = pollution:addValue("Station2", "Temperature", {{2020,4,13}, {18, 10, 10}}, 19, S8),
    S10 = pollution:addValue("Station3", "Temperature", {{2020,4,13}, {18, 10, 10}}, 18, S9),
    pollution:addValue("Station3", "Temperature", {{2020,4,13}, {18, 10, 10}}, 36, S10).

valueRetrievalTests()->
    S = setUpStation(),
    ?assertEqual(99, pollution:getOneValue("Station3", "Ozone", {{2020,4,13}, {18,50,10}}, S)),
    ?assertEqual(83.5, pollution:getStationMean("Station3", "Ozone", S)),
    ?assertEqual(73.5,pollution:getDailyMean("Ozone", {2020,4,13}, S)),
    ?assertEqual(99, pollution:getMaxOfTypeStation("Station3", "Ozone", S)),
    ?assertEqual({"Ozone", 2}, pollution:getMostFrequentTypeStation("Station3", S)).

