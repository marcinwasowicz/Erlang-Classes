Modules PollutionData.ex and PollutionDataStreams.ex are Elixir modules capable of reading pollution data from CSV files and 
loading them into pollution_server from pollution_app. Both modules do exactly the same job, but PollutionData.exuses only 
Elixir enumerables while PollutionDataStreams.ex does most of its work on Elixir streams and uses enumerables only to "run" 
final streams.
