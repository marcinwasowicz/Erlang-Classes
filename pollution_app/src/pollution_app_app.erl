%%%-------------------------------------------------------------------
%% @doc pollution_app public API
%% @end
%%%-------------------------------------------------------------------

-module(pollution_app_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _InitialValue) ->
    pollution_app_supervisor:start_link(#{}).

stop(_State) ->
    ok.

%% internal functions
