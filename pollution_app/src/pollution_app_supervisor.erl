%%%-------------------------------------------------------------------
%% @doc pollution_app top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(pollution_app_supervisor).

-behaviour(supervisor).

-export([(start_link/1),(init/1)]).

start_link(InitialValue) ->
    supervisor:start_link({local, pollution_supervisor}, ?MODULE, [InitialValue]).

init(InitialValue)->
    Strategy = #{strategy => one_for_one, intensity => 2, period => 3},
    Children = [#{
                    id => pol_srvr,
                    start => {pollution_app_server, start_link, InitialValue},
                    restart => permanent,
                    shutdown => 2000,
                    type => worker,
                    modules => [pollution_gen_server]
                }],
    {ok, {Strategy, Children}}.
