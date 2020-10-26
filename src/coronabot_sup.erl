-module(coronabot_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
    {
     coronabot_slack_sup,
     {coronabot_slack_sup, start_link, []},
     permanent,
     3000,
     supervisor,
     [coronabot_slack_sup]
    }
    ],
    {ok, { {one_for_one, 5, 10}, Children} }.
