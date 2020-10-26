-module(coronabot_slack_sup).
-compile([{parse_transform, lager_transform}]).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    join_all(),
    {ok, Pid}.

join_all() ->
    {ok, Slack} = application:get_env(coronabot, slack),
    Tokens = proplists:get_value(api_tokens, Slack),
    lists:foreach(
        fun({Name, Token}) ->
            case Token of
                B when is_binary(B) ->
                    lager:info("Starting slack connection for ~s", [Name]),
                    supervisor:start_child(?MODULE, [Name, Token, "api.slack.com"]);
                {T, Domain} ->
                    lager:info("Starting slack connection for ~s, api domain ~s", [Name, Domain]),
                    supervisor:start_child(?MODULE, [Name, T, Domain])
            end
        end,
        Tokens
    ).

child_spec() -> {
    coronabot_slack,
    {coronabot_slack, start_link, []},
    permanent,
    3000,
    worker,
    [coronabot_slack]
}.

init([]) ->
    {ok, { {simple_one_for_one, 5, 10}, [child_spec()]} }.
