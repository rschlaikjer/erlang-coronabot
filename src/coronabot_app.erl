-module(coronabot_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    httpc:set_options([{keep_alive_timeout, 15000}]),
    coronabot_sup:start_link().

stop(_State) ->
    ok.
