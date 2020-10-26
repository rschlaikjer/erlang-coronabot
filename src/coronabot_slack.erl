-module(coronabot_slack).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).
-compile(export_all).

-include_lib("slack_rtm/include/records.hrl").

-export([start_link/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
    slack_id,
    api_domain,
    slack_token
}).

start_link(SlackId, Token, Domain) ->
    gen_server:start_link(?MODULE, [SlackId, Token, Domain], []).

init([SlackId, SlackToken, ApiDomain]) ->
    lager:info("Starting slack worker with token ~p", [SlackToken]),
    State = #state{
        slack_id=SlackId,
        api_domain=ApiDomain,
        slack_token=SlackToken
    },
    slack_rtm:connect(SlackToken, record, api_url(State, "/api/rtm.start")),
    {ok, State}.

handle_call(Request, From, State) ->
    lager:info("Call ~p From ~p", [Request, From]),
    {reply, ignored, State}.

handle_cast(Msg, State) ->
    lager:info("Cast ~p", [Msg]),
    {noreply, State}.

handle_info({slack_connected, _From, UserId}, State) ->
    {noreply, State};
handle_info({slack_msg, _From, Message}, State) ->
    State1 = handle_slack_message(State, Message),
    {noreply, State1};
handle_info(Info, State) ->
    lager:info("Got info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(OldVsn, State, _Extra) ->
    lager:info("~p updated from vsn ~p", [?MODULE, OldVsn]),
    {ok, State}.

%% Internal functions

api_url(State, Endpoint) ->
    "https://" ++ State#state.api_domain ++ Endpoint.

handle_slack_message(State, Message=#slack_rtm_message{}) ->
    Channel = Message#slack_rtm_message.channel,
    User = Message#slack_rtm_message.user,
    Text = Message#slack_rtm_message.text,
    BotId = Message#slack_rtm_message.bot_id,
    case BotId of
        B when is_binary(B) ->
            ok; % Don't reply to bots
        _ ->
            case message_is_for_coronabot(State, Text, Channel, User) of
                false ->
                    ok;
                true ->
                    spawn(fun() -> handle_command(State, User, Channel, Text) end)
            end
    end,
    State;
handle_slack_message(State, _Message) ->
    State.

% Ignore messages coronabot sent itself
message_is_for_coronabot(_, _, _, <<"USLACKBOT">>) -> false;
% Plain to accept
message_is_for_coronabot(_, <<"!covid ", _/binary>>, _, Msg) when is_binary(Msg) -> true;
message_is_for_coronabot(_, <<"!corona ", _/binary>>, _, Msg) when is_binary(Msg) -> true;
message_is_for_coronabot(_, <<"!covidbot ", _/binary>>, _, Msg) when is_binary(Msg) -> true;
message_is_for_coronabot(_, <<"!coronabot ", _/binary>>, _, Msg) when is_binary(Msg) -> true;
message_is_for_coronabot(_, _, _, _) -> false.

strip_designator(_, <<"!covid ", Rest/binary>>) -> Rest;
strip_designator(_, <<"!corona ", Rest/binary>>) -> Rest;
strip_designator(_, <<"!covidbot ", Rest/binary>>) -> Rest;
strip_designator(_, <<"!coronabot ", Rest/binary>>) -> Rest;
strip_designator(_, Msg) -> Msg.

strip(<<" ", B/binary>>) -> strip(B);
strip(<<B/binary>>) -> B.

handle_command(State, User, Channel, Text) ->
    Command = strip(strip_designator(State, Text)),
    CommandList = binary:split(Command, <<" ">>, [global]),
    case CommandList of
        [] ->
            ok;
        [Action|Args] ->
            handle_command_word(State, User, Channel, Action, Args)
    end.

binary_join(Sep, List) when is_binary(Sep) andalso is_list(List) ->
    binary_join(Sep, tl(List), hd(List)).
binary_join(_Sep, [], Acc) -> Acc;
binary_join(Sep, [L|List], Acc) ->
    binary_join(Sep, List, <<Acc/binary, Sep/binary, L/binary>>).

handle_command_word(State, _User, Channel, <<"help">>, _Args) ->
    respond_help(State, Channel);
handle_command_word(State, _User, Channel, FIPSCode, Args) ->
    case Args of
        [] ->
            respond_chart_daily(State, Channel, FIPSCode);
        [<<"daily">>] ->
            respond_chart_daily(State, Channel, FIPSCode);
        [<<"cumulative">>] ->
            respond_chart_cumulative(State, Channel, FIPSCode);
        [<<"cum">>] ->
            respond_chart_cumulative(State, Channel, FIPSCode);
        [<<"infection">>] ->
            respond_chart_infection(State, Channel, FIPSCode);
        _ ->
            respond_help(State, Channel, [FIPSCode|Args])
    end.

make_url(ChartName) ->
    "https://covid.rhye.org/" ++ ChartName.

respond_chart_daily(State, Channel, FIPS) ->
    lager:info("Generating daily chart for ~s~n", [FIPS]),
    spawn(fun() ->
        case can_api:state_hist(FIPS) of
            {ok, Json} ->
                Metrics = can_api:parse_json(Json),
                {Y, M, D} = date(),
                ChartName = lists:flatten(io_lib:format("~s.daily.~p.~p.~p.png", [FIPS, Y, M, D])),
                OutFile = "/images/" ++ ChartName,
                gnuplot:plot_daily_case_count(Metrics, OutFile),
                Url = make_url(ChartName),
                post_chat_message(State, Channel, list_to_binary(Url));
            {error, {404, _}} ->
                post_chat_message(State, Channel, <<"Failed to query data for '", FIPS/binary, "'">>);
            Other ->
                lager:info("Query failed: ~p~n", [Other])
        end
    end).

respond_chart_cumulative(State, Channel, FIPS) ->
    lager:info("Generating cumulative chart for ~s~n", [FIPS]),
    spawn(fun() ->
        case can_api:state_hist(FIPS) of
            {ok, Json} ->
                Metrics = can_api:parse_json(Json),
                {Y, M, D} = date(),
                ChartName = lists:flatten(io_lib:format("~s.cumulative.~p.~p.~p.png", [FIPS, Y, M, D])),
                OutFile = "/images/" ++ ChartName,
                gnuplot:plot_cum_case_count(Metrics, OutFile),
                Url = make_url(ChartName),
                post_chat_message(State, Channel, list_to_binary(Url));
            {error, {404, _}} ->
                post_chat_message(State, Channel, <<"Failed to query data for '", FIPS/binary, "'">>);
            Other ->
                lager:info("Query failed: ~p~n", [Other])
        end
    end).

respond_chart_infection(State, Channel, FIPS) ->
    lager:info("Generating infection chart for ~s~n", [FIPS]),
    spawn(fun() ->
        case can_api:state_hist(FIPS) of
            {ok, Json} ->
                Metrics = can_api:parse_json(Json),
                {Y, M, D} = date(),
                ChartName = lists:flatten(io_lib:format("~s.infection.~p.~p.~p.png", [FIPS, Y, M, D])),
                OutFile = "/images/" ++ ChartName,
                gnuplot:plot_infection_rate(Metrics, OutFile),
                Url = make_url(ChartName),
                post_chat_message(State, Channel, list_to_binary(Url));
            {error, {404, _}} ->
                post_chat_message(State, Channel, <<"Failed to query data for '", FIPS/binary, "'">>);
            Other ->
                lager:info("Query failed: ~p~n", [Other])
        end
    end).

help_text() ->
    HelpText = "Usage: !coronabot state daily|cumulative|infection\n",
    HelpText.

respond_help(State, Channel) ->
    post_chat_message(State, Channel, list_to_binary(help_text())).
respond_help(State, Channel, Request) ->
    Err = io_lib:format("Bad command '~s'~n~s", [binary_join(<<" ">>, Request), help_text()]),
    post_chat_message(State, Channel, list_to_binary(lists:flatten(Err))).

post_chat_message(State, Channel, Text) when is_binary(Text) ->
    post_chat(State, Channel, [{<<"text">>, Text}]).

post_chat_attachment(State, Channel, Attachments) when is_list(Attachments) ->
    post_chat(State, Channel, [{<<"attachments">>, Attachments}]).

post_chat(State, Channel, Extras) when is_list(Extras) ->
    Json = jsx:encode([
        {<<"channel">>, Channel},
        {<<"as_user">>, false},
        {<<"icon_emoji">>, <<":coronavirus-shitty-version:">>},
        {<<"username">>, <<"Mr. Rona">>},
        {<<"unfurl_links">>, <<"true">>}
    ] ++ Extras),
    RtmToken = State#state.slack_token,
    Headers = [
       {"authorization", binary_to_list(<<"Bearer ", RtmToken/binary>>)}
    ],
    Type = "application/json",
    Url = api_url(State, "/api/chat.postMessage"),
    Result = httpc:request(post, {Url, Headers, Type, Json}, [], []),
    case Result of
        {ok, {{_HttpVer, 200, _Msg}, _ResponseHeaders, _ResponseBody}} ->
            ok;
        {ok, {{_HttpVer, _Code, _Msg}, _ResponseHeaders, ResponseBody}} ->
            lager:info("Unexpected response from API: ~p~n", [ResponseBody]),
            ok
    end.
