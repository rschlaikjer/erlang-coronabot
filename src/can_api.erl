-module(can_api).
-compile(export_all).
-include_lib("include/records.hrl").

api_key() ->
    {ok, CANInfo} = application:getenv(coronabot, covidactnow),
    proplists:get_value(api_key, CANInfo).

get_actuals_record_with_date(Date, Actuals) ->
    lists:foldl(
      fun(R, AccIn) ->
            case R#ts_actuals.date =:= Date of
                true -> R;
                false -> AccIn
            end
      end,
      not_found,
      Actuals
    ).

merge_timeseries(Metrics, Actuals) ->
    % For each datum in the metrics list, try to look up + merge actuals
    Merged = lists:foldl(
        fun(R, AccIn) ->
            Date = R#ts_metrics.date,
            Matched = get_actuals_record_with_date(Date, Actuals),
            R1 = #merged_timeseries{
                date=R#ts_metrics.date,
                positivity=R#ts_metrics.positivity,
                density=R#ts_metrics.density,
                infection_rate=R#ts_metrics.infection_rate
            },
            R2 = case Matched of
                not_found -> R1;
                _ ->
                    R1#merged_timeseries{
                        cases=Matched#ts_actuals.cases,
                        deaths=Matched#ts_actuals.deaths,
                        positive_tests=Matched#ts_actuals.positive_tests,
                        negative_tests=Matched#ts_actuals.negative_tests
                    }
            end,
            [R2|AccIn]
        end,
        [],
        Metrics
    ),
    lists:reverse(Merged).

null_sub(N1, N2) when is_integer(N1) andalso is_integer(N2) ->
    N1 - N2;
null_sub(_, _) -> null.

null_add(N1, N2) when is_integer(N1) andalso is_integer(N2) ->
    N1 + N2;
null_add(N1, _) when is_integer(N1) ->
    N1;
null_add(_, N2) when is_integer(N2) ->
    N2;
null_add(_, _) ->
    null.

takeprefix(N, List) ->
    takeprefix(N, List, []).
takeprefix(_, [], Acc) -> lists:reverse(Acc);
takeprefix(0, _, Acc) -> lists:reverse(Acc);
takeprefix(N, [I|Items], Acc) ->
    takeprefix(N-1, Items, [I|Acc]).

fill_daily_stats(Merged) ->
    {Hd, Tl} = {hd(Merged), tl(Merged)},
    {_, _, _, Ret} = lists:foldl(
      fun(Record, {LastRecord, CaseList, DeathList, Acc}) ->
        % Update the case/death running average lists
        RawNewCases = null_sub(Record#merged_timeseries.cases, LastRecord#merged_timeseries.cases),
        RawNewDeaths = null_sub(Record#merged_timeseries.deaths, LastRecord#merged_timeseries.deaths),
        NewCases = case RawNewCases < 0 of true -> 0; false -> RawNewCases end,
        NewDeaths = case RawNewDeaths < 0 of true -> 0; false -> RawNewDeaths end,
        CaseList1 = [NewCases|CaseList],
        DeathList1 = [NewDeaths|DeathList],
        CaseList2 = takeprefix(7, CaseList1),
        DeathList2 = takeprefix(7, DeathList1),
        Cases7Day = lists:foldl(fun can_api:null_add/2, 0, CaseList2) / 7,
        Deaths7Day = lists:foldl(fun can_api:null_add/2, 0, DeathList2) / 7,
        Updated = Record#merged_timeseries{
            new_cases=NewCases,
            new_deaths=NewDeaths,
            cases_7day=Cases7Day,
            deaths_7day=Deaths7Day
        },
        {Record, CaseList2, DeathList2, [Updated|Acc]}
      end,
      {Hd, [], [], []},
      Tl
    ),
    lists:reverse(Ret).

merged_timeseries_to_rows(Merged) ->
    [io_lib:format("~s ~p ~p ~p ~p ~p ~p ~p ~p ~p ~p ~p~n",
        [M#merged_timeseries.date,
         M#merged_timeseries.positivity,
         M#merged_timeseries.density,
         M#merged_timeseries.infection_rate,
         M#merged_timeseries.cases,
         M#merged_timeseries.deaths,
         M#merged_timeseries.positive_tests,
         M#merged_timeseries.negative_tests,
         M#merged_timeseries.new_cases,
         M#merged_timeseries.new_deaths,
         M#merged_timeseries.cases_7day,
         M#merged_timeseries.deaths_7day
        ])
        || M <- Merged].

parse_json(Json) ->
    Metrics = proplists:get_value(<<"metrics">>, Json),
    MetricsTimeseries = proplists:get_value(<<"metricsTimeseries">>, Json),
    ActualsTimeseries = proplists:get_value(<<"actualsTimeseries">>, Json),
    MetricsRecords = [parse_ts_metric(M) || M <- MetricsTimeseries],
    ActualsRecords = [parse_ts_actuals(M) || M <- ActualsTimeseries],
    #metrics{
        country=proplists:get_value(<<"country">>, Json),
        state=proplists:get_value(<<"state">>, Json),
        county=proplists:get_value(<<"county">>, Json),
        positivity=proplists:get_value(<<"testPositivityRatio">>, Metrics),
        density=proplists:get_value(<<"caseDensity">>, Metrics),
        infection_rate=proplists:get_value(<<"infectionRate">>, Metrics),
        metrics_ts=MetricsRecords,
        actuals_ts=ActualsRecords
    }.

parse_ts_metric(Json) ->
    #ts_metrics{
        date=proplists:get_value(<<"date">>, Json),
        positivity=proplists:get_value(<<"testPositivityRatio">>, Json),
        density=proplists:get_value(<<"caseDensity">>, Json),
        infection_rate=proplists:get_value(<<"infectionRate">>, Json)
    }.

parse_ts_actuals(Json) ->
    #ts_actuals{
        date=proplists:get_value(<<"date">>, Json),
        cases=proplists:get_value(<<"cases">>, Json),
        deaths=proplists:get_value(<<"deaths">>, Json),
        positive_tests=proplists:get_value(<<"positiveTests">>, Json),
        negative_tests=proplists:get_value(<<"negativeTests">>, Json)
    }.

state_hist(FipsCode) ->
    Url = "https://api.covidactnow.org/v2/state/" ++ FipsCode
            ++ ".timeseries.json?apiKey=" ++ api_key(),
    Headers = [],
    Request = {Url, Headers},
    HttpOptions = [{autoredirect, false}],
    Options = [{body_format, binary}],
    case httpc:request(get, Request, HttpOptions, Options) of
        {error, Reason} ->
            {error, Reason};
        {ok, {{_, 503, _}, _RespHeaders, _RespBody}} ->
            {error, service_unavailable};
        {ok, {{_, 200, _}, _RespHeaders, RespBody}} ->
            Json = jsx:decode(RespBody),
            {ok, Json};
        {ok, {{_, Code, _}, _RespHeaders, RespBody}} ->
            {error, {Code, RespBody}}
    end.
