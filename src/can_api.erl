-module(can_api).
-compile(export_all).
-compile([{parse_transform, lager_transform}]).
-include_lib("include/records.hrl").

api_key() ->
    {ok, CANInfo} = application:get_env(coronabot, covidactnow),
    proplists:get_value(api_key, CANInfo).

get_metrics_record_with_date(Date, Metrics) ->
    lists:foldl(
      fun(R, AccIn) ->
            case R#ts_metrics.date =:= Date of
                true -> R;
                false -> AccIn
            end
      end,
      not_found,
      Metrics
    ).

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

safe_div(Num, Denom) when (is_integer(Num) orelse is_float(Num))
                     andalso (is_integer(Denom) orelse is_float(Denom)) ->
    Num / Denom;
safe_div(A, B) ->
    lager:info("Can't divide ~p by ~p~n", [A, B]),
    null.

safe_mul(Num, Denom) when (is_integer(Num) orelse is_float(Num))
                     andalso (is_integer(Denom) orelse is_float(Denom)) ->
    Num * Denom;
safe_mul(A, B) ->
    lager:info("Can't mul ~p by ~p~n", [A, B]),
    null.

safe_pop_perc(Val, Pop) ->
    safe_mul(100, safe_div(Val, Pop)).

merge_timeseries(Metrics) ->
    MetricsTs = Metrics#metrics.metrics_ts,
    Actuals = Metrics#metrics.actuals_ts,
    Population = Metrics#metrics.population,
    lager:info("Population for ~s: ~p~n", [Metrics#metrics.state, Population]),
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
                        negative_tests=Matched#ts_actuals.negative_tests,
                        new_cases=Matched#ts_actuals.new_cases,
                        new_cases_per_cap=safe_div(Matched#ts_actuals.new_cases, Population),
                        vaccines_distributed=safe_pop_perc(Matched#ts_actuals.vaccines_distributed, Population),
                        vaccines_initiated=safe_pop_perc(Matched#ts_actuals.vaccines_initiated, Population),
                        vaccines_completed=safe_pop_perc(Matched#ts_actuals.vaccines_completed, Population)
                    }
            end,
            [R2|AccIn]
        end,
        [],
        MetricsTs
    ),
    lists:reverse(Merged).

null_sub(N1, N2) when is_integer(N1) andalso is_integer(N2) ->
    N1 - N2;
null_sub(_, _) -> null.

null_add(N1, N2) when (is_integer(N1) orelse is_float(N1)) andalso (is_integer(N2) orelse is_float(N2)) ->
    N1 + N2;
null_add(N1, _) when is_integer(N1) orelse is_float(N1) ->
    N1;
null_add(_, N2) when is_integer(N2) orelse is_float(N2)->
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
    {_, _, _, _, Ret} = lists:foldl(
      fun(Record, {LastRecord, CaseList, CaseCapList, DeathList, Acc}) ->
        % Update the case/death running average lists
        % RawNewCases = null_sub(Record#merged_timeseries.cases, LastRecord#merged_timeseries.cases),
        RawNewDeaths = null_sub(Record#merged_timeseries.deaths, LastRecord#merged_timeseries.deaths),
        % NewCases = case RawNewCases < 0 of true -> 0; false -> RawNewCases end,
        NewCases = Record#merged_timeseries.new_cases,
        NewCapCases = Record#merged_timeseries.new_cases_per_cap,
        NewDeaths = case RawNewDeaths < 0 of true -> 0; false -> RawNewDeaths end,
        CaseList1 = [NewCases|CaseList],
        CaseCapList1 = [NewCapCases|CaseCapList],
        DeathList1 = [NewDeaths|DeathList],
        CaseList2 = takeprefix(7, CaseList1),
        CaseCapList2 = takeprefix(7, CaseCapList1),
        DeathList2 = takeprefix(7, DeathList1),
        Cases7Day = lists:foldl(fun can_api:null_add/2, 0, CaseList2) / 7,
        Cases7DayCap = lists:foldl(fun can_api:null_add/2, 0, CaseCapList2) / 7,
        Deaths7Day = lists:foldl(fun can_api:null_add/2, 0, DeathList2) / 7,
        Updated = Record#merged_timeseries{
            % new_cases=NewCases,
            new_deaths=NewDeaths,
            cases_7day=Cases7Day,
            deaths_7day=Deaths7Day,
            cases_7day_per_cap=Cases7DayCap
        },
        {Record, CaseList2, CaseCapList2, DeathList2, [Updated|Acc]}
      end,
      {Hd, [], [], [], []},
      Tl
    ),
    lists:reverse(Ret).

merged_timeseries_to_rows(Merged) ->
    [io_lib:format("~s ~p ~p ~p ~p ~p ~p ~p ~p ~p ~p ~p ~p ~p ~p ~p ~p~n",
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
         M#merged_timeseries.deaths_7day,
         M#merged_timeseries.new_cases_per_cap,
         M#merged_timeseries.cases_7day_per_cap,
         M#merged_timeseries.vaccines_distributed,
         M#merged_timeseries.vaccines_initiated,
         M#merged_timeseries.vaccines_completed
        ])
        || M <- Merged].

metric_is_null(M=#ts_metrics{}) ->
    M#ts_metrics.infection_rate =:= null;
metric_is_null(M=#ts_actuals{}) ->
    M#ts_actuals.cases =:= null andalso
    M#ts_actuals.deaths =:= null andalso
    M#ts_actuals.positive_tests =:= null andalso
    M#ts_actuals.negative_tests =:= null.

parse_json(Json) ->
    Metrics = proplists:get_value(<<"metrics">>, Json),
    MetricsTimeseries = proplists:get_value(<<"metricsTimeseries">>, Json),
    ActualsTimeseries = proplists:get_value(<<"actualsTimeseries">>, Json),
    MetricsRecordsRaw = [parse_ts_metric(M) || M <- MetricsTimeseries],
    ActualsRecordsRaw = [parse_ts_actuals(M) || M <- ActualsTimeseries],
    MetricsRecords = lists:dropwhile(fun metric_is_null/1, MetricsRecordsRaw),
    ActualsRecords = lists:dropwhile(fun metric_is_null/1, ActualsRecordsRaw),
    #metrics{
        country=proplists:get_value(<<"country">>, Json),
        state=proplists:get_value(<<"state">>, Json),
        county=proplists:get_value(<<"county">>, Json),
        population=proplists:get_value(<<"population">>, Json),
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
        negative_tests=proplists:get_value(<<"negativeTests">>, Json),
        new_cases=proplists:get_value(<<"newCases">>, Json),
        vaccines_distributed=proplists:get_value(<<"vaccinesDistributed">>, Json),
        vaccines_initiated=proplists:get_value(<<"vaccinationsInitiated">>, Json),
        vaccines_completed=proplists:get_value(<<"vaccinationsCompleted">>, Json)
    }.

states() -> [
    <<"AL">>, <<"AK">>, <<"AZ">>, <<"AR">>, <<"CA">>, <<"CO">>, <<"CT">>,
    <<"DE">>, <<"FL">>, <<"GA">>, <<"HI">>, <<"ID">>, <<"IL">>, <<"IN">>,
    <<"IA">>, <<"KS">>, <<"KY">>, <<"LA">>, <<"ME">>, <<"MD">>, <<"MA">>,
    <<"MI">>, <<"MN">>, <<"MS">>, <<"MO">>, <<"MT">>, <<"NE">>, <<"NV">>,
    <<"NH">>, <<"NJ">>, <<"NH">>, <<"NY">>, <<"NC">>, <<"ND">>, <<"OH">>,
    <<"OK">>, <<"OR">>, <<"PA">>, <<"RI">>, <<"SC">>, <<"SD">>, <<"TN">>,
    <<"TX">>, <<"UT">>, <<"VT">>, <<"VA">>, <<"WA">>, <<"WV">>, <<"WI">>,
    <<"WY">>
    ].

% get_metrics_record_with_date(Date, Metrics) ->
% get_actuals_record_with_date(Date, Actuals) ->

merge_ts_metrics(WAddFunc, A, B) -> merge_ts_metrics(WAddFunc, A, B, []).
merge_ts_metrics(_WAddFunc, [], [], Acc) -> lists:reverse(Acc);
merge_ts_metrics(_WAddFunc, [M|Metrics], [], Acc) -> lists:reverse(Acc) ++ [M|Metrics];
merge_ts_metrics(_WAddFunc, [], [M|Metrics], Acc) -> lists:reverse(Acc) ++ [M|Metrics];
merge_ts_metrics(WAddFunc, [M1=#ts_metrics{}|M1Metrics], [M2=#ts_metrics{}|M2Metrics], Acc) ->
    % Use the M1 date as a reference
    M2Matched = get_metrics_record_with_date(M1#ts_metrics.date, [M2|M2Metrics]),
    % Attempt to merge
    Merged = case M2Matched of
        #ts_metrics{} ->
            #ts_metrics{
                date=M1#ts_metrics.date,
                positivity=WAddFunc(M1#ts_metrics.positivity, M2#ts_metrics.positivity),
                density=WAddFunc(M1#ts_metrics.density, M2#ts_metrics.density),
                infection_rate=WAddFunc(M1#ts_metrics.infection_rate, M2#ts_metrics.infection_rate)
            };
        _ -> % Not found just take M1
            M1
    end,
    % Recurse, filtering this date
    M2Filtered = [M || M <- [M2|M2Metrics], M#ts_metrics.date =/= M1#ts_metrics.date],
    merge_ts_metrics(WAddFunc, M1Metrics, M2Filtered, [Merged|Acc]);
merge_ts_metrics(WAddFunc, [M1=#ts_actuals{}|M1Metrics], [M2=#ts_actuals{}|M2Metrics], Acc) ->
    % Use the M1 date as a reference
    M2Matched = get_actuals_record_with_date(M1#ts_actuals.date, [M2|M2Metrics]),
    % Attempt to merge
    Merged = case M2Matched of
        #ts_actuals{} ->
            #ts_actuals{
                date=M1#ts_actuals.date,
                cases=null_add(M1#ts_actuals.cases, M2Matched#ts_actuals.cases),
                deaths=null_add(M1#ts_actuals.deaths, M2Matched#ts_actuals.deaths),
                positive_tests=null_add(M1#ts_actuals.positive_tests, M2Matched#ts_actuals.positive_tests),
                negative_tests=null_add(M1#ts_actuals.negative_tests, M2Matched#ts_actuals.negative_tests),
                new_cases=null_add(M1#ts_actuals.new_cases, M2Matched#ts_actuals.new_cases),
                vaccines_distributed=null_add(M1#ts_actuals.vaccines_distributed, M2Matched#ts_actuals.vaccines_distributed),
                vaccines_initiated=null_add(M1#ts_actuals.vaccines_initiated, M2Matched#ts_actuals.vaccines_initiated),
                vaccines_completed=null_add(M1#ts_actuals.vaccines_completed, M2Matched#ts_actuals.vaccines_completed)
            };
        _ -> % Not found just take M1
            M1
    end,
    % Recurse, filtering this date
    M2Filtered = [M || M <- [M2|M2Metrics], M#ts_actuals.date =/= M1#ts_actuals.date],
    merge_ts_metrics(WAddFunc, M1Metrics, M2Filtered, [Merged|Acc]).

merge_metrics(A=#metrics{}, B=#metrics{}) ->
    % Get the population counts for weighting
    PopA = A#metrics.population,
    PopB = B#metrics.population,
    WeightedAdd = fun(AVal, BVal) ->
        ASane = case AVal of V1 when is_integer(V1) orelse is_float(V1) -> V1; _ -> 1 end,
        BSane = case BVal of V2 when is_integer(V2) orelse is_float(V2) -> V2; _ -> 1 end,
        ((PopA * ASane) + (PopB * BSane)) / (PopA + PopB)
    end,
    % Merge the metrics/actuals lists
    MergedMetrics = merge_ts_metrics(WeightedAdd, A#metrics.metrics_ts, B#metrics.metrics_ts),
    MergedActuals = merge_ts_metrics(WeightedAdd, A#metrics.actuals_ts, B#metrics.actuals_ts),
    % Ensure the metrics/actuals are ordered correctly by date
    SortedMetrics = lists:keysort(#ts_metrics.date, MergedMetrics),
    SortedActuals = lists:keysort(#ts_actuals.date, MergedActuals),
    _Result = #metrics{
        population = PopA + PopB,
        metrics_ts=SortedMetrics,
        actuals_ts=SortedActuals
    }.

usa_hist() ->
    % Fetch raw JSON for every state
    StateResults = [state_hist(Code) || Code <- states()],

    % Parse all the successful resps
    StateMetrics = [M || {ok, M} <- StateResults],
    lager:info("Loaded metrics for ~p states~n", [length(StateMetrics)]),

    % Fold them down
    Folded = lists:foldl(fun can_api:merge_metrics/2, hd(StateMetrics), tl(StateMetrics)),

    % Fill in the country
    {ok, Folded#metrics{country = <<"USA">>}}.

state_hist(FipsCode) when is_binary(FipsCode) ->
    state_hist(binary_to_list(FipsCode));
state_hist(FipsCode) when is_list(FipsCode) ->
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
            {ok, parse_json(Json)};
        {ok, {{_, Code, _}, _RespHeaders, RespBody}} ->
            {error, {Code, RespBody}}
    end.
