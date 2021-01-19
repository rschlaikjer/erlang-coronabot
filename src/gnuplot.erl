-module(gnuplot).
-compile(export_all).
-compile([{parse_transform, lager_transform}]).
-include_lib("include/records.hrl").

%         [M#merged_timeseries.date,
%          M#merged_timeseries.positivity,
%          M#merged_timeseries.density,
%          M#merged_timeseries.infection_rate,
%          M#merged_timeseries.cases,
%          M#merged_timeseries.deaths,
%          M#merged_timeseries.positive_tests,
%          M#merged_timeseries.negative_tests])

gnuplot_colours() -> [
    "black", "dark-grey", "red", "web-green", "web-blue", "dark-magenta",
    "dark-cyan", "dark-orange", "dark-yellow", "royalblue", "goldenrod",
    "dark-spring-green", "purple", "steelblue", "dark-red", "dark-chartreuse",
    "orchid", "aquamarine", "brown", "yellow", "turquoise", "grey", "light-red",
    "light-green", "light-blue", "light-magenta", "light-cyan",
    "light-goldenrod", "light-pink", "light-turquoise", "gold", "green",
    "dark-green", "spring-green", "forest-green", "sea-green", "blue",
    "dark-blue", "midnight-blue", "navy", "medium-blue", "skyblue", "cyan",
    "magenta", "dark-turquoise", "dark-pink", "coral", "light-coral",
    "orange-red", "salmon", "dark-salmon", "khaki", "dark-khaki",
    "dark-goldenrod", "beige", "olive", "orange", "violet", "dark-violet",
    "plum", "dark-plum", "dark-olivegreen", "orangered4", "brown4", "sienna4",
    "orchid4", "mediumpurple3", "slateblue1", "yellow4", "sienna1", "tan1",
    "sandybrown", "light-salmon", "pink", "khaki1", "lemonchiffon", "bisque",
    "honeydew", "slategrey", "seagreen", "antiquewhite", "chartreuse",
    "greenyellow", "gray", "light-gray", "light-grey", "dark-gray", "slategray"
    ].

state_colour(State) ->
    D1 = crypto:hash_init(sha),
    D2 = crypto:hash_update(D1, State),
    Hash =crypto:hash_final(D2),
    Int = crypto:bytes_to_integer(Hash),
    Idx = Int rem length(gnuplot_colours()),
    lists:nth(Idx + 1, gnuplot_colours()).

format_title(ChartType, Metrics) ->
    Country = Metrics#metrics.country,
    State = Metrics#metrics.state,
    County = Metrics#metrics.county,
    case {Country, State, County} of
        {C1, S, C2} when is_binary(C1) andalso is_binary(S) andalso is_binary(C2) ->
            io_lib:format("~s for ~s county, ~s, ~s", [ChartType, County, State, Country]);
        {C1, S, _} when is_binary(C1) andalso is_binary(S) ->
            io_lib:format("~s for ~s, ~s", [ChartType, State, Country]);
        {C1, _, _} when is_binary(C1) ->
            io_lib:format("~s for ~s", [ChartType, Country])
    end.

valid_dates_for_series(Records, SeriesList) ->
    % Return all dates in date list IFF at least one of the series specified
    % in serieslist is _not_ undefined for that date
    TestFun = fun(R=#merged_timeseries{}) ->
        lists:any(fun(X) -> X =/= null andalso X =/= undefined end, [element(N, R) || N <- SeriesList])
    end,
    _Dates = [R#merged_timeseries.date || R <- Records, TestFun(R)].

execute_plot(Cmd) ->
    FullCmd = lists:flatten("gnuplot -e \"" ++ Cmd ++"\""),
    lager:info("Executing command \"~s\"~n", [FullCmd]),
    Ret = os:cmd(FullCmd),
    lager:info("Result: ~s~n", [Ret]).

gen_data_file(Merged) ->
    Now = os:system_time(nanosecond),
    TempFile = "/tmp/coronabot." ++ integer_to_list(Now),
    Rows = can_api:merged_timeseries_to_rows(Merged),
    file:write_file(TempFile, lists:join("", Rows)),
    TempFile.

plot_infection_rate(Metrics, OutFile) ->
    Title = format_title("Infection Rate", Metrics),
    Merged = can_api:merge_timeseries(Metrics),
    Dates = valid_dates_for_series(Merged, [
        #merged_timeseries.infection_rate
    ]),
    StartDate = binary_to_list(lists:min(Dates)),
    EndDate = binary_to_list(lists:max(Dates)),
    TempFile = gen_data_file(Merged),
    Header = plot_header(StartDate, EndDate, Title, OutFile) ++ [
        "set cbrange [0:1.2]",
        "set palette defined (0 'dark-green', 1 'yellow', 1.2 'red')"
    ],
    Series = [
        "'" ++ TempFile ++ "' using 1:4:4 with lines linewidth 4 lc pal z title 'Infection Rate'"
    ],
    Cmd = lists:join(";", Header) ++ "; plot " ++ lists:join(", ", Series),
    execute_plot(Cmd),
    file:delete(TempFile).

plot_cum_case_count(Metrics, OutFile) ->
    Title = format_title("Cumulative Case Count", Metrics),
    Merged = can_api:merge_timeseries(Metrics),
    Dates = valid_dates_for_series(Merged, [
        #merged_timeseries.cases,
        #merged_timeseries.deaths
    ]),
    StartDate = binary_to_list(lists:min(Dates)),
    EndDate = binary_to_list(lists:max(Dates)),
    TempFile = gen_data_file(Merged),
    Header = plot_header(StartDate, EndDate, Title, OutFile) ++ [
        "set y2tics",
        "set ylabel 'Cases'",
        "set y2label 'Deaths'",
        "set link y2 via y/10 inverse y*10"
    ],
    Series = [
        "'" ++ TempFile ++ "' using 1:5 smooth bezier with filledcurves x1 fs solid 1.0 lc rgb 'goldenrod' title 'Total Cases'",
        "'" ++ TempFile ++ "' using 1:6 smooth bezier with filledcurves x1 fs transparent solid 0.5 noborder lc rgb 'red' title 'Total Deaths' axes x1y2"
    ],
    Cmd = lists:join(";", Header) ++ "; plot " ++ lists:join(", ", Series),
    execute_plot(Cmd),
    file:delete(TempFile).

plot_daily_case_count(Metrics, OutFile) ->
    Title = format_title("Daily Case Count", Metrics),
    Merged = can_api:merge_timeseries(Metrics),
    Filled = can_api:fill_daily_stats(Merged),
    Dates = valid_dates_for_series(Filled, [
        #merged_timeseries.new_cases,
        #merged_timeseries.new_deaths
    ]),
    StartDate = binary_to_list(lists:min(Dates)),
    EndDate = binary_to_list(lists:max(Dates)),
    TempFile = gen_data_file(Filled),
    Header = plot_header(StartDate, EndDate, Title, OutFile) ++ [
        "set y2tics",
        "set ylabel 'Cases'",
        "set y2label 'Deaths'",
        "set link y2 via y/10 inverse y*10"
    ],
    Series = [
        "'" ++ TempFile ++ "' using 1:9 with boxes fs solid 1.0 lc rgb 'goldenrod' title 'New Cases (Raw)'",
        "'" ++ TempFile ++ "' using 1:10 with boxes fs transparent solid 0.6 noborder lc rgb 'red' title 'New Deaths (Raw)' axes x1y2",
        "'" ++ TempFile ++ "' using 1:9 smooth bezier with lines lc rgb '#55EA26FA' lw 4 title 'New Cases (Bezier)'",
        "'" ++ TempFile ++ "' using 1:10 smooth bezier with lines lc rgb '#55546CFF' lw 4 title 'New Deaths (Bezier)' axes x1y2"
    ],
    Cmd = lists:join(";", Header) ++ "; plot " ++ lists:join(", ", Series),
    execute_plot(Cmd),
    file:delete(TempFile).

plot_vaccination_rate(Metrics, OutFile) ->
    Title = format_title("Vaccination Stats", Metrics),
    Merged = can_api:merge_timeseries(Metrics),
    Filled = can_api:fill_daily_stats(Merged),
    Dates = valid_dates_for_series(Filled, [
        #merged_timeseries.vaccines_distributed,
        #merged_timeseries.vaccines_initiated,
        #merged_timeseries.vaccines_completed
    ]),
    StartDate = binary_to_list(lists:min(Dates)),
    EndDate = binary_to_list(lists:max(Dates)),
    TempFile = gen_data_file(Filled),
    Header = plot_header(StartDate, EndDate, Title, OutFile) ++ [
        "set ylabel 'Doses'"
    ],
    Series = [
        "'" ++ TempFile ++ "' using 1:15 with lines lw 4 lc rgb 'royalblue' title 'Vaccines Distributed'",
        "'" ++ TempFile ++ "' using 1:16 with lines lw 4 lc rgb 'goldenrod' title 'Vaccines Initiated'",
        "'" ++ TempFile ++ "' using 1:17 with lines lw 4 lc rgb 'green' title 'Vaccines Completed'"
    ],
    Cmd = lists:join(";", Header) ++ "; plot " ++ lists:join(", ", Series),
    execute_plot(Cmd),
    file:delete(TempFile).

plot_compare(MetricList, OutFile) ->
    States = [ binary_to_list(M#metrics.state) || M <- MetricList ],
    Title = lists:flatten("7-Day New Cases for " ++ lists:join(", ", States)),
    MergedList = [can_api:merge_timeseries(M) || M <- MetricList],
    FilledList = [ can_api:fill_daily_stats(M) || M <- MergedList ],
    Dates = lists:foldl(
        fun(Filled, Acc) -> Acc ++ [R#merged_timeseries.date || R <- Filled] end,
        [],
        FilledList
    ),
    StartDate = binary_to_list(lists:min(Dates)),
    EndDate = binary_to_list(lists:max(Dates)),
    TempFiles = [gen_data_file(Filled) || Filled <- FilledList],
    Header = plot_header(StartDate, EndDate, Title, OutFile) ++ [
    ],
    StatePairs = lists:zip(States, TempFiles),
    lager:info("~p", [hd(StatePairs)]),
    Series = [
        lists:flatten(io_lib:format(
            "'~s' using 1:11 with lines lw 4 lc rgb '~s' title '~s'",
            [File, state_colour(State), State]
        )) || {State, File} <- StatePairs
    ],
    Cmd = lists:join(";", Header) ++ "; plot " ++ lists:join(", ", Series),
    execute_plot(Cmd),
    [file:delete(TempFile) || TempFile <- TempFiles].

plot_compare_capita(MetricList, OutFile) ->
    States = [ binary_to_list(M#metrics.state) || M <- MetricList ],
    Title = lists:flatten("7-Day New Cases/Capita for " ++ lists:join(", ", States)),
    MergedList = [can_api:merge_timeseries(M) || M <- MetricList],
    FilledList = [ can_api:fill_daily_stats(M) || M <- MergedList ],
    Dates = lists:foldl(
        fun(Filled, Acc) -> Acc ++ [R#merged_timeseries.date || R <- Filled] end,
        [],
        FilledList
    ),
    StartDate = binary_to_list(lists:min(Dates)),
    EndDate = binary_to_list(lists:max(Dates)),
    TempFiles = [gen_data_file(Filled) || Filled <- FilledList],
    Header = plot_header(StartDate, EndDate, Title, OutFile) ++ [
    ],
    StatePairs = lists:zip(States, TempFiles),
    lager:info("~p", [hd(StatePairs)]),
    Series = [
        lists:flatten(io_lib:format(
            "'~s' using 1:14 with lines lw 4 lc rgb '~s' title '~s'",
            [File, state_colour(State), State]
        )) || {State, File} <- StatePairs
    ],
    Cmd = lists:join(";", Header) ++ "; plot " ++ lists:join(", ", Series),
    execute_plot(Cmd).
 %[file:delete(TempFile) || TempFile <- TempFiles].

plot_header(StartDate, EndDate, Title, OutFile) ->
    [
        "set term pngcairo enhanced font 'arial,24' fontscale 1.0 size 1920, 1080",
        "set output '" ++ OutFile ++ "'",
        "set clip two",
        "set style fill transparent solid 0.5 noborder",
        "set key title '' center",
        "set key fixed left top vertical Left reverse enhanced autotitle nobox",
        "set key noinvert samplen 1 spacing 1 width 0 height 0",
        "set style increment default",
        "set style data lines",
        "set style function filledcurves y1=0",
        "set title '" ++ Title ++ "'",
        "unset colorbox",
        "set xdata time",
        "set timefmt '%Y-%m-%d'",
        "set format x '%Y-%m-%d'",
        "set xrange ['" ++ StartDate ++ "':'" ++ EndDate ++ "']",
        "set xtics rotate by 20 right scale 0.5",
        "set decimal locale",
        "set format y '%g'",
        "set ytics nomirror",
        "set grid xtics ytics layerdefault front"
    ].
