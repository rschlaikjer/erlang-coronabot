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
    lists:nth(Int rem length(gnuplot_colours()), gnuplot_colours()).

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
    Merged = can_api:merge_timeseries(Metrics#metrics.metrics_ts,
                                      Metrics#metrics.actuals_ts),
    Dates = [R#merged_timeseries.date || R <- Merged],
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
    Merged = can_api:merge_timeseries(Metrics#metrics.metrics_ts,
                                      Metrics#metrics.actuals_ts),
    Dates = [R#merged_timeseries.date || R <- Merged],
    StartDate = binary_to_list(lists:min(Dates)),
    EndDate = binary_to_list(lists:max(Dates)),
    TempFile = gen_data_file(Merged),
    Header = plot_header(StartDate, EndDate, Title, OutFile) ++ [
    ],
    Series = [
        "'" ++ TempFile ++ "' using 1:5 with boxes fs solid 1.0 lc rgb 'goldenrod' title 'Total Cases'",
        "'" ++ TempFile ++ "' using 1:6 with boxes fs solid 1.0 lc rgb 'red' title 'Total Deaths'"
    ],
    Cmd = lists:join(";", Header) ++ "; plot " ++ lists:join(", ", Series),
    execute_plot(Cmd),
    file:delete(TempFile).

plot_daily_case_count(Metrics, OutFile) ->
    Title = format_title("Daily Case Count", Metrics),
    Merged = can_api:merge_timeseries(Metrics#metrics.metrics_ts,
                                      Metrics#metrics.actuals_ts),
    Filled = can_api:fill_daily_stats(Merged),
    Dates = [R#merged_timeseries.date || R <- Filled],
    StartDate = binary_to_list(lists:min(Dates)),
    EndDate = binary_to_list(lists:max(Dates)),
    TempFile = gen_data_file(Filled),
    Header = plot_header(StartDate, EndDate, Title, OutFile) ++ [
    ],
    Series = [
        "'" ++ TempFile ++ "' using 1:9 with boxes fs solid 1.0 lc rgb 'goldenrod' title 'New Cases'",
        "'" ++ TempFile ++ "' using 1:10 with boxes fs solid 1.0 lc rgb 'red' title 'New Deaths'",
        "'" ++ TempFile ++ "' using 1:11 with lines lc rgb '#EA26FA' lw 4 title 'New Cases (7 Day)'",
        "'" ++ TempFile ++ "' using 1:12 with lines lc rgb '#546CFF' lw 4 title 'New Deaths (7 Day)'"
    ],
    Cmd = lists:join(";", Header) ++ "; plot " ++ lists:join(", ", Series),
    execute_plot(Cmd),
    file:delete(TempFile).

plot_compare(MetricList, OutFile) ->
    States = [ binary_to_list(M#metrics.state) || M <- MetricList ],
    Title = lists:flatten("Daily Case Counts for " ++ lists:join(", ", States)),
    MergedList = [can_api:merge_timeseries(M#metrics.metrics_ts,
                                           M#metrics.actuals_ts)
                  || M <- MetricList],
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
            "'~s' using 1:9 with lines lw 4 lc rgb '~s' title '~s: New Cases'",
            [File, state_colour(State), State]
        )) || {State, File} <- StatePairs
    ],
    Cmd = lists:join(";", Header) ++ "; plot " ++ lists:join(", ", Series),
    execute_plot(Cmd),
    [file:delete(TempFile) || TempFile <- TempFiles].

plot_header(StartDate, EndDate, Title, OutFile) ->
    [
        "set term png enhanced font 'arial,24' fontscale 1.0 size 1920, 1080",
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
        "set ytics",
        "set grid xtics ytics layerdefault front"
    ].
