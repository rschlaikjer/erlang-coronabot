-record(metrics, {country, state, county, positivity, density, infection_rate, metrics_ts, actuals_ts}).
-record(ts_actuals, {date, cases, deaths, positive_tests, negative_tests}).
-record(ts_metrics, {date, positivity, density, infection_rate}).
-record(merged_timeseries, {date, cases, deaths, positive_tests, negative_tests,
                            positivity, density, infection_rate,
                            new_cases, new_deaths, cases_7day, deaths_7day}).
