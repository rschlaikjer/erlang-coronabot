-record(metrics, {country, state, county, population, positivity, density, infection_rate, metrics_ts, actuals_ts}).
-record(ts_actuals, {date, cases, deaths, positive_tests, negative_tests,
                     new_cases, vaccines_distributed, vaccines_initiated,
                     vaccines_completed}).
-record(ts_metrics, {date, positivity, density, infection_rate}).
-record(merged_timeseries, {date, cases, deaths, positive_tests, negative_tests,
                            positivity, density, infection_rate,
                            new_cases, new_deaths,
                            new_cases_per_cap,
                            cases_7day, deaths_7day,
                            cases_7day_per_cap,
                            vaccines_distributed, vaccines_initiated, vaccines_completed
                           }).
