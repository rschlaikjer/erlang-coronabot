FROM erlang:20.3.8.2 as builder

WORKDIR /src
COPY . /src
RUN rebar3 as prod tar

RUN mkdir -p /release
RUN tar -zxvf /src/_build/prod/rel/*/*.tar.gz -C /release

FROM debian:buster-slim

RUN apt-get update && apt-get install -y openssl daemontools gnuplot

WORKDIR /deploy

COPY --from=builder /release /deploy

EXPOSE 80

CMD /deploy/bin/coronabot foreground
