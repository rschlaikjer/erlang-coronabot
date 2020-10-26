FROM erlang:22.3.4.12 as builder

WORKDIR /src
COPY . /src
RUN rebar3 as prod tar

RUN mkdir -p /release
RUN tar -zxvf /src/_build/prod/rel/*/*.tar.gz -C /release

FROM debian:buster-slim

RUN apt-get update && apt-get install -y openssl daemontools gnuplot libtinfo6

WORKDIR /deploy

COPY --from=builder /release /deploy

CMD /deploy/bin/coronabot foreground
