FROM debian:jessie

MAINTAINER <Mathieu>

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update && \
    apt-get -q install -y erlang-nox make git wget && \
    apt-get clean

RUN useradd -d /opt/fipes fipes


ADD Makefile /opt/fipes/
ADD erlang.mk /opt/fipes/
ADD include /opt/fipes/include
ADD priv /opt/fipes/priv
ADD public /opt/fipes/public
ADD src /opt/fipes/src

RUN chown -R fipes:fipes /opt/fipes

USER fipes
WORKDIR /opt/fipes
RUN make

ENV HOME /opt/fipes
CMD erl -sname fipes@localhost -pa ebin -pa deps/*/ebin -boot start_sasl -s fipes

EXPOSE 3473
