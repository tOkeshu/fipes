FROM debian:7.5

MAINTAINER <Mathieu>

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update
RUN apt-get -q install -y erlang-nox make git wget
RUN apt-get clean

RUN useradd -d /opt/fipes fipes


ADD Makefile /opt/fipes/
ADD erlang.mk /opt/fipes/
ADD include /opt/fipes/include
ADD priv /opt/fipes/priv
ADD public /opt/fipes/public
ADD src /opt/fipes/src

RUN chown -R fipes:fipes /opt/fipes

USER fipes
RUN cd /opt/fipes && make

CMD cd /opt/fipes/ && HOME=/opt/fipes erl -sname fipes@localhost -pa ebin -pa deps/*/ebin -boot start_sasl -s fipes

EXPOSE 3473
