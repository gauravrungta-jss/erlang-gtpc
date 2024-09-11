#!/bin/bash

rebar3 compile

erl -pa "_build/default/lib/gtplib/ebin/" -s gtpc_client -noshell -noinput
