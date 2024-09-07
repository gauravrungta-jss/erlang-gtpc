gtplib
======

GTPC code using erlang

BUILDING
--------
Install rebar3 if not available:

    # brew install rebar3

Download dependencies:

    # rebar3 get-deps
    
Build:

    # rebar3 compile

Run: gtpc_client.erl
    
    $ cd src
    $ erl
Erlang/OTP 26 [erts-14.2.5] [source] [64-bit] [smp:12:12] [ds:12:12:10] [async-threads:1] [jit] [dtrace]

Eshell V14.2.5 (press Ctrl+G to abort, type help(). for help)

Complie : gtpc_client.erl
--------------------------
1> c(gtpc_client).

{ok,gtpc_client}

Start: gtpc_client module Start function
----------------------------------------
2> gtpc_client:start().

Received Create Session Request: <<72,32,0,185,0,0,0,0,0,0,0,0,1,0,8,0,36,34,
                                   34,34,34,33,67,245,76,0,5,0,57,98,55,119,51,
                                   75,0,8,0,17,34,51,68,85,102,119,248,86,0,6,
                                   0,8,18,84,99,18,52,83,0,3,0,33,83,103,82,0,
                                   1,0,6,87,0,9,0,138,16,1,0,1,192,168,20,2,87,
                                   0,9,1,135,0,0,0,0,192,168,30,3,71,0,10,0,9,
                                   105,110,116,101,114,115,104,97,116,128,0,1,
                                   0,253,99,0,1,0,1,79,0,5,0,1,0,0,0,0,127,0,1,
                                   0,0,72,0,8,0,0,0,0,119,0,0,0,135,93,0,31,0,
                                   73,0,1,0,5,80,0,22,0,8,5,0,0,0,0,0,0,0,0,0,
                                   0,0,0,0,0,0,0,0,0,0,0,3,0,1,0,10,95,0,2,0,
                                   18,52>>
                                   
Sent Create Session Response



