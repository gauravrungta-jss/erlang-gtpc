-module(gtpc_client).
-export([start/0, loop/1, send_create_session_response/3]).

hexstr2bin(S) ->
    list_to_binary(hexstr2list(S)).

hexstr2list([X,Y|T]) ->
    [mkint(X)*16 + mkint(Y) | hexstr2list(T)];
hexstr2list([]) ->
    [].
mkint(C) when $0 =< C, C =< $9 ->
    C - $0;
mkint(C) when $A =< C, C =< $F ->
    C - $A + 10;
mkint(C) when $a =< C, C =< $f ->
    C - $a + 10.

%% Start the UDP client
start() ->
    %% Define the port and address
    Port = 2123,
    Address = {192, 168, 10, 1}, % Bind to the specific IP address
    %% Start listening on the UDP port
    {ok, Socket} = gen_udp:open(Port, [binary, {active, true}, {ip, Address}]),
    %% Start the loop to handle incoming requests
    loop(Socket).

%% Main loop to handle incoming messages
loop(Socket) ->
    receive
        {udp, Socket, Ip, Port, Data} ->
            %% Process Create Session Request here (you can decode and handle the request as needed)
            io:format("Received Create Session Request: ~p~n", [Data]),
            DecodedMsg = gtp_packet:decode(Data),
            io:format("Decoded Create Session Request Message: ~p~n", [DecodedMsg]),

            DecodeHeader = gtp_packet:decode_header(Data),

            io:format("Decoded Create Session Request Header: ~p~n", [DecodeHeader]),

            {_, _, _, _, _, _, _, IEsMap} = DecodedMsg,
            Key = {v2_fully_qualified_tunnel_endpoint_identifier, 1},
            Value = maps:get(Key, IEsMap),

            io:format("PGW TEID: ~p~n", [Value]),
            %% Send a Create Session Response
            send_create_session_response(Socket, Ip, Port),
            %% Continue listening for more messages
            loop(Socket);

        {udp, Socket, Ip, Port} ->
            %% Log unexpected data
            io:format("Received unexpected IP: ~p~n", Ip),
            io:format("Received unexpected Port: ~d", Port),
            %% Continue listening
            loop(Socket)
    end.

%% Function to send Create Session Response
send_create_session_response(Socket, Ip, Port) ->
   Bin =  hexstr2bin("48210067100100020000020002000200"
                     "10004F000500010C000002570009008B"
                     "11000003C0A80A015700090187700100"
                     "02C0A81E035D002D0049000100050200"
                     "02001000570009008140010EACC0A832"
                     "055700090285800112A4C0A81E035E00"
                     "0400000000047F00010003"),
    %% Prepare the Create Session Response message
    % Here you would construct the GTP-C Create Session Response message
    %%Response = <<0, 2, 0, 2, 0, 0, 0, 0>>,
    %% Send the Create Session Response message back to the sender
    gen_udp:send(Socket, Ip, Port, Bin),
    io:format("Sent Create Session Response~n").


%% handler -> decode / build / encode

