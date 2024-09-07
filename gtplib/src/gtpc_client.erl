-module(gtpc_client).
-export([start/0, loop/1, send_create_session_response/3]).

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
    %% Prepare the Create Session Response message
    % Here you would construct the GTP-C Create Session Response message
    Response = <<0, 2, 0, 2, 0, 0, 0, 0>>,
    %% Send the Create Session Response message back to the sender
    gen_udp:send(Socket, Ip, Port, Response),
    io:format("Sent Create Session Response~n").


%% handler -> decode / build / encode

