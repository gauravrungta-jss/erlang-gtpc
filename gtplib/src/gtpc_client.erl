-module(gtpc_client).
-include("gtp_packet.hrl").
-export([start/0]).

%% Start the UDP client
start() ->
    %% Define teid mapping table
    ets:new(teid_table, [named_table, public, set, {keypos, 1}]),
    set_value(teid_counter, 1),

    %% Define the port and address
    Port = 2123,
    Address = {192, 168, 56, 10}, % Bind to the specific IP address

    %% Start listening on the UDP port
    {ok, Socket} = gen_udp:open(Port, [binary, {active, true}, {ip, Address}]),

    %% Start the loop to handle incoming requests
    io:format("Starting incoming request loop ~n"),
    loop(Socket).

%% Insert in teid table. It will replace if entry already exist
set_value(Key, Value) ->
    ets:insert(teid_table, {Key, Value}).

%% Get from the teid table
get_value(Key) ->
    case ets:lookup(teid_table, Key) of
        [{Key, Value}] -> Value;
        [] -> undefined
    end.

%% Delete from teid table.
delete_value(Key) ->
    ets:delete(teid_table,  Key).

%% Main loop to handle incoming messages
loop(Socket) ->
    receive
        {udp, Socket, Ip, Port, Data} ->
            %% Decode and process incoming Gtp Msg
            DecodedMsg = gtp_packet:decode(Data),
            %%io:format("Decoded Request Message: ~p~n", [DecodedMsg]),
            process_request(Socket, Ip, Port, DecodedMsg),

            %% Continue listening for more messages
            loop(Socket);

        {udp, Socket, Ip, Port} ->
            %% Log unexpected data
            io:format("Received unexpected IP: ~p~n", Ip),
            io:format("Received unexpected Port: ~d", Port),
            %% Continue listening
            loop(Socket)
    end.

%% Function to Process Request Msg
process_request(Socket, Ip, Port, DecodedMsg) ->
    MsgType = DecodedMsg#gtp.type,
    case MsgType of
        create_session_request -> process_create_session_request(Socket, Ip, Port, DecodedMsg);
        modify_bearer_request -> process_modify_bearer_request(Socket, Ip, Port, DecodedMsg);
        delete_session_request -> process_delete_session_request(Socket, Ip, Port, DecodedMsg)
    end.

%% Function to Process Create Session Request Msg
process_create_session_request(Socket, Ip, Port, DecodedMsg) ->
    IEsMap = DecodedMsg#gtp.ie,
    Seq = DecodedMsg#gtp.seq_no,
    %%io:format("CSReq Seq: ~p~n", [Seq]),

    MmeCtrlKey = {v2_fully_qualified_tunnel_endpoint_identifier, 0},
    MmeCtrlValue = maps:get(MmeCtrlKey, IEsMap),
    MmeTeid = MmeCtrlValue#v2_fully_qualified_tunnel_endpoint_identifier.key,
    %%io:format("CSReq MME TEID: ~p~n", [MmeTeid]),

    PaaKey = {v2_pdn_address_allocation,0},
    PaaValue = maps:get(PaaKey, IEsMap),
    PaaType = PaaValue#v2_pdn_address_allocation.type,
    %%io:format("CSReq PaaType: ~p~n", [PaaType]),

    %% Get SGW Cntrl Teid and update teid_table with teid_counter and sgw_ctrl_teid->mme_ctrl_teid mapping
    SgwCtrlTeid = get_value(teid_counter),
    set_value(teid_counter, SgwCtrlTeid+1),
    set_value(SgwCtrlTeid,  MmeTeid),

    %% Building CSRsp
    RspIEs = [#v2_cause{v2_cause = request_accepted},
        #v2_apn_restriction{restriction_type_value =  3},
        #v2_fully_qualified_tunnel_endpoint_identifier{instance = 0,interface_type = 11, key = SgwCtrlTeid, ipv4 = <<192,168,56,10>>},
        #v2_fully_qualified_tunnel_endpoint_identifier{instance = 1,interface_type = 7, key = 1879113730, ipv4 = <<192,168,56,30>>},
        #v2_pdn_address_allocation{type = PaaType, address = <<12,0,0,2>>},
        #v2_bearer_context{ group = [#v2_cause{v2_cause = request_accepted},
        #v2_charging_id{id = <<0,0,0,4>>},
            #v2_eps_bearer_id{eps_bearer_id = 5},
            #v2_fully_qualified_tunnel_endpoint_identifier{instance = 0,interface_type = 1,key = 1073811116,ipv4 = <<192,168,56,50>>},
            #v2_fully_qualified_tunnel_endpoint_identifier{instance = 2,interface_type = 5,key = 2147553956,ipv4 = <<192,168,56,30>>}]}],

    Rsp = #gtp{version = v2,type =create_session_response, tei = MmeTeid,seq_no = Seq, ie = RspIEs},

    %% Send CSRsp Msg
    send_response(Socket, Ip, Port, Rsp).

%% Function to Process Modify Bearer Request Msg
process_modify_bearer_request(Socket, Ip, Port, DecodedMsg) ->
    Seq = DecodedMsg#gtp.seq_no,
    %%io:format("MBReq Seq: ~p~n", [Seq]),
    SgwTeid = DecodedMsg#gtp.tei,
    %%io:format("MBReq Teid: ~p~n", [SgwTeid]),

    %% Get MBRsp Teid from teid_table
    RspTeid = get_value(SgwTeid),

    RspIEs = [#v2_cause{v2_cause = request_accepted},
        #v2_bearer_context{ group = [#v2_cause{v2_cause = request_accepted},
            #v2_eps_bearer_id{eps_bearer_id = 5},
            #v2_fully_qualified_tunnel_endpoint_identifier{instance = 0,interface_type = 1,key = 1073811116,ipv4 = <<192,168,56,50>>}]}],

    Rsp = #gtp{version = v2,type =modify_bearer_response, tei = RspTeid, seq_no = Seq, ie = RspIEs},

    %% Send MBRsp Msg
    send_response(Socket, Ip, Port, Rsp).

%% Function to Process Delete Session Request Msg
process_delete_session_request(Socket, Ip, Port, DecodedMsg) ->
    Seq = DecodedMsg#gtp.seq_no,
    %io:format("DSReq Seq: ~p~n", [Seq]),
    SgwTeid = DecodedMsg#gtp.tei,
    %io:format("DSReq Teid: ~p~n", [SgwTeid]),

    %% Get DSRsp Teid from teid_table
    RspTeid = get_value(SgwTeid),

    %% Delete Teid mapping from table
    %%io:format("Deleting Teid mapping for key: ~p~n", [SgwTeid]),
    delete_value(SgwTeid),

    RspIEs = [#v2_cause{v2_cause = request_accepted},
        #v2_bearer_context{ group = [#v2_cause{v2_cause = request_accepted},
            #v2_eps_bearer_id{eps_bearer_id = 5}]}],

    Rsp = #gtp{version = v2,type =delete_session_response, tei = RspTeid, seq_no = Seq, ie = RspIEs},

    %% Send DSRsp Msg
    send_response(Socket, Ip, Port, Rsp).

%% Function to send Response Msg
send_response(Socket, Ip, Port, GtpMsg) ->
    %%io:format("Encoding : ~p~n", [GtpMsg#gtp.type]),
    EncodedBytes =gtp_packet:encode(GtpMsg),
    %%io:format("Sending Encoded Response Message: ~p~n", [EncodedBytes]),

    %% Send Response message back to the sender
    gen_udp:send(Socket, Ip, Port, EncodedBytes).


