%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%    message structure
%%% @end
%%% Created : 16 Aug 2015 by Tony Rogvall <tony@rogvall.se>

-module(telegram).

-compile(export_all).

%%
%% Telegram structure: (big-endian fields)
%%  <<SD, DN, OBJ, DATA, CS:16>>
%%
%% SD = 
%%   TransmissionType:2,
%%   CastType:1,
%%   Direction:1,
%%   Length:4
%%
%% DN = DeviceNode,  = 0 for one DC output model
%%
%% OBJ = Object Number
%%
%% CS = Check Sum   ( 16 bit sum of all bytes )
%%
-define(TRANSMISSION_TYPE_RESERVED, 2#00).
-define(TRANSMISSION_TYPE_QUERY,    2#01).
-define(TRANSMISSION_TYPE_ANSWER,   2#10).
-define(TRANSMISSION_TYPE_SEND,     2#11).

-define(CAST_TYPE_REQUEST,     1).
-define(CAST_TYPE_REPLY,       0).

-define(DIRECTION_FROM_DEVICE, 0).
-define(DIRECTION_TO_DEVICE,   1).


%% Object number
-define(DEVICE_TYPE,          0).
-define(DEVICE_SERIAL_NO,     1).
-define(NOMINAL_VOLTAGE,      2).
-define(NOMINAL_CURRENT,      3).
-define(NOMINAL_POWER,        4).
-define(DEVICE_ARTICLE_NO,    6).
-define(MANUFACTURER,         8).
-define(SOFTWARE_VERSION,     9).
-define(DEVICE_CLASS,         19).
-define(OVP_THRESHOLD,        38).
-define(OCP_THRESHOLD,        39).
-define(SET_VOLTAGE,          50).
-define(SET_CURRENT,          51).
-define(POWER_SUPPLY_CONTROL, 54).
-define(STATUS_ACTUAL,        71).
-define(STATUS_SET,           72).

-define(debug(Fmt,As), io:format((Fmt), (As))).

open(Device) ->
    uart:open(Device, [{baud, 115200}, {parity,odd}, {stopb, 1}, 
		       {mode,binary}]).

close(U) ->
    uart:close(U).
    
flush(U) ->
    uart:setopts(U, [{active, once}]),
    uart:setopts(U, [{active, false}]),
    receive
	{uart,U,_Data} ->
	    ?debug("flush: data=~p\n", [_Data]),
	    ok
    after 0 ->
	    ok
    end.

get_device_type(U) ->
    query(U, 0, ?DEVICE_TYPE, 16),
    recv(U, string).

get_device_serial_no(U) ->
    query(U, 0, ?DEVICE_SERIAL_NO, 16),
    recv(U, string).

get_nominal_voltage(U) ->
    query(U, 0, ?NOMINAL_VOLTAGE, 4),
    recv(U, float32).

get_nominal_current(U) ->
    query(U, 0, ?NOMINAL_CURRENT, 4),
    recv(U, float32).

get_nominal_power(U) ->
    query(U, 0, ?NOMINAL_POWER, 4),
    recv(U, float32).

get_device_article_no(U) ->
    query(U, 0, ?DEVICE_ARTICLE_NO, 16),
    recv(U, string).

get_manufacturer(U) ->
    query(U, 0, ?MANUFACTURER, 16),
    recv(U, string).

get_software_version(U) ->
    query(U, 0, ?SOFTWARE_VERSION, 16),
    recv(U, string).

get_device_class(U) ->
    query(U, 0, ?DEVICE_CLASS, 2),
    recv(U, int16).

get_ovp_threshold(U) ->
    query(U, 0, ?OVP_THRESHOLD, 2),
    recv(U, int16).

set_ovp_threshold(U, Value) ->
    send(U, 0, ?OVP_THRESHOLD, <<Value:16>>),
    recv(U, status).

get_ocp_threshold(U) ->
    query(U, 0, ?OCP_THRESHOLD, 2),
    recv(U, int16).

set_ocp_threshold(U, Value) ->
    send(U, 0, ?OCP_THRESHOLD, <<Value:16>>),
    recv(U, status).

get_actual_voltage(U) ->
    {ok,NV} = get_nominal_voltage(U),
    get_actual_voltage(U, NV).

get_actual_voltage(U, NV) ->
    {ok,Value} = get_voltage(U),
    {ok,actual_value(Value, NV)}.

get_voltage(U) ->
    query(U, 0, ?SET_VOLTAGE, 2),
    recv(U, int16).

set_actual_voltage_wait(U, V) ->
    {ok,NV} = get_nominal_voltage(U),
    set_actual_voltage(U, V, NV),
    T0 = erlang:system_time(milli_seconds),
    voltage_wait(U, V, NV, T0, 0.1).

voltage_wait(U, V0, NV, T0, Eps) ->
    {ok,State} = get_state(U, NV, undefined),
    V1 = proplists:get_value(actual_voltage, State, -1.0),
    T1 = erlang:system_time(milli_seconds),
    Td = T1-T0,
    if abs(V0-V1) =< Eps ->
	    {ok, V1, Td};
       Td > 5000 ->
	    {ok, V1, too_long_time};
       true ->
	    timer:sleep(100),
	    voltage_wait(U, V0, NV, T0, Eps)
    end.
	    

	    
	    

    
    


set_actual_voltage(U, V) ->
    {ok,NV} = get_nominal_voltage(U),
    set_actual_voltage(U, V, NV).

set_actual_voltage(U, V, NV) ->
    set_voltage(U, set_value(V, NV)).

set_voltage(U, Value) ->
    send(U, 0, ?SET_VOLTAGE, <<Value:16>>),
    recv(U, status).

get_actual_current(U) ->
    {ok,NV} = get_nominal_voltage(U),
    get_actual_voltage(U, NV).

get_actual_current(U, NA) ->
    {ok,Value} = get_current(U),
    {ok,actual_value(Value, NA)}.

get_current(U) ->
    query(U, 0, ?SET_CURRENT, 2),
    recv(U, int16).

set_actual_current(U, A) ->
    {ok,NA} = get_nominal_current(U),
    set_actual_voltage(U, A, NA).

set_actual_current(U, A, NA) ->
    set_current(U, set_value(A, NA)).

set_current(U, Value) ->
    send(U, 0, ?SET_CURRENT, <<Value:16>>),
    recv(U, status).

switch_power_output_on(U) ->
    set_power_supply_control(U, 0, 16#01, 16#01).

switch_power_output_off(U) ->
    set_power_supply_control(U, 0, 16#01, 16#00).

acknowledge_alarms(U) ->
    set_power_supply_control(U, 0, 16#0A, 16#0A).

switch_to_remote_control(U) ->
    set_power_supply_control(U, 0, 16#10, 16#10).

switch_to_manual_control(U) ->
    set_power_supply_control(U, 0, 16#10, 16#00).

tracking_on(U) ->
    set_power_supply_control(U, 0, 16#F0, 16#F0).

tracking_off(U) ->
    set_power_supply_control(U, 0, 16#F0, 16#E0).

set_power_supply_control(U, DN, Code1, Code2) ->
    send(U, DN, ?POWER_SUPPLY_CONTROL, <<Code1,Code2>>),
    recv(U, status).

get_actual_state(U) ->
    {ok,NV} = get_nominal_voltage(U),
    {ok,NA} = get_nominal_current(U),
    get_state(U,NV,NA).

get_state(U) ->
    get_state(U,undefined,undefined).

get_state(U,NV,NA) ->
    query(U, 0, ?STATUS_ACTUAL, 6),
    case recv(U, binary) of
	{ok, <<DeviceState,
	       OTP_active:1, OPP_active:1, OCP_active:1, OVP_active:1,
	       Tracking_active:1, ControllerState:2, Output_on:1,
	       ActualVoltage:16,
	       ActualCurrent:16>>} ->
	    {ok, [{device_state, DeviceState},
		  {otp_active, OTP_active},
		  {opp_active, OPP_active},
		  {ocp_active, OCP_active},
		  {ovp_active, OVP_active},
		  {tracking_active, Tracking_active},
		  {controller_state, ControllerState},
		  {output_on, Output_on},
		  {actual_voltage, actual_value(ActualVoltage,NV)},
		  {actual_current, actual_value(ActualCurrent,NA)}]};
	{ok, _} ->
	    {error, unable_to_decode_state};
	Error ->
	    Error
    end.

get_momentary(U) ->
    get_momentary(U,undefined,undefined).

get_momentary(U,NV,NA) ->
    query(U, 0, ?STATUS_SET, 6),
    case recv(U, binary) of
	{ok, <<DeviceState,
	       OTP_active:1, OPP_active:1, OCP_active:1, OVP_active:1,
	       Tracking_active:1, ControllerState:2, Output_on:1,
	       ActualVoltage:16,
	       ActualCurrent:16>>} ->
	    {ok, [{device_state, DeviceState},
		  {otp_active, OTP_active},
		  {opp_active, OPP_active},
		  {ocp_active, OCP_active},
		  {ovp_active, OVP_active},
		  {tracking_active, Tracking_active},
		  {controller_state, ControllerState},
		  {output_on, Output_on},
		  {actual_voltage, actual_value(ActualVoltage,NV)},
		  {actual_current, actual_value(ActualCurrent,NA)}]};
	{ok, _} ->
	    {error, unable_to_decode_state};
	Error ->
	    Error
    end.

actual_value(Value, undefined) ->
    Value;
actual_value(Value, Nominal) ->    
    (Nominal*Value)/25600.

set_value(Value, undefined) when is_integer(Value) ->
    Value;
set_value(Value, Nominal) when is_number(Value) ->    
    trunc((25600*Value)/Nominal).


send(U, DN, ObjectNumber, Arg) ->
    TransMissionType = ?TRANSMISSION_TYPE_SEND,
    BinData = iolist_to_binary(Arg),
    Size = byte_size(BinData),
    Data0 = <<TransMissionType:2, ?CAST_TYPE_REQUEST:1, ?DIRECTION_TO_DEVICE:1,
	      (Size-1):4,
	      DN:8,
	      ObjectNumber:8,
	      BinData/binary>>,
    CheckSum = checksum(Data0),
    Data = <<Data0/binary, CheckSum:16>>,
    %% delay 50ms between telegram transmissions
    uart:send(U, Data).
    
query(U, DN, ObjectNumber, Size) ->
    TransMissionType = ?TRANSMISSION_TYPE_QUERY,
    Data0 = <<TransMissionType:2, ?CAST_TYPE_REQUEST:1, ?DIRECTION_TO_DEVICE:1,
	      (Size-1):4,
	      DN:8,
	      ObjectNumber:8>>,
    CheckSum = checksum(Data0),
    Data = <<Data0/binary, CheckSum:16>>,
    %% delay 50ms between telegram transmissions
    uart:send(U, Data).

recv(U, Type) ->
    case uart:recv(U, 3) of
	{ok,Header=
	     <<?TRANSMISSION_TYPE_ANSWER:2,
	       ?CAST_TYPE_REPLY:1,
	       ?DIRECTION_FROM_DEVICE:1,
	       Length:4,_DN,OBJ>>} ->
	    ObjLength = Length+1,
	    case uart:recv(U, ObjLength+2) of
		{ok, <<Data:ObjLength/binary,Checksum:16>>} ->
		    Checksum1 = (checksum(Header)+checksum(Data)) band 16#ffff,
		    if Checksum =/= Checksum1 ->
			    {error, bad_checksum};
		       true ->
			    reply(Type, Data, OBJ)
		    end;
		Error ->
		    Error
	    end;
	{ok, Header} ->
	    ?debug("bad telegram header ~p\n", [Header]),
	    {error, bad_telegram_header};
	Error ->
	    Error
    end.

checksum(Binary) ->
    checksum(Binary, 0).
checksum(<<B,Bs/binary>>, Sum) ->
    checksum(Bs, B+Sum);
checksum(<<>>, Sum) ->
    Sum.

reply(status, <<0>>, 16#ff) ->
    ok;
reply(_, <<ErrorCode>>, 16#ff) ->
    {error, ErrorCode};
reply(int16, <<Value:16>>, _OBJ) ->
    {ok,Value};
reply(float32, <<Value:32/float>>, _OBJ) ->
    {ok,Value};
reply(string, Value, _OBJ) ->
    [String | _] = binary:split(Value, <<0>>),
    {ok, binary_to_list(String)};
reply(binary, Value, _OBJ) ->
    {ok, Value}.
