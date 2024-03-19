-module(erlangZ21).
-export([start/0, send_collect/3, get_serial_num/1]).
-export([drive_train/5, get_loco_info/2, get_loco_info/3]).

udp_details() ->
	{8799, {192,168,0,111}, 21105}.

open_socket(Port_number, Active_state) ->
	{ok, Socket} = gen_udp:open(Port_number, [binary, {active, Active_state}]),
	Socket.

send_collect({Local_port, Dest_IP, Dest_port}, Message, Response) ->
	Socket = open_socket(Local_port, false),
	gen_udp:send(Socket, Dest_IP, Dest_port, Message),
	case Response of
		true ->
			{ok, {_, _, Package}} = gen_udp:recv(Socket, 0);
		false ->
			Package = empty
	end,
	gen_udp:close(Socket),
	Package.

get_serial_num(UDP_details) ->
	Message = <<4,0,16,0>>, % get serial number
	<<_:4/binary, Rest/binary>> = send_collect(UDP_details, Message, true),
	Serial = binary:decode_unsigned(Rest, little),
	io:fwrite("Serial number: ~p~n", [Serial]).

calc_prim_funcs(Number) ->
	F0 = Number band 16 /= 0,
	F4 = Number band  1 /= 0,
	F3 = Number band  2 /= 0,
	F2 = Number band  4 /= 0,
	F1 = Number band  8 /= 0,
	{F0, F4, F3, F2, F1}.

calc_exte_funcs([], Out_list) ->
	Out_list;

calc_exte_funcs([Number|Tail], In_list) ->
	State1 = Number band   1 /= 0,
	State2 = Number band   2 /= 0,
	State3 = Number band   4 /= 0,
	State4 = Number band   8 /= 0,
	State5 = Number band  16 /= 0,
	State6 = Number band  32 /= 0,
	State7 = Number band  64 /= 0,
	State8 = Number band 128 /= 0,
	Out_list = [State8, State7, State6, State5,
		    State4, State3, State2, State1] ++ In_list,
	calc_exte_funcs(Tail, Out_list).

get_loco_info(UDP_details, Address) ->
	get_loco_info(UDP_details, Address, false).

get_loco_info(UDP_details, Address, Extend) ->
	Message = <<9,0,64,0,227,240,0,Address,16>>, % get loco info for Address
	Package = send_collect(UDP_details, Message, true),
	<<_:7/binary, Tail/binary>> = Package,
	<<DB2, DB3, DB4, Funcs:(byte_size(Tail)-4)/binary, _>> = Tail,
	Busy = DB2 band 0 /= 0,
	case DB2 band 7 of
		0 ->
			S_steps = 14;
		2 ->
			S_steps = 18;
		_ ->
			S_steps = 128
	end,
	case DB3 band 128 /= 0 of
		true ->
			Direction = "forward";
		false ->
			Direction = "reverse"
	end,
	Speed = DB3 band 127,
	D_trac = DB4 band 64 /= 0,
	S_sear = DB4 band 32 /= 0,
	{F0, F4, F3, F2, F1} = calc_prim_funcs(DB4),
	io:fwrite("Address: ~p, Busy: ~p, Speed_steps: ~p, Direction: ~p, Speed: ~p~n",
		  [Address, Busy, S_steps, Direction, Speed]),
	io:fwrite("Doubletraction: ~p, Smartsearch: ~p, Light: ~p, F4: ~p, F3: ~p, F2: ~p, F1: ~p~n",
		  [D_trac, S_sear, F0, F4, F3, F2, F1]),
	if
		Extend == true ->
			Func_states = calc_exte_funcs(binary_to_list(Funcs), []),
			io:fwrite("Extended_functions: ~p~n", [Func_states]);
		true ->
			pass
	end.

drive_train(UDP_details, Loco_address, Direction, Speed, Brake_state) ->
	if
		Brake_state == "normal" ->
			Calc_speed = 0;
		Brake_state == "emergency" ->
			Calc_speed = 1;
		Speed == 0 ->
			Calc_speed = 0;
		true ->
			Calc_speed = Speed + 1
	end,
	if
		Direction == "forward" ->
			Locomotion = Calc_speed bor 128;
		true ->
			Locomotion = Calc_speed band -129
	end,
	XOR = 228 bxor 19 bxor 0 bxor Loco_address bxor Locomotion,
	Message = <<10,0,64,0,228,19,0,Loco_address,Locomotion,XOR>>,
	send_collect(UDP_details, Message, false).

start() ->
	Address = 3,
% 	get_serial_num(udp_details()),
% 	send_collect(udp_details(), Message),
% 	drive_train(udp_details(), Address, "forward", 30, "none"),
	get_loco_info(udp_details(), Address).
