-module(erlangZ21).
-export([start/0, send_collect/3, get_serial_num/1, drive_train/5]).

open_socket(Port_number, Active_state)  ->
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
	Package = send_collect(UDP_details, Message, true),
	Serial = binary:decode_unsigned(binary:part(Package, {byte_size(Package), -4}), little),
	io:fwrite("Serial number: ~p~n", [Serial]).

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
	Local_port = 8799, 
	Dest_IP = {192,168,0,111},
	Dest_port = 21105,
	UDP_details = {Local_port, Dest_IP, Dest_port},
% 	get_serial_num(UDP_details),
% 	send_collect({Local_port, Dest_IP, Dest_port}, Message).
	drive_train(UDP_details, 3, "forward", 30, "none").
