%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(actuator).
-compile(export_all).
-include("records.hrl").

gen(ExoSelfProcess,Node)->
	spawn(Node,?MODULE,loop,[ExoSelfProcess]).

loop(ExoSelfProcess) -> 
	receive 
		{ExoSelfProcess,{Id,CortexProcess,ActuatorName,FaninProcess}} ->
			loop(Id,CortexProcess,ActuatorName,{FaninProcess,FaninProcess},[])
	end.
%When gen/2 is executed it spawns the actuator element and immediately begins to wait for its initial state message.

loop(Id,CortexProcess,ActuatorName,{[FromProcess|FaninProcess],MFaninProcess},Acc) ->
	receive
		{FromProcess,forward,Input} ->
			loop(Id,CortexProcess,ActuatorName,{FaninProcess,MFaninProcess},lists:append(Input,Acc));
		{CortexProcess,terminate} ->
			ok
	end;
loop(Id,CortexProcess,ActuatorName,{[],MFaninProcess},Acc)->
	actuator:ActuatorName(lists:reverse(Acc)),
	CortexProcess ! {self(),sync},
	loop(Id,CortexProcess,ActuatorName,{MFaninProcess,MFaninProcess},[]).
%The actuator process gathers the control signals from the neurons, appending them to the accumulator. The order in which the signals are accumulated into a vector is in the same order as the neuron ids are stored within NeuronIDs. Once all the signals have been gathered, the actuator sends cortex the sync signal, executes its function, and then again begins to wait for the neural signals from the output layer by reseting the FaninProcess from the second copy of the list.

pts(Result)->
	io:format("actuator:pts(Result): ~p~n",[Result]).
%The pts actuation function simply prints to screen the vector passed to it.

