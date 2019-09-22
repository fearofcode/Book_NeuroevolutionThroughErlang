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
	spawn(Node,?MODULE,prep,[ExoSelfProcess]).

prep(ExoSelfProcess) -> 
	receive 
		{ExoSelfProcess,{Id,CortexProcess,Scape,ActuatorName,Parameters,FaninProcess,OpMode}} ->
			put(opmode,OpMode),
			loop(Id,ExoSelfProcess,CortexProcess,Scape,ActuatorName,Parameters,{FaninProcess,FaninProcess},[])
	end.
%When gen/2 is executed it spawns the actuator element and immediately begins to wait for its initial state message.

loop(Id,ExoSelfProcess,CortexProcess,Scape,ActuatorName,Parameters,{[FromProcess|FaninProcess],MFaninProcess},Acc) ->
	receive
		{FromProcess,forward,Input} ->
			loop(Id,ExoSelfProcess,CortexProcess,Scape,ActuatorName,Parameters,{FaninProcess,MFaninProcess},lists:append(Input,Acc));
		{ExoSelfProcess,terminate} ->
			%io:format("Actuator:~p is terminating.~n",[self()])
			ok
	end;
loop(Id,ExoSelfProcess,CortexProcess,Scape,ActuatorName,Parameters,{[],MFaninProcess},Acc)->
	{Fitness,EndFlag} = actuator:ActuatorName(ExoSelfProcess,lists:reverse(Acc),Parameters,Scape),
	CortexProcess ! {self(),sync,Fitness,EndFlag},
	loop(Id,ExoSelfProcess,CortexProcess,Scape,ActuatorName,Parameters,{MFaninProcess,MFaninProcess},[]).
%The actuator process gathers the control signals from the neurons, appending them to the accumulator. The order in which the signals are accumulated into a vector is in the same order as the neuron ids are stored within NeuronIDs. Once all the signals have been gathered, the actuator sends cortex the sync signal, executes its function, and then again begins to wait for the neural signals from the output layer by reseting the FaninProcess from the second copy of the list.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ACTUATORS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pts(ExoSelfProcess,Result,_Scape)->
	io:format("actuator:pts(Result): ~p~n",[Result]),
	{1,0}.
%The pts/2 actuation function simply prints to screen the vector passed to it.

xor_SendOutput(ExoSelfProcess,Output,_Parameters,Scape)->
	Scape ! {self(),action,Output},
	receive 
		{Scape,Fitness,HaltFlag}->
			{Fitness,HaltFlag}
	end.
%xor_sim/2 function simply forwards the Output vector to the XOR simulator, and waits for the resulting Fitness and EndFlag from the simulation process.

pb_SendOutput(ExoSelfProcess,Output,Parameters,Scape)->
	Scape ! {self(),push,Parameters,Output},
	receive 
		{Scape,Fitness,HaltFlag}->
			{Fitness,HaltFlag}
	end.
	
dtm_SendOutput(ExoSelfProcess,Output,Parameters,Scape)->
	Scape ! {self(),move,Parameters,Output},
	receive 
		{Scape,Fitness,HaltFlag}->
			{Fitness,HaltFlag}
	end.
	
fx_Trade(ExoSelfProcess,Output,Parameters,Scape)->
	[TradeSignal] = Output,
	Scape ! {self(),trade,'EURUSD15',functions:trinary(TradeSignal)},
	receive 
		{Scape,Fitness,HaltFlag}->
			{Fitness,HaltFlag}
	end.
