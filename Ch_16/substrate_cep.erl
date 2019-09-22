%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(substrate_cep).
-compile(export_all).
-include("records.hrl").

gen(ExoSelfProcess,Node)->
	spawn(Node,?MODULE,prep,[ExoSelfProcess]).

prep(ExoSelfProcess) -> 
	receive 
		{ExoSelfProcess,{Id,CortexProcess,SubstrateProcess,CEPName,Parameters,FaninProcess}} ->
			loop(Id,ExoSelfProcess,CortexProcess,SubstrateProcess,CEPName,Parameters,{FaninProcess,FaninProcess},[])
	end.
%When gen/2 is executed it spawns the actuator element and immediately begins to wait for its initial state message.

loop(Id,ExoSelfProcess,CortexProcess,SubstrateProcess,CEPName,Parameters,{[FromProcess|FaninProcess],MFaninProcess},Acc) ->
	receive
		{FromProcess,forward,Input} ->
			loop(Id,ExoSelfProcess,CortexProcess,SubstrateProcess,CEPName,Parameters,{FaninProcess,MFaninProcess},lists:append(Input,Acc));
		{ExoSelfProcess,terminate} ->
			%io:format("Substrate_CEP:~p is terminating.~n",[self()])
			ok
	end;
loop(Id,ExoSelfProcess,CortexProcess,SubstrateProcess,CEPName,Parameters,{[],MFaninProcess},Acc)->
	ProperlyOrdered_Input=lists:reverse(Acc),
	substrate_cep:CEPName(ProperlyOrdered_Input,Parameters,SubstrateProcess),
	loop(Id,ExoSelfProcess,CortexProcess,SubstrateProcess,CEPName,Parameters,{MFaninProcess,MFaninProcess},[]).
%The substrate_cep process gathers the control signals from the neurons, appending them to the accumulator. The order in which the signals are accumulated into a vector is in the same order that the neuron ids are stored within NeuronIDs. Once all the signals have been gathered, the substrate_cep executes its function, forwards the processed signal to the substrate, and then again begins to wait for the neural signals from the output layer by reseting the FaninProcess from the second copy of the list.


%%%%%%%% Substrate_CEPs %%%%%%%% 
set_weight(Output,_Parameters,SubstrateProcess)->
	[Val] = Output,
	Threshold = 0.33,
	Weight = if 
		Val > Threshold ->
			(functions:scale(Val,1,Threshold)+1)/2;
		Val < -Threshold ->
			(functions:scale(Val,-Threshold,-1)-1)/2;
		true ->
			0
	end,
	SubstrateProcess ! {self(),set_weight,[Weight]}.
%
