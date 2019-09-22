%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(substrate_cpp).
-compile(export_all).
-include("records.hrl").

gen(ExoSelfProcess,Node)->
	spawn(Node,?MODULE,prep,[ExoSelfProcess]).

prep(ExoSelfProcess) ->
	receive 
		{ExoSelfProcess,{Id,CortexProcess,SubstrateProcess,CPPName,VL,Parameters,FanoutProcess}} ->
			loop(Id,ExoSelfProcess,CortexProcess,SubstrateProcess,CPPName,VL,Parameters,FanoutProcess)
	end.
%When gen/2 is executed, it spawns the substrate_cpp element and immediately begins to wait for its initial state message.

loop(Id,ExoSelfProcess,CortexProcess,SubstrateProcess,CPPName,VL,Parameters,FanoutProcess)->
	receive
		{SubstrateProcess,Presynaptic_Coords,Postsynaptic_Coords}->
			SensoryVector = functions:CPPName(Presynaptic_Coords,Postsynaptic_Coords),
			[Pid ! {self(),forward,SensoryVector} || Pid <- FanoutProcess],
			loop(Id,ExoSelfProcess,CortexProcess,SubstrateProcess,CPPName,VL,Parameters,FanoutProcess);
		{SubstrateProcess,Presynaptic_Coords,Postsynaptic_Coords,IOW}->
			SensoryVector = functions:CPPName(Presynaptic_Coords,Postsynaptic_Coords,IOW),
			[Pid ! {self(),forward,SensoryVector} || Pid <- FanoutProcess],
			loop(Id,ExoSelfProcess,CortexProcess,SubstrateProcess,CPPName,VL,Parameters,FanoutProcess);
		{ExoSelfProcess,terminate} ->
			%io:format("substrate_cpp:~p is terminating.~n",[Id]),
			ok
	end.
