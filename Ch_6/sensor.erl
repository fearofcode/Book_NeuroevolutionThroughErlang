%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(sensor).
-compile(export_all).
-include("records.hrl").

gen(ExoSelfProcess,Node)->
	spawn(Node,?MODULE,loop,[ExoSelfProcess]).

loop(ExoSelfProcess) ->
	receive 
		{ExoSelfProcess,{Id,CortexProcess,SensorName,VL,FanoutProcess}} ->
			loop(Id,CortexProcess,SensorName,VL,FanoutProcess)
	end.
%When gen/2 is executed it spawns the sensor element and immediately begins to wait for its initial state message.

loop(Id,CortexProcess,SensorName,VL,FanoutProcess)->
	receive
		{CortexProcess,sync}->
			SensoryVector = sensor:SensorName(VL),
			[Pid ! {self(),forward,SensoryVector} || Pid <- FanoutProcess],
			loop(Id,CortexProcess,SensorName,VL,FanoutProcess);
		{CortexProcess,terminate} ->
			ok
	end.
%The sensor process accepts only 2 types of messages, both from the cortex. The sensor can either be triggered to begin gathering sensory data based on its sensory role, or terminate if the cortex requests so.

rng(VL)->
	rng(VL,[]).
rng(0,Acc)->
	Acc;
rng(VL,Acc)-> 
	rng(VL-1,[rand:uniform()|Acc]).

%'rng' is a simple random number generator that produces a vector of random values, each between 0 and 1. The length of the vector is defined by the VL, which itself is specified within the sensor record.

