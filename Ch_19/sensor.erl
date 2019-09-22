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
	spawn(Node,?MODULE,prep,[ExoSelfProcess]).

prep(ExoSelfProcess) ->
	receive 
		{ExoSelfProcess,{Id,CortexProcess,Scape,SensorName,VL,Parameters,FanoutProcess,OpMode}} ->
			put(opmode,OpMode),
			loop(Id,ExoSelfProcess,CortexProcess,Scape,SensorName,VL,Parameters,FanoutProcess)
	end.
%When gen/2 is executed it spawns the sensor element and immediately begins to wait for its initial state message.

loop(Id,ExoSelfProcess,CortexProcess,Scape,SensorName,VL,Parameters,FanoutProcess)->
	receive
		{CortexProcess,sync}->
			SensoryVector = sensor:SensorName(ExoSelfProcess,VL,Parameters,Scape),
			[Pid ! {self(),forward,SensoryVector} || Pid <- FanoutProcess],
			loop(Id,ExoSelfProcess,CortexProcess,Scape,SensorName,VL,Parameters,FanoutProcess);
		{ExoSelfProcess,terminate} ->
			%io:format("Sensor:~p is terminating.~n",[Id]),
			ok
	end.
%The sensor process accepts only 2 types of messages, both from the cortex. The sensor can either be triggered to begin gathering sensory data based on its sensory role, or terminate if the cortex requests so.

rng(ExoSelfProcess,VL,_Scape)->
	rng1(VL,[]).
rng1(0,Acc)->
	Acc;
rng1(VL,Acc)-> 
	rng1(VL-1,[rand:uniform()|Acc]).
%rng/2 is a simple random number generator that produces a vector of random values, each between 0 and 1. The length of the vector is defined by the VL, which itself is specified within the sensor record.

xor_GetInput(ExoSelfProcess,VL,_Parameters,Scape)->
	Scape ! {self(),sense},
	receive
		{Scape,percept,SensoryVector}->
			case length(SensoryVector)==VL of
				true ->
					SensoryVector;
				false ->
					io:format("Error in sensor:xor_sim/3, VL:~p SensoryVector:~p~n",[VL,SensoryVector]),
					lists:duplicate(VL,0)
			end
	end.
%xor_GetInput/2 contacts the XOR simulator and requests the sensory vector, which in this case should be a binary vector of length 2. The sensor checks that the incoming sensory signal, the percept, is indeed of length 2. If the vector length differs, then this is printed to the console and a dummy vector of appropriate length is constructed.

pb_GetInput(ExoSelfProcess,VL,Parameters,Scape)->
	Scape ! {self(),sense,Parameters},
	receive
		{Scape,percept,SensoryVector}->
			case length(SensoryVector)==VL of
				true ->
					SensoryVector;
				false ->
					io:format("Error in sensor:pb_GetInput/3, VL:~p SensoryVector:~p~n",[VL,SensoryVector]),
					lists:duplicate(VL,0)
			end
	end.
	
dtm_GetInput(ExoSelfProcess,VL,Parameters,Scape)->
	Scape ! {self(),sense,Parameters},
	receive
		{Scape,percept,SensoryVector}->
			%io:format("self():~p SensoryVector:~p~n",[self(),SensoryVector]),
			case length(SensoryVector)==VL of
				true ->
					SensoryVector;
				false ->
					io:format("Error in sensor:dtm_GetInput/3, VL:~p SensoryVector:~p~n",[VL,SensoryVector]),
					lists:duplicate(VL,0)
			end
	end.
	
fx_PCI(ExoselfID,VL,Parameters,Scape)->
	[HRes,VRes] = Parameters,
	case get(opmode) of
		gt	->
			%Normal, assuming we have 10000 rows, we start from 1000 to 6000
			Scape ! {self(),sense,'EURUSD15',close,[HRes,VRes,graph_sensor],1000,200};
		benchmark ->
			Scape ! {self(),sense,'EURUSD15',close,[HRes,VRes,graph_sensor],200,last}
	end,
	receive 
		{_From,Result}->
			Result
	end.

fx_PLI(ExoselfID,VL,Parameters,Scape)->
	[HRes,Type] = Parameters,%Type=open|close|high|low
	case get(opmode) of
		gt	->
			%Normal, assuming we have 10000 rows, we start from 1000 to 6000
			Scape ! {self(),sense,'EURUSD15',close,[HRes,list_sensor],1000,200};
		benchmark ->
			Scape ! {self(),sense,'EURUSD15',close,[HRes,list_sensor],200,last}
	end,
	receive 
		{_From,Result}->
			normalize(Result)
	end.
	
	normalize(Vector)->
		Normalizer=math:sqrt(lists:sum([Val*Val||Val<-Vector])),
		[Val/Normalizer || Val <- Vector].
	
fx_Internals(ExoselfID,VL,Parameters,Scape)->
	Scape ! {self(),sense,internals,Parameters},
	receive
		{ProcessID,Result}->
			Result
	end.
