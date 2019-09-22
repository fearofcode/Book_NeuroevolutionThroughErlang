%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(cortex).
-compile(export_all).
-include("records.hrl").
-record(state,{id,exoself_pid,spids,npids,apids,cycle_acc=0,fitness_acc=0,endflag=0,status}).

gen(ExoSelfProcess,Node)->
	spawn(Node,?MODULE,prep,[ExoSelfProcess]).

prep(ExoSelfProcess) ->
	{V1,V2,V3} = now(),
	random:seed(V1,V2,V3),
	receive 
		{ExoSelfProcess,Id,SensorProcess,NeuronProcess,ActuatorProcess} ->
			put(start_time,now()),
			[SensorProcess ! {self(),sync} || SensorProcess <- SensorProcess],
			loop(Id,ExoSelfProcess,SensorProcess,{ActuatorProcess,ActuatorProcess},NeuronProcess,1,0,0,active)
	end.
%The gen/2 function spawns the cortex element, which immediately starts to wait for a the state message from the same process that spawned it, exoself. The initial state message contains the sensor, actuator, and neuron ProcessID lists. The message also specifies how many total Sense-Think-Act cycles the Cortex should execute before terminating the NN system. Once we implement the learning algorithm, the termination criteria will depend on the fitness of the NN, or some other useful property

loop(Id,ExoSelfProcess,SensorProcess,{[ActuatorProcess|ActuatorProcess],MActuatorProcess},NeuronProcess,CycleAcc,FitnessAcc,EFAcc,active) ->
	receive 
		{ActuatorProcess,sync,Fitness,EndFlag} ->%io:format("FitnessAcc:~p~n",[FitnessAcc]),
			case Fitness == goal_reached of
				true ->
					put(goal_reached,true),
					loop(Id,ExoSelfProcess,SensorProcess,{ActuatorProcess,MActuatorProcess},NeuronProcess,CycleAcc,FitnessAcc+Fitness,EFAcc+EndFlag,active);
				false ->
					loop(Id,ExoSelfProcess,SensorProcess,{ActuatorProcess,MActuatorProcess},NeuronProcess,CycleAcc,FitnessAcc+Fitness,EFAcc+EndFlag,active)
			end;
		terminate ->
			%io:format("Cortex:~p is terminating.~n",[Id]),
			[ProcessID ! {self(),terminate} || ProcessID <- SensorProcess],
			[ProcessID ! {self(),terminate} || ProcessID <- MActuatorProcess],
			[ProcessID ! {self(),termiante} || ProcessID <- NeuronProcess]
	end;
loop(Id,ExoSelfProcess,SensorProcess,{[],MActuatorProcess},NeuronProcess,CycleAcc,FitnessAcc,EFAcc,active)->
	case EFAcc > 0 of
		true ->%Organism finished evaluation
			TimeDif=timer:now_diff(now(),get(start_time)),
			ExoSelfProcess ! {self(),evaluation_completed,FitnessAcc,CycleAcc,TimeDif,get(goal_reached)},
			cortex:loop(Id,ExoSelfProcess,SensorProcess,{MActuatorProcess,MActuatorProcess},NeuronProcess,CycleAcc,FitnessAcc,EFAcc,inactive);
		false ->
			[ProcessID ! {self(),sync} || ProcessID <- SensorProcess],
			cortex:loop(Id,ExoSelfProcess,SensorProcess,{MActuatorProcess,MActuatorProcess},NeuronProcess,CycleAcc+1,FitnessAcc,EFAcc,active)
	end;
loop(Id,ExoSelfProcess,SensorProcess,{MActuatorProcess,MActuatorProcess},NeuronProcess,_CycleAcc,_FitnessAcc,_EFAcc,inactive)->
	receive
		{ExoSelfProcess,reactivate}->
			put(start_time,now()),
			[SensorProcess ! {self(),sync} || SensorProcess <- SensorProcess],%io:format("_FitnessAcc:~p~n",[_FitnessAcc]),
			cortex:loop(Id,ExoSelfProcess,SensorProcess,{MActuatorProcess,MActuatorProcess},NeuronProcess,1,0,0,active);
		{ExoSelfProcess,terminate}->
			%io:format("Cortex:~p is terminating.~n",[Id]),
			ok
	end.
%The cortex's goal is to synchronize the the NN system such that when the actuators have received all their control signals, the sensors are once again triggered to gather new sensory information. Thus the cortex waits for the sync messages from the actuator ProcessIDs in its system, and once it has received all the sync messages, it triggers the sensors and then drops back to waiting for a new set of sync messages. The cortex stores 2 copies of the actuator ProcessIDs: the ActuatorProcess, and the MemoryActuatorProcess (MActuatorProcess). Once all the actuators have sent it the sync messages, it can restore the ActuatorProcess list from the MActuatorProcess. Finally, there is also the Step variable which decrements every time a full cycle of Sense-Think-Act completes, once this reaches 0, the NN system begins its termination and backup process.
