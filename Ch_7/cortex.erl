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
%The gen/2 function spawns the cortex element, which immediately starts to wait for its initial state message from the same process that spawned it, exoself. The initial state message contains the sensor, actuator, and neuron ProcessID lists. Before dropping into the main loop, CycleAcc, FitnessAcc, and HFAcc (HaltFlag Acc), are all set to 0, and the status of the cortex is set to ac- tive, prompting it to begin the synchronization process and call the sensors to action.

loop(Id,ExoSelfProcess,SensorProcess,{[ActuatorProcess|ActuatorProcess],MActuatorProcess},NeuronProcess,CycleAcc,FitnessAcc,EFAcc,active) ->
	receive 
		{ActuatorProcess,sync,Fitness,EndFlag} ->
			loop(Id,ExoSelfProcess,SensorProcess,{ActuatorProcess,MActuatorProcess},NeuronProcess,CycleAcc,FitnessAcc+Fitness,EFAcc+EndFlag,active);
		terminate ->
			io:format("Cortex:~p is terminating.~n",[Id]),
			[ProcessID ! {self(),terminate} || ProcessID <- SensorProcess],
			[ProcessID ! {self(),terminate} || ProcessID <- MActuatorProcess],
			[ProcessID ! {self(),termiante} || ProcessID <- NeuronProcess]
	end;
loop(Id,ExoSelfProcess,SensorProcess,{[],MActuatorProcess},NeuronProcess,CycleAcc,FitnessAcc,EFAcc,active)->
	case EFAcc > 0 of
		true ->
			TimeDif=timer:now_diff(now(),get(start_time)),
			ExoSelfProcess ! {self(),evaluation_completed,FitnessAcc,CycleAcc,TimeDif},
			loop(Id,ExoSelfProcess,SensorProcess,{MActuatorProcess,MActuatorProcess},NeuronProcess,CycleAcc,FitnessAcc,EFAcc,inactive);
		false ->
			[ProcessID ! {self(),sync} || ProcessID <- SensorProcess],
			loop(Id,ExoSelfProcess,SensorProcess,{MActuatorProcess,MActuatorProcess},NeuronProcess,CycleAcc+1,FitnessAcc,EFAcc,active)
	end;
loop(Id,ExoSelfProcess,SensorProcess,{MActuatorProcess,MActuatorProcess},NeuronProcess,_CycleAcc,_FitnessAcc,_EFAcc,inactive)->
	receive
		{ExoSelfProcess,reactivate}->
			put(start_time,now()),
			[SensorProcess ! {self(),sync} || SensorProcess <- SensorProcess],
			loop(Id,ExoSelfProcess,SensorProcess,{MActuatorProcess,MActuatorProcess},NeuronProcess,1,0,0,active);
		{ExoSelfProcess,terminate}->
			ok
	end.
%The cortex’s goal is to synchronize the NN system’s sensors and actuators. When the actuators have received all their control signals, they forward the sync messages, the Fitness, and the HaltFlag messages to the cortex. The cortex accumulates these Fitness and HaltFlag signals, and if any of the HaltFlag signals have been set to 1, HFAcc will be greater than 0, signifying that the cortex should halt. When EFAcc > 0, the cortex calculates the total amount of time it has ran (TimeDiff), and forwards to exoself the values: FitnessAcc, CycleAcc, and TimeDiff. Afterwards, the cortex enters the inactive mode and awaits further instructions from the exoself. If none of the HaltFlags were set to 0, then the value HFAcc == 0, and the cortex triggers off another Sense-Think-Act cycle. The reason the cortex process stores 2 copies of the actuator ProcessIDs: the ActuatorProcess, and the MemoryActuatorProcess (MActuatorProcess), is so that once all the actuators have sent it the sync messages, it can restore the ActuatorProcess list from the MActuatorProcess.

