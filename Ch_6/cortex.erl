%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(cortex).
-compile(export_all).
-include("records.hrl").

gen(ExoSelfProcess,Node)->
	spawn(Node,?MODULE,loop,[ExoSelfProcess]).

loop(ExoSelfProcess) ->
	receive 
		{ExoSelfProcess,{Id,SensorProcess,ActuatorProcess,NeuronProcess},TotalSteps} ->
			put(start_time,now()),
			[SensorProcess ! {self(),sync} || SensorProcess <- SensorProcess],
			loop(Id,ExoSelfProcess,SensorProcess,{ActuatorProcess,ActuatorProcess},NeuronProcess,TotalSteps)
	end.
%The gen/2 function spawns the cortex element, which immediately starts to wait for a the state message from the same process that spawned it, exoself. The initial state message contains the sensor, actuator, and neuron ProcessID lists. The message also specifies how many total Sense-Think-Act cycles the Cortex should execute before terminating the NN system. Once we implement the learning algorithm, the termination criteria will depend on the fitness of the NN, or some other useful property

loop(Id,ExoSelfProcess,SensorProcess,{_ActuatorProcess,MActuatorProcess},NeuronProcess,0) ->
	TimeDif = timer:now_diff(now(),get(start_time)),
	io:format("Cortex:~p is backing up and terminating.~n",[Id]),
	io:format("Operational time:~p~n",[TimeDif]),
	NeuronIDsNWeights = get_backup(NeuronProcess,[]),
	ExoSelfProcess ! {self(),backup,NeuronIDsNWeights},
	[ProcessID ! {self(),terminate} || ProcessID <- SensorProcess],
	[ProcessID ! {self(),terminate} || ProcessID <- MActuatorProcess],
	[ProcessID ! {self(),termiante} || ProcessID <- NeuronProcess];
loop(Id,ExoSelfProcess,SensorProcess,{[ActuatorProcess|ActuatorProcess],MActuatorProcess},NeuronProcess,Step) ->
	receive 
		{ActuatorProcess,sync} ->
			loop(Id,ExoSelfProcess,SensorProcess,{ActuatorProcess,MActuatorProcess},NeuronProcess,Step);
		terminate ->
			io:format("Cortex:~p is terminating.~n",[Id]),
			[ProcessID ! {self(),terminate} || ProcessID <- SensorProcess],
			[ProcessID ! {self(),terminate} || ProcessID <- MActuatorProcess],
			[ProcessID ! {self(),termiante} || ProcessID <- NeuronProcess]
	end;
loop(Id,ExoSelfProcess,SensorProcess,{[],MActuatorProcess},NeuronProcess,Step)->
	[ProcessID ! {self(),sync} || ProcessID <- SensorProcess],
	loop(Id,ExoSelfProcess,SensorProcess,{MActuatorProcess,MActuatorProcess},NeuronProcess,Step-1).
%The cortex's goal is to synchronize the the NN system such that when the actuators have received all their control signals, the sensors are once again triggered to gather new sensory information. Thus the cortex waits for the sync messages from the actuator ProcessIDs in its system, and once it has received all the sync messages, it triggers the sensors and then drops back to waiting for a new set of sync messages. The cortex stores 2 copies of the actuator ProcessIDs: the ActuatorProcess, and the MemoryActuatorProcess (MActuatorProcess). Once all the actuators have sent it the sync messages, it can restore the ActuatorProcess list from the MActuatorProcess. Finally, there is also the Step variable which decrements every time a full cycle of Sense-Think-Act completes, once this reaches 0, the NN system begins its termination and backup process.

	get_backup([NeuronProcess|NeuronProcess],Acc)->
		NeuronProcess ! {self(),get_backup},
		receive
			{NeuronProcess,NeuronID,WeightTuples}->
				get_backup(NeuronProcess,[{NeuronID,WeightTuples}|Acc])
		end;
	get_backup([],Acc)->
		Acc.
%During backup, cortex contacts all the neurons in its NN and requests for the neuron's Ids and their WeightedInputs. Once the updated WeightedInputs from all the neurons have been accumulated, the list is sent to exoself for the actual backup and storage.
