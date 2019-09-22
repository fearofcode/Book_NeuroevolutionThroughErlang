%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(exoself).
-compile(export_all).
-include("records.hrl").
-record(state,{file_name,genotype,neuron_process_ids,cx_pid,spids,npids,apids,highest_fitness,tot_evaluations,tot_cycles}).
-define(MAX_ATTEMPTS,10).

start(AgentID)->
	case whereis(monitor) of
		undefined ->
			io:format("start(AgentID):: 'monitor' is not registered~n");
		ProcessID ->
			start(AgentID,ProcessID)
	end.
start(AgentID,PMProcess)->
	spawn(exoself,prep,[AgentID,PMProcess]).
%The start/2 function spawns a new AgentID exoself process, belonging to the population_monitor process with the pid PMProcess.

prep(AgentID,PMProcess)->
	random:seed(now()),
	NeuronPIDs = ets:new(neuron_process_ids,[set,private]), 
	A = genotype:dirty_read({agent,AgentID}),
	Cortex = genotype:dirty_read({cortex,A#agent.cortex_id}),
	SensorIDs = Cortex#cortex.sensor_ids,
	ActuatorIDs = Cortex#cortex.actuator_ids,
	NeuronIDs = Cortex#cortex.neuron_ids,
	ScapeProcessIDs = spawn_Scapes(NeuronPIDs,SensorIDs,ActuatorIDs),
	spawnCerebralUnits(NeuronPIDs,cortex,[Cortex#cortex.id]),
	spawnCerebralUnits(NeuronPIDs,sensor,SensorIDs),
	spawnCerebralUnits(NeuronPIDs,actuator,ActuatorIDs),
	spawnCerebralUnits(NeuronPIDs,neuron,NeuronIDs),
	link_Sensors(SensorIDs,NeuronPIDs),
	link_Actuators(ActuatorIDs,NeuronPIDs),
	link_Neurons(NeuronIDs,NeuronPIDs),
	{SensorProcess,NeuronProcess,ActuatorProcess}=linkCortex(Cortex,NeuronPIDs),
	CortexProcess = ets:lookup_element(NeuronPIDs,Cortex#cortex.id,2),
	loop(AgentID,PMProcess,NeuronPIDs,CortexProcess,SensorProcess,NeuronProcess,ActuatorProcess,ScapeProcessIDs,0,0,0,0,1).
%The prep/2 function prepares and sets up the exoself's state before dropping into the main loop. The function first reads the agent and cortex records belonging to the AgentID NN based system. The function then reads the sensor, actuator, and neuron ids, then spawns the private scapes using the spawn_Scapes/3 function, spawns the cortex, sensor, actuator, and neuron processes, and then finally links up all these processes together using the link_.../2 processes. Once the phenotype has been generated from the genotype, the exoself drops into its main loop.

loop(AgentID,PMProcess,NeuronPIDs,CortexProcess,SensorProcess,NeuronProcess,ActuatorProcess,ScapeProcessIDs,HighestFitness,EvalAcc,CycleAcc,TimeAcc,Attempt)->
	receive
		{CortexProcess,evaluation_completed,Fitness,Cycles,Time}->
			{U_HighestFitness,U_Attempt}=case Fitness > HighestFitness of
				true ->
					[NeuronProcess ! {self(),weight_backup} || NeuronProcess <- NeuronProcess],
					{Fitness,0};
				false ->
					Perturbed_NeuronProcess=get(perturbed),
					[NeuronProcess ! {self(),weight_restore} || NeuronProcess <- Perturbed_NeuronProcess],
					{HighestFitness,Attempt+1}
			end,
			[ProcessID ! {self(), reset_prep} || ProcessID <- NeuronProcess],
			gather_acks(length(NeuronProcess)),
			[ProcessID ! {self(), reset} || ProcessID <- NeuronProcess],
			%io:format("HighestFitness:~p U_Attempt:~p~n",[HighestFitness,U_Attempt]),
			case U_Attempt >= ?MAX_ATTEMPTS of
				true ->	%End training
					U_CycleAcc = CycleAcc+Cycles,
					U_TimeAcc = TimeAcc+Time,
					A=genotype:dirty_read({agent,AgentID}),
					genotype:write(A#agent{fitness=U_HighestFitness}),
					backup_genotype(NeuronPIDs,NeuronProcess),
					terminate_phenotype(CortexProcess,SensorProcess,NeuronProcess,ActuatorProcess,ScapeProcessIDs),
					io:format("Agent:~p terminating. Genotype has been backed up.~n Fitness:~p~n TotalEvaluations:~p~n TotalCycles:~p~n TimeAcc:~p~n",
						[self(),U_HighestFitness,EvalAcc,U_CycleAcc,U_TimeAcc]),
					gen_server:cast(PMProcess,{self(),terminated,U_HighestFitness,EvalAcc+1,U_CycleAcc,U_TimeAcc});
				false -> %Continue training
					Total_Neurons = length(NeuronProcess),
					MP = 1/math:sqrt(Total_Neurons),
					Perturb_NeuronProcess=[NeuronProcess || NeuronProcess <- NeuronProcess,rand:uniform()<MP],
					%io:format("Perturb_NeuronProcess:~p~n",[Perturb_NeuronProcess]),
					put(perturbed,Perturb_NeuronProcess),
					[NeuronProcess ! {self(),weight_perturb} || NeuronProcess <- Perturb_NeuronProcess],
					CortexProcess ! {self(),reactivate},
					loop(AgentID,PMProcess,NeuronPIDs,CortexProcess,SensorProcess,NeuronProcess,ActuatorProcess,ScapeProcessIDs,U_HighestFitness,EvalAcc+1,CycleAcc+Cycles,TimeAcc+Time,U_Attempt)
			end
	end.
%The exoself process' main loop awaits from its cortex proccess the evoluation_completed message. Once the message is received, based on the fitness achieved, exoself decides whether to continue tunning the weights or terminate the system. Exoself tries to improve the fitness by perturbing/tuning the weights of its neurons, after each tuning session, the Neural Network based system performs another evaluation by interacting with the scape until completion (the NN solves a problem, or dies within the scape or...). The order of events is important: When evaluation_completed message is received, the function first checks whether the newly achieved fitness is higher than the highest fitness achieved so far. If it is not, the function sends the neurons a message to restore their weights to previous state, during which it last acehived the highest fitness instead of their current state which yielded the current lower fitness score. If on the other hand the new fitness is higher than the previously highest achieved fitness, then the function tells the neurons to backup their current weights, as these weights represent the NN's best, most fit form yet. Exoself then tells all the neurons to prepare for a reset by sending each neuron the {self(),reset_prep} message. Since the NN can have recursive connections, and the manner in which initial recursive messages are sent, it is important for each neuron to flush their buffers to be reset into an initial fresh state, which is achieved after the neurons receive the reset_prep message. The function then sends the reset message to the neurons, which returns them into their main loop. Finally, the function checks whether exoself has already tried to improve the NN's fitness a maximum (?MAX_ATTEMPTS) number of times. If that is the case, the exoself process backs up the updated NN (the updated, tuned weights) to database using the backup_genotype/2 function, prints to screen that it is terminating, and sends to the population_monitor the acumulated statistics (highest fitness, evaluation count, cycle count...). On the other hand, if the exoself is not yet done tuning the neural weights, it has not yet reached its ending condition, it then randomly selects a set of neurons from its NeuronProcess list, and request that they perturb their weights, and then reactivates the cortex, and drops back into the main loop. Each neuron in the NeuronProcess list has a probability 1/math(sqrt(Total_Neurons) of being selected for weight perturbation, a number that is proportional to the total number of neurons in the NN, and grows with the NN size.

	spawnCerebralUnits(NeuronPIDs,CerebralUnitType,[Id|Ids])-> 
		ProcessID = CerebralUnitType:gen(self(),node()),
		ets:insert(NeuronPIDs,{Id,ProcessID}), 
		ets:insert(NeuronPIDs,{ProcessID,Id}), 
		spawnCerebralUnits(NeuronPIDs,CerebralUnitType,Ids); 
	spawnCerebralUnits(NeuronPIDs,_CerebralUnitType,[])-> 
		true.
%We spawn the process for each element based on its type: CerebralUnitType, and the gen function that belongs to the CerebralUnitType module. We then enter the {Id,ProcessID} tuple into our ETS table for later use.

	spawn_Scapes(NeuronPIDs,SensorIDs,ActuatorIDs)-> 
		Sensor_Scapes = [(genotype:dirty_read({sensor,Id}))#sensor.scape || Id<-SensorIDs], 
		Actuator_Scapes = [(genotype:dirty_read({actuator,Id}))#actuator.scape || Id<-ActuatorIDs], 
		Unique_Scapes = Sensor_Scapes++(Actuator_Scapes--Sensor_Scapes), 
		SN_Tuples=[{scape:gen(self(),node()),ScapeName} || {private,ScapeName}<-Unique_Scapes], 
		[ets:insert(NeuronPIDs,{ScapeName,ProcessID}) || {ProcessID,ScapeName} <- SN_Tuples], 
		[ets:insert(NeuronPIDs,{ProcessID,ScapeName}) || {ProcessID,ScapeName} <-SN_Tuples], 
		[ProcessID ! {self(),ScapeName} || {ProcessID,ScapeName} <- SN_Tuples],
		[ProcessID || {ProcessID,_ScapeName} <-SN_Tuples].
%The spawn_Scapes/3 function first extracts all the scapes that the sensors and actuators interface with, it then creates a filtered scape list which only holds unique scape records, after which it further only selects those scapes that are private, and spawns them.

	link_Sensors([SensorID|SensorIDs],NeuronPIDs) ->
		S=genotype:dirty_read({sensor,SensorID}),
		SensorProcess = ets:lookup_element(NeuronPIDs,SensorID,2),
		CortexProcess = ets:lookup_element(NeuronPIDs,S#sensor.cortex_id,2),
		SensorName = S#sensor.name,
		FanoutIDs = S#sensor.fanout_ids,
		FanoutProcess = [ets:lookup_element(NeuronPIDs,Id,2) || Id <- FanoutIDs],
		Scape=case S#sensor.scape of
			{private,ScapeName}->
				ets:lookup_element(NeuronPIDs,ScapeName,2)
		end,
		SensorProcess ! {self(),{SensorID,CortexProcess,Scape,SensorName,S#sensor.vector_length,FanoutProcess}},
		link_Sensors(SensorIDs,NeuronPIDs);
	link_Sensors([],NeuronPIDs)->
		ok.
%The link_Sensors/2 function sends to the already spawned and waiting sensors their states, composed of the ProcessID lists and other information which are needed by the sensors to link up and interface with other elements in the distributed phenotype.

	link_Actuators([ActuatorID|ActuatorIDs],NeuronPIDs) ->
		A=genotype:dirty_read({actuator,ActuatorID}),
		ActuatorProcess = ets:lookup_element(NeuronPIDs,ActuatorID,2),
		CortexProcess = ets:lookup_element(NeuronPIDs,A#actuator.cortex_id,2),
		ActuatorName = A#actuator.name,
		FaninIDs = A#actuator.fanin_ids,
		FaninProcess = [ets:lookup_element(NeuronPIDs,Id,2) || Id <- FaninIDs],
		Scape=case A#actuator.scape of
			{private,ScapeName}->
				ets:lookup_element(NeuronPIDs,ScapeName,2)
		end,
		ActuatorProcess ! {self(),{ActuatorID,CortexProcess,Scape,ActuatorName,FaninProcess}},
		link_Actuators(ActuatorIDs,NeuronPIDs);
	link_Actuators([],NeuronPIDs)->
		ok.
%The link_Actuators2 function sends to the already spawned and waiting actuators their states, composed of the ProcessID lists and other information which are needed by the actuators to link up and interface with other elements in the distributed phenotype.

	link_Neurons([NeuronID|NeuronIDs],NeuronPIDs) ->
		N=genotype:dirty_read({neuron,NeuronID}),
		NeuronProcess = ets:lookup_element(NeuronPIDs,NeuronID,2),
		CortexProcess = ets:lookup_element(NeuronPIDs,N#neuron.cortex_id,2),
		ActivationFunctionName = N#neuron.af,
		WeightedInputs = N#neuron.weighted_inputs,
		OutputIDs = N#neuron.output_ids,
		ROIDs = N#neuron.ro_ids,
		WeightedInputProcess = convertWeightedInputsToWeightedInputProcessIDs(NeuronPIDs,WeightedInputs,[]),
		OutputProcess = [ets:lookup_element(NeuronPIDs,Id,2) || Id <- OutputIDs],
		ROProcess = [ets:lookup_element(NeuronPIDs,Id,2) || Id <- ROIDs],
		NeuronProcess ! {self(),{NeuronID,CortexProcess,ActivationFunctionName,WeightedInputProcess,OutputProcess,ROProcess}},
		link_Neurons(NeuronIDs,NeuronPIDs);
	link_Neurons([],NeuronPIDs)->
		ok.
%The link_Neurons/2 function sends to the already spawned and waiting neurons their states, composed of the ProcessID lists and other information needed by the neurons to link up and interface with other elements in the distributed phenotype.

		convertWeightedInputsToWeightedInputProcessIDs(NeuronPIDs,[{bias,[Bias]}],Acc)->
			lists:reverse([Bias|Acc]);
		convertWeightedInputsToWeightedInputProcessIDs(NeuronPIDs,[{Id,Weights}|FaninWeightedInputs],Acc)->
			convertWeightedInputsToWeightedInputProcessIDs(NeuronPIDs,FaninWeightedInputs,[{ets:lookup_element(NeuronPIDs,Id,2),Weights}|Acc]);
		convertWeightedInputsToWeightedInputProcessIDs(NeuronPIDs,[],Acc)->
			lists:reverse(Acc).
%The convertWeightedInputsToWeightedInputProcessIDs/3 converts the IdPs tuples into tuples that use ProcessIDs instead of Ids, such that the Neuron will know which weights are to be associated with which incoming vector signals. The last element is the bias, which is added to the list in a non tuple form. Afterwards, the list is reversed to take its proper order.

	linkCortex(Cortex,NeuronPIDs) ->
		CortexID = Cortex#cortex.id,
		CortexProcess = ets:lookup_element(NeuronPIDs,CortexID,2),
		SensorIDs = Cortex#cortex.sensor_ids,
		ActuatorIDs = Cortex#cortex.actuator_ids,
		NeuronIDs = Cortex#cortex.neuron_ids,
		SensorProcess = [ets:lookup_element(NeuronPIDs,SensorID,2) || SensorID <- SensorIDs],
		NeuronProcess = [ets:lookup_element(NeuronPIDs,NeuronID,2) || NeuronID <- NeuronIDs],
		ActuatorProcess = [ets:lookup_element(NeuronPIDs,ActuatorID,2) || ActuatorID <- ActuatorIDs],
		CortexProcess ! {self(),CortexID,SensorProcess,NeuronProcess,ActuatorProcess},
		{SensorProcess,NeuronProcess,ActuatorProcess}.
%The linkCortex/2 function sends to the already spawned and waiting cortex its state, composed of the ProcessID lists and other information which is needed by the cortex to link up and interface with other elements in the distributed phenotype.

backup_genotype(NeuronPIDs,NeuronProcess)->
	NeuronIDsNWeights = get_backup(NeuronProcess,[]),
	updateGenotype(NeuronPIDs,NeuronIDsNWeights),
	io:format("Finished updating genotype~n").

	get_backup([NeuronProcess|NeuronProcess],Acc)->
		NeuronProcess ! {self(),get_backup},
		receive
			{NeuronProcess,NeuronID,WeightTuples}->
				get_backup(NeuronProcess,[{NeuronID,WeightTuples}|Acc])
		end;
	get_backup([],Acc)->
		Acc.
%The backup_genotype/2 uses get_backup/2 to contact all the neurons in its NN and request for the neuron's Ids and their WeightedInputs. Once the updated WeightedInputs from all the neurons have been accumulated, they are passed through the updateGenotype/2 function to produce updated neurons, and write them to database.

	updateGenotype(NeuronPIDs,[{NeuronID,WeightedInputProcessIDs}|WeightPs])->
		Neuron = genotype:dirty_read({neuron,NeuronID}),
		UpdatedWeightedInput = convertWeightedInputProcessesToWeightedInputs(NeuronPIDs,WeightedInputProcessIDs,[]),
		UpdatedNeuron = Neuron#neuron{weighted_inputs = UpdatedWeightedInput},
		genotype:write(UpdatedNeuron),
		%io:format("N:~p~n UpdatedNeuron:~p~n Genotype:~p~n UpdatedGenotype:~p~n",[N,UpdatedNeuron,Genotype,UpdatedGenotype]),
		updateGenotype(NeuronPIDs,WeightPs);
	updateGenotype(NeuronPIDs,[])->
		ok.
%For every {NeuronID,WeightedInputProcessIDs} tuple the updateGenotype/3 function extracts the neuron with the id: NeuronID, updates the neuron's inputWeightedInputs, and writes the updated neuron to database.

		convertWeightedInputProcessesToWeightedInputs(NeuronPIDs,[{ProcessID,Weights}|WeightedInputProcess],Acc)->
			convertWeightedInputProcessesToWeightedInputs(NeuronPIDs,WeightedInputProcess,[{ets:lookup_element(NeuronPIDs,ProcessID,2),Weights}|Acc]);
		convertWeightedInputProcessesToWeightedInputs(NeuronPIDs,[Bias],Acc)->
			lists:reverse([{bias,[Bias]}|Acc]);
		convertWeightedInputProcessesToWeightedInputs(NeuronPIDs,[],Acc)->
			lists:reverse(Acc).
%The convertWeightedInputProcessesToWeightedInputs/3 performs the conversion from ProcessIDs to Ids of every {ProcessID,Weights} tuple in the WeightedInputProcess list. The updated WeightedInputs are then returned to the caller.
	
terminate_phenotype(CortexProcess,SensorProcess,NeuronProcess,ActuatorProcess,ScapeProcessIDs)->
	io:format("Terminating the phenotype:~nCortexProcess:~p~nSensorProcess:~p~nNeuronProcess:~p~nActuatorProcess:~p~nScapePids:~p~n",[CortexProcess,SensorProcess,NeuronProcess,ActuatorProcess,ScapeProcessIDs]),
	[ProcessID ! {self(),terminate} || ProcessID <- SensorProcess],
	[ProcessID ! {self(),terminate} || ProcessID <- ActuatorProcess],
	[ProcessID ! {self(),termiante} || ProcessID <- NeuronProcess],
	[ProcessID ! {self(),terminate} || ProcessID <- ScapeProcessIDs],
	CortexProcess ! {self(),terminate}.
%The terminate_phenotype/5 function termiantes sensors, actuators, neurons, all private scapes, and the cortex which composes the NN based system.

gather_acks(0)->
	done;	
gather_acks(ProcessIDIndex)->
	receive
		{_From,ready}->
			gather_acks(ProcessIDIndex-1)
		after 100000 ->
			io:format("******** Not all acks received:~p~n",[ProcessIDIndex])
	end.
%gather_acks/1 ensures that the X number of {From,ready} messages are sent to it, before it returns with done. X is set by the caller of the function.
