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
-define(MAX_ATTEMPTS,50).

map()-> map(ffnn).
map(FileName)->
	Genotype=genotype:load_from_file(FileName),
	spawn(exoself,prep,[FileName,Genotype]).

prep(FileName,Genotype)->
	{V1,V2,V3} = now(),
	random:seed(V1,V2,V3),
	NeuronPIDs = ets:new(neuron_process_ids,[set,private]), 
	Cortex = genotype:read(Genotype,cortex),
	SensorIDs = Cortex#cortex.sensor_ids,
	ActuatorIDs = Cortex#cortex.actuator_ids,
	NeuronIDs = Cortex#cortex.neuron_ids,
	ScapeProcessIDs = spawn_Scapes(NeuronPIDs,Genotype,SensorIDs,ActuatorIDs),
	spawnCerebralUnits(NeuronPIDs,cortex,[Cortex#cortex.id]),
	spawnCerebralUnits(NeuronPIDs,sensor,SensorIDs),
	spawnCerebralUnits(NeuronPIDs,actuator,ActuatorIDs),
	spawnCerebralUnits(NeuronPIDs,neuron,NeuronIDs),
	link_Sensors(Genotype,SensorIDs,NeuronPIDs),
	link_Actuators(Genotype,ActuatorIDs,NeuronPIDs),
	link_Neurons(Genotype,NeuronIDs,NeuronPIDs),
	{SensorProcess,NeuronProcess,ActuatorProcess}=linkCortex(Cortex,NeuronPIDs),
	CortexProcess = ets:lookup_element(NeuronPIDs,Cortex#cortex.id,2),
	loop(FileName,Genotype,NeuronPIDs,CortexProcess,SensorProcess,NeuronProcess,ActuatorProcess,ScapeProcessIDs,0,0,0,0,1).
%Once the FileName and the Genotype are dropped into the prep/2 function, the function uses the current time to create a new random seed. Then the cortex is extracted from the genotype and the Sensor, Actuator, and Neural Ids are extracted from it. The sensors and actuators are dropped into the spawn_Scapes/4, which extracts the scapes that need to be spawned, and then spawns them. Afterwards, the sensor, actuator, neuron, and the cortex elements are spawned. Then the exoself process sends these spawned elements the ProcessIDs of the elements they are connected to, thus linking all the elements together into a proper interconnected structure. The cortex element is the last one to be linked, because once it receives the message from the exoself with all the data, it immediately starts synchronizing the NN by prompting the sensors to action. Afterwards, prep/2 drops into the exoself’s main process loop.


loop(FileName,Genotype,NeuronPIDs,CortexProcess,SensorProcess,NeuronProcess,ActuatorProcess,ScapeProcessIDs,HighestFitness,EvalAcc,CycleAcc,TimeAcc,Attempt)->
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
			%io:format("HighestFitness:~p U_Attempt:~p~n",[HighestFitness,U_Attempt]),
			case U_Attempt >= ?MAX_ATTEMPTS of
				true ->	%End training
					U_CycleAcc = CycleAcc+Cycles,
					U_TimeAcc = TimeAcc+Time,
					backup_genotype(FileName,NeuronPIDs,Genotype,NeuronProcess),
					terminate_phenotype(CortexProcess,SensorProcess,NeuronProcess,ActuatorProcess,ScapeProcessIDs),
					io:format("Cortex:~p finished training. Genotype has been backed up.~n Fitness:~p~n TotalEvaluations:~p~n TotalCycles:~p~n TimeAcc:~p~n",
						[CortexProcess,U_HighestFitness,EvalAcc,U_CycleAcc,U_TimeAcc]),
					
					case whereis(trainer) of
						undefined ->
							ok;
						ProcessID -> 
							ProcessID ! {self(),U_HighestFitness,EvalAcc,U_CycleAcc,U_TimeAcc}
					end;
				false -> %Continue training
					Total_Neurons = length(NeuronProcess),
					MP = 1/math:sqrt(Total_Neurons),
					Perturb_NeuronProcess=[NeuronProcess || NeuronProcess <- NeuronProcess,rand:uniform()<MP],
					%io:format("Perturb_NeuronProcess:~p~n",[Perturb_NeuronProcess]),
					put(perturbed,Perturb_NeuronProcess),
					[NeuronProcess ! {self(),weight_perturb} || NeuronProcess <- Perturb_NeuronProcess],
					CortexProcess ! {self(),reactivate},
					loop(FileName,Genotype,NeuronPIDs,CortexProcess,SensorProcess,NeuronProcess,ActuatorProcess,ScapeProcessIDs,U_HighestFitness,EvalAcc+1,CycleAcc+Cycles,TimeAcc+Time,U_Attempt)
			end
	end.
%The map/1 function maps the tuple encoded genotype into a process based phenotype. The map function expects for the Cortex record to be the leading tuple in the tuple list it reads from the FileName. We create an ets table to map Ids to ProcessIDs and back again. Since the Cortex element contains all the Sensor, Actuator, and Neuron Ids, we are able to spawn each neuron using its own gen function, and in the process construct a map from Ids to ProcessIDs. We then use linkCerebralUnits to link all non Cortex elements to each other by sending each spawned process the information contained in its record, but with Ids converted to Pids where appropriate. Finally, we provide the Cortex process with all the ProcessIDs in the NN system by executing the linkCortex/2 function. Once the NN is up and running, exoself starts its wait until the NN has finished its job and is ready to backup. When the cortex initiates the backup process it sends exoself the updated WeightedInputProcess from its neurons. Exoself uses the updateGenotype/3 function to update the old genotype with new weights, and then stores the updated version back to its file.

	spawnCerebralUnits(NeuronPIDs,CerebralUnitType,[Id|Ids])-> 
		ProcessID = CerebralUnitType:gen(self(),node()),
		ets:insert(NeuronPIDs,{Id,ProcessID}), 
		ets:insert(NeuronPIDs,{ProcessID,Id}), 
		spawnCerebralUnits(NeuronPIDs,CerebralUnitType,Ids); 
	spawnCerebralUnits(NeuronPIDs,_CerebralUnitType,[])-> 
		true.
%We spawn the process for each element based on its type: CerebralUnitType, and the gen function that belongs to the CerebralUnitType module. We then enter the {Id,ProcessID} tuple into our ETS table for later use.
%-record(sensor,{id,name,cortex_id,format,scape,vector_length,fanout_ids,parameters,objects=[],vis=[]}).
%-record(actuator,{id,name,cortex_id,format,scape,vector_length,fanin_ids,parameters,objects=[],vis=[]}).

	spawn_Scapes(NeuronPIDs,Genotype,SensorIDs,ActuatorIDs)-> 
		Sensor_Scapes = [(genotype:read(Genotype,Id))#sensor.scape || Id<-SensorIDs], 
		Actuator_Scapes = [(genotype:read(Genotype,Id))#actuator.scape || Id<-ActuatorIDs], 
		Unique_Scapes = Sensor_Scapes++(Actuator_Scapes--Sensor_Scapes), 
		SN_Tuples=[{scape:gen(self(),node()),ScapeName} || {private,ScapeName}<-Unique_Scapes], 
		[ets:insert(NeuronPIDs,{ScapeName,ProcessID}) || {ProcessID,ScapeName} <- SN_Tuples], 
		[ets:insert(NeuronPIDs,{ProcessID,ScapeName}) || {ProcessID,ScapeName} <-SN_Tuples], 
		[ProcessID ! {self(),ScapeName} || {ProcessID,ScapeName} <- SN_Tuples],
		[ProcessID || {ProcessID,_ScapeName} <-SN_Tuples].

	link_Sensors(Genotype,[SensorID|SensorIDs],NeuronPIDs) ->
		R=genotype:read(Genotype,SensorID),
		SensorProcess = ets:lookup_element(NeuronPIDs,SensorID,2),
		CortexProcess = ets:lookup_element(NeuronPIDs,R#sensor.cortex_id,2),
		SensorName = R#sensor.name,
		FanoutIDs = R#sensor.fanout_ids,
		FanoutProcess = [ets:lookup_element(NeuronPIDs,Id,2) || Id <- FanoutIDs],
		Scape=case R#sensor.scape of
			{private,ScapeName}->
				ets:lookup_element(NeuronPIDs,ScapeName,2)
		end,
		SensorProcess ! {self(),{SensorID,CortexProcess,Scape,SensorName,R#sensor.vector_length,FanoutProcess}},
		link_Sensors(Genotype,SensorIDs,NeuronPIDs);
	link_Sensors(_Genotype,[],NeuronPIDs)->
		ok.
	link_Actuators(Genotype,[ActuatorID|ActuatorIDs],NeuronPIDs) ->
		R=genotype:read(Genotype,ActuatorID),
		ActuatorProcess = ets:lookup_element(NeuronPIDs,ActuatorID,2),
		CortexProcess = ets:lookup_element(NeuronPIDs,R#actuator.cortex_id,2),
		ActuatorName = R#actuator.name,
		FaninIDs = R#actuator.fanin_ids,
		FaninProcess = [ets:lookup_element(NeuronPIDs,Id,2) || Id <- FaninIDs],
		Scape=case R#actuator.scape of
			{private,ScapeName}->
				ets:lookup_element(NeuronPIDs,ScapeName,2)
		end,
		ActuatorProcess ! {self(),{ActuatorID,CortexProcess,Scape,ActuatorName,FaninProcess}},
		link_Actuators(Genotype,ActuatorIDs,NeuronPIDs);
	link_Actuators(_Genotype,[],NeuronPIDs)->
		ok.
	link_Neurons(Genotype,[NeuronID|NeuronIDs],NeuronPIDs) ->
		R=genotype:read(Genotype,NeuronID),
		NeuronProcess = ets:lookup_element(NeuronPIDs,NeuronID,2),
		CortexProcess = ets:lookup_element(NeuronPIDs,R#neuron.cortex_id,2),
		ActivationFunctionName = R#neuron.af,
		WeightedInputs = R#neuron.weighted_inputs,
		OutputIDs = R#neuron.output_ids,
		WeightedInputProcess = convertWeightedInputsToWeightedInputProcessIDs(NeuronPIDs,WeightedInputs,[]),
		OutputProcess = [ets:lookup_element(NeuronPIDs,Id,2) || Id <- OutputIDs],
		NeuronProcess ! {self(),{NeuronID,CortexProcess,ActivationFunctionName,WeightedInputProcess,OutputProcess}},
		link_Neurons(Genotype,NeuronIDs,NeuronPIDs);
	link_Neurons(_Genotype,[],NeuronPIDs)->
		ok.

		convertWeightedInputsToWeightedInputProcessIDs(NeuronPIDs,[{bias,Bias}],Acc)->
			lists:reverse([Bias|Acc]);
		convertWeightedInputsToWeightedInputProcessIDs(NeuronPIDs,[{Id,Weights}|FaninWeightedInputs],Acc)->
			convertWeightedInputsToWeightedInputProcessIDs(NeuronPIDs,FaninWeightedInputs,[{ets:lookup_element(NeuronPIDs,Id,2),Weights}|Acc]).
%The linkCerebralUnits/2 converts the Ids to ProcessIDs using the created IdsNPids ETS table. At this point all the elements are spawned, and the processes are waiting for their initial states. 'convertWeightedInputsToWeightedInputProcessIDs' converts the IdPs tuples into tuples that use ProcessIDs instead of Ids, such that the Neuron will know which weights are to be associated with which incoming vector signals. The last element is the bias, which is added to the list in a non tuple form. Afterwards, the list is reversed to take its proper order.

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
%The cortex is initialized to its proper state just as other elements. Because we have not yet implemented a learning algorithm for our NN system, we need to specify when the NN should shutdown. We do this by specifying the total number of cycles the NN should execute before terminating, which is 1000 in this case.

backup_genotype(FileName,NeuronPIDs,Genotype,NeuronProcess)->
	NeuronIDsNWeights = get_backup(NeuronProcess,[]),
	updateGenotype(NeuronPIDs,Genotype,NeuronIDsNWeights),
	genotype:save_to_file(Genotype,FileName),
	io:format("Finished updating genotype to file:~p~n",[FileName]).

	get_backup([NeuronProcess|NeuronProcess],Acc)->
		NeuronProcess ! {self(),get_backup},
		receive
			{NeuronProcess,NeuronID,WeightTuples}->
				get_backup(NeuronProcess,[{NeuronID,WeightTuples}|Acc])
		end;
	get_backup([],Acc)->
		Acc.
%The backup_genotype/4 uses get_backup/2 to contact all the neurons in its NN and request for the neuron's Ids and their WeightedInputs. Once the updated WeightedInputs from all the neurons have been accumulated, they are passed through the updateGenotype/3 function to produce the genotype with updated weights, which is then saved to file.

	updateGenotype(NeuronPIDs,Genotype,[{NeuronID,WeightedInputProcessIDs}|WeightPs])->
		Neuron = genotype:read(Genotype,NeuronID),
		UpdatedWeightedInput = convertWeightedInputProcessesToWeightedInputs(NeuronPIDs,WeightedInputProcessIDs,[]),
		UpdatedNeuron = Neuron#neuron{weighted_inputs = UpdatedWeightedInput},
		genotype:write(Genotype,UpdatedNeuron),
		updateGenotype(NeuronPIDs,Genotype,WeightPs);
	updateGenotype(NeuronPIDs,_Genotype,[])->
		ok.
	
		convertWeightedInputProcessesToWeightedInputs(NeuronPIDs,[{ProcessID,Weights}|WeightedInputProcess],Acc)->
			convertWeightedInputProcessesToWeightedInputs(NeuronPIDs,WeightedInputProcess,[{ets:lookup_element(NeuronPIDs,ProcessID,2),Weights}|Acc]);
		convertWeightedInputProcessesToWeightedInputs(NeuronPIDs,[Bias],Acc)->
			lists:reverse([{bias,Bias}|Acc]).
%For every {NeuronID,WeightedInputProcessIDs} tuple the updateGenotype/3 function extracts the neuron with the id: NeuronID, and updates its weights. The convertWeightedInputProcessesToWeightedInputs/3 performs the conversion from ProcessIDs to Ids of every {ProcessID,Weights} tuple in the WeightedInputProcess list. The updated Genotype is then returned back to the caller.
	
terminate_phenotype(CortexProcess,SensorProcess,NeuronProcess,ActuatorProcess,ScapeProcessIDs)->
	[ProcessID ! {self(),terminate} || ProcessID <- SensorProcess],
	[ProcessID ! {self(),terminate} || ProcessID <- ActuatorProcess],
	[ProcessID ! {self(),termiante} || ProcessID <- NeuronProcess],
	[ProcessID ! {self(),terminate} || ProcessID <- ScapeProcessIDs],
	CortexProcess ! {self(),terminate}.
