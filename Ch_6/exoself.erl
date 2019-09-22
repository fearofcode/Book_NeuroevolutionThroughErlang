%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(exoself).
-compile(export_all).
-include("records.hrl").

map()-> map(ffnn).
map(FileName)->
	{ok,Genotype} = file:consult(FileName),
	spawn(exoself,map,[FileName,Genotype]).
map(FileName,Genotype)->
	NeuronPIDs = ets:new(neuron_process_ids,[set,private]), 
	[Cortex|CerebralUnits] = Genotype,
	SensorIDs = Cortex#cortex.sensor_ids,
	ActuatorIDs = Cortex#cortex.actuator_ids,
	NeuronIDs = Cortex#cortex.neuron_ids,
	spawnCerebralUnits(NeuronPIDs,cortex,[Cortex#cortex.id]),
	spawnCerebralUnits(NeuronPIDs,sensor,SensorIDs),
	spawnCerebralUnits(NeuronPIDs,actuator,ActuatorIDs),
	spawnCerebralUnits(NeuronPIDs,neuron,NeuronIDs),
	linkCerebralUnits(CerebralUnits,NeuronPIDs),
	linkCortex(Cortex,NeuronPIDs),
	CortexProcess = ets:lookup_element(NeuronPIDs,Cortex#cortex.id,2),
	receive
		{CortexProcess,backup,NeuronIDsNWeights}->
			UpdatedGenotype = updateGenotype(NeuronPIDs,Genotype,NeuronIDsNWeights),
			{ok, File} = file:open(FileName, write),
			lists:foreach(fun(X) -> io:format(File, "~p.~n",[X]) end, UpdatedGenotype),
			file:close(File),
			io:format("Finished updating to file:~p~n",[FileName])
	end.
%The map/1 function maps the tuple encoded genotype into a process based phenotype. The map function expects for the Cortex record to be the leading tuple in the tuple list it reads from the FileName. We create an ets table to map Ids to ProcessIDs and back again. Since the Cortex element contains all the Sensor, Actuator, and Neuron Ids, we are able to spawn each neuron using its own gen function, and in the process construct a map from Ids to ProcessIDs. We then use linkCerebralUnits to link all non Cortex elements to each other by sending each spawned process the information contained in its record, but with Ids converted to Pids where appropriate. Finally, we provide the Cortex process with all the ProcessIDs in the NN system by executing the linkCortex/2 function. Once the NN is up and running, exoself starts its wait until the NN has finished its job and is ready to backup. When the cortex initiates the backup process it sends exoself the updated WeightedInputProcess from its neurons. Exoself uses the updateGenotype/3 function to update the old genotype with new weights, and then stores the updated version back to its file.

	spawnCerebralUnits(NeuronPIDs,CerebralUnitType,[Id|Ids])-> 
		ProcessID = CerebralUnitType:gen(self(),node()),
		ets:insert(NeuronPIDs,{Id,ProcessID}), 
		ets:insert(NeuronPIDs,{ProcessID,Id}), 
		spawnCerebralUnits(NeuronPIDs,CerebralUnitType,Ids); 
	spawnCerebralUnits(_NeuronPIDs,_CerebralUnitType,[])-> 
		true.
%We spawn the process for each element based on its type: CerebralUnitType, and the gen function that belongs to the CerebralUnitType module. We then enter the {Id,ProcessID} tuple into our ETS table for later use.

linkCerebralUnits([R|Records],NeuronPIDs) when is_record(R,sensor) ->
	SensorID = R#sensor.id,
	SensorProcess = ets:lookup_element(NeuronPIDs,SensorID,2),
	CortexProcess = ets:lookup_element(NeuronPIDs,R#sensor.cortex_id,2),
	SensorName = R#sensor.name,
	FanoutIDs = R#sensor.fanout_ids,
	FanoutProcess = [ets:lookup_element(NeuronPIDs,Id,2) || Id <- FanoutIDs],
	SensorProcess ! {self(),{SensorID,CortexProcess,SensorName,R#sensor.vector_length,FanoutProcess}},
	linkCerebralUnits(Records,NeuronPIDs);

linkCerebralUnits([R|Records],NeuronPIDs) when is_record(R,actuator) ->
	ActuatorID = R#actuator.id,
	ActuatorProcess = ets:lookup_element(NeuronPIDs,ActuatorID,2),
	CortexProcess = ets:lookup_element(NeuronPIDs,R#actuator.cortex_id,2),
	ActuatorName = R#actuator.name,
	FaninIDs = R#actuator.fanin_ids,
	FaninProcess = [ets:lookup_element(NeuronPIDs,Id,2) || Id <- FaninIDs],
	ActuatorProcess ! {self(),{ActuatorID,CortexProcess,ActuatorName,FaninProcess}},
	linkCerebralUnits(Records,NeuronPIDs);

linkCerebralUnits([R|Records],NeuronPIDs) when is_record(R,neuron) ->
	NeuronID = R#neuron.id,
	NeuronProcess = ets:lookup_element(NeuronPIDs,NeuronID,2),
	CortexProcess = ets:lookup_element(NeuronPIDs,R#neuron.cortex_id,2),
	ActivationFunctionName = R#neuron.af,
	WeightedInputs = R#neuron.weighted_inputs,
	OutputIDs = R#neuron.output_ids,
	WeightedInputProcess = convertWeightedInputsToWeightedInputProcessIDs(NeuronPIDs,WeightedInputs,[]),
	OutputProcess = [ets:lookup_element(NeuronPIDs,Id,2) || Id <- OutputIDs],
	NeuronProcess ! {self(),{NeuronID,CortexProcess,ActivationFunctionName,WeightedInputProcess,OutputProcess}},
	linkCerebralUnits(Records,NeuronPIDs);
linkCerebralUnits([],_NeuronPIDs)->
	ok.

convertWeightedInputsToWeightedInputProcessIDs(_NeuronPIDs,[{bias,Bias}],Acc)->
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
	ActuatorProcess = [ets:lookup_element(NeuronPIDs,ActuatorID,2) || ActuatorID <- ActuatorIDs],
	NeuronProcess = [ets:lookup_element(NeuronPIDs,NeuronID,2) || NeuronID <- NeuronIDs],
	CortexProcess ! {self(),{CortexID,SensorProcess,ActuatorProcess,NeuronProcess},1000}.
%The cortex is initialized to its proper state just as other elements. Because we have not yet implemented a learning algorithm for our NN system, we need to specify when the NN should shutdown. We do this by specifying the total number of cycles the NN should execute before terminating, which is 1000 in this case.

updateGenotype(NeuronPIDs,Genotype,[{NeuronID,WeightedInputProcessIDs}|WeightPs])->
	Neuron = lists:keyfind(NeuronID, 2, Genotype),
	%io:format("WeightedInputProcessIDs:~p~n",[WeightedInputProcessIDs]),
	UpdatedWeightedInput = convertWeightedInputProcessesToWeightedInputs(NeuronPIDs,WeightedInputProcessIDs,[]),
	UpdatedNeuron = Neuron#neuron{weighted_inputs = UpdatedWeightedInput},
	UpdatedGenotype = lists:keyreplace(NeuronID, 2, Genotype, UpdatedNeuron),
	%io:format("N:~p~n UpdatedNeuron:~p~n Genotype:~p~n UpdatedGenotype:~p~n",[N,UpdatedNeuron,Genotype,UpdatedGenotype]),
	updateGenotype(NeuronPIDs,UpdatedGenotype,WeightPs);

updateGenotype(_NeuronPIDs,Genotype,[])->
	Genotype.
	
convertWeightedInputProcessesToWeightedInputs(NeuronPIDs,[{ProcessID,Weights}|WeightedInputProcess],Acc)->
	convertWeightedInputProcessesToWeightedInputs(NeuronPIDs,WeightedInputProcess,[{ets:lookup_element(NeuronPIDs,ProcessID,2),Weights}|Acc]);

convertWeightedInputProcessesToWeightedInputs(_NeuronPIDs,[Bias],Acc)->
	lists:reverse([{bias,Bias}|Acc]).
%For every {NeuronID,WeightedInputProcessIDs} tuple the updateGenotype/3 function extracts the neuron with the id: NeuronID, and updates its weights. The convertWeightedInputProcessesToWeightedInputs/3 performs the conversion from ProcessIDs to Ids of every {ProcessID,Weights} tuple in the WeightedInputProcess list. The updated Genotype is then returned back to the caller.
