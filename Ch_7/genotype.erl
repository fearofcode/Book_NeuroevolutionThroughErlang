%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(genotype).
-compile(export_all).
-include("records.hrl").

construct(Morphology,HiddenLayerDensities)->
	construct(ffnn,Morphology,HiddenLayerDensities).
construct(FileName,Morphology,HiddenLayerDensities)->
	{V1,V2,V3} = now(),
	random:seed(V1,V2,V3),
	S = morphology:get_InitSensor(Morphology),
	A = morphology:get_InitActuator(Morphology),
	OutputVL = A#actuator.vector_length,
	LayerDensities = lists:append(HiddenLayerDensities,[OutputVL]),
	CortexID = cortex,
	
	Neurons = createNeuroLayers(CortexID,S,A,LayerDensities), 
	[InputLayer|_] = Neurons, 
	[OutputLayer|_] = lists:reverse(Neurons), 
	InputNeuronIDs = [N#neuron.id || N <- InputLayer], 
	OutputNeuronIDs = [N#neuron.id || N <-  OutputLayer], 
	NeuronIDs = [N#neuron.id || N <- lists:flatten(Neurons)],
	Sensor = S#sensor{cortex_id = CortexID,fanout_ids = InputNeuronIDs},
	Actuator = A#actuator{cortex_id=CortexID,fanin_ids = OutputNeuronIDs},
	Cortex = createCortex(CortexID,[S#sensor.id],[A#actuator.id],NeuronIDs), 
	Genotype = lists:flatten([Cortex,Sensor,Actuator,Neurons]),
	save_genotype(FileName,Genotype),
	Genotype.
%The constructGenotype function accepts the name of the file to which we'll save the genotype, sensor name, actuator name, and the hidden layer density parameters. We have to generate unique Ids for every sensor and actuator. The sensor and actuator names are used as input to the createSensor and createActuator functions, which in turn generate the actual Sensor and Actuator representing tuples. We create unique Ids for sensors and actuators so that when in the future a NN uses 2 or more sensors or actuators of the same type, we will be able to differentiate between them using their Ids. After the Sensor and Actuator tuples are generated, we extract the NN's input and output vector lengths from the sensor and actuator used by the system. The InputVL is then used to specify how many weights the neurons in the input layer will need, and the OutputVL specifies how many neurons are in the output layer of the NN. After appending the HiddenLayerDensites to the now known number of neurons in the last layer to generate the full LayerDensities list, we use the createNeuroLayers function to generate the Neuron representing tuples. We then update the Sensor and Actuator records with proper fanin and fanout ids from the freshly created Neuron tuples, composes the Cortex, and write the genotype to file.

	createNeuroLayers(CortexID,Sensor,Actuator,LayerDensities) ->
		WeightedInputs = [{Sensor#sensor.id,Sensor#sensor.vector_length}],
		TotalLayers = length(LayerDensities),
		[InputNeurons|NextLayerDensities] = LayerDensities, 
		NeuronIDs = [{neuron,{1,Id}}|| Id <- generate_ids(InputNeurons,[])],
		createNeuroLayers(CortexID,Actuator#actuator.id,1,TotalLayers,WeightedInputs,NeuronIDs,NextLayerDensities,[]). 
%The function createNeuroLayers/3 prepares the initial step before starting the recursive createNeuroLayers/7 function which will create all the Neuron records. We first generate the place holder Input Ids “plus”(WeightedInputs), which are tuples composed of Ids and the vector lengths of the incoming signals associated with them. The proper weighted_inputs will have a weight list in the tuple instead of the vector length. Because we are only building NNs each with only a single Sensor and Actuator, the IdP to the first layer is composed of the single Sensor Id with the vector length of its sensory signal, likewise in the case of the Actuator. We then generate unique ids for the neurons in the first layer, and drop into the recursive createNeuroLayers/7 function.

	createNeuroLayers(CortexID,ActuatorID,LayerIndex,TotalLayers,WeightedInputs,NeuronIDs,[NextLayerDensity|LayerDensities],Acc) ->
		OutputNeuronIDs = [{neuron,{LayerIndex+1,Id}} || Id <- generate_ids(NextLayerDensity,[])], 
		LayerNeurons = createNeuroLayer(CortexID,WeightedInputs,NeuronIDs,OutputNeuronIDs,[]), 
		NextWeightedInput = [{NeuronID,1}|| NeuronID <- NeuronIDs],
		createNeuroLayers(CortexID,ActuatorID,LayerIndex+1,TotalLayers,NextWeightedInput,OutputNeuronIDs,LayerDensities,[LayerNeurons|Acc]);
	createNeuroLayers(CortexID,ActuatorID,TotalLayers,TotalLayers,WeightedInputs,NeuronIDs,[],Acc) -> 
		OutputIDs = [ActuatorID], 
		LayerNeurons = createNeuroLayer(CortexID,WeightedInputs,NeuronIDs,OutputIDs,[]), 
		lists:reverse([LayerNeurons|Acc]).
%During the first iteration, the first layer neuron ids constructed in createNeuroLayers/3 are held in the NeuronIDs variable. In createNeuroLayers/7, with every iteration we generate the OutputNeuronIDs, which are the Ids of the neurons in the next layer. The last layer is a special case which occurs when LayerIndex == TotalLayers. Having the WeightedInputs, and the OutputNeuronIDs, we are able to construct a neuron record for every Id in NeuronIDs using the function create_layer/4. The Ids of the constructed OutputNeuronIDs will become the NeuronIDs variable of the next iteration, and the Ids of the neurons in the current layer will be extended and become NextWeightedInput. We then drop into the next iteration with the newly prepared NextWeightedInput and OutputNeuronIDs. Finally, when we reach the last layer, the OutputIDs is the list containing a single Id of the Actuator element. We use the same function, createNeuroLayer/4, to construct the last layer and return the result.

		createNeuroLayer(CortexID,WeightedInputs,[Id|NeuronIDs],OutputIDs,Acc) ->
			Neuron = create_Neuron(WeightedInputs,Id,CortexID,OutputIDs), 
			createNeuroLayer(CortexID,WeightedInputs,NeuronIDs,OutputIDs,[Neuron|Acc]); 
		createNeuroLayer(_CortexID,_WeightedInputs,[],_OutputIDs,Acc) ->
			Acc.
%To create neurons from the same layer, all that is needed are the Ids for those neurons, a list of WeightedInputs for every neuron so that we can create the proper number of weights, and a list of OutputIDs. Since in our simple feed forward neural network all neurons are fully connected to the neurons in the next layer, the WeightedInputs and OutputIDs are the same for every neuron belonging to the same layer.
		
		create_Neuron(WeightedInputs,Id,CortexID,OutputIDs)-> 
			ProperWeightedInput = createNeuralInput(WeightedInputs,[]), 
			#neuron{id=Id,cortex_id = CortexID,af=tanh,weighted_inputs=ProperWeightedInput,output_ids=OutputIDs}. 

			createNeuralInput([{InputID,InputVL}|WeightedInputs],Acc) ->
				Weights = createNeuralWeights(InputVL,[]),
				createNeuralInput(WeightedInputs,[{InputID,Weights}|Acc]); 
			createNeuralInput([],Acc)-> 
				lists:reverse([{bias,rand:uniform()-0.5}|Acc]).
			 
				createNeuralWeights(0,Acc) ->
					Acc; 
				createNeuralWeights(Index,Acc) ->
					W = rand:uniform()-0.5, 
					createNeuralWeights(Index-1,[W|Acc]). 
%Each neuron record is composed by the create_Neuron/3 function. The create_Neuron/3 function creates the Input list from the tuples [{Id,Weights}...] using the vector lengths specified in the place holder WeightedInputs. The createNeuralInput/2 function uses createNeuralWeights/2 to generate the random weights in the range of -0.5 to 0.5, adding the bias to the end of the list.

			generate_ids(0,Acc) ->
				Acc; 
			generate_ids(Index,Acc)-> 
				Id = generate_id(), 
				generate_ids(Index-1,[Id|Acc]). 
	 
			generate_id() ->
				{MegaSeconds,Seconds,MicroSeconds} = now(), 
				1/(MegaSeconds*1000000 + Seconds + MicroSeconds/1000000). 
%The generate_id/0 creates a unique Id using current time, the Id is a floating point value. The generate_ids/2 function creates a list of unique Ids.

	createCortex(CortexID,SensorIDs,ActuatorIDs,NeuronIDs) ->
		#cortex{id = CortexID, sensor_ids=SensorIDs, actuator_ids=ActuatorIDs, neuron_ids = NeuronIDs}.
%The createCortex/4 function generates the record encoded genotypical representation of the cortex element. The Cortex element needs to know the Id of every Neuron, Sensors, and Actuator in the NN. 

save_genotype(FileName,Genotype)->
	TId = ets:new(FileName, [public,set,{keypos,2}]),
	[ets:insert(TId,Element) || Element <- Genotype],
	ets:tab2file(TId,FileName).
%The save_genotype/2 function expects that the Genotype is a list composed of the neuron, sensor, actuator, cortex, and exoself elements. The function creates a new ets table, writes all the element representing tuples from the Genotype list to the ets table, and then writes the ets table to file.
		
save_to_file(Genotype,FileName)->
	ets:tab2file(Genotype,FileName).
%The save_to_file/2 function saves the ets table by the name Genotype to the file by the name FileName.
	
load_from_file(FileName)->
	{ok,TId} = ets:file2tab(FileName),
	TId.
%The load_from_file/1 loads an ets representing file by the name FileName, returning the ets table id to the caller.

read(TId,Key)->
	[R] = ets:lookup(TId,Key),
	R.
%The read/2 function reads a record associated with Key from the ets table with the id TId, returning the record R to the caller. It expects that only a single record exists with the specified Key.

write(TId,R)->
	ets:insert(TId,R).
%The function write/2 writes the record R to the ets table with the id TId.
	
print(FileName)->
	Genotype = load_from_file(FileName),
	Cortex = read(Genotype,cortex),
	SensorIDs = Cortex#cortex.sensor_ids,
	NeuronIDs = Cortex#cortex.neuron_ids,
	ActuatorIDs = Cortex#cortex.actuator_ids,
	io:format("~p~n",[Cortex]),
	[io:format("~p~n",[read(Genotype,Id)]) || Id <- SensorIDs],
	[io:format("~p~n",[read(Genotype,Id)]) || Id <- NeuronIDs],
	[io:format("~p~n",[read(Genotype,Id)]) || Id <- ActuatorIDs].
%The function print/1 reads a stored Genotype from the file FileName, and then prints to console all the elements making up the NN’s genotype.

