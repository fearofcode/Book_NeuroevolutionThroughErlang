%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(constructor).
-compile(export_all).
-include("records.hrl").

constructGenotype(SensorName,ActuatorName,HiddenLayerDensities)->
	constructGenotype(ffnn,SensorName,ActuatorName,HiddenLayerDensities).

constructGenotype(FileName,SensorName,ActuatorName,HiddenLayerDensities)->
	S = createSensor(SensorName),
	A = createActuator(ActuatorName),
	OutputVL = A#actuator.vector_length,
	LayerDensities = lists:append(HiddenLayerDensities,[OutputVL]),
	CortexID = {cortex,generate_id()},
	
	Neurons = createNeuroLayers(CortexID,S,A,LayerDensities), 
	[InputLayer|_] = Neurons, 
	[OutputLayer|_] = lists:reverse(Neurons), 
	InputNeuronIDs = [N#neuron.id || N <- InputLayer], 
	OutputNeuronIDs = [N#neuron.id || N <-  OutputLayer], 
	NeuronIDs = [N#neuron.id || N <- lists:flatten(Neurons)],
	Sensor = S#sensor{cortex_id = CortexID, fanout_ids = InputNeuronIDs},
	Actuator = A#actuator{cortex_id=CortexID,fanin_ids = OutputNeuronIDs},
	Cortex = createCortex(CortexID,[S#sensor.id],[A#actuator.id],NeuronIDs), 
	Genotype = lists:flatten([Cortex,Sensor,Actuator|Neurons]),
	{ok, File} = file:open(FileName, write),
	lists:foreach(fun(X) -> io:format(File, "~p.~n",[X]) end, Genotype),
	file:close(File),
	Genotype.
%The constructGenotype function accepts the name of the file to which we'll save the genotype, sensor name, actuator name, and the hidden layer density parameters. We have to generate unique Ids for every sensor and actuator. The sensor and actuator names are used as input to the createSensor and createActuator functions, which in turn generate the actual Sensor and Actuator representing tuples. We create unique Ids for sensors and actuators so that when in the future a NN uses 2 or more sensors or actuators of the same type, we will be able to differentiate between them using their Ids. After the Sensor and Actuator tuples are generated, we extract the NN's input and output vector lengths from the sensor and actuator used by the system. The InputVL is then used to specify how many weights the neurons in the input layer will need, and the OutputVL specifies how many neurons are in the output layer of the NN. After appending the HiddenLayerDensites to the now known number of neurons in the last layer to generate the full LayerDensities list, we use the createNeuroLayers function to generate the Neuron representing tuples. We then update the Sensor and Actuator records with proper fanin and fanout ids from the freshly created Neuron tuples, composes the Cortex, and write the genotype to file.

createSensor(SensorName) -> 
	case SensorName of
		rng ->
			#sensor{id={sensor,generate_id()},name=rng,vector_length=2};
		_ ->
			exit("System does not yet support a sensor by the name:~p.",[SensorName])
	end.

createActuator(ActuatorName) ->
	case ActuatorName of
		pts ->
			#actuator{id={actuator,generate_id()},name=pts,vector_length=1};
		_ ->
			exit("System does not yet support an actuator by the name:~p.",[ActuatorName])
	end.
%Every sensor and actuator uses some kind of function associated with it. A function that either polls the environment for sensory signals (in the case of a sensor) or acts upon the environment (in the case of an actuator). It is a function that we need to define and program before it is used, and the name of the function is the same as the name of the sensor or actuator it self. For example, the createSensor/1 has specified only the rng sensor, because that is the only sensor function we've finished developing. The rng function has its own vector_length specification, which will determine the number of weights that a neuron will need to allocate if it is to accept this sensor's output vector. The same principles apply to the createActuator function. Both, createSensor and createActuator function, given the name of the sensor or actuator, will return a record with all the specifications of that element, each with its own unique Id.

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
