%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(genotype).
-compile(export_all).
-include("records.hrl").

sync()->make:all([load]).

constructAgent(Specie_Id,Agent_Id,SpecCon)->
	random:seed(now()),
	Generation = 0,
	Encoding_Type = random_element(SpecCon#constraint.agent_encoding_types),
	SPlasticity=random_element(SpecCon#constraint.substrate_plasticities),
	SLinkform =random_element(SpecCon#constraint.substrate_linkforms),
	{CortexID,Pattern,Substrate_Id} = constructCortex(Agent_Id,Generation,SpecCon,Encoding_Type,SPlasticity,SLinkform),
	Agent = #agent{
		id = Agent_Id,
		encoding_type = Encoding_Type,
		cortex_id = CortexID,
		specie_id = Specie_Id,
		constraint = SpecCon,
		generation = Generation,
		pattern = Pattern,
		tuning_selection_f = random_element(SpecCon#constraint.tuning_selection_fs),
		annealing_parameter = random_element(SpecCon#constraint.annealing_parameters),
		tuning_duration_f = SpecCon#constraint.tuning_duration_f,
		perturbation_range = random_element(SpecCon#constraint.perturbation_ranges),
		mutation_operators = SpecCon#constraint.mutation_operators,
		tot_topological_mutations_f = random_element(SpecCon#constraint.tot_topological_mutations_fs),
		heredity_type = random_element(SpecCon#constraint.heredity_types),
		evo_hist = [],
		substrate_id = Substrate_Id
	},
	write(Agent),
	update_fingerprint(Agent_Id).
%The population monitor should have all the information with regards to the morphologies and specie constraint under which the agent's genotype should be created. Thus the constructAgent/3 is run with the Specie_Id to which this NN based system will belong, the Agent_Id that this NN based intelligent agent will have, and the SpecCon (specie constraint) that will define the list of activation functions and other parameters from which the seed agent can choose its parameters. First the generation is set to 0, since the agent is just created, then the constructCortex/3 is ran, which creates the NN and returns its CortexID. Once the NN is created and the the cortex's id is returned, we can fill out the information needed by the agent record, and write it to the mnesia database

constructCortex(Agent_Id,Generation,SpecCon,Encoding_Type,SPlasticity,SLinkform)->
	CortexID = {{origin,generate_UniqueId()},cortex},
	Morphology = SpecCon#constraint.morphology,
	case Encoding_Type of
		neural ->
			Sensors = [S#sensor{id={{-1,generate_UniqueId()},sensor},cortex_id=CortexID,generation=Generation}|| S<- morphology:get_InitSensors(Morphology)],
			Actuators = [A#actuator{id={{1,generate_UniqueId()},actuator},cortex_id=CortexID,generation=Generation}||A<-morphology:get_InitActuators(Morphology)],
			N_Ids=constructInitialNeuroLayer(CortexID,Generation,SpecCon,Sensors,Actuators,[],[]),
			S_Ids = [S#sensor.id || S<-Sensors],
			A_Ids = [A#actuator.id || A<-Actuators],
			Cortex = #cortex{
				id = CortexID,
				agent_id = Agent_Id,
				neuron_ids = N_Ids,
				sensor_ids = S_Ids,
				actuator_ids = A_Ids
			},
			Substrate_Id = undefined;
		substrate ->
			Substrate_Id={{void,generate_UniqueId()},substrate},
			Sensors = [S#sensor{id={{-1,generate_UniqueId()},sensor},cortex_id=CortexID,generation=Generation,fanout_ids=[Substrate_Id]}|| S<- morphology:get_InitSensors(Morphology)],
			Actuators = [A#actuator{id={{1,generate_UniqueId()},actuator},cortex_id=CortexID,generation=Generation,fanin_ids=[Substrate_Id]}||A<-morphology:get_InitActuators(Morphology)],
			[write(S) || S <- Sensors],
			[write(A) || A <- Actuators],
			Dimensions=calculate_OptimalSubstrateDimension(Sensors,Actuators),
			Density = 5,
			Depth = 1,
			Densities = [Depth,1|lists:duplicate(Dimensions-2,Density)], %[X,Y,Z,T...]
			Substrate_CPPs = [CPP#sensor{id={{-1,generate_UniqueId()},sensor},cortex_id=CortexID,generation=Generation}|| CPP<- morphology:get_InitSubstrateCPPs(Dimensions,SPlasticity)],
			Substrate_CEPs = [CEP#actuator{id={{1,generate_UniqueId()},actuator},cortex_id=CortexID,generation=Generation}||CEP<-morphology:get_InitSubstrateCEPs(Dimensions,SPlasticity)],
			N_Ids=constructInitialNeuroLayer(CortexID,Generation,SpecCon,Substrate_CPPs,Substrate_CEPs,[],[]),
			%io:format("Sensors:~p~n Actuators:~p~n Substate_CPPs:~p~n Substrate_CEPs:~p~n",[Sensors,Actuators,Substrate_CPPs,Substrate_CEPs]),
			S_Ids = [S#sensor.id || S<-Sensors],
			A_Ids = [A#actuator.id || A<-Actuators],
			CPP_Ids = [CPP#sensor.id || CPP<-Substrate_CPPs],
			CEP_Ids = [CEP#actuator.id || CEP<-Substrate_CEPs],
			Substrate = #substrate{
				id = Substrate_Id,
				agent_id = Agent_Id,
				cpp_ids = CPP_Ids,
				cep_ids = CEP_Ids,
				densities = Densities,
				plasticity=SPlasticity,
				linkform = SLinkform
			},
			write(Substrate),
			Cortex = #cortex{
				id = CortexID,
				agent_id = Agent_Id,
				neuron_ids = N_Ids,
				sensor_ids = S_Ids,
				actuator_ids = A_Ids
			}
	end,
	write(Cortex),
	{CortexID,[{0,N_Ids}],Substrate_Id}.
%constructCortex/3 generates a new CortexID, extracts the morphology from the Constraint record passed to it in SpecCon, and then extracts the initial sensors and actuators for that morphology. After the sensors and actuators are extracted, the function calls constructInitialNeuroLayer/7, which creates a single layer of neurons connected to the specified sensors and actuators, and returns the ids of the created neurons. Finally, the sensors and actuator ids are extracted from the sensors and actuators, and the cortex record is composed and stored to the database.

	constructInitialNeuroLayer(CortexID,Generation,SpecCon,Sensors,[A|Actuators],AAcc,NeuronIDAcc)->
		N_Ids = [{{0,Unique_Id},neuron}|| Unique_Id<-generate_ids(A#actuator.vector_length,[])],
		UpdatedSensors=constructInitialNeurons(CortexID,Generation,SpecCon,N_Ids,Sensors,A),
		UpdatedActuator = A#actuator{fanin_ids=N_Ids},
		constructInitialNeuroLayer(CortexID,Generation,SpecCon,UpdatedSensors,Actuators,[UpdatedActuator|AAcc],lists:append(N_Ids,NeuronIDAcc));
	constructInitialNeuroLayer(_CortexID,_Generation,_SpecCon,Sensors,[],AAcc,NeuronIDAcc)->
		[write(S) || S <- Sensors],
		[write(A) || A <- AAcc],
		NeuronIDAcc.
%constructInitialNeuroLayer/7 creates a set of neurons for each Actuator in the actuator list. The neurons are initialized in the constructInitialNeurons/6, where they are connected to the actuator, and from a random subset of the sensors passed to the function. The constructInitialNEurons/6 function returns the updated sensors, some of which have now an updated set of fanout_ids which includes the new neuron ids they were connected to. The actuator's fanin_ids is then updated to include the neuron ids that were connected to it. Once all the actuators have been connected to, the sensors and the actuators are written to the database, and the set of neuron ids created within the function is returned to the caller.

		constructInitialNeurons(CortexID,Generation,SpecCon,[N_Id|N_Ids],Sensors,Actuator)->
			case rand:uniform() >= 0.5 of
				true ->
					S = lists:nth(rand:uniform(length(Sensors)),Sensors),
					UpdatedSensors = lists:keyreplace(S#sensor.id, 2, Sensors, S#sensor{fanout_ids=[N_Id|S#sensor.fanout_ids]}),
					InputSpecs = [{S#sensor.id,S#sensor.vector_length}];
				false ->
					UpdatedSensors = [S#sensor{fanout_ids=[N_Id|S#sensor.fanout_ids]} || S <-Sensors],
					InputSpecs=[{S#sensor.id,S#sensor.vector_length}||S<-Sensors]
			end,
			construct_Neuron(CortexID,Generation,SpecCon,N_Id,InputSpecs,[Actuator#actuator.id]),
			constructInitialNeurons(CortexID,Generation,SpecCon,N_Ids,UpdatedSensors,Actuator);
		constructInitialNeurons(_CortexID,_Generation,_SpecCon,[],Sensors,_Actuator)->
			Sensors.
%constructInitialNeurons/6 accepts the list of sensors and a single actuator, connects each neuron to the actuator, and randomly chooses whether to connect it from all the sensors or a subset of the given sensors. Once all the neurons have been connected to the actuator and from the sensors, the updated sensors, whose fanout_ids have been updated with the ids of the neurons, are returned to the caller.

		construct_Neuron(CortexID,Generation,SpecCon,N_Id,InputSpecs,OutputIDs)-> 
			PF = {PFName,NLParameters} = generate_NeuronPF(SpecCon#constraint.neural_pfns),
			WeightedInputs = createWeightedInput(PFName,InputSpecs,[]), 
			Neuron=#neuron{
				id=N_Id,
				cortex_id = CortexID,
				generation=Generation,
				af=generate_NeuronActivationFunction(SpecCon#constraint.neural_afs),
				pf = PF,
				aggr_f=generate_NeuronAggrF(SpecCon#constraint.neural_aggr_fs),
				weighted_inputs=WeightedInputs,
				output_ids=OutputIDs,
				ro_ids = calculate_ROIds(N_Id,OutputIDs,[])
			},
			write(Neuron).

			createWeightedInput(PF,[{Input_Id,InputVL}|WeightedInputs],Acc) ->
				WeightsP = createNeuralWeightsP(PF,InputVL,[]),
				createWeightedInput(PF,WeightedInputs,[{Input_Id,WeightsP}|Acc]); 
			createWeightedInput(_PF,[],Acc)-> 
				Acc.
			 
				createNeuralWeightsP(_PFName,0,Acc) ->
					Acc; 
				createNeuralWeightsP(PFName,Index,Acc) ->
					W = rand:uniform()-0.5, 
					createNeuralWeightsP(PFName,Index-1,[{W,plasticity:PFName(weight_parameters)}|Acc]). 
%Each neuron record is composed by the construct_Neuron/6 function. The construct_Neuron/6 creates the Input list from the tuples [{Id,Weights}...] using the vector lengths specified in the InputSpecs list. The createWeightedInput/3 function uses createNeuralWeightsP/2 to generate a tuple list with random weights in the range of -0.5 to 0.5, and plasticity parameters dependent on the PF function. The activation function that the neuron uses is chosen randomly from the neural_afs list within the constraint record passed to the construct_Neuron/6 function. construct_Neuron uses calculate_ROIds/3 to extract the list of recursive connection ids from the OutputIDs passed to it. Once the neuron record is filled in, it is saved to the database.
		
		generate_NeuronActivationFunction(Activation_Functions)-> 
			case Activation_Functions of
				[] ->
					tanh;
				Other ->
					lists:nth(rand:uniform(length(Other)),Other)
			end.
%The generate_NeuronActivationFunction/1 accepts a list of activation function tags, and returns a randomly chosen one. If an empty list was passed as the parameter, the function returns the default tanh tag.

		generate_NeuronPF(PFNames)->
			case PFNames of
				[] ->
					{none,[]};
				Other ->
					PFName = lists:nth(rand:uniform(length(Other)),Other),
					NLParameters = plasticity:PFName(neural_parameters),
					{PFName,NLParameters}
			end.
%The generate_NeuronPF/1 accepts a list of plasticity function tags, and returns a randomly chosen one. If an empty list was passed as the parameter, the function returns the default none tag.

		generate_NeuronAggrF(Aggregation_Functions)->
			case Aggregation_Functions of
				[] ->
					none;
				Other ->
					lists:nth(rand:uniform(length(Other)),Other)
			end.
%The generate_NeuronAggrF/1 accepts a list of aggregation function tags, and returns a randomly chosen one. If an empty list was passed as the parameter, the function returns the default dot_product tag.

		calculate_ROIds(Self_Id,[Output_Id|Ids],Acc)->
			case Output_Id of
				{_,actuator} ->
					calculate_ROIds(Self_Id,Ids,Acc);
				Output_Id ->
					{{TLI,_},_NodeType} = Self_Id,
					{{LI,_},_} = Output_Id,
					case LI =< TLI of
						true ->
							calculate_ROIds(Self_Id,Ids,[Output_Id|Acc]);
						false ->
							calculate_ROIds(Self_Id,Ids,Acc)
					end
			end;
		calculate_ROIds(_Self_Id,[],Acc)->
			lists:reverse(Acc).
%The function calculate_ROIds/3 accepts as input the Self_Id of the neuron, and the OutputIDs of the elements the neuron connects to. Since each element specifies its type and, in the case of neurons, specifies the layer index it belongs to, the function checks if the Output_Id's layer index is lower than the Self_Id's layer index, if it is, the output connection is recursive and the Output_Id is added to the recursive output list. Once the recursive connection ids have been extracted from the OutputIDs, the extracted id list is returned to the caller.

			generate_ids(0,Acc) ->
				Acc; 
			generate_ids(Index,Acc)-> 
				Id = generate_UniqueId(), 
				generate_ids(Index-1,[Id|Acc]). 
	 
			generate_UniqueId()->
				{MegaSeconds,Seconds,MicroSeconds} = now(),
				1/(MegaSeconds*1000000 + Seconds + MicroSeconds/1000000).
%The generate_UniqueId/0 creates a unique Id using current time, the Id is a floating point value. The generate_ids/2 function creates a list of unique Ids.

random_element(List)->
	lists:nth(rand:uniform(length(List)),List).
%The random_element/1 function accepts a list as input, and returns a single, randomly chosen element as output.

calculate_OptimalSubstrateDimension(Sensors,Actuators)->
		S_Formats = [S#sensor.format || S<-Sensors],
		A_Formats = [A#actuator.format || A<-Actuators],
		extract_maxdim(S_Formats++A_Formats,[]) + 2.
%
		
		extract_maxdim([F|Formats],Acc)->
			DS=case F of
				{symetric,Dims}->
					length(Dims);
				no_geo ->
					1;
				undefined ->
					1
			end,
			extract_maxdim(Formats,[DS|Acc]);
		extract_maxdim([],Acc)->
			lists:max(Acc).
%
	
update_fingerprint(Agent_Id)->
	A = read({agent,Agent_Id}),
	%io:format("A:~p~n",[A]),
	Cortex = read({cortex,A#agent.cortex_id}),
	GeneralizedSensors = [(read({sensor,S_Id}))#sensor{id=undefined,cortex_id=undefined,fanout_ids=[]} || S_Id<-Cortex#cortex.sensor_ids],
	GeneralizedActuators = [(read({actuator,A_Id}))#actuator{id=undefined,cortex_id=undefined,fanin_ids=[]} || A_Id<-Cortex#cortex.actuator_ids],
	GeneralizedPattern = [{LayerIndex,length(LNeuronIDs)}||{LayerIndex,LNeuronIDs}<-A#agent.pattern],
	GeneralizedEvoHist = generalize_EvoHist(A#agent.evo_hist,[]),
	N_Ids = Cortex#cortex.neuron_ids,
	{Total_Neuron_ILs,Total_Neuron_OLs,Total_Neuron_ROs,ActivationFunction_Distribution} = get_NodeSummary(N_Ids),
	Type = A#agent.encoding_type,
	TopologySummary = #topology_summary{
		type = Type,
		tot_neurons = length(N_Ids),
		tot_n_ils = Total_Neuron_ILs,
		tot_n_ols = Total_Neuron_OLs,
		tot_n_ros = Total_Neuron_ROs,
		af_distribution = ActivationFunction_Distribution},
	Fingerprint = {GeneralizedPattern,GeneralizedEvoHist,GeneralizedSensors,GeneralizedActuators,TopologySummary},
	write(A#agent{fingerprint=Fingerprint}).
%update_fingerprint/1 calculates the fingerprint of the agent, where the fingerprint is just a tuple of the various general features of the NN based system, a list of features that play some role in distinguishing its genotype's general properties from those of other NN systems. The fingerprint here is composed of the generalized pattern (pattern minus the unique ids), generalized evolutionary history (evolutionary history minus the unique ids of the elements), a generalized sensor set, and a generalized actuator set.-record(topology_summary,{type,tot_neurons,tot_n_ils,tot_n_ols,tot_n_ros,af_distribution}).

	generalize_EvoHist([{MO,{{ALI,_AUId},AType},{{BLI,_BUId},BType},{{CLI,_CUId},CType}}|EvoHist],Acc)->
		generalize_EvoHist(EvoHist,[{MO,{ALI,AType},{BLI,BType},{CLI,CType}}|Acc]);
	generalize_EvoHist([{MO,{{ALI,_AUId},AType},{{BLI,_BUId},BType}}|EvoHist],Acc)->
		generalize_EvoHist(EvoHist,[{MO,{ALI,AType},{BLI,BType}}|Acc]);
	generalize_EvoHist([{MO,{{ALI,_AUId},AType}}|EvoHist],Acc)->
		generalize_EvoHist(EvoHist,[{MO,{ALI,AType}}|Acc]);
	generalize_EvoHist([{MO,_EId}|EvoHist],Acc)->
		generalize_EvoHist(EvoHist,[{MO}|Acc]);
	generalize_EvoHist([],Acc)->
		lists:reverse(Acc).
%generalize_EvoHist/2 generalizes the evolutionary history tuples by removing the unique element ids. Two neurons which are using exactly the same activation function, located exactly in the same layer, and using exactly the same weights will still have different unique ids, thus these ids must be removed to produce a more general set of tuples. There are 3 types of tuples in evo_hist list, with 3, 2 and 1 element ids. Once the evolutionary history list is generalized, it is returned to the caller.

update_NNTopologySummary(Agent_Id)->
	A = mnesia:read({agent,Agent_Id}),
	CortexID = A#agent.cortex_id,
	Cortex = mnesia:read({cortex,CortexID}),
	N_Ids = Cortex#cortex.neuron_ids,
	{Total_Neuron_ILs,Total_Neuron_OLs,Total_Neuron_ROs,ActivationFunction_Distribution} = get_NodeSummary(N_Ids),
	Type = A#agent.encoding_type,
	Topology_Summary = #topology_summary{
		type = Type,
		tot_neurons = length(N_Ids),
		tot_n_ils = Total_Neuron_ILs,
		tot_n_ols = Total_Neuron_OLs,
		tot_n_ros = Total_Neuron_ROs,
		af_distribution = ActivationFunction_Distribution},
	Topology_Summary.
		
	get_NodeSummary(N_Ids)->
		get_NodeSummary(N_Ids,0,0,0,{0,0,0,0,0,0,0,0,0}).
	get_NodeSummary([N_Id|N_Ids],ILAcc,OLAcc,ROAcc,FunctionDistribution)->
		Neuron = genotype:read({neuron,N_Id}),
		IL_Count = length(N#neuron.weighted_inputs),
		OL_Count = length(N#neuron.output_ids),
		RO_Count = length(N#neuron.ro_ids),
		ActivationFunction = N#neuron.af,
		{TotalTanh,TotalSin,TotalCos,TotalGaussian,TotalAbsolute,TotalSgn,TotalLog,TotalSqrt,TotalLin} = FunctionDistribution,
		U_FunctionDistribution= case ActivationFunction of
			tanh ->{TotalTanh+1,TotalSin,TotalCos,TotalGaussian,TotalAbsolute,TotalSgn,TotalLog,TotalSqrt,TotalLin};
			sin ->{TotalTanh,TotalSin+1,TotalCos,TotalGaussian,TotalAbsolute,TotalSgn,TotalLog,TotalSqrt,TotalLin};
			cos ->{TotalTanh,TotalSin,TotalCos+1,TotalGaussian,TotalAbsolute,TotalSgn,TotalLog,TotalSqrt,TotalLin};
			gaussian->{TotalTanh,TotalSin,TotalCos,TotalGaussian+1,TotalAbsolute,TotalSgn,TotalLog,TotalSqrt,TotalLin};
			absolute->{TotalTanh,TotalSin,TotalCos,TotalGaussian,TotalAbsolute+1,TotalSgn,TotalLog,TotalSqrt,TotalLin};
			sgn ->{TotalTanh,TotalSin,TotalCos,TotalGaussian,TotalAbsolute,TotalSgn+1,TotalLog,TotalSqrt,TotalLin};
			log ->{TotalTanh,TotalSin,TotalCos,TotalGaussian,TotalAbsolute,TotalSgn,TotalLog+1,TotalSqrt,TotalLin};
			sqrt ->{TotalTanh,TotalSin,TotalCos,TotalGaussian,TotalAbsolute,TotalSgn,TotalLog,TotalSqrt+1,TotalLin};
			linear ->{TotalTanh,TotalSin,TotalCos,TotalGaussian,TotalAbsolute,TotalSgn,TotalLog,TotalSqrt,TotalLin+1};
			Other -> io:format("Unknown ActivationFunction, please update ActivationFunction_Distribution tuple with:~p~n.",[Other])
		end,
		get_NodeSummary(N_Ids,IL_Count+ILAcc,OL_Count+OLAcc,RO_Count+ROAcc,U_FunctionDistribution);
	get_NodeSummary([],ILAcc,OLAcc,ROAcc,FunctionDistribution)->
		{ILAcc,OLAcc,ROAcc,FunctionDistribution}.

read(TnK)->
	case mnesia:read(TnK) of
		[] ->
			undefined;
		[R] ->
			R
	end.
	
dirty_read(TnK)->
	case mnesia:dirty_read(TnK) of
		[] ->
			undefined;
		[R] ->
			R
	end.

write(R)->
	F = fun()->
		mnesia:write(R)
	end,
	mnesia:transaction(F). 

delete(TnK)->
	F = fun()->
		mnesia:delete(TnK)
	end,
	mnesia:transaction(F). 
%read/1 accepts the tuple composed of a table name and a key: {TableName,Key}, which it then uses to read from the mnesia database and return the record to the caller. write/1 accepts a record and writes it to the database. delete/1 accepts a the tuple {TableName,Key}, and deletes the associated record from the table.
	
print(Agent_Id)->
	F = fun()->
		A = read({agent,Agent_Id}),
		Cortex = read({cortex,A#agent.cortex_id}),
		io:format("~p~n",[A]),
		io:format("~p~n",[Cortex]),
		[io:format("~p~n",[read({sensor,Id})]) || Id <- Cortex#cortex.sensor_ids],
		[io:format("~p~n",[read({neuron,Id})]) || Id <- Cortex#cortex.neuron_ids],
		[io:format("~p~n",[read({actuator,Id})]) || Id <- Cortex#cortex.actuator_ids],
		case A#agent.substrate_id of
			undefined ->
				ok;
			Substrate_Id->
				Substrate = read({substrate,Substrate_Id}),
				io:format("~p~n",[Substrate]),
				[io:format("~p~n",[read({sensor,Id})]) || Id <- Substrate#substrate.cpp_ids],
				[io:format("~p~n",[read({actuator,Id})]) || Id <- Substrate#substrate.cep_ids]
		end
	end,
	mnesia:transaction(F).
%print/1 accepts an agent's id, and prints out the complete genotype of that agent.

deleteAgent(Agent_Id)->
	A = read({agent,Agent_Id}),
	Cortex = read({cortex,A#agent.cortex_id}),
	[delete({neuron,Id}) || Id <- Cortex#cortex.neuron_ids],
	[delete({sensor,Id}) || Id <- Cortex#cortex.sensor_ids],
	[delete({actuator,Id}) || Id <- Cortex#cortex.actuator_ids],
	delete({cortex,A#agent.cortex_id}),
	delete({agent,Agent_Id}),
	case A#agent.substrate_id of
		undefined ->
			ok;
		Substrate_Id ->
			Substrate = read({substrate,Substrate_Id}),
			[delete({sensor,Id}) || Id <- Substrate#substrate.cpp_ids],
			[delete({actuator,Id})|| Id <- Substrate#substrate.cep_ids],
			delete({substrate,Substrate_Id})
	end.
%deleteAgent/1 accepts the id of an agent, and then delets that agent's genotype. This function assumes that the id of the agent will be removed from the specie's agent_ids list, and any other clean up procedures, by the calling function.

deleteAgent(Agent_Id,safe)->
	F = fun()->
		A = genotype:read({agent,Agent_Id}),
		S = genotype:read({specie,A#agent.specie_id}),
		Agent_Ids = S#specie.agent_ids,
		write(S#specie{agent_ids = lists:delete(Agent_Id,Agent_Ids)}),
		deleteAgent(Agent_Id)
	end,
	Result=mnesia:transaction(F),
	ok.
%deleteAgent/2 accepts the id of an agent, and then delets that agent's genotype, but ensures that the specie to which the agent belongs, has its agent_ids element updated. Unlinke deleteAgent/1, this function updates the specie record.

cloneAgent(Agent_Id)->
	CloneAgent_Id = {generate_UniqueId(),agent},
	cloneAgent(Agent_Id,CloneAgent_Id).
cloneAgent(Agent_Id,CloneAgent_Id)->
	F = fun()->
		A = read({agent,Agent_Id}),
		Cortex = read({cortex,A#agent.cortex_id}),
		IdsNCloneIds = ets:new(idsNcloneids,[set,private]),
		ets:insert(IdsNCloneIds,{bias,bias}),
		ets:insert(IdsNCloneIds,{Agent_Id,CloneAgent_Id}),
		[CloneCortexID] = map_ids(IdsNCloneIds,[A#agent.cortex_id],[]),
		CloneN_Ids = map_ids(IdsNCloneIds,Cortex#cortex.neuron_ids,[]),
		CloneS_Ids = map_ids(IdsNCloneIds,Cortex#cortex.sensor_ids,[]),
		CloneA_Ids = map_ids(IdsNCloneIds,Cortex#cortex.actuator_ids,[]),
		case A#agent.substrate_id of
			undefined ->
				clone_neurons(IdsNCloneIds,Cortex#cortex.neuron_ids),
				clone_sensors(IdsNCloneIds,Cortex#cortex.sensor_ids),
				clone_actuators(IdsNCloneIds,Cortex#cortex.actuator_ids),
				U_EvoHist=map_EvoHist(IdsNCloneIds,A#agent.evo_hist),
				write(Cortex#cortex{
					id = CloneCortexID,
					agent_id = CloneAgent_Id,
					sensor_ids = CloneS_Ids,
					actuator_ids = CloneA_Ids,
					neuron_ids = CloneN_Ids
				}),
				write(A#agent{
					id = CloneAgent_Id,
					cortex_id = CloneCortexID,
					evo_hist = U_EvoHist
				});
			Substrate_Id ->
				Substrate = read({substrate,A#agent.substrate_id}),
				[CloneSubstrate_Id] = map_ids(IdsNCloneIds,[A#agent.substrate_id],[]),
				CloneCPP_Ids = map_ids(IdsNCloneIds,Substrate#substrate.cpp_ids,[]),
				CloneCEP_Ids = map_ids(IdsNCloneIds,Substrate#substrate.cep_ids,[]),
				clone_neurons(IdsNCloneIds,Cortex#cortex.neuron_ids),
				clone_sensors(IdsNCloneIds,Cortex#cortex.sensor_ids),
				clone_actuators(IdsNCloneIds,Cortex#cortex.actuator_ids),
				Substrate = read({substrate,A#agent.substrate_id}),
				clone_sensors(IdsNCloneIds,Substrate#substrate.cpp_ids),
				clone_actuators(IdsNCloneIds,Substrate#substrate.cep_ids),
				U_EvoHist=map_EvoHist(IdsNCloneIds,A#agent.evo_hist),
				write(Substrate#substrate{
					id = CloneSubstrate_Id,
					agent_id = CloneAgent_Id,
					cpp_ids = CloneCPP_Ids,
					cep_ids = CloneCEP_Ids
				}),
				write(Cortex#cortex{
					id = CloneCortexID,
					agent_id = CloneAgent_Id,
					sensor_ids = CloneS_Ids,
					actuator_ids = CloneA_Ids,
					neuron_ids = CloneN_Ids
				}),
				write(A#agent{
					id = CloneAgent_Id,
					cortex_id = CloneCortexID,
					substrate_id = CloneSubstrate_Id,
					evo_hist = U_EvoHist
				})
		end,
		ets:delete(IdsNCloneIds)
	end,
	mnesia:transaction(F),
	CloneAgent_Id.
%cloneAgent/2 accepts Agent_Id, and CloneAgent_Id, and then clones the agent, giving the clone CloneAgent_Id. The function first creates an ETS table to which it writes the ids of all the elements of the genotype, and their corresponding clone ids. Once all ids and clone ids have been generated, the function then begins to clone the actual elements. cloneAgent/2 first clones the neurons using clone_neurons/2, then the sensors using clone_sensonrs/2, and finally the actuators using clone_actuators. Once these elements are cloned, the function writes to database the clone versions of the cortex and the agent records, by writing to databse the original records with updated ids.

	map_ids(TableName,[Id|Ids],Acc)->
		CloneId=case Id of
			{{LayerIndex,_NumId},Type}->%maps neuron and cortex ids.
				{{LayerIndex,generate_UniqueId()},Type};
			{_NumId,Type}->%mapes sensor and actuator ids.
				{generate_UniqueId(),Type}
		end,
		ets:insert(TableName,{Id,CloneId}),
		map_ids(TableName,Ids,[CloneId|Acc]);
	map_ids(_TableName,[],Acc)->
		Acc.
%map_ids/3 accepts the name of the ets table, and a list of ids. It then goes through every id and creates a clone version of the id by generating a new unique id. The function is able to generate new id structures for neuron, cortex, sensor, and actuator id types.

	clone_sensors(TableName,[S_Id|S_Ids])->
		S = read({sensor,S_Id}),
		CloneS_Id = ets:lookup_element(TableName,S_Id,2),
		CloneCortexID = ets:lookup_element(TableName,S#sensor.cortex_id,2),
		CloneFanout_Ids =[ets:lookup_element(TableName,Fanout_Id,2)|| Fanout_Id <- S#sensor.fanout_ids],
		write(S#sensor{
			id = CloneS_Id,
			cortex_id = CloneCortexID,
			fanout_ids = CloneFanout_Ids
		}),
		clone_sensors(TableName,S_Ids);
	clone_sensors(_TableName,[])->
		done.	
%clone_sensors/2 accepts as input the name of the ets table and the list of sensor ids. It then goes through every sensor id, reads the sensor from the database, and updates all the ids (id, cortex_id, and fanout_ids) from their original values, to their clone values stored in the ets table. Then the new version of the sensor is written to the database.

	clone_actuators(TableName,[A_Id|A_Ids])->
		A = read({actuator,A_Id}),
		CloneA_Id = ets:lookup_element(TableName,A_Id,2),
		CloneCortexID = ets:lookup_element(TableName,A#actuator.cortex_id,2),
		CloneFanin_Ids =[ets:lookup_element(TableName,Fanin_Id,2)|| Fanin_Id <- A#actuator.fanin_ids],
		write(A#actuator{
			id = CloneA_Id,
			cortex_id = CloneCortexID,
			fanin_ids = CloneFanin_Ids
		}),
		clone_actuators(TableName,A_Ids);
	clone_actuators(_TableName,[])->
		done.	
%clone_actuators/2 accepts as input the name of the ets table and the list of actuator ids. It then goes through every actuator id, reads the actuator from the database, and updates all the ids (id, cortex_id, and fanin_ids) from their original values, to their clone values stored in the ets table. Then the new version of the actuator is written to the database.

	clone_neurons(TableName,[N_Id|N_Ids])->
		Neuron = read({neuron,N_Id}),
		CloneN_Id = ets:lookup_element(TableName,N_Id,2),
		CloneCortexID = ets:lookup_element(TableName,N#neuron.cortex_id,2),
		CloneWeightedInputs =  [{ets:lookup_element(TableName,I_Id,2),WeightsP}|| {I_Id,WeightsP} <- N#neuron.weighted_inputs],
		CloneWeightedInputs_Modulation =  [{ets:lookup_element(TableName,I_Id,2),WeightsP}|| {I_Id,WeightsP} <- N#neuron.weighted_inputs_modulation],
		CloneOutputIDs = [ets:lookup_element(TableName,O_Id,2)|| O_Id <- N#neuron.output_ids],
		CloneRO_Ids =[ets:lookup_element(TableName,RO_Id,2)|| RO_Id <- N#neuron.ro_ids],
		write(N#neuron{
			id = CloneN_Id,
			cortex_id = CloneCortexID,
			weighted_inputs = CloneWeightedInputs,
			weighted_inputs_modulation = CloneWeightedInputs_Modulation,
			output_ids = CloneOutputIDs,
			ro_ids = CloneRO_Ids
		}),
		clone_neurons(TableName,N_Ids);
	clone_neurons(_TableName,[])->
		done.	
%clone_neuron/2 accepts as input the name of the ets table and the list of neuron ids. It then goes through every neuron id, reads the neuron from the database, and updates all the ids (id, cortex_id, output_ids, ro_ids) and weighted_inputs from their original values, to their clone values stored in the ets table. Once the everything is updated, the new (clone) version of the neuron is written to the database.
	
	map_EvoHist(TableName,EvoHist)-> map_EvoHist(TableName,EvoHist,[]).
	map_EvoHist(TableName,[{MO,E1Id,E2Id,E3Id}|EvoHist],Acc)->
		Clone_E1Id = ets:lookup_element(TableName,E1Id,2),
		Clone_E2Id = ets:lookup_element(TableName,E2Id,2),
		Clone_E3Id = ets:lookup_element(TableName,E3Id,2),
		map_EvoHist(TableName,EvoHist,[{MO,Clone_E1Id,Clone_E2Id,Clone_E3Id}|Acc]);
	map_EvoHist(TableName,[{MO,E1Id,E2Id}|EvoHist],Acc)->
		Clone_E1Id = ets:lookup_element(TableName,E1Id,2),
		Clone_E2Id = ets:lookup_element(TableName,E2Id,2),
		map_EvoHist(TableName,EvoHist,[{MO,Clone_E1Id,Clone_E2Id}|Acc]);
	map_EvoHist(TableName,[{MO,E1Id}|EvoHist],Acc)->
		Clone_E1Id = ets:lookup_element(TableName,E1Id,2),
		map_EvoHist(TableName,EvoHist,[{MO,Clone_E1Id}|Acc]);
	map_EvoHist(_TableName,[],Acc)->
		lists:reverse(Acc).
%map_EvoHist/2 is a wrapper for map_EvoHist/3, which in turn accepts the evo_hist list containing the mutation operator tuples that have been appplied to the NN system. The function is used when a clone of a NN system is created. The function updates the original Ids of the elements the mutation oeprators have been applied to, to the clone's Ids, so that the updated evo_hist can reflect the clone's topology, as if the mutation operators have been applied to it, and that it is not a clone. Once all the tuples in the evo_hist have been updated with the clone element ids, the list is reverted to its proper order, and the updated list is returned to the caller.
	
speciate(Agent_Id)->
	update_fingerprint(Agent_Id),
	A = read({agent,Agent_Id}),
	case A#agent.id of
		test ->%Test agent belongs to no specie and no population.
			write(A#agent{fitness = undefined});
		_ ->
			Parent_S = read({specie,A#agent.specie_id}),
			P = read({population,Parent_S#specie.population_id}),
			case [Id || Id <- P#population.specie_ids, (read({specie,Id}))#specie.fingerprint == A#agent.fingerprint] of
				[] ->
					Specie_Id = population_monitor:create_specie(P#population.id,A#agent.constraint,A#agent.fingerprint),
					S = read({specie,Specie_Id}),
					UpdatedActuator = A#agent{specie_id=Specie_Id,fitness = undefined},
					U_S = S#specie{agent_ids = [Agent_Id]},
					write(UpdatedActuator),
					write(U_S);
				[Specie_Id] ->
					S = read({specie,Specie_Id}),
					UpdatedActuator = A#agent{specie_id=Specie_Id,fitness = undefined},
					U_S = S#specie{agent_ids = [Agent_Id|S#specie.agent_ids]},
					write(UpdatedActuator),
					write(U_S)
			end
	end.			
%The function speciate/1 reads a newly created agent record, calculates that agent's fingerprint, and then based on that fingerprint either inserts it into an already existing specie, or creates a new specie of which the agent is the first of a kind. The function first creates the fingerprint of the agent using the genotype:create_fingerprint/1 function. Then the function checks whether this is a test agent, in which case it is only used for testing, and does not belong to any specie or population. If the agent is not a test agent, then the specie and population to which its parent belonged is retreived from the database (the specie and population ids are conserved in the offspring during mutation, so the agent already holds his parent's specie and population ids). Afterwards, a specie is found which has the same fingerprint as the agent. If there is no such specie, then a new specie is created, a specie that belongs to the same population as the agent, and has the same constriants and fingerprint as the agent fathering the specie. Then the agent's id is entered into the specie, and the updated specie and agent are written to database. If on the other hand a specie already exists with the same fingerprint as the agent, then the agent's id is added to the existing specie, and the updated specie and agent are written to database.

test()->
	Specie_Id = test,
	Agent_Id = test,
	CloneAgent_Id = test_clone,
	SpecCon = #constraint{morphology=pole_balancing,connection_architecture=feedforward, population_evo_alg_f=generational,neural_afs=[tanh],agent_encoding_types=[substrate],substrate_plasticities=[none]},
	F = fun()->
		constructAgent(Specie_Id,Agent_Id,SpecCon),
		cloneAgent(Specie_Id,CloneAgent_Id),
		print(Agent_Id),
		print(CloneAgent_Id),
		deleteAgent(Agent_Id),
		deleteAgent(CloneAgent_Id)
	end,
	mnesia:transaction(F).
%test/0 performs a test of the standard functions of the genotype module, by first creating a new agent, then cloning that agent, then printing the genotype of the original agent and its clone, and then finally deleting both of the agents.

create_test()->
	Specie_Id = test,
	Agent_Id = test,
	SpecCon = #constraint{morphology=pole_balancing,connection_architecture=feedforward, population_evo_alg_f=generational,neural_afs=[tanh],agent_encoding_types=[substrate],substrate_plasticities=[iterative]},
	F = fun()->
		case genotype:read({agent,test}) of
			undefined ->
				constructAgent(Specie_Id,Agent_Id,SpecCon),
				print(Agent_Id);
			_ ->
				deleteAgent(Agent_Id),
				constructAgent(Specie_Id,Agent_Id,SpecCon),
				print(Agent_Id)
		end
	end,
	mnesia:transaction(F).
%create_test/0 creates a simple NN based agent using the default constraint record. The function first checks if an agent with the id 'test' already exists, if it does, the function deletes that agent and creates a new one. Otherwise, the function just creates a brand new agent with the 'test' id.
