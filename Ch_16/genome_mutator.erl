%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(genome_mutator).
-compile(export_all).
-include("records.hrl").
-define(DELTA_MULTIPLIER,math:pi()*2).
-define(SAT_LIMIT,math:pi()*2).
-define(SEARCH_PARAMTERS_MUTATION_PROBABILITY,0.1).
-define(ES_MUTATORS,[
	mutate_tuning_selection,
	mutate_tuning_duration,
	mutate_tuning_annealing,
	mutate_tot_topological_mutations,
	mutate_heredity_type
]).
-define(ACTUATORS,morphology:get_Actuators(A#agent.morphology)).
-define(SENSORS,morphology:get_Sensors(A#agent.morphology)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
long_test(TotalMutations) when TotalMutations > 0->
	genotype:create_test(),
	short_test(TotalMutations).

	short_test(0)->
		exoself:start(test,void);
	short_test(Index)->
		test(),
		short_test(Index-1).
%This is a simple function that executes the test() function the number of times with which the long_test/1 function was initially called. The test/0 function executes mutate(test), which applies a random number of mutation operators to the genotype, where that number ranges from 1 to sqrt(Total_neurons). After all the mutation operators have been applied succesfully, the function executes exoself:start(test,void), mapping the genotype to phenotype, to test whether the resulting NN system is functional.

test()->
	Result = mutate(test),
	case Result of
		{atomic,_} ->
			io:format("******** Mutation Succesful.~n");
		_->
			io:format("******** Mutation Failure:~p~n",[Result])
	end.
%The test/1 function simply tests the mutate/1 function with an agent whose id is 'test'.

test(AgentID,Mutator)->
	F = fun()->
		genome_mutator:Mutator(AgentID)
	end,
	mnesia:transaction(F).
%test/2 function tests the mutation operator "Mutator" on the agent with an id AgentID.

mutate(AgentID)->
	random:seed(now()),
	F = fun()->
		mutate_SearchParameters(AgentID),
		A = genotype:read({agent,AgentID}),
		{TTM_Name,Parameter} = A#agent.tot_topological_mutations_f,
		TotalMutations = tot_topological_mutations:TTM_Name(Parameter,AgentID),
		OldGeneration = A#agent.generation,
		NewGeneration = OldGeneration+1,
		genotype:write(A#agent{generation = NewGeneration}),
		apply_Mutators(AgentID,TotalMutations),
		genotype:update_fingerprint(AgentID)
	end,
	mnesia:transaction(F).
%The function mutate/1 first updates the generation of the agent to be mutated, then calculates the number of mutation operators to be applied to it by executing the tot_topological_mutations:TTM_Name/2 function, and then finally runs the apply_Mutators/2 function, which mutates the agent. Once the agent is mutated, the function updates its fingerprint by executing genotype:update_finrgerprint/1.

	mutate_SearchParameters(AgentID)->
		case rand:uniform() < ?SEARCH_PARAMTERS_MUTATION_PROBABILITY of
			true ->
				TotalMutations = rand:uniform(length(?ES_MUTATORS)),
				apply_ESMutators(AgentID,TotalMutations);
			false ->
				ok
		end.
%mutate_SearchParamters/1 with a probability of ?SEARCH_PARAMETERS_MUTATION_PROBABILITY applies a random number between 1 and length(?ES_MUTATORS) of evolutioanry strategy mutation operators from the ?ES_MUTATORS list.

		apply_ESMutators(_AgentID,0)->
			done;
		apply_ESMutators(AgentID,MutationIndex)->
			ES_Mutators = ?ES_MUTATORS,
			ES_Mutator = lists:nth(rand:uniform(length(ES_Mutators)),ES_Mutators),
			io:format("Evolutionary Strategy Mutation Operator:~p~n",[ES_Mutator]),
			Result = genome_mutator:ES_Mutator(AgentID),
			case Result of
				{atomic,_} ->
					apply_ESMutators(AgentID,MutationIndex-1);
				Error ->
					io:format("******** Error:~p~nRetrying with new Mutation...~n",[Error]),
					apply_ESMutators(AgentID,MutationIndex-1)
			end.
%apply_ESMutators/2 with uniform distribution chooses a random evolutionary strategy mutation operator from the ?ES_MUTATORS list of such functions, and applies it to the agent. Whether the mutation succcessful or not, the function counts down the total number of mutation operators left to apply. This is to ensure that if the researcher set for each such evolutionary strategy to be static, having only one available mutatable parameter for every agent, the system will eventually try to mutate the strategy TotalMutations number of times, and then return to the caller.

	apply_Mutators(_AgentID,0)->
		done;
	apply_Mutators(AgentID,MutationIndex)->
		Result = apply_NeuralMutator(AgentID),
		case Result of
			{atomic,_} ->
				apply_Mutators(AgentID,MutationIndex-1);
			Error ->
				io:format("******** Error:~p~nRetrying with new Mutation...~n",[Error]),
				apply_Mutators(AgentID,MutationIndex)
		end.
%apply_Mutators/2 applies the set number of successfull mutation operators to the Agent. If a mutaiton operator exits with an error, the function tries another mutaiton operator. It is only after a sucessfull mutation operator is applied that the MutationIndex is decremented.

		apply_NeuralMutator(AgentID)->
			F = fun()->
				A = genotype:read({agent,AgentID}),
				MutatorsP = A#agent.mutation_operators,
				Mutator = select_random_MO(MutatorsP),
				io:format("Mutation Operator:~p~n",[Mutator]),
				genome_mutator:Mutator(AgentID)
			end,
			mnesia:transaction(F).
%apply_NeuralMutator/1 applies the available mutation operators to the NN. Because the genotype is stored in mnesia, if the mutation operator function exits with an error, the database made changes are retracted, and a new mutation operator can then be applied to the agent, as if the previous unsuccessful mutation operator was never applied. The mutation operator to be applied to the agent is chosen randomly from the agent's mutation_operators list.
			
			select_random_MO(MutatorsP)->
				TotalSize = lists:sum([SliceSize || {_MO,SliceSize} <- MutatorsP]),
				Choice=rand:uniform(TotalSize),
				select_random_MO(MutatorsP,Choice,0).
				
				select_random_MO([{MO,SliceSize}|MOs],Choice,Range_From)->
					Range_To = Range_From+SliceSize,
					case (Choice >= Range_From) and (Choice =< Range_To) of
						true ->
							MO;
						false ->
							select_random_MO(MOs,Choice,Range_To)
					end;
				select_random_MO([],_Choice,_Range_From)->
					exit("********ERROR:select_random_MO:: reached [] without selecting a mutation operator.").
%select_random_MO/1, using the analogy of a roulette wheel, first calculates the entire are of the wheel by summing together all the slice sizes of the parts. The function than chooses randomly a spot on the wheel, and through select_random_MO/3 calculates where that spot is located, with regards to the mutation operator that it falls on. Since some slices are larger then others, they will have uniformly larger probabilities of being selected.

mutate_tuning_selection(AgentID)->
	A = genotype:read({agent,AgentID}),
	case (A#agent.constraint)#constraint.tuning_selection_fs -- [A#agent.tuning_selection_f] of
		[] ->
			exit("********ERROR:mutate_tuning_selection/1:: Nothing to mutate, only a single function available.");
		Tuning_Selection_Functions->
			New_TSF = lists:nth(rand:uniform(length(Tuning_Selection_Functions)),Tuning_Selection_Functions),
			UpdatedActuator = A#agent{tuning_selection_f = New_TSF},
			genotype:write(UpdatedActuator)
	end.
%mutate_tuning_selection/1 function checks if there are any other than the currently used tuning selection functions available in the agent's constraint. If there is, then it chooses a random one from this list, and sets the agent's tuning_selection_f to it. If there are no other tuning selection functions, then it exits with an error.
	
mutate_tuning_annealing(AgentID)->
	A = genotype:read({agent,AgentID}),
	case (A#agent.constraint)#constraint.annealing_parameters -- [A#agent.annealing_parameter] of
		[] ->
			exit("********ERROR:mutate_tuning_annealing/1:: Nothing to mutate, only a single function available.");
		Tuning_Annealing_Parameters->
			New_TAP = lists:nth(rand:uniform(length(Tuning_Annealing_Parameters)),Tuning_Annealing_Parameters),
			UpdatedActuator = A#agent{annealing_parameter = New_TAP},
			genotype:write(UpdatedActuator)
	end.
%mutate_annealing_parameter/1 function checks if there are any other than the currently used tuning annealing parameters available in the agent's constraint. If there is, then it chooses a random one from this list, and sets the agent's annealing_parameter to it. If there are no other tuning annealing parameters, then it exits with an error.

mutate_tot_topological_mutations(AgentID)->
	A = genotype:read({agent,AgentID}),
	case (A#agent.constraint)#constraint.tuning_selection_fs -- [A#agent.tuning_selection_f] of
		[] ->
			exit("********ERROR:mutate_tuning_selection/1:: Nothing to mutate, only a single function available.");
		Tuning_Selection_Functions->
			New_TSF = lists:nth(rand:uniform(length(Tuning_Selection_Functions)),Tuning_Selection_Functions),
			UpdatedActuator = A#agent{tuning_selection_f = New_TSF},
			genotype:write(UpdatedActuator)
	end.
%mutate_tot_topological_mutations/1 function checks if there are any other than the currently used tuning tot topological mutation functions available in the agent's constraint. If there is, then it chooses a random one from this list, and sets the agent's tot_topological_mutations_f to it. If there are no other functions that can calculate tot topological mutations, then it exits with an error.

mutate_heredity_type(AgentID)->
	A = genotype:read({agent,AgentID}),
	case (A#agent.constraint)#constraint.heredity_types -- [A#agent.heredity_type] of
		[] ->
			exit("********ERROR:mutate_heredity_type/1:: Nothing to mutate, only a single function available.");
		Heredity_Type_Pool->
			New_HT = lists:nth(rand:uniform(length(Heredity_Type_Pool)),Heredity_Type_Pool),
			UpdatedActuator = A#agent{heredity_type = New_HT},
			genotype:write(UpdatedActuator)
	end.
%mutate_heredity_type/1 function checks if there are any other heredity types in the agent's constraint record. If any other than the one currently used by the agent are present, the agent exchanges the heredity type it currently uses to a random one from the remaining list. If no other heredity types are available, the mutation operator exits with an error, and the neuroevolutionary system tries another mutation operator.

mutate_weights(AgentID)->
	A = genotype:read({agent,AgentID}),
	CortexID = A#agent.cortex_id,
	Cortex = genotype:read({cortex,CortexID}),
	NeuronIDs = Cortex#cortex.neuron_ids,

	NeuronID = lists:nth(rand:uniform(length(NeuronIDs)),NeuronIDs),
	Neuron = genotype:read({neuron,NeuronID}),
	WeightedInputs = N#neuron.weighted_inputs,
	U_WeightedInputs = perturbWeightedInputs(WeightedInputs),
	UpdatedNeuron = Neuron#neuron{weighted_inputs = U_WeightedInputs},
	EvoHist = A#agent.evo_hist,
	U_EvoHist = [{mutate_weights,NeuronID}|EvoHist],
	UpdatedActuator = A#agent{evo_hist = U_EvoHist},
	genotype:write(UpdatedNeuron),
	genotype:write(UpdatedActuator).
%The mutate_weights/1 function accepts the AgentID parameter, extracts the NN's cortex, and then chooses a random neuron belonging to the NN with a uniform distribution probability. Then the neuron's weighted_inputs list is extracted, and the function perturbWeightedInputs/1 is used to perturb/mutate the weights. Once the WeightedInputs have been perturbed, the agent's evolutionary history, EvoHist is updated to include the successfully applied mutate_weights mutation operator. Then the updated Agent and the updated neuron are written to the database.
	
	perturbWeightedInputs(WeightedInputs)->
		Total_WeightsP=lists:sum([length(WeightsP) || {_InputID,WeightsP}<-WeightedInputs]),
		MP = 1/math:sqrt(Total_WeightsP),
		perturbWeightedInputs(MP,WeightedInputs,[]).
	perturbWeightedInputs(MP,[{InputID,WeightsP}|WeightedInputs],Acc)->
		U_WeightsP = perturb_weightsP(MP,WeightsP,[]),
		perturbWeightedInputs(MP,WeightedInputs,[{InputID,U_WeightsP}|Acc]);
	perturbWeightedInputs(_MP,[],Acc)->
		lists:reverse(Acc).
%perturbWeightedInputs/1 accepts the WeightedInputs list of the format:[{Id,Weights}...], calculates the total number of weights in the WeightedInputs, and then calculates the mutation probability MP, which is 1/sqrt(Total_Weights). Once the mutation probability is calculated, each weight in the WeightedInputs list has a chance of MP to be perturbed/mutated. Once all the weights in the WeightedInputs list had a chance of being mutated, the updated WeightedInputs is returned to the caller.

	perturb_weightsP(MP,[{W,PL}|Weights],Acc)->
		U_W = case rand:uniform() < MP of
			true->
				sat((rand:uniform()-0.5)*?DELTA_MULTIPLIER+W,-?SAT_LIMIT,?SAT_LIMIT);
			false ->
				W
		end,
		perturb_weightsP(MP,Weights,[{U_W,PL}|Acc]);
	perturb_weightsP(_MP,[],Acc)->
		lists:reverse(Acc).
%perturb_weightsP/3 is called with the mutation probability MP, a weights list, and an empty list, [], to be used as an accumulator. The function goes through every weight, where every weight has a chance of MP to be mutated/perturbed. The perturbations have a random intensity between -Pi/2 and Pi/2. Once all the weights in the weights list had a chance of being perturbed, the updated weights list is reversed back to its original order, and returned back to the caller.
		sat(Val,Min,Max)->
			if
				Val < Min -> Min;
				Val > Max -> Max;
				true -> Val
			end.
%sat/3 calculates whether Val is between Min and Max values, if it is, then val is returned as is. If Val is less than Min, then Min is returned, if Val is greater than Max, then Max is returned.

add_bias(AgentID)->
	A = genotype:read({agent,AgentID}),
	CortexID = A#agent.cortex_id,
	Cortex = genotype:read({cortex,CortexID}),
	NeuronIDs = Cortex#cortex.neuron_ids,
	NeuronID = lists:nth(rand:uniform(length(NeuronIDs)),NeuronIDs),
	Generation = A#agent.generation,
	
	Neuron = genotype:read({neuron,NeuronID}),
	SIWeightedInputs = N#neuron.weighted_inputs,
	MIWeightedInputs = N#neuron.weighted_inputs_modulation,
	{PFName,_NLParameters} = N#neuron.pf,
	case {lists:keymember(bias,1,SIWeightedInputs), lists:keymember(bias,1,MIWeightedInputs), PFName == neuromodulation, rand:uniform(2)} of
		{_,false,true,2} ->
			U_MIWeightedInputs = lists:append(MIWeightedInputs,[{bias,[{rand:uniform()-0.5,plasticity:PFName(weight_parameters)}]}]),
			UpdatedNeuron = Neuron#neuron{
				weighted_inputs_modulation = U_MIWeightedInputs,
				generation = Generation},
			EvoHist = A#agent.evo_hist,
			U_EvoHist = [{{add_bias,m},NeuronID}|EvoHist],
			UpdatedActuator = A#agent{evo_hist=U_EvoHist},
			genotype:write(UpdatedNeuron),
			genotype:write(UpdatedActuator);
		{true,_,_,_} ->
			exit("********ERROR:add_bias:: This Neuron already has a bias in weighted_inputs.");
		{false,_,_,_} ->
			U_SIWeightedInputs = lists:append(SIWeightedInputs,[{bias,[{rand:uniform()-0.5,plasticity:PFName(weight_parameters)}]}]),
			UpdatedNeuron = Neuron#neuron{
				weighted_inputs = U_SIWeightedInputs,
				generation = Generation},
			EvoHist = A#agent.evo_hist,
			U_EvoHist = [{{add_bias,s},NeuronID}|EvoHist],
			UpdatedActuator = A#agent{evo_hist=U_EvoHist},
			genotype:write(UpdatedNeuron),
			genotype:write(UpdatedActuator)
	end.
%The add_bias/1 function is called with the AgentID parameter. The function first extracts the neuron_ids list from the cortex element and chooses a random neuron from the id list. After the neuron is read from the database, we check whether weighted_inputs and weighted_inputs_modulation lists already have bias, and we randomly generate a value 1 or 2. If the value 1 is generated and the weighted_inputs list does not have a bias, it is added. If the value 2 is generated, and the weighted_inputs_modulation does not have a bias, it is added. Otherwise an error is returned. 

remove_bias(AgentID)->
	A = genotype:read({agent,AgentID}),
	CortexID = A#agent.cortex_id,
	Cortex = genotype:read({cortex,CortexID}),
	NeuronIDs = Cortex#cortex.neuron_ids,
	NeuronID = lists:nth(rand:uniform(length(NeuronIDs)),NeuronIDs),
	Generation = A#agent.generation,
	
	Neuron = genotype:read({neuron,NeuronID}),
	SIWeightedInputs = N#neuron.weighted_inputs,
	MIWeightedInputs = N#neuron.weighted_inputs_modulation,
	{PFName,_NLParameters} = N#neuron.pf,
	case {lists:keymember(bias,1,SIWeightedInputs), lists:keymember(bias,1,MIWeightedInputs), PFName == neuromodulation, rand:uniform(2)} of
		{_,true,true,2} ->%Remove modulatory bias
			U_MIWeightedInputs = lists:keydelete(bias,1,MIWeightedInputs),
			UpdatedNeuron = Neuron#neuron{
				weighted_inputs_modulation = U_MIWeightedInputs,
				generation = Generation},
			EvoHist = A#agent.evo_hist,
			U_EvoHist = [{{remove_bias,m},NeuronID}|EvoHist],
			UpdatedActuator = A#agent{evo_hist=U_EvoHist},
			genotype:write(UpdatedNeuron),
			genotype:write(UpdatedActuator);
		{false,_,_,_} ->
			exit("********ERROR:remove_bias:: This Neuron does not have a bias in weighted_inputs.");
		{true,_,_,_} ->%Remove synaptic bias
			U_SIWeightedInputs = lists:keydelete(bias,1,SIWeightedInputs),
			UpdatedNeuron = Neuron#neuron{
				weighted_inputs = U_SIWeightedInputs,
				generation = Generation},
			EvoHist = A#agent.evo_hist,
			U_EvoHist = [{{remove_bias,s},NeuronID}|EvoHist],
			UpdatedActuator = A#agent{evo_hist=U_EvoHist},
			genotype:write(UpdatedNeuron),
			genotype:write(UpdatedActuator)
	end.
%The remove_bias/1 function is called with the AgentID parameter. The function first extracts the neuron_ids list from the cortex element and chooses a random neuron from the id list. After the neuron is read from the database, we check whether weighted_inputs and weighted_inputs_modulation lists already have bias, and we randomly generate a value 1 or 2. If the value 1 is generated and the weighted_inputs list has a bias, it is removed. If the value 2 is generated, and the weighted_inputs_modulation has a bias, it is removed. Otherwise an error is returned. 

mutate_af(AgentID)->
	A = genotype:read({agent,AgentID}),
	CortexID = A#agent.cortex_id,
	Cortex = genotype:read({cortex,CortexID}),
	NeuronIDs = Cortex#cortex.neuron_ids,
	NeuronID = lists:nth(rand:uniform(length(NeuronIDs)),NeuronIDs),
	Generation = A#agent.generation,
	
	Neuron = genotype:read({neuron,NeuronID}),
	ActivationFunction = N#neuron.af,
	case (A#agent.constraint)#constraint.neural_afs -- [ActivationFunction] of
		[] ->
			exit("********ERROR:mutate_af:: There are no other activation functions to use.");
		Activation_Functions ->
			NewActivationFunction = lists:nth(rand:uniform(length(Activation_Functions)),Activation_Functions),
			UpdatedNeuron = Neuron#neuron{af=NewActivationFunction,generation=Generation},
			EvoHist = A#agent.evo_hist,
			U_EvoHist = [{mutate_af,NeuronID}|EvoHist],
			UpdatedActuator = A#agent{evo_hist=U_EvoHist},
			genotype:write(UpdatedNeuron),
			genotype:write(UpdatedActuator)
	end.
%The mutate_af/1 function chooses a random neuron, and then changes its currently used activation function into another one available from the neural_afs list of the agent's constraint record.

mutate_pf(AgentID)->
	A = genotype:read({agent,AgentID}),
	CortexID = A#agent.cortex_id,
	Cortex = genotype:read({cortex,CortexID}),
	NeuronIDs = Cortex#cortex.neuron_ids,
	NeuronID = lists:nth(rand:uniform(length(NeuronIDs)),NeuronIDs),
	Generation = A#agent.generation,
	
	Neuron = genotype:read({neuron,NeuronID}),
	{PFName,_NLParameters} = N#neuron.pf,
	case (A#agent.constraint)#constraint.neural_pfns -- [PFName] of
		[] ->
			exit("********ERROR:mutate_pf:: There are no other plasticity functions to use.");
		Other_PFNames ->
			New_PFName = lists:nth(rand:uniform(length(Other_PFNames)),Other_PFNames),
			New_NLParameters = plasticity:New_PFName(neural_parameters),
			NewPF = {New_PFName,New_NLParameters},
			InputIdPs = N#neuron.weighted_inputs,
			UWeightedInput = [{InputIDP,plasticity:New_PFName(weight_parameters)} || {InputIDP,_OldPL} <- InputIdPs],
			UpdatedNeuron = Neuron#neuron{pf=NewPF,weighted_inputs = UWeightedInput, generation=Generation},
			EvoHist = A#agent.evo_hist,
			U_EvoHist = [{mutate_pf,NeuronID}|EvoHist],
			UpdatedActuator = A#agent{evo_hist=U_EvoHist},
			genotype:write(UpdatedNeuron),
			genotype:write(UpdatedActuator)
	end.
%The mutate_pf/1 function chooses a random neuron, and then changes its currently used plasticity function into another one available from the neural_pfs list of the agent's constraint record.

mutate_plasticity_parameters(AgentID)->
	A = genotype:read({agent,AgentID}),
	CortexID = A#agent.cortex_id,
	Cortex = genotype:read({cortex,CortexID}),
	NeuronIDs = Cortex#cortex.neuron_ids,
	NeuronID = lists:nth(rand:uniform(length(NeuronIDs)),NeuronIDs),
	Neuron = genotype:read({neuron,NeuronID}),
	{PFName,_Parameters} = N#neuron.pf,
	UpdatedNeuron = plasticity:PFName({NeuronID,mutate}),
	EvoHist = A#agent.evo_hist,
	U_EvoHist = [{mutate_plasticity_parameters,NeuronID}|EvoHist],
	UpdatedActuator = A#agent{evo_hist=U_EvoHist},
	genotype:write(UpdatedNeuron),
	genotype:write(UpdatedActuator).
%The mutate_plasticity_parameters/1 chooses a random neuron from the NN, and mutates the parameters of its plasticity function, if present.

mutate_aggrf(AgentID)->
	A = genotype:read({agent,AgentID}),
	CortexID = A#agent.cortex_id,
	Cortex = genotype:read({cortex,CortexID}),
	NeuronIDs = Cortex#cortex.neuron_ids,
	NeuronID = lists:nth(rand:uniform(length(NeuronIDs)),NeuronIDs),
	Generation = A#agent.generation,
	
	Neuron = genotype:read({neuron,NeuronID}),
	AggrF = N#neuron.aggr_f,
	case (A#agent.constraint)#constraint.neural_aggr_fs -- [AggrF] of
		[] ->
			exit("********ERROR:mutate_aggrf:: There are no other aggregation functions to use.");
		Aggregation_Functions ->
			NewAggrF = lists:nth(rand:uniform(length(Aggregation_Functions)),Aggregation_Functions),
			UpdatedNeuron = Neuron#neuron{aggr_f=NewAggrF,generation=Generation},
			EvoHist = A#agent.evo_hist,
			U_EvoHist = [{mutate_aggrf,NeuronID}|EvoHist],
			UpdatedActuator = A#agent{evo_hist=U_EvoHist},
			genotype:write(UpdatedNeuron),
			genotype:write(UpdatedActuator)
	end.
%The mutate_aggrf/1 function chooses a random neuron, and then changes its currently used aggregation function into another one available from the neural_aggr_fs list of the agent's constraint record.
	
link_FromElementToElement(AgentID,From_ElementId,To_ElementId)->
	case {From_ElementId,To_ElementId} of
		{{_FromSensorID,neuron},{_ToSensorID,neuron}} ->
			link_FromNeuronToNeuron(AgentID,From_ElementId,To_ElementId);
		{{_FromSensorID,sensor},{_ToSensorID,neuron}} ->
			link_FromSensorToNeuron(AgentID,From_ElementId,To_ElementId);
		{{_FromNeuronID,neuron},{_ToActuatorID,actuator}} ->
			link_FromNeuronToActuator(AgentID,From_ElementId,To_ElementId)
	end.
%The function link_FromElementToElement/3 first calculates what type of link is going to be established (neuron to neuron, sensor to neuron, or neuron to actuator), and then calls the specific linking function based on that.

link_FromNeuronToNeuron(AgentID,From_NeuronId,To_NeuronId)->
	A = genotype:read({agent,AgentID}),
	Generation = A#agent.generation,
%From Partf
	FromN = genotype:read({neuron,From_NeuronId}),
	U_FromN = link_FromNeuron(FromN,To_NeuronId,Generation),
	genotype:write(U_FromN),
%To Part
	ToN = genotype:read({neuron,To_NeuronId}),%We read it afterwards, in the case that it's the same Element. Thus we do not overwrite the earlier changes.
	FromOVL = 1,
	U_ToN = link_ToNeuron(From_NeuronId,FromOVL,ToN,Generation),
	genotype:write(U_ToN).
%link_FromNeuronToNeuron/3 establishes a link from neuron with id From_NeuronId, to a neuron with id To_NeuronId. The function then calls link_FromNeuron/4, which establishes the link on the From_NeuronId's side. The updated neuron associated with the From_NeuronId is then written to database. To decide how long the weight list that is going to be added to the To_NeuronId's weighted_inputs, the function calculates From_NeuronId's output vector length. Since the connection is from a neuron, FromOVL is set to 1. link_ToNeuron/4 is then called, and the link is established on the To_NeuronId's side. Finally, the updated neuron associated with the id To_NeuronId is written to database. The order of reading the FromN and ToN neuron records from the database is important. It is essential that ToN is read after the U_FromN is written to database, in the case that From_NeuronId and To_NeuronId refer to the same neuron (a recurrent connection from the neuron to itself). If both neurons are read at the same time for example before the links are established, then the link established in the U_FromN will be overwritten when the U_ToN is written to file. Thus order is important in this function.

	link_FromNeuron(FromN,ToId,Generation)->
		{{FromLI,_},_} = FromN#neuron.id,
		{{ToLI,_},_} = ToId,
		FromOutputIDs = FromN#neuron.output_ids,
		FromROIDs = FromN#neuron.ro_ids,
		case lists:member(ToId, FromOutputIDs) of
			true ->
				exit("******** ERROR:add_NeuronO[can not add OID to Neuron]: ~p already a member of ~p~n",[ToId,FromN#neuron.id]);
			false ->
				{U_FromOutputIDs,U_FromROIDs} = case FromLI >= ToLI of
					true ->
						{[ToId|FromOutputIDs],[ToId|FromROIDs]};
					false ->
						{[ToId|FromOutputIDs],FromROIDs}
				end,
				FromN#neuron{
					output_ids = U_FromOutputIDs,
					ro_ids = U_FromROIDs,
					generation = Generation
				}
		end.
%link_FromNeuron/4 updates the record of the neuron from whom the link is being created. FromN is the record of the neuron from whom the link/connection eminates, and ToId is the id of the element to whom the link is headed towards. The function extracts the layer index of the neuron FromN, and the layer index of the element with the id ToId. Then the two layer indecies are compared, and the ToId is either added only to the FromN's output_ids list, or if the connection is recursive, ToLayerIndex =< FromLayerIndex, to output_ids and ro_ids lists. The FromN's generation is updated to the value Generation, which is the current, most recent generation, since this neuron has just been modified. Finally, the updated neuron record is then returned to the caller. On the other hand, if ToId, the id of the element to which the connection is being established, is already a member of the FromN's output_ids list, then the function exits with error.

	link_ToNeuron(FromId,FromOVL,ToN,Generation)->%TODO: Only allows a single connection from a presynaptic element.
		ToSIWeightedInputs = ToN#neuron.weighted_inputs,
		ToMIWeightedInputs = ToN#neuron.weighted_inputs_modulation,
		{PFName,_NLParameters}=ToN#neuron.pf,
		case {lists:keymember(FromId,1,ToSIWeightedInputs),lists:keymember(FromId,1,ToMIWeightedInputs)} of
			{false,false} ->
				case {PFName == neuromodulation, rand:uniform(2)} of
					{true,2} ->
						U_ToMIWeightedInputs = [{FromId, genotype:createNeuralWeightsP(PFName,FromOVL,[])}|ToMIWeightedInputs],
						ToN#neuron{
							weighted_inputs = U_ToMIWeightedInputs,
							generation = Generation
						};
					_ ->
						U_ToSIWeightedInputs = [{FromId, genotype:createNeuralWeightsP(PFName,FromOVL,[])}|ToSIWeightedInputs],
						ToN#neuron{
							weighted_inputs = U_ToSIWeightedInputs,
							generation = Generation
						}
				end;
			_ ->
				exit("ERROR:add_NeuronI::[can not add IID]: ~p already connected to ~p~n",[FromId,ToN#neuron.id])
		end.
%link_ToNeuron/4 updates the record of ToN, so that its updated to receive a connection from the element FromId. The link eminates from element with the id FromId, whose output vector length is FromOVL, and the connection is made to the neuron ToN, the record whose is updated in this function. Randomly chosen, either the ToN's weighted_inputs_modulation or weighted_inputs list is updated with the tuple {FromId,[{W_1,WPs}...{W_FromOVL,WPs}]}, then the neuron's generation is updated to Generation (the current, most recent generation), and the updated ToN's record is returned to the caller. On the other hand, if the FromId is already part of the ToN's weighted_inputs or weighted_inputs_modulation list (dependent on which was randomly chosen), which means that the standard or modulatory link already exists between the neuron ToN and element FromId, the the function exits with an error.

link_FromSensorToNeuron(AgentID,From_SensorId,To_NeuronId)->
	A = genotype:read({agent,AgentID}),
	Generation = A#agent.generation,
%From Part
	FromS = genotype:read({sensor,From_SensorId}),
	U_FromS = link_FromSensor(FromS,To_NeuronId,Generation),
	genotype:write(U_FromS),
%To Part
	ToN = genotype:read({neuron,To_NeuronId}),
	FromOVL = FromS#sensor.vector_length,
	U_ToN = link_ToNeuron(From_SensorId,FromOVL,ToN,Generation),
	genotype:write(U_ToN).
%The function link_FromSensorToNeuron/3 establishes a connection from the sensor with id From_SensorId, and the neuron with id To_NeuronId. First the sensor record is updated with the connection details using the function link_FromSensor, and the updated sensor record is written to database. Then the record of the neuron to whom the link is being established is updated using the function link_ToNeuron/4, after which the updated neuron is written to database.

	link_FromSensor(FromS,ToId,Generation)->
		FromFanoutIDs = FromS#sensor.fanout_ids,
		case lists:member(ToId, FromFanoutIDs) of
			true ->
				exit("******** ERROR:link_FromSensor[can not add ToId to Sensor]: ~p already a member of ~p~n",[ToId,FromS#sensor.id]);
			false ->
				FromS#sensor{
					fanout_ids = [ToId|FromFanoutIDs],
					generation=Generation
				}
		end.
%The function link_FromSensor/2 updates the record of the sensor FromS, from whom the link eminates towards the element with id ToId. First the function ensures that there is no connection yet established between FromS and ToId, if a connection between these two elements already exists, then the function exits with error. If there is no connection between the two elements, then ToId is added to the sensor's fanout_ids list, and the updated record of the sensor is returned to the caller.
		
link_FromNeuronToActuator(AgentID,From_NeuronId,To_ActuatorId)->
	A = genotype:read({agent,AgentID}),
	Generation = A#agent.generation,
%From Part
	FromN = genotype:read({neuron,From_NeuronId}),
	U_FromN = link_FromNeuron(FromN,To_ActuatorId,Generation),
	genotype:write(U_FromN),
%To Part
	ToA = genotype:read({actuator,To_ActuatorId}),
	FaninIDs = ToA#actuator.fanin_ids,
	case length(FaninIDs) >= ToA#actuator.vector_length of
		true ->
			exit("******** ERROR:link_FromNeuronToActuator:: Actuator already fully connected");
		false ->
			U_FaninIDs = [From_NeuronId|FaninIDs],
			genotype:write(ToA#actuator{
				fanin_ids = U_FaninIDs,
				generation=Generation})
	end.
%The function Link_FromNeuronToActuator/4 establishes a link eminating from the neuron with an id From_NeuronId, to an actuator with the id To_ActuatorId. First the From_NeuronId's record is updated using the function link_FromNeuron/3, after which the updated neuron record is written to database. Then the function checks whether the actuator to which the neuron is establishing the link, still has space for that link (length(FaninIDs) is less than the actuator's vector length, vector_length). If there is no more room, then the function exits with error, if there is room, then the actuator's fanin_ids list is updated by appending to it the id of the neuron's id. The updated actuator is then written to the database.

cutlink_FromElementToElement(AgentID,From_ElementId,To_ElementId)->
	case {From_ElementId,To_ElementId} of
		{{_FromId,neuron},{_ToId,neuron}} ->
			cutlink_FromNeuronToNeuron(AgentID,From_ElementId,To_ElementId);
		{{_FromId,sensor},{_ToId,neuron}} ->
			cutlink_FromSensorToNeuron(AgentID,From_ElementId,To_ElementId);
		{{_FromId,neuron},{_ToId,actuator}} ->
			cutlink_FromNeuronToActuator(AgentID,From_ElementId,To_ElementId)
	end.
%cutlink_FromElementToElement/3 first checks which of the three types of connections is between the From_ElementId and To_ElementId (neuron to neuron, sensor to neuron, or neuron to actuator), and then disconnects the two elements using one of the three specialised cutlink_... functions.

cutlink_FromNeuronToNeuron(AgentID,From_NeuronId,To_NeuronId)->
	A = genotype:read({agent,AgentID}),
	Generation = A#agent.generation,
%From Part
	FromN = genotype:read({neuron,From_NeuronId}),
	U_FromN = cutlink_FromNeuron(FromN,To_NeuronId,Generation),
	genotype:write(U_FromN),
%To Part
	ToN = genotype:read({neuron,To_NeuronId}),
	U_ToN = cutlink_ToNeuron(From_NeuronId,ToN,Generation),
	genotype:write(U_ToN).
%The cutlink_FromNeuronToNeuron/3 function disconnections the connection from the From_NeuronId to the To_NeuronId. The function first disconnects the neuron associated with From_NeuronId by calling the cutlink_FromNeuron/3, and then writing to database the updated neuron. The function then disconnects the neuron associated with the To_NeuronId from the connection using the cutlink_ToNeuron/3, and writes to database the updated ToN record. If the From_NeuronId and the To_NeuronId are ids of the same neuron, then it is important to first write U_FromN to database, before reading the ToN neuron from the database, so as not to lose the update made by the cutlink_FromNeuron/3, before reading the updated neuron from the database and calling the cutlink_ToNeuron. Thus this order of reading and writing the neurons from the database is essential to cover the corner cases.

	cutlink_FromNeuron(FromN,ToId,Generation)->
		FromOutputIDs = FromN#neuron.output_ids,
		FromROIDs = FromN#neuron.ro_ids,
		case lists:member(ToId, FromOutputIDs) of
			true ->
				U_FromOutputIDs = FromOutputIDs--[ToId],
				U_FromROIDs = FromROIDs--[ToId],%Not necessary if not recursive...
				FromN#neuron{
					output_ids = U_FromOutputIDs,
					ro_ids = U_FromROIDs,
					generation = Generation};
			false ->
				exit("ERROR:: cutlink_FromNeuron [can not remove OID]: ~p not a member of ~p~n",[ToId,FromN#neuron.id])
		end.
%cutlink_FromNeuron/3 cuts the connection on the FromNeuron (FromN) side. The function first checks if the ToId is a member of the output_ids list, if its not then the function exits with an error. If the ToId is a member of the output_ids list, then the function removes the ToId from the FromOutputIDs and from the FromROIDs. Even if the ToId is a recursive connection, then removing it from ro_ids updates the FromROIDs list, if its not, then no change is made to the ro_ids list. Once the lists are updated, the updated neuron record of FromN is returned to the caller.

	cutlink_ToNeuron(FromId,ToN,Generation)->
		ToSIWeightedInputs = ToN#neuron.weighted_inputs,
		ToMIWeightedInputs = ToN#neuron.weighted_inputs_modulation,
		Guard1 = lists:keymember(FromId, 1, ToSIWeightedInputs),
		Guard2 = lists:keymember(FromId, 1, ToMIWeightedInputs),
		if 
			Guard1->
				U_ToSIWeightedInputs = lists:keydelete(FromId,1,ToSIWeightedInputs),
				ToN#neuron{
					weighted_inputs = U_ToSIWeightedInputs,
					generation = Generation};
			Guard2 ->
				U_ToMIWeightedInputs = lists:keydelete(FromId,1,ToMIWeightedInputs),
				ToN#neuron{
					weighted_inputs = U_ToMIWeightedInputs,
					generation = Generation};
			true ->
				exit("ERROR[can not remove IID]: ~p not a member of ~p~n",[FromId,ToN#neuron.id])
		end.
%cutlink_ToNeuron/3 cuts the connection on the ToNeuron (ToN) side. The function first checks if the FromId is a member of the ToN's weighted_inputs list, if its not, then the function checks if it is a member of the weighted_inputs_modulation list. If it is not a member of either, the function exits with error. If FromId is a member of one of these lists, then that tuple is removed from that list, and the updated ToN record is returned to the caller.

cutlink_FromSensorToNeuron(AgentID,From_SensorId,To_NeuronId)->
	A = genotype:read({agent,AgentID}),
	Generation = A#agent.generation,
%From Part
	FromS = genotype:read({sensor,From_SensorId}),
	U_FromS = cutlink_FromSensor(FromS,To_NeuronId,Generation),
	genotype:write(U_FromS),
%To Part
	ToN = genotype:read({neuron,To_NeuronId}),
	U_ToN = cutlink_ToNeuron(From_SensorId,ToN,Generation),
	genotype:write(U_ToN).
%The cutlink_FromSensorToNeuron/3 cuts the connection from the From_SensorId to To_NeuronId. The function first cuts the cunnection on the From_SensorId side using the cutlink_FromSensor/3 side, and writes the updated sensor to database. The function then cuts the connection on the To_NeuronId side using the cutlink_ToNeuron/3 function, and writes the updated neuron record to database.
	
	cutlink_FromSensor(FromS,ToId,Generation)->
		FromFanoutIDs = FromS#sensor.fanout_ids,
		case lists:member(ToId, FromFanoutIDs) of
			true ->
				U_FromFanoutIDs = FromFanoutIDs--[ToId],
				FromS#sensor{
					fanout_ids = U_FromFanoutIDs,
					generation=Generation};
			false ->
				exit("ERROR:: cutlink_FromSensor [can not remove ToId]: ~p not a member of ~p~n",[ToId,FromS#sensor.id])
		end.
%The cutlink_FromSensor/3 function first checks whether ToId is a member of the sensor's FromS fanout_ids list. If its not, then the function exits with an error. If ToId is a member of FromS's fanout_ids list, then it is removed from that list, and the updated sensor record of FromS is returned to the caller.

cutlink_FromNeuronToActuator(AgentID,From_NeuronId,To_ActuatorId)->
	A = genotype:read({agent,AgentID}),
	Generation = A#agent.generation,
%From Part
	FromN = genotype:read({neuron,From_NeuronId}),
	U_FromN = cutlink_FromNeuron(FromN,To_ActuatorId,Generation),
	genotype:write(U_FromN),
%To Part
	ToA = genotype:read({actuator,To_ActuatorId}),
	U_ToA = cutlink_ToActuator(From_NeuronId,ToA,Generation),
	genotype:write(U_ToA).
%cutlink_FromNeuronToActuator/3 cuts the connection from the From_NeuronId to To_ActuatorId. The function first cuts the connection on the From_NeuronId side using the cutlink_FromNeuron/3 function, and writes the updated U_FromN to database. Then the connection on the To_ActuatorId is cut using the cutlink_ToActuator/3 function, after which the updated actuator record is written to the database.

	cutlink_ToActuator(FromId,ToA,Generation)->
		ToFaninIDs = ToA#actuator.fanin_ids,
		case lists:member(FromId, ToFaninIDs) of
			true ->
				U_ToFaninIDs = ToFaninIDs--[FromId],
				ToA#actuator{
					fanin_ids = U_ToFaninIDs,
					generation=Generation};
			false ->
				exit("ERROR:: cutlink_ToActuator [can not remove FromId]: ~p not a member of ~p~n",[FromId,ToA])
		end.
%The cutlink_ToActuator/3 function cuts the connection on the ToActuator's side. The function first checks if the FromId is a member of the actuator ToA's fanin_ids list. If its not, the function exits with an error. If FromId is a member of the actuator's fanin_ids list, then the id is removed from the list, and the updated actuator record is returned to the caller.

add_outlink(AgentID)->
	A = genotype:read({agent,AgentID}),
	CortexID = A#agent.cortex_id,
	Cortex = genotype:read({cortex,CortexID}),
	NeuronIDs = Cortex#cortex.neuron_ids,
	NeuronID = lists:nth(rand:uniform(length(NeuronIDs)),NeuronIDs),
	Neuron = genotype:read({neuron,NeuronID}),
	OutputIDs = N#neuron.output_ids,
	Outlink_NeuronIDPool = filter_OutlinkIdPool(A#agent.constraint,NeuronID,NeuronIDs),
	case Outlink_NeuronIDPool -- OutputIDs of
		[] ->
			exit("********ERROR:add_outlink:: Neuron already connected to all ids");
		AvailableIDs ->
			ToID = lists:nth(rand:uniform(length(AvailableIDs)),AvailableIDs),
			link_FromElementToElement(AgentID,NeuronID,ToID),
			EvoHist = A#agent.evo_hist,
			U_EvoHist = [{add_outlink,NeuronID,ToID}|EvoHist],
			UpdatedActuator = A#agent{evo_hist=U_EvoHist},
			genotype:write(UpdatedActuator)
	end.
%The add_outlink/1 function reads the cortex record from the database based on the cortex id extracted from the agent record. The function then selects a random neuron from the neuron_ids stored in the cortex record. The function then subtracts the neuron's output_ids from the combined list of the actuator and neuron ids belonging to the neural network to get a list of ids belonging to the elements to which this neuron is not yet connected. If this list is empty, the function exits with error. If the list is not empty, it is given the name AvailableIDs, from which a random id is chosen, and the neuron is then connected to the element to whom the id belongs. Finally, the agent's evo_hist list is updated, and the updated agent record is written to the database.

	filter_OutlinkIdPool(C,NeuronID,NeuronIDs)->
		case C#constraint.connection_architecture of
			recurrent ->
				NeuronIDs;
			feedforward ->
				{{LI,_},neuron} = NeuronID,
				[{{Outlink_LI,Outlink_UniqueId},neuron} || {{Outlink_LI,Outlink_UniqueId},neuron} <- NeuronIDs, Outlink_LI > LI]
		end.
%The function filter_OutlinkIdPool/3 uses the connection_architecture specification in the constraint record of the agent to return a filtered neuron id pool. For the feedforward connection_architecture, the function ensures that only the neurons in the forward facing layers are allowed in the id pool.

add_inlink(AgentID)->
	A = genotype:read({agent,AgentID}),
	CortexID = A#agent.cortex_id,
	Cortex = genotype:read({cortex,CortexID}),
	NeuronIDs = Cortex#cortex.neuron_ids,
	SensorIDs = case A#agent.encoding_type of
		neural ->
			Cortex#cortex.sensor_ids;
		substrate ->
			SubstrateID=A#agent.substrate_id,
			Substrate=genotype:read({substrate,SubstrateID}),
			Substrate#substrate.cpp_ids
	end,
	NeuronID = lists:nth(rand:uniform(length(NeuronIDs)),NeuronIDs),
	Neuron = genotype:read({neuron,NeuronID}),
	{IIDs,_WeightPLists} = lists:unzip(N#neuron.weighted_inputs),
	Inlink_NeuronIDPool = filter_InlinkIdPool(A#agent.constraint,NeuronID,NeuronIDs),
	case lists:append(SensorIDs,Inlink_NeuronIDPool) -- IIDs of
		[] ->
			exit("********ERROR:add_INLink:: Neuron already connected from all ids");
		AvailableIDs ->
			FromID = lists:nth(rand:uniform(length(AvailableIDs)),AvailableIDs),
			link_FromElementToElement(AgentID,FromID,NeuronID),
			EvoHist = A#agent.evo_hist,
			U_EvoHist = [{add_inlink,FromID,NeuronID}|EvoHist],
			genotype:write(A#agent{evo_hist=U_EvoHist})
	end.
%The add_inlink/1 function extracts the list of neuron ids within the NN, and chooses a random id from this list. The input ids belonging to the neuron's inputIDps list are then subtracted from the combined neuron and sensor ids belonging to the NN. The result is a list of element ids from which the neuron is not yet connected. If this list is empty, the function exits with an error, otherwise the function chooses a random id from this list and establishes a connection between the neuron and the element correlated with randomly chosen id. Finally, the agent's evo_hist list is updated, and the updated agent is written to database.

	filter_InlinkIdPool(C,NeuronID,NeuronIDs)->
		case C#constraint.connection_architecture of
			recurrent ->
				NeuronIDs;
			feedforward ->
				{{LI,_},neuron} = NeuronID,
				[{{Inlink_LI,Inlink_UniqueId},neuron} || {{Inlink_LI,Inlink_UniqueId},neuron} <- NeuronIDs, Inlink_LI < LI]
		end.
%The function filter_InlinkIdPool/3 uses the connection_architecture specification in the constraint record of the agent to return a filtered neuron id pool. For the feedforward connection_architecture, the function ensures that  only the neurons in the previous layers are allowed in the filtered neuron id pool.

add_neuron(AgentID)->%Adds neuron and connects it to other neurons, not sensors or actuators.
	A = genotype:read({agent,AgentID}),
	Generation = A#agent.generation,
	Pattern = A#agent.pattern,
	CortexID = A#agent.cortex_id,
	Cortex = genotype:read({cortex,CortexID}),
	NeuronIDs = Cortex#cortex.neuron_ids,
	SensorIDs = case A#agent.encoding_type of
		neural ->
			Cortex#cortex.sensor_ids;
		substrate ->
			SubstrateID=A#agent.substrate_id,
			Substrate=genotype:read({substrate,SubstrateID}),
			Substrate#substrate.cpp_ids
	end,
	{TargetLayer,TargetNeuronIDs} = lists:nth(rand:uniform(length(Pattern)),Pattern),
	NewNeuronID = {{TargetLayer,genotype:generate_UniqueId()},neuron},
	U_NeuronIDs = [NewNeuronID|NeuronIDs],
	U_Pattern = lists:keyreplace(TargetLayer, 1, Pattern, {TargetLayer,[NewNeuronID|TargetNeuronIDs]}),
	SpecCon = A#agent.constraint,
	genotype:construct_Neuron(CortexID,Generation,SpecCon,NewNeuronID,[],[]),
	Inlink_NeuronIDPool = filter_InlinkIdPool(A#agent.constraint,NewNeuronID,NeuronIDs),
	Outlink_NeuronIDPool = filter_OutlinkIdPool(A#agent.constraint,NewNeuronID,NeuronIDs),
	FromElementId_Pool = Inlink_NeuronIDPool++SensorIDs,
	ToElementId_Pool = Outlink_NeuronIDPool,
	case (FromElementId_Pool == []) or (ToElementId_Pool == []) of
		true ->
			exit("********ERROR::add_neuron(AgentID)::Can't add new neuron here, Inlink_NeuronIDPool or Outlink_NeuronIDPool is empty.");
		false ->
			From_ElementId = lists:nth(rand:uniform(length(FromElementId_Pool)),FromElementId_Pool),
			To_ElementId = lists:nth(rand:uniform(length(ToElementId_Pool)),ToElementId_Pool),
			link_FromElementToElement(AgentID,From_ElementId,NewNeuronID),
			link_FromElementToElement(AgentID,NewNeuronID,To_ElementId),
			U_EvoHist = [{add_neuron,From_ElementId,NewNeuronID,To_ElementId}|A#agent.evo_hist],
			genotype:write(Cortex#cortex{neuron_ids = U_NeuronIDs}),
			genotype:write(A#agent{pattern=U_Pattern,evo_hist=U_EvoHist})
	end.
%The function add_neuron/1 creats a new neuron, and connects it to a randomly selected element in the NN, and form a randomly selected element in the NN. The function first reads the agent's pattern list, selects a random layer from the pattern, and then creates a new neuron id for that layer. Then, a new, unconnected neuron, is created with that neuron id. From the cortex's neuron_ids list, two random neuron ids are chosen: From_ElementId, and To_ElementId, (they can be the same ids). The function then establishes a connection from the neuron to To_ElemenId, and to the neuron from From_ElementId. Finally, the cortex's neuron_ids list is updated by appending to it the id of the newly created neuron, the agent's evo_hist is updated, and the updated cortex and agent records are written to database.

outsplice(AgentID)->
	A = genotype:read({agent,AgentID}),
	Generation = A#agent.generation,
	Pattern = A#agent.pattern,
	CortexID = A#agent.cortex_id,
	Cortex = genotype:read({cortex,CortexID}),
	NeuronIDs = Cortex#cortex.neuron_ids,
	NeuronID = lists:nth(rand:uniform(length(NeuronIDs)),NeuronIDs),
	Neuron = genotype:read({neuron,NeuronID}),
	{{LayerIndex,_UId},neuron} = NeuronID,
%Choose a random neuron in the output_ids for splicing.
	OID = lists:nth(rand:uniform(length(N#neuron.output_ids)),N#neuron.output_ids),
	{{OutputLayerIndex,_Output_UId},_OutputType} = OID,
%Create a new Layer, or select an existing one between NeuronID and the OID, and create the new unlinked neuron.
	NewLI = case OutputLayerIndex >= LayerIndex of
		true ->
			get_NewLI(LayerIndex,OutputLayerIndex,next,Pattern);
		false ->
			get_NewLI(LayerIndex,OutputLayerIndex,prev,Pattern)
	end,
	NewNeuronID={{NewLI,genotype:generate_UniqueId()},neuron},
	SpecCon = A#agent.constraint,
	genotype:construct_Neuron(CortexID,Generation,SpecCon,NewNeuronID,[],[]),
%Update pattern.
	U_Pattern=case lists:keymember(NewLI,1,Pattern) of
		true->
			{NewLI,InLayerIds}=lists:keyfind(NewLI, 1, Pattern),
			lists:keyreplace(NewLI, 1, Pattern, {NewLI,[NewNeuronID|InLayerIds]});
		false ->
			lists:sort([{NewLI,[NewNeuronID]}|Pattern])
	end,
%Disconnect the NeuronID from the OID, and reconnect through NewNeuronID
	cutlink_FromElementToElement(AgentID,NeuronID,OID),
	link_FromElementToElement(AgentID,NeuronID,NewNeuronID),
	link_FromElementToElement(AgentID,NewNeuronID,OID),
%Updated agent
	EvoHist = A#agent.evo_hist,
	U_EvoHist = [{outsplice,NeuronID,NewNeuronID,OID}|EvoHist],
	U_Cortex = Cortex#cortex{neuron_ids = [NewNeuronID|Cortex#cortex.neuron_ids]},
	genotype:write(U_Cortex),
	genotype:write(A#agent{pattern=U_Pattern,evo_hist=U_EvoHist}).
%The function outsplice/1 chooses a random neuron id from the cortex's neuron_ids list, disconnects it from a randomly chosen id in its output_ids list, and then reconnects it to the same element through a newly created neuron. The function first chooses a random neuron N with the neuron id NeuronID from the cortex's neuron_ids list. Then the neuron N's output_ids list is extracted, and a new id list OIDPool is created from the ids in the output_ids list that are located in the layer after the NeuronID's layer (the ids of elements to whom the NeuronID forms a feed forward connection). From that sublist of N's output_ids list, a random OID is chosen, and if the sublist is empty, then the function exits with an error. Then NeuronID is disconnected from the OID. The function then creates or extracts a new layer index, NewLI, located between NeuronID and OID. If there exists a layer between NeuronID and OID, NewLI is simply that layer, if on the other hand OID's layer comes imedietly after NeuronID's then a new layer is created between OID and NeuronID, whose layer index is in the middle of the two elements. A new unconnected neuron is then created in that layer, with a neuron id NewNeuronID, and connected to the OID, and from the NeuronID, thus establishing a path from NeuronID to OID through the NewNeuronID. The cortex's neuron_ids is updated with the NewNeuronID, and the agent's evo_hist list is updated with the new mutation operator tuple {outsplice,NeuronID,NewnID,OID}. Finally, the updated cortex and agent are written to database.

get_NewLI(LI,LI,_Direction,_Pattern)->
	LI;
get_NewLI(FromLI,ToLI,Direction,Pattern)->
	NewLI = case Direction of
		next ->
			get_NextLI(Pattern,FromLI,ToLI);
		prev ->
			get_PrevLI(lists:reverse(Pattern),FromLI,ToLI)
	end,
	NewLI.
%get_NewLI/4 calculates or creats a new layer index located between FromLI and ToLI. The function calls get_NextLI/3 or get_PrevLI/3, depending on whether the direction of the connection is forward, from sensors towards actuators (Direction = next) or from actuators towards sensors (Direction = prev), which is the case when executing an insplice/1 function, which calculates or creates a new layer between the NeuronID and one of the ids in its weighted_inputs list. If the FromLI == ToLI, the function exits with an error.

	get_NextLI([{FromLI,_LastLayerNeuronIDs}],FromLI,ToLI)->
		(FromLI+ToLI)/2;
	get_NextLI([{LI,_LayerNeuronIDs}|Pattern],FromLI,ToLI)->
		case LI == FromLI of
			true ->
				[{NextLI,_NextLayerNeuronIDs}|_] = Pattern,
				case NextLI == ToLI of
					true ->
						(FromLI + ToLI)/2;
					false ->
						NextLI
				end;
			false ->
				get_NextLI(Pattern,FromLI,ToLI)
		end.
%get_NextLI checks whether the ToLI comes directly after FromLI, or whether there is another layer between them. If there is another layer between them, then that layer is returned, and the splice neuron should then be put into that layer. If there is no layer between FromLI and ToLI, then a new layer is created in the middle, the new layer index has the value of (FromLI+ToLI)/2.

	get_PrevLI([{FromLI,_FirstLayerNeuronIDs}],FromLI,ToLI)->
		(FromLI+ToLI)/2;
	get_PrevLI([{LI,_LayerNeuronIDs}|Pattern],FromLI,ToLI)->
		case LI == FromLI of
			true ->
				[{PrevLI,_PrevLayerNeuronIDs}|_] = Pattern,
				case PrevLI == ToLI of
					true ->
						(FromLI + ToLI)/2;
					false ->
						PrevLI
				end;
			false ->
				get_PrevLI(Pattern,FromLI,ToLI)
		end.
%get_PrevLI checks whether the The ToLI comes directly before FromLI, or whetehr there is another layer in between them. If there is another layer, then the function returns that layer, if no such layer is found, the the function creates a new layer indeex, (FromLI+ToLI)/2.

add_sensorlink(AgentID)->
	A = genotype:read({agent,AgentID}),
	CortexID = A#agent.cortex_id,
	Cortex = genotype:read({cortex,CortexID}),
	NeuronIDs = Cortex#cortex.neuron_ids,
	SensorIDs = case A#agent.encoding_type of
		neural ->
			Cortex#cortex.sensor_ids;
		substrate ->
			SubstrateID=A#agent.substrate_id,
			Substrate=genotype:read({substrate,SubstrateID}),
			Substrate#substrate.cpp_ids
	end,
	SID = lists:nth(rand:uniform(length(SensorIDs)),SensorIDs),
	S = genotype:read({sensor,SID}),
	case NeuronIDs -- S#sensor.fanout_ids of
		[] ->
			exit("********ERROR:add_sensorlink:: Sensor already connected to all NeuronIDs");
		AvailableIDs ->
			NeuronID = lists:nth(rand:uniform(length(AvailableIDs)),AvailableIDs),
			link_FromElementToElement(AgentID,SID,NeuronID),
			EvoHist = A#agent.evo_hist,
			U_EvoHist = [{add_sensorlink,SID,NeuronID}|EvoHist],
			genotype:write(A#agent{evo_hist=U_EvoHist})
	end.
%The function add_sensorlink/1, randomly selects a SID from the cortex's sensor_ids list, and then establishes from that sensor a connection to a still unlinked to this sensor, randomly selected neuron from the cortex's neuron_ids list. The function first selects a random sensor id SID from the cortex's sensor_ids list. Then a list of NeuronIDs to which SID is not yet connected is calculated by subtracting from the NeuronIDs the SensorIDs fanout_ids list. If the resulting list is empty, then the function exits with an error since there is no other neurons to which the sensor can establish a new connection to. If the list is not empty, then a random neuron id, NeuronID, is selected from this list, and a connection is established from SID to NeuronID. Finally, the agent's evo_hist is then updated, and it is written to database.

add_actuatorlink(AgentID)->%TODO: There should be a preference towards non fully connected actuators.
	Agent = genotype:read({agent,AgentID}),
	CortexID = Agent#agent.cortex_id,
	Cortex = genotype:read({cortex,CortexID}),
	NeuronIDs = Cortex#cortex.neuron_ids,
	ActuatorIDs = case Agent#agent.encoding_type of
		neural ->
			Cortex#cortex.actuator_ids;
		substrate ->
			SubstrateID=Agent#agent.substrate_id,
			Substrate=genotype:read({substrate,SubstrateID}),
			Substrate#substrate.cep_ids
	end,
	ActuatorID = lists:nth(rand:uniform(length(ActuatorIDs)),ActuatorIDs),
	A = genotype:read({actuator,ActuatorID}),
	case NeuronIDs -- A#actuator.fanin_ids of
		[] ->
			exit("********ERROR:add_actuatorlink:: Actuator already connected from all NeuronIDs");
		AvailableIDs ->
			NeuronID = lists:nth(rand:uniform(length(AvailableIDs)),AvailableIDs),
			link_FromElementToElement(AgentID,NeuronID,ActuatorID),
			EvoHist = Agent#agent.evo_hist,
			U_EvoHist = [{add_actuatorlink,NeuronID,ActuatorID}|EvoHist],
			genotype:write(Agent#agent{evo_hist=U_EvoHist})
	end.
%The add_actuatorlink/1 selects a random actuator id ActuatorID from the cortex's actuator_ids list, and then connects ActuatorID to randomly selected neuron to which the ActuatorID is not yet connected to. The function first selects a random actuator id ActuatorID from the cortex's actuator_ids list. Then the function creates a list of neuron ids to which it is not yet connected by subtracting its fanin_ids list from the cortex's neuron_ids list. If the resulting id pool is empty, then the function exits with error. If the resulting id pool is not empty, a neuron id NeuronID is randomly chosen from this id list, and the actuator is connected to this randomly chosen neuron. Finally, the agent's evo_hist is updated, and the updated agent is written to database.

add_sensor(AgentID)->%TODO: There should be a preference towards adding sensors not yet used.
	Agent = genotype:read({agent,AgentID}),
	CortexID = Agent#agent.cortex_id,
	Cortex = genotype:read({cortex,CortexID}),
	SensorIDs = Cortex#cortex.sensor_ids,
	SpeCon = Agent#agent.constraint,
	Morphology = SpeCon#constraint.morphology,
	case morphology:get_Sensors(Morphology)--[(genotype:read({sensor,SID}))#sensor{id=undefined,cortex_id=undefined,fanout_ids=[],generation=undefined} || SID<-SensorIDs] of
		[] ->
			exit("********ERROR:add_sensor(AgentID):: NN system is already using all available sensors");
		Available_Sensors ->
			NewSID = {{-1,genotype:generate_UniqueId()},sensor},
			NewSensor=(lists:nth(rand:uniform(length(Available_Sensors)),Available_Sensors))#sensor{id=NewSID,cortex_id=CortexID},
			EvoHist = Agent#agent.evo_hist,
			case Agent#agent.encoding_type of
				neural->
					genotype:write(NewSensor),
					NeuronIDs = Cortex#cortex.neuron_ids,
					NeuronID = lists:nth(rand:uniform(length(NeuronIDs)),NeuronIDs),
					link_FromElementToElement(AgentID,NewSID,NeuronID),
					U_EvoHist = [{add_sensor,NewSID,NeuronID}|EvoHist];
				substrate ->
					SubstrateID = Agent#agent.substrate_id,
					genotype:write(NewSensor#sensor{fanout_ids=[SubstrateID]}),
					U_EvoHist = [{add_sensor,NewSID,SubstrateID}|EvoHist]
			end,
			U_Cortex = Cortex#cortex{sensor_ids=[NewSID|SensorIDs]},
			genotype:write(U_Cortex),
			genotype:write(Agent#agent{evo_hist=U_EvoHist})
	end.
%The add_sensor/1 function adds and connects a new sensor to the neural network, a sensor type to which the NN is not yet connected from. After retrieving the morphology name from the constraints record retrived from the agent, the complete set of available sensors is retrevied using the morphology:get_Sensors/1 function. From this complete sensor list we subtract the sensor tuples used by the NN based system, but first we revert those sensor's id and cortex_id back to undefined, since that is what the initial state of the sensor tuples are. With the NN's sensors ids and cortex_ids reverted back to undefined, they can be subtracted from the compelete set of sensors. If the resulting list is empty, then the function exits with an error. On the other hand if ther esulting list is not empty, then there are still sensors which the NN is not yet using (though it does not mean that using the sensors would make the NN better, these sensors might be simply useless, and hence not previously incorporated during evolution). From this resulting list we then select a random sensor, and create for it a unique sensor id NewSID. A random neuron id NeuronID is then selected from the cortex's neuron_ids list, and a connection is established from NewSID to the NeuronID. The cortex's sensor_ids is updated with the new sensor's id, and the agent's evo_hist is updated with the new tuple. The updated cortex and agent records are then written to database.

add_actuator(AgentID)->%TODO: There should be a preference towards adding actuators not yet used.
	Agent = genotype:read({agent,AgentID}),
	CortexID = Agent#agent.cortex_id,
	Cortex = genotype:read({cortex,CortexID}),
	ActuatorIDs = Cortex#cortex.actuator_ids,%TODO: Should we fill in all the fanin_ids locations, or just 1? and let evolution fill the rest?
	SpeCon = Agent#agent.constraint,
	Morphology = SpeCon#constraint.morphology,
	case morphology:get_Actuators(Morphology)--[(genotype:read({actuator,ActuatorID}))#actuator{cortex_id=undefined,id=undefined,fanin_ids=[],generation=undefined} || ActuatorID<-ActuatorIDs] of
		[] ->
			exit("********ERROR:add_actuator(AgentID):: NN system is already using all available actuators");
		Available_Actuators ->
			NewActuatorID = {{1,genotype:generate_UniqueId()},actuator},
			NewActuator=(lists:nth(rand:uniform(length(Available_Actuators)),Available_Actuators))#actuator{id=NewActuatorID,cortex_id=CortexID},
			EvoHist = Agent#agent.evo_hist,
			case Agent#agent.encoding_type of
				neural ->
					genotype:write(NewActuator),
					NeuronIDs = Cortex#cortex.neuron_ids,
					NeuronID = lists:nth(rand:uniform(length(NeuronIDs)),NeuronIDs),
					link_FromElementToElement(AgentID,NeuronID,NewActuatorID),
					U_EvoHist = [{add_actuator,NeuronID,NewActuatorID}|EvoHist];
				substrate ->
					SubstrateID = Agent#agent.substrate_id,
					genotype:write(NewActuator#actuator{fanin_ids=[SubstrateID]}),
					U_EvoHist = [{add_actuator,SubstrateID,NewActuatorID}|EvoHist]
			end,
			U_Cortex = Cortex#cortex{actuator_ids=[NewActuatorID|ActuatorIDs]},
			genotype:write(U_Cortex),
			genotype:write(Agent#agent{evo_hist=U_EvoHist})
	end.
%The add_actuator/1 function adds and connects a new actuator to the neural network, an actuator type to which the NN is noet yet connected to. After the morphology name from the constraints record, a complete actuator list available to the NN from which to draw its actuators from during evolution is created. From that list the actuator list that the NN is already connected to is subtracted, after the ids and cortex_ids of those actuators is set to undefined. The resulting list is the list of actuators to which the NN is not yet connected to. A random actuator is chosen from that list, and a random neuron id NeuronID from cortex's neuron_ids is chosen and connected to the new actuator. The cortex's actuator_ids list is then updated with the id of the newly created actuator, the agent's evo_hist is updated with the new tuple, and then both the updated cortex and the agent are written to database.

add_cpp(AgentID)->%TODO: There should be a preference towards adding substrate_cpps not yet used.
	Agent = genotype:read({agent,AgentID}),
	case Agent#agent.encoding_type of
		neural->
			exit("********ERROR:add_cpp(AgentID):: NN is neural encoded, can not apply mutation operator.");
		substrate->
			CortexID = Agent#agent.cortex_id,
			Cortex = genotype:read({cortex,CortexID}),
			SubstrateID = Agent#agent.substrate_id,
			Substrate=genotype:read({substrate,SubstrateID}),
			Dimensions = length(Substrate#substrate.densities),
			Plasticity = Substrate#substrate.plasticity,
			CPPIDs = Substrate#substrate.cpp_ids,
			case morphology:get_SubstrateCPPs(Dimensions,Plasticity)--[(genotype:read({sensor,CPPID}))#sensor{id=undefined,cortex_id=undefined,fanout_ids=[],generation=undefined} || CPPID<-CPPIDs] of
				[] ->
					exit("********ERROR:add_cpp(AgentID):: NN system is already using all available substrate_cpps");
				Available_CPPs ->
					NewCPPID = {{-1,genotype:generate_UniqueId()},sensor},
					NewCPP=(lists:nth(rand:uniform(length(Available_CPPs)),Available_CPPs))#sensor{id=NewCPPID,cortex_id=CortexID},
					EvoHist = Agent#agent.evo_hist,
					genotype:write(NewCPP),
					NeuronIDs = Cortex#cortex.neuron_ids,
					NeuronID = lists:nth(rand:uniform(length(NeuronIDs)),NeuronIDs),
					link_FromElementToElement(AgentID,NewCPPID,NeuronID),
					U_EvoHist = [{add_cpp,NewCPPID,NeuronID}|EvoHist],
					U_Substrate = Substrate#substrate{cpp_ids=[NewCPPID|CPPIDs]},
					genotype:write(U_Substrate),
					genotype:write(Agent#agent{evo_hist=U_EvoHist})
			end
	end.
	
add_cep(AgentID)->
	Agent = genotype:read({agent,AgentID}),
	case Agent#agent.encoding_type of
		neural->
			exit("********ERROR:add_cep(AgentID):: NN is neural encoded, can not apply mutation operator.");
		substrate->
			CortexID = Agent#agent.cortex_id,
			Cortex = genotype:read({cortex,CortexID}),
			SubstrateID = Agent#agent.substrate_id,
			Substrate=genotype:read({substrate,SubstrateID}),
			Dimensions = length(Substrate#substrate.densities),
			Plasticity = Substrate#substrate.plasticity,
			CEPIDs = Substrate#substrate.cep_ids,
			case morphology:get_SubstrateCEPs(Dimensions,Plasticity)--[(genotype:read({actuator,CEPID}))#actuator{id=undefined,cortex_id=undefined,fanin_ids=[],generation=undefined} || CEPID<-CEPIDs] of
				[] ->
					exit("********ERROR:add_cep(AgentID):: NN system is already using all available substrate_cpps");
				Available_CEPs ->
					NewCEPID = {{-1,genotype:generate_UniqueId()},actuator},
					NewCEP=(lists:nth(rand:uniform(length(Available_CEPs)),Available_CEPs))#actuator{id=NewCEPID,cortex_id=CortexID},
					EvoHist = Agent#agent.evo_hist,
					genotype:write(NewCEP),
					NeuronIDs = Cortex#cortex.neuron_ids,
					NeuronID = lists:nth(rand:uniform(length(NeuronIDs)),NeuronIDs),
					link_FromElementToElement(AgentID,NeuronID,NewCEPID),
					U_EvoHist = [{add_cep,NewCEPID,NeuronID}|EvoHist],
					U_Substrate = Substrate#substrate{cep_ids=[NewCEPID|CEPIDs]},
					genotype:write(U_Substrate),
					genotype:write(Agent#agent{evo_hist=U_EvoHist})
			end
	end.
