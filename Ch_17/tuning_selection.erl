%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(tuning_selection).
-compile(export_all).
-include("records.hrl").

dynamic(NeuronIDs,AgentGeneration,PerturbationRange,AnnealingParameter)->
	AgeLimit = math:sqrt(1/rand:uniform()),
	ChosenNWeightedInputs = case extract_CurGenNeuronIDPs(NeuronIDs,AgentGeneration,AgeLimit,PerturbationRange,AnnealingParameter,[]) of
		[] ->
			[NeuronID|_] = NeuronIDs,
			[{NeuronID,PerturbationRange*math:pi()}];
		ExtractedNWeightedInputs->
			ExtractedNWeightedInputs
	end,
	%io:format("ChosenNWeightedInputs:~p~n",[ChosenNWeightedInputs]),
	ChosenNWeightedInputs.
%The dynamic/4 selection function randomly selects an age limit for its neuron id pool. The age limit is chosen by executing math:sqrt(1/rand:uniform()), which creates a value between 1 and infinity. Using this function there is 75% that the number will be =<2, 25% that it will be >=2, 11% that it will be >=3... Everytime this selection function is executed, the AgeLimit is generated anew, thus different times it will produce different neuron id pools for tuning.

	extract_CurGenNeuronIDPs([NeuronID|NeuronIDs],Generation,AgeLimit,PR,AP,Acc)->
		Neuron = genotype:dirty_read({neuron,NeuronID}),
		NeuronGen = N#neuron.generation,
		case NeuronGen >= (Generation-AgeLimit) of
			true ->
				Age = Generation-NeuronGen,
				Spread = PR*math:pi()*math:pow(AP,Age),
				extract_CurGenNeuronIDPs(NeuronIDs,Generation,AgeLimit,PR,AP,[{NeuronID,Spread}|Acc]);
			false ->
				extract_CurGenNeuronIDPs(NeuronIDs,Generation,AgeLimit,PR,AP,Acc)
		end;
	extract_CurGenNeuronIDPs([],_Generation,_AgeLimit,_PR,_AP,Acc)->
		Acc.
%The extract_CurGenNeuronIDPs/6 composes a neuron id pool from neurons who are younger than the AgeLimit parameter. This is calculated by comparing the generation when they were created or touched by mutation, with that of the agent which ages with every topological mutation phase. Id pool accumulates not just the neurons but also the spread which will be used for the synaptic weight perturbation. The spread is calculated by multiplying the perturbation_range variable by math:pi(), and then multiplied by the annealing factor which is math:pow(AnnealingParameter,Age). Annealing parameter is less than 1, thus the greater the age of the neuron, the lower the Spread will be.

dynamic_random(NeuronIDs,AgentGeneration,PerturbationRange,AnnealingParameter) ->
			ChosenNWeightedInputs = case extract_CurGenNeuronIDPs(NeuronIDs,AgentGeneration,math:sqrt(1/rand:uniform()),PerturbationRange,AnnealingParameter,[]) of
				[] ->
					[NeuronID|_] = NeuronIDs,
					[{NeuronID,PerturbationRange*math:pi()}];
				ExtractedNWeightedInputs->
					ExtractedNWeightedInputs
			end,
			%io:format("ChosenNWeightedInputs:~p~n",[ChosenNWeightedInputs]),
			Total_Neurons = length(ChosenNWeightedInputs),
			MutationP = 1/math:sqrt(Total_Neurons),
			choose_randomNeuronIDPs(MutationP,ChosenNWeightedInputs).
%dyanimic_random/4 selection function composes the neuron id pool the same way as the dynamic/4 selection function, but after this id pool is generated, this selection function extracts ids from it randomly with a probability of 1/math:sqrt(Total_Neurons). Thus the probability of a neuron being selected from this pool is proportional to the number of ids in that pool. If through chance no ids are selected, then the first element in the id pool is automatically selected, and given the highest spread.

	choose_randomNeuronIDPs(MutationP,NWeightedInputs)->
		case choose_randomNeuronIDPs(NWeightedInputs,MutationP,[]) of
			[] ->
				{NeuronID,Spread} = lists:nth(rand:uniform(length(NWeightedInputs)),NWeightedInputs),
				[{NeuronID,Spread}];
			Acc ->
				Acc
		end.
	choose_randomNeuronIDPs([{NeuronID,Spread}|NWeightedInputs],MutationP,Acc)->
		U_Acc = case rand:uniform() < MutationP of
			true ->
				[{NeuronID,Spread}|Acc];
			false ->
				Acc
		end,
		choose_randomNeuronIDPs(NWeightedInputs,MutationP,U_Acc);
	choose_randomNeuronIDPs([],_MutationP,Acc)->
		Acc.
%choose_randomNeuronIDPs/2 and choose_randomNeuronIDPs/3 accepts a mutation probability parameter and a list of tuples composed of neuron ids and their spreads, and then selects from this list randomly with a probability MutationP, composing a new sub list.
		
active(NeuronIDs,AgentGeneration,PerturbationRange,AnnealingParameter)->
	extract_CurGenNeuronIDPs(NeuronIDs,AgentGeneration,3,PerturbationRange,AnnealingParameter,[]).
%active/4 selection algorithm composes a neuron id pool from all neurons who are younger than 3 generations.

active_random(NeuronIDs,AgentGeneration,PerturbationRange,AnnealingParameter)->
	ChosenNWeightedInputs = case extract_CurGenNeuronIDPs(NeuronIDs,AgentGeneration,3,PerturbationRange,AnnealingParameter,[]) of
		[] ->
			[NeuronID|_] = NeuronIDs,
			[{NeuronID,PerturbationRange*math:pi()}];
		ExtractedNWeightedInputs->
			ExtractedNWeightedInputs
	end,
	Total_Neurons = length(ChosenNWeightedInputs),
	MutationP = 1/math:sqrt(Total_Neurons),
	choose_randomNeuronIDPs(MutationP,ChosenNWeightedInputs).
%active_random/4 is a selection algorithm that composes an id pool by first creating a list of all neurons who are younger than 3 generations, and then composing a sub list from it by randomly choosing elements from this list with a probability of 1/math:sqrt(Total_Neurons).

current(NeuronIDs,AgentGeneration,PerturbationRange,AnnealingParameter)->
	case extract_CurGenNeuronIDPs(NeuronIDs,AgentGeneration,0,PerturbationRange,AnnealingParameter,[]) of
		[] ->
			[NeuronID|_] = NeuronIDs,
			[{NeuronID,PerturbationRange*math:pi()}];
		IdPs ->
			IdPs
	end.
%current/4 is a selection algorithm that returns a list of all neurons which have been added to the NN, or affected by mutation, during the last generation.

current_random(NeuronIDs,AgentGeneration,PerturbationRange,AnnealingParameter)->
	ChosenNWeightedInputs = case extract_CurGenNeuronIDPs(NeuronIDs,AgentGeneration,0,PerturbationRange,AnnealingParameter,[]) of
		[] ->
			[NeuronID|_] = NeuronIDs,
			[{NeuronID,PerturbationRange*math:pi()}];
		IdPs ->
			IdPs
	end,
	Total_Neurons = length(ChosenNWeightedInputs),
	MutationP = 1/math:sqrt(Total_Neurons),
	choose_randomNeuronIDPs(MutationP,ChosenNWeightedInputs).
%current_random/4 composes the list of tuples in the same way as current/4 does, but then composes a sublist by randomly selecting elements from that list with a probability of 1/math:sqrt(Total_Neurons), and returning that to the caller.

all(NeuronIDs,AgentGeneration,PerturbationRange,AnnealingParameter)->
	extract_CurGenNeuronIDPs(NeuronIDs,AgentGeneration,AgentGeneration,PerturbationRange,AnnealingParameter,[]).
%all/4 returns a list of tuples composed of all ids (and their spread values) belonging to the NN, to the caller.

all_random(NeuronIDs,AgentGeneration,PerturbationRange,AnnealingParameter)->
	ChosenNWeightedInputs = extract_CurGenNeuronIDPs(NeuronIDs,AgentGeneration,AgentGeneration,PerturbationRange,AnnealingParameter,[]),
	Total_Neurons = length(ChosenNWeightedInputs),
	MutationP = 1/math:sqrt(Total_Neurons),
	choose_randomNeuronIDPs(MutationP,ChosenNWeightedInputs).
%all_random/4 first composes a list of tuples from neuron_ids and their spreads, and then creates a sublist by choosing each element with a probability of 1/math:sqrt(Total_neurons).
