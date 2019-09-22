%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(tuning_duration).
-compile(export_all).
-include("records.hrl").

const(Parameter,_NeuronIDs,_Generation)->
	ConstMaxAttempts = Parameter,
	ConstMaxAttempts.
%const/3 returns the preset const max_attempts value.

wsize_proportional(Parameter,NeuronIDs,Generation)->
	Power = Parameter,
	ActiveNeuronIDs = extract_RecGenNeuronIDs(NeuronIDs,Generation,3,[]),
	Total_ActiveNeuron_Weights = extract_NWeightCount(ActiveNeuronIDs,0),
	20 + functions:sat(round(math:pow(Total_ActiveNeuron_Weights,Power)),100,0).
%wsize_proportional/3 calculats the max_attempts value based on the individual agent's parameters, in this case the max_attempts is proportional to the agent's number of weights belonging to the neurons which were added or mutated within the last 3 generations.

	extract_RecGenNeuronIDs([NeuronID|NeuronIDs],Generation,AgeLimit,Acc)->
		Neuron = genotype:dirty_read({neuron,NeuronID}),
		NeuronGen = N#neuron.generation,
		case NeuronGen >= (Generation-AgeLimit) of
			true ->
				extract_RecGenNeuronIDs(NeuronIDs,Generation,AgeLimit,[NeuronID|Acc]);
			false ->
				extract_RecGenNeuronIDs(NeuronIDs,Generation,AgeLimit,Acc)
		end;
	extract_RecGenNeuronIDs([],_Generation,_AgeLimit,Acc)->
		Acc.
%extract_RecGenNeuronIDs/4 extracts the NeuronIDs of all neurons whose age is lower or equal to the AgeLimit.

	extract_NWeightCount([NeuronID|RecGenNeuronIDs],Acc)->
		Neuron = genotype:dirty_read({neuron,NeuronID}),
		WeightedInputs = N#neuron.weighted_inputs,
		TotalWeights = lists:sum([length(Weights) || {_IId,Weights} <- WeightedInputs]),
		extract_NWeightCount(RecGenNeuronIDs,TotalWeights+Acc);
	extract_NWeightCount([],Acc)->
		Acc.
%extract_NWeightCount/2 counts the number of weights in total belonging to the list of neuron ids that the function was called with.

nsize_proportional(Parameter,NeuronIDs,Generation)->
	Power = Parameter,
	Total_Neurons = length(extract_RecGenNeuronIDs(NeuronIDs,Generation,3,[])),
	20 + functions:sat(round(math:pow(Total_Neurons,Power)),100,0).
%nsize_proportional/3 calculates the max_attempts to be proportional to the number of neurons which were within the last 3 generations mutated or added to the NN.
