%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(tot_topological_mutations).

-compile(export_all).

-include("records.hrl").

%ncount_exponential/2 calculates TotalMutations by putting the size of the NN to some power Power.
ncount_exponential(Power, AgentID) ->
    A = genotype:read({agent, AgentID}),
    Cortex = genotype:read({cortex, A#agent.cortex_id}),
    TotalNeurons = length(Cortex#cortex.neuron_ids),
    TotalMutations = rand:uniform(round(math:pow(TotalNeurons,
						 Power))),
    io:format("Total neurons:~p Performing Total mutations:~p "
	      "on:~p~n",
	      [TotalNeurons, TotalMutations, AgentID]),
    TotalMutations.

%ncount_linear/2 calcualtes TotalMutations by multiplying the size of the NN by the value Multiplier.
ncount_linear(Multiplier, AgentID) ->
    A = genotype:read({agent, AgentID}),
    Cortex = genotype:read({cortex, A#agent.cortex_id}),
    TotalNeurons = length(Cortex#cortex.neuron_ids),
    TotalMutations = TotalNeurons * Multiplier,
    io:format("Total neurons:~p Performing Total mutations:~p "
	      "on:~p~n",
	      [TotalNeurons, TotalMutations, AgentID]),
    TotalMutations.
