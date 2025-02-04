%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(selection_algorithm).

-compile(export_all).

-include("records.hrl").

-define(SURVIVAL_PERCENTAGE, 5.0e-1).

competition(ProperlySorted_AgentSummaries,
	    NeuralEnergyCost, PopulationLimit) ->
    TotalSurvivors =
	round(length(ProperlySorted_AgentSummaries) *
		(?SURVIVAL_PERCENTAGE)),
    Valid_AgentSummaries =
	lists:sublist(ProperlySorted_AgentSummaries,
		      TotalSurvivors),
    Invalid_AgentSummaries = ProperlySorted_AgentSummaries
			       -- Valid_AgentSummaries,
    {_, _, Invalid_AgentIds} =
	lists:unzip3(Invalid_AgentSummaries),
    [genotype:deleteAgent(AgentID)
     || AgentID <- Invalid_AgentIds],
    io:format("Valid_AgentSummaries:~p~n",
	      [Valid_AgentSummaries]),
    io:format("Invalid_AgentSummaries:~p~n",
	      [Invalid_AgentSummaries]),
    TopAgentSummaries = lists:sublist(Valid_AgentSummaries,
				      3),
    {_TopFitnessList, _TopTotalNs, TopAgentIDs} =
	lists:unzip3(TopAgentSummaries),
    io:format("NeuralEnergyCost:~p~n", [NeuralEnergyCost]),
    {AlotmentsP, NextGenSize_Estimate} =
	calculate_alotments(Valid_AgentSummaries,
			    NeuralEnergyCost, [], 0),
    Normalizer = NextGenSize_Estimate / PopulationLimit,
    io:format("Population size normalizer:~p~n",
	      [Normalizer]),
    NewGenAgentIDs = gather_survivors(AlotmentsP,
				       Normalizer, []),
    {NewGenAgentIDs, TopAgentIDs}.

%The competition/3 is part of the selection algorithm dubbed "competition". The function first executes calculate_alotments/4 to calculate the number of offspring alloted for each agent in the Sorted_AgentSummaries list. The function then calculates the Normalizer value, which is used then used to proportionalize the alloted number of offspring to each agent, to ensure that the final specie size is within PopulationLimit. The function then drops into the gather_survivors/3 function which, using the normalized offspring allotment values, creates the actual mutant offspring.

calculate_alotments([{Fitness, TotalNeurons, AgentID}
		     | Sorted_AgentSummaries],
		    NeuralEnergyCost, Acc, NewPopAcc) ->
    NeuralAlotment = Fitness / NeuralEnergyCost,
    MutantAlotment = NeuralAlotment / TotalNeurons,
    U_NewPopAcc = NewPopAcc + MutantAlotment,
    calculate_alotments(Sorted_AgentSummaries,
			NeuralEnergyCost,
			[{MutantAlotment, Fitness, TotalNeurons, AgentID} | Acc],
			U_NewPopAcc);
calculate_alotments([], _NeuralEnergyCost, Acc,
		    NewPopAcc) ->
    %io:format("NewPopAcc:~p~n",[NewPopAcc]),
    {Acc, NewPopAcc}.

%The calculate_alotments/4 function accepts the AgentSummaries list and for each agent, using the NeuralEnergyCost, calcualtes how many offspring that agent can produce by using the agent's Fitness, TotalNEurons, and NeuralEnergyCost values. The function first calculates how many neurons the agent is alloted, based on the agent's fitness and the cost of each neuron (which itself was calculated based on the average performance of the population). From the number of neurons alloted to the agent, the function then calculates how many offspring the agent should be alloted, by deviding the agent's NN size by the number of neurons it is alloted. The function also keeps track of how many offspring will be created from all these agents in general, by adding up all the offspring alotements. The calcualte_alotments/4 function does this for each tuple in the AgentSummaries, and then returns the calculated alotment list and NewPopAcc to the caller.

gather_survivors([{MutantAlotment, Fitness, TotalNeurons,
		   AgentID}
		  | AlotmentsP],
		 Normalizer, Acc) ->
    Normalized_MutantAlotment = round(MutantAlotment /
					Normalizer),
    io:format("AgentID:~p Normalized_MutantAlotment:~p~n",
	      [AgentID, Normalized_MutantAlotment]),
    SurvivingAgentIDs = case Normalized_MutantAlotment >= 1
			     of
			   true ->
			       MutantAgentIDs = case Normalized_MutantAlotment
							>= 2
						     of
						   true ->
						       [population_monitor:create_MutantAgentCopy(AgentID)
							|| _
							       <- lists:seq(1,
									    Normalized_MutantAlotment
									      -
									      1)];
						   false -> []
						 end,
			       [AgentID | MutantAgentIDs];
			   false ->
			       io:format("Deleting agent:~p~n", [AgentID]),
			       genotype:deleteAgent(AgentID),
			       []
			 end,
    gather_survivors(AlotmentsP, Normalizer,
		     SurvivingAgentIDs ++ Acc);
gather_survivors([], _Normalizer, Acc) ->
    io:format("New Population:~p PopSize:~p~n",
	      [Acc, length(Acc)]),
    Acc.

%The gather_survivors/3 function accepts the list composed of the alotment tuples and a population normalizer value calculated by the competition/3 function, and from those values calculates the actual number of offspring that each agent should produce, creating those mutant offspring and accumulating the new generation agent ids. FOr each AgentID the function first calculates the noramlized offspring alotment value, to ensure that the final nubmer of agents in the specie is within the popualtion limit of that specie. If the offspring alotment value is less than 0, the agent is killed. If the offspring alotment is 1, the parent agent is allowed to survive to the next generation, but is not allowed to create any new offspring. If the offspring alotment is greater than one, then the Normalized_MutantAlotment-1 offspring are created from this fit agent, by calling upon the create_MutantAgentCopy/1 function, which rerns the id of the new mutant offspring. Once all the offspring have been created, the function returns to the caller a list of ids, composed of the surviving parent agent ids, and their offspring.

top3(ProperlySorted_AgentSummaries, NeuralEnergyCost,
     PopulationLimit) ->
    TotalSurvivors = 3,
    Valid_AgentSummaries =
	lists:sublist(ProperlySorted_AgentSummaries,
		      TotalSurvivors),
    Invalid_AgentSummaries = ProperlySorted_AgentSummaries
			       -- Valid_AgentSummaries,
    {_, _, Invalid_AgentIds} =
	lists:unzip3(Invalid_AgentSummaries),
    {_, _, Valid_AgentIds} =
	lists:unzip3(Valid_AgentSummaries),
    [genotype:deleteAgent(AgentID)
     || AgentID <- Invalid_AgentIds],
    io:format("Valid_AgentSummaries:~p~n",
	      [Valid_AgentSummaries]),
    io:format("Invalid_AgentSummaries:~p~n",
	      [Invalid_AgentSummaries]),
    TopAgentSummaries = lists:sublist(Valid_AgentSummaries,
				      3),
    {_TopFitnessList, _TopTotalNs, TopAgentIDs} =
	lists:unzip3(TopAgentSummaries),
    io:format("NeuralEnergyCost:~p~n", [NeuralEnergyCost]),
    NewGenAgentIDs = breed(Valid_AgentIds,
			    PopulationLimit - TotalSurvivors, []),
    {NewGenAgentIDs, TopAgentIDs}.

breed(_Valid_AgentIds, 0, Acc) -> Acc;
breed(Valid_AgentIds, OffspringIndex,
      Acc) ->%TODO
    Parent_AgentId =
	lists:nth(rand:uniform(length(Valid_AgentIds)),
		  Valid_AgentIds),
    MutantAgentID =
	population_monitor:create_MutantAgentCopy(Parent_AgentId),
    breed(Valid_AgentIds, OffspringIndex - 1,
	  [MutantAgentID | Acc]).

%The breed/3 function is part of a very simple selection algorithm, which just selects the top 3 most fit agents, and then uses the create_MutantAgentCopy/1 function to create their offspring.

competition(ProperlySorted_AgentSummaries) ->
    TotalEnergy = lists:sum([Fitness
			   || {Fitness, _TotalN, _AgentID}
				  <- ProperlySorted_AgentSummaries]),
    TotalNeurons = lists:sum([TotalN
			    || {_Fitness, TotalN, _AgentID}
				   <- ProperlySorted_AgentSummaries]),
    NeuralEnergyCost = TotalEnergy / TotalNeurons,
    {AlotmentsP, Normalizer} =
	calculate_alotments(ProperlySorted_AgentSummaries,
			    NeuralEnergyCost, [], 0),
    Choice = rand:uniform(),
    {WinnerFitness, WinnerTotalN, WinnerAgentID} =
	choose_CompetitionWinner(AlotmentsP, Normalizer, Choice,
				 0),
    {WinnerFitness, WinnerTotalN, WinnerAgentID}.

choose_CompetitionWinner([{MutantAlotment, Fitness,
			   TotalN, AgentID}
			  | AlotmentsP],
			 Normalizer, Choice, Range_From) ->
    Range_To = Range_From + MutantAlotment / Normalizer,
    case (Choice >= Range_From) and (Choice =< Range_To) of
      true -> {Fitness, TotalN, AgentID};
      false ->
	  choose_CompetitionWinner(AlotmentsP, Normalizer, Choice,
				   Range_To)
    end;
choose_CompetitionWinner([], _Normalizer, _Choice,
			 _Range_From) ->
    exit("********ERROR:choose_CompetitionWinner:: "
	 "reached [] without selecting a winner.").
