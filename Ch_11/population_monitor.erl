%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(population_monitor).
-include("records.hrl").
%% API
-export([start_link/1,start_link/0,start/1,start/0,stop/0,init/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,create_MutantAgentCopy/1,test/0, create_specie/3, continue/2, continue/3,init_population/1,extract_AgentIds/2,delete_population/1]).
-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Population Monitor Options & Parameters %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-define(SELECTION_ALGORITHM,competition).
%-define(EFF,0.1). %Efficiency., TODO: this should further be changed from absolute number of neurons, to diff in lowest or avg, and the highest number of neurons
-define(INIT_CONSTRAINTS,[#constraint{morphology=Morphology,connection_architecture=CA, population_evo_alg_f=steady_state} || Morphology<-[xor_mimic],CA<-[feedforward]]).
-define(SURVIVAL_PERCENTAGE,0.5).
-define(SPECIE_SIZE_LIMIT,10).
-define(INIT_SPECIE_SIZE,10).
-define(INIT_POPULATION_ID,test).
-define(OP_MODE,gt).
-define(INIT_POLIS,mathema).
-define(GENERATION_LIMIT,100).
-define(EVALUATIONS_LIMIT,100000).
-define(DIVERSITY_COUNT_STEP,500).
-define(GEN_UID,genotype:generate_UniqueId()).
-define(CHAMPION_COUNT_STEP,500).
-define(FITNESS_GOAL,1000).
-record(state,{
	op_mode,
	population_id,
	activeAgentWeightedInputs=[],
	agent_ids=[],
	tot_agents,
	agents_left,
	op_tag,
	agent_summaries=[],
	pop_gen=0,
	eval_acc=0,
	cycle_acc=0,
	time_acc=0,
	step_size,
	next_step,
	goal_status,
	evolutionary_algorithm,
	fitness_postprocessor,
	selection_algorithm,
	best_fitness
}).
%%==================================================================== API
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Start_Parameters) ->
	gen_server:start_link(?MODULE, Start_Parameters, []).

start(Start_Parameters) -> 
	gen_server:start(?MODULE, Start_Parameters, []).
	
start_link() ->
	gen_server:start_link(?MODULE, [], []).
    
start() -> 
	gen_server:start(?MODULE, [], []).

stop() ->
	gen_server:cast(monitor,{stop,normal}).
	
init(Pid,InitState)->
	gen_server:cast(Pid,{init,InitState}).

%%==================================================================== gen_server callbacks
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(Parameters) ->
	process_flag(trap_exit,true),
	register(monitor,self()),
	io:format("******** Population monitor started with parameters:~p~n",[Parameters]),
	State = case Parameters of
		{OpMode,PopulationID}->
			AgentIDs = extract_AgentIds(PopulationID,all),
			ActiveAgentWeightedInputs = summon_agents(OpMode,AgentIDs),
			P = genotype:dirty_read({population,PopulationID}),
			#state{op_mode=OpMode,
				population_id = PopulationID,
				activeAgentWeightedInputs = ActiveAgentWeightedInputs,
				tot_agents = length(AgentIDs),
				agents_left = length(AgentIDs),
				op_tag = continue,
				evolutionary_algorithm = P#population.evo_alg_f,
				fitness_postprocessor = P#population.fitness_postprocessor_f,
				selection_algorithm = P#population.selection_f,
				best_fitness = 0}
	end,
	{ok, State}.
%In init/1 the population_monitor proces registers itself with the node under the name monitor, and sets all the needed parameters within its #state record. The function first extracts all the AgentIDs that belong to the population using the extract_AgentIds/2 function. Each agent is then spawned/activated, converted from genotype to phenotype in the summon_agents/2 function. The summon_agents/2 function summons the agents and returns to the caller a list of tuples with the following format: [{AgentID,AgentProcess}...]. Once the state record's parameters have been set, the function drops into the main gen_server loop.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({stop,normal},_From, S)->
	ActiveAgentWeightedInputs = S#state.activeAgentWeightedInputs,
	[AgentProcess ! {self(),terminate} || {_DAgentID,AgentProcess}<-ActiveAgentWeightedInputs],
	{stop, normal, S};
handle_call({stop,shutdown},_From,State)->
	{stop, shutdown, State}.
%If the population_monitor process receives a {stop,normal} call, it checks if there are any still active agents. If there are any, it terminates them, and then itself terminates.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({AgentID,terminated,Fitness,AgentEvalAcc,AgentCycleAcc,AgentTimeAcc},S) when S#state.evolutionary_algorithm == generational ->
	PopulationID = S#state.population_id,
	OpTag = S#state.op_tag,
	AgentsLeft = S#state.agents_left,
	OpMode = S#state.op_mode,
	U_EvalAcc = S#state.eval_acc+AgentEvalAcc,
	U_CycleAcc = S#state.cycle_acc+AgentCycleAcc,
	U_TimeAcc = S#state.time_acc+AgentTimeAcc,
	case (AgentsLeft-1) =< 0 of
		true ->
			mutate_population(PopulationID,?SPECIE_SIZE_LIMIT,S#state.fitness_postprocessor,S#state.selection_algorithm),
			U_PopGen = S#state.pop_gen+1,
			io:format("Population Generation:~p Ended.~n~n~n",[U_PopGen]),
			case OpTag of
				continue ->
					SpecieIDs = (genotype:dirty_read({population,PopulationID}))#population.specie_ids,
					SpecFitList=[(genotype:dirty_read({specie,SpecieID}))#specie.fitness || SpecieID <- SpecieIDs],
					BestFitness=lists:nth(1,lists:reverse(lists:sort([MaxFitness || {_,_,MaxFitness,_} <- SpecFitList]))),
					case (U_PopGen >= ?GENERATION_LIMIT) or (S#state.eval_acc >= ?EVALUATIONS_LIMIT) or (BestFitness > ?FITNESS_GOAL) of
						true ->%ENDING_CONDITION_REACHED
							AgentIDs = extract_AgentIds(PopulationID,all),
							TotalAgents=length(AgentIDs),
							U_S=S#state{agent_ids=AgentIDs, tot_agents=TotalAgents, agents_left=TotalAgents, pop_gen=U_PopGen, eval_acc=U_EvalAcc, cycle_acc=U_CycleAcc, time_acc=U_TimeAcc},
							{stop,normal,U_S};
						false ->%IN_PROGRESS
							AgentIDs = extract_AgentIds(PopulationID,all),
							U_ActiveAgentWeightedInputs=summon_agents(OpMode,AgentIDs),
							TotalAgents=length(AgentIDs),
							U_S=S#state{activeAgentWeightedInputs=U_ActiveAgentWeightedInputs, tot_agents=TotalAgents, agents_left=TotalAgents, pop_gen=U_PopGen, eval_acc=U_EvalAcc, cycle_acc=U_CycleAcc, time_acc=U_TimeAcc},
							{noreply,U_S}
					end;
				done ->
					io:format("Shutting down Population Monitor~n"),
					U_S = S#state{agents_left = 0,pop_gen=U_PopGen,eval_acc=U_EvalAcc,cycle_acc=U_CycleAcc,time_acc=U_TimeAcc},
					{stop,normal,U_S};
				pause ->
					io:format("Population Monitor has paused.~n"),
					U_S = S#state{agents_left=0,pop_gen=U_PopGen,eval_acc=U_EvalAcc,cycle_acc=U_CycleAcc,time_acc=U_TimeAcc},
					{noreply,U_S}
			end;
		false ->
			io:format("Agents Left:~p~n ",[AgentsLeft-1]),
			ActiveAgentWeightedInputs = S#state.activeAgentWeightedInputs,
			U_ActiveAgentIDs = lists:keydelete(AgentID,1,ActiveAgentWeightedInputs),
			U_S = S#state{activeAgentWeightedInputs = U_ActiveAgentIDs,agents_left = AgentsLeft-1,eval_acc=U_EvalAcc,cycle_acc=U_CycleAcc,time_acc=U_TimeAcc},
			{noreply,U_S}
	end;
%This clause accepts the cast signals sent by the agents which terminate after finishing with their evaluations. The clause specialises in the "competition" selection algorithm, which is a generational selection algorithm. As a generation selection algorithm, it waits untill the entire population has finished being evaluated, and only then selects the fit from the unfit, and creates the updated population of the next generation. The OpTag can be set from the outsie to shutdown the population_monitor by setting it to done. Once an ending condition is reached, either through a generation limit, an evaluations limit, or fitness goal, the population_monitor exits normally. If the ending condition is not reached, the population_monitor spawns the new generation of agents and awaits again for all the agents in the population to complete their evaluations. If the OpTag is set to pause, it does not generate a new population, and instead goes into a waiting mode, and awaits to be restarted or terminated.

handle_cast({AgentID,terminated,Fitness,AgentEvalAcc,AgentCycleAcc,AgentTimeAcc},S) when S#state.evolutionary_algorithm == steady_state ->
	PopulationID = S#state.population_id,
	NewBestFitness = case Fitness >= S#state.best_fitness of
		true ->
			Fitness;
		false ->
			S#state.best_fitness
	end,
	U_EvalAcc = S#state.eval_acc+AgentEvalAcc,
	U_CycleAcc = S#state.cycle_acc+AgentCycleAcc,
	U_TimeAcc = S#state.time_acc+AgentTimeAcc,
	case (S#state.eval_acc >= ?EVALUATIONS_LIMIT) or (NewBestFitness > ?FITNESS_GOAL) of
		true ->
			case lists:keydelete(AgentID,1,S#state.activeAgentWeightedInputs) of
				[] ->
					U_S=S#state{activeAgentWeightedInputs=[], eval_acc=U_EvalAcc, cycle_acc=U_CycleAcc, time_acc=U_TimeAcc},
					{stop,normal,U_S};
				U_ActiveAgentWeightedInputs ->
					U_S=S#state{activeAgentWeightedInputs=U_ActiveAgentWeightedInputs, eval_acc=U_EvalAcc, cycle_acc=U_CycleAcc, time_acc=U_TimeAcc},
					{noreply,U_S}
			end;
		false ->
			io:format("Total Evaluations:~p~n",[S#state.eval_acc+AgentEvalAcc]),
			FitnessPostprocessorName = S#state.fitness_postprocessor,
			SelectionAlgorithmName = S#state.selection_algorithm,
			A = genotype:dirty_read({agent,AgentID}),
			Morphology= (A#agent.constraint)#constraint.morphology,
			io:format("AgentID:~p of morphology:~p with fitness:~p terminated.~n",[AgentID,Morphology,Fitness]),
			SpecieID = A#agent.specie_id,
			Specie = genotype:dirty_read({specie,SpecieID}),
			Old_DeadPool_AgentSummaries = Specie#specie.dead_pool,
			Old_AgentIDs = Specie#specie.agent_ids,
			[AgentSummary] = constructAgentSummaries([AgentID],[]),
			DeadPool_AgentSummaries = [AgentSummary|Old_DeadPool_AgentSummaries],
			ProperlySorted_AgentSummaries = fitness_postprocessor:FitnessPostprocessorName(DeadPool_AgentSummaries),
	
			Valid_AgentSummaries = case length(ProperlySorted_AgentSummaries) >= ?SPECIE_SIZE_LIMIT of
				true ->
					[{InvalidFitness,InvalidTotalN,InvalidAgentID}|Remaining_AgentSummaries] = lists:reverse(ProperlySorted_AgentSummaries),
					io:format("Informationtheoretic Death:~p::~p~n",[InvalidAgentID,{InvalidFitness,InvalidTotalN,InvalidAgentID}]),
					genotype:deleteAgent(InvalidAgentID,safe),
					lists:reverse(Remaining_AgentSummaries);
				false ->
					ProperlySorted_AgentSummaries
			end,
			io:format("Valid_AgentSummaries:~p~n",[Valid_AgentSummaries]),
			{WinnerFitness,WinnerProfile,WinnerAgentID} = selection_algorithm:SelectionAlgorithmName(Valid_AgentSummaries),
			ActiveAgentIDP = case rand:uniform() < 0.1 of
				true ->
					U_DeadPool_AgentSummaries = lists:delete({WinnerFitness,WinnerProfile,WinnerAgentID},Valid_AgentSummaries),
					WinnerAgentProcess = AgentProcess = exoself:start(WinnerAgentID,self()),
					{WinnerAgentID,WinnerAgentProcess};
				false ->
					U_DeadPool_AgentSummaries = Valid_AgentSummaries,
					CloneAgentID = create_MutantAgentCopy(WinnerAgentID,safe),
					CloneAgentProcess = exoself:start(CloneAgentID,self()),
					{CloneAgentID,CloneAgentProcess}
			end,
			Top_AgentSummaries = lists:sublist(U_DeadPool_AgentSummaries,round(?SPECIE_SIZE_LIMIT*?SURVIVAL_PERCENTAGE)),
			{_,_,TopAgentIDs} = lists:unzip3(lists:sublist(Top_AgentSummaries,3)),
			io:format("TopAgentIDs:~p~n",[TopAgentIDs]),
			USpecie=genotype:dirty_read({specie,SpecieID}),
			genotype:write(USpecie#specie{dead_pool = U_DeadPool_AgentSummaries,champion_ids = TopAgentIDs}),
			ActiveAgentWeightedInputs = S#state.activeAgentWeightedInputs,
			U_ActiveAgentWeightedInputs = [ActiveAgentIDP|lists:keydelete(AgentID,1,ActiveAgentWeightedInputs)],
			U_S=S#state{
				activeAgentWeightedInputs=U_ActiveAgentWeightedInputs,
				eval_acc=U_EvalAcc,
				cycle_acc=U_CycleAcc,
				time_acc=U_TimeAcc,
				best_fitness=NewBestFitness
			},
			{noreply,U_S}
	end;

handle_cast({op_tag,pause},S) when S#state.op_tag == continue ->
	U_S = S#state{op_tag = pause},
	{noreply,U_S};
%The population_monitor process accepts a pause command cast, which if it recieves, it then goes into pause mode after all the agents have completed with their evaluations. The process can only go into pause mode if it is currently in the continue mode (its op_tag is set to continue).

handle_cast({op_tag,continue},S) when S#state.op_tag == pause ->
	PopulationID = S#state.population_id,
	OpMode = S#state.op_mode,
	AgentIDs = extract_AgentIds(PopulationID,all),
	U_ActiveAgentWeightedInputs=summon_agents(OpMode,AgentIDs),
	TotalAgents=length(AgentIDs),
	U_S=S#state{activeAgentWeightedInputs=U_ActiveAgentWeightedInputs,tot_agents=TotalAgents,agents_left=TotalAgents,op_tag=continue},
	{noreply,U_S};
%The population_monitor process can accept a continue command if its current op_tag is set to pause. When it receives a continue command, it summons all the agents in the population, and continues with its neuroevolution synchronization duties.
	
handle_cast({init,InitState},_State)->
	{noreply,InitState};
handle_cast({stop,normal},State)->
	{stop, normal,State};
handle_cast({stop,shutdown},State)->
	{stop, shutdown, State}.
%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, S) ->
	case S of
		[] ->
			io:format("******** Population_Monitor shut down with Reason:~p, with State: []~n",[Reason]);
		_ ->
			PopulationID = S#state.population_id,
			OpTag = S#state.op_tag,
			OpMode = S#state.op_mode,
			io:format("******** Population_Monitor:~p shut down with Reason:~p OpTag:~p, while in OpMode:~p~n",[PopulationID,Reason,OpTag,OpMode]),
			io:format("******** Total Agents:~p Population Generation:~p Eval_Acc:~p Cycle_Acc:~p Time_Acc:~p~n",[S#state.tot_agents,S#state.pop_gen,S#state.eval_acc,S#state.cycle_acc,S#state.time_acc])
	end.
%When the population_monitor process terminates, it states so, notifies with what op_tag and op_mode it terminated, all the stats gathered, and then shuts down.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
extract_AgentIds(PopulationID,AgentType)->
	P = genotype:dirty_read({population,PopulationID}),
	SpecieIDs = P#population.specie_ids,
	%io:format("SpecieIDs:~p~n",[SpecieIDs]),
	case AgentType of
		champions ->
			extract_ChampionAgentIds(SpecieIDs,[]);
		all ->
			extract_AllAgentIds(SpecieIDs,[])
	end.
%The extract_AgentIds/2 function accepts the PopulationID and a parameter which specifies what type of agents (all agent ids, or just those of the champions) to extract from the population, after which it extracts those agents. Depending on the AgentType parameter, the function either calls extract_ChampionAgentIds/2 or extract_AllAgentIds/2, which return the list of agent ids to the caller.

	extract_ChampionAgentIds([SpecieID|SpecieIDs],Acc)->
		S = genotype:dirty_read({specie,SpecieID}),
		ChampionAgentIDs = S#specie.champion_ids,
		extract_ChampionAgentIds(SpecieIDs,lists:append(ChampionAgentIDs,Acc));
	extract_ChampionAgentIds([],Acc)->
		Acc.
%extract_ChampionAgentIds/2 accumulates the ids of champion agents from every specie in the SpecieIDs list, and then returns that list to the caller.
	
	extract_AllAgentIds([SpecieID|SpecieIDs],Acc)->
		extract_AllAgentIds(SpecieIDs,lists:append(extract_SpecieAgentIds(SpecieID),Acc));
	extract_AllAgentIds([],Acc)->
		Acc.
%extract_AllAgentIds/2 accumulates and returns to the caller an id list of all the agents belonging to the species in the SpecieIDs list.
		
extract_SpecieAgentIds(SpecieID)->
	S = genotype:dirty_read({specie,SpecieID}),
	S#specie.agent_ids.
%extract_SpecieAgentIds/1 returns a list of agent ids to the caller.

summon_agents(OpMode,AgentIDs)->
	summon_agents(OpMode,AgentIDs,[]).
summon_agents(OpMode,[AgentID|AgentIDs],Acc)->
%	io:format("AgentID:~p~n",[AgentID]),
	AgentProcess = exoself:start(AgentID,self()),
	summon_agents(OpMode,AgentIDs,[{AgentID,AgentProcess}|Acc]);
summon_agents(_OpMode,[],Acc)->
	Acc.
%The summon_agents/2 and summon_agents/3 spawns all the agents in the Agent_ids list, and returns to the caller a list of tuples as follows: [{AgentID,AgentProcess}...].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test()->
	init_population({?INIT_POPULATION_ID,?INIT_CONSTRAINTS,?OP_MODE}).
%The test/0 function starts the population monitor through init_population/1 with a set of default parameters specified by the macros of this module.

init_population({PopulationID,Specie_Constraints,OpMode})->
	random:seed(now()),
	F = fun()->
		case genotype:read({population,PopulationID}) of
			undefined ->
				create_Population(PopulationID,Specie_Constraints);
			_ ->
				delete_population(PopulationID),
				create_Population(PopulationID,Specie_Constraints)
		end
	end,
	Result = mnesia:transaction(F),
	case Result of
		{atomic,_} ->
			population_monitor:start({OpMode,PopulationID});
		Error ->
			io:format("******** ERROR in PopulationMonitor:~p~n",[Error])
	end.
%The function init_population/1 creates a new population with the id PopulationID, composed of length(Specie_Constraints) species, where each specie uses the particular specie constraint specified within the Specie_Constraints list. The function first checks if a population with the noted PopulationID already exists, if a population does exist, then the function first delets it, and then creates a fresh one. Since the ids are usually generated with the genotype:create_UniqueId/0, the only way an already existing PopulationID is dropped into the function as a parameter is if it is intended, usually when runing tests, with the PopulationID = test.

	create_Population(PopulationID,Specie_Constraints)->
		SpecieSize = ?INIT_SPECIE_SIZE,
		SpecieIDs = [create_specie(PopulationID,SpecCon,origin,SpecieSize) || SpecCon <- Specie_Constraints],
		[C|_]=Specie_Constraints,
		Population = #population{
			id = PopulationID,
			specie_ids = SpecieIDs,
			evo_alg_f = C#constraint.population_evo_alg_f,
			fitness_postprocessor_f = C#constraint.population_fitness_postprocessor_f,
			selection_f = C#constraint.population_selection_f
		},
		genotype:write(Population).

		create_specie(PopulationID,SpeCon,Fingerprint)->
			SpecieID = genotype:generate_UniqueId(),
			create_specie(PopulationID,SpecieID,0,[],SpeCon,Fingerprint).
		create_specie(PopulationID,SpeCon,Fingerprint,SpecieSize)->
			SpecieID = genotype:generate_UniqueId(),
			create_specie(PopulationID,SpecieID,SpecieSize,[],SpeCon,Fingerprint).
		create_specie(PopulationID,SpecieID,0,IdAcc,SpeCon,Fingerprint)->
			io:format("SpecieID:~p Morphology:~p~n",[SpecieID,SpeCon#constraint.morphology]),
			Specie = #specie{
				id = SpecieID,
				population_id = PopulationID,
				fingerprint = Fingerprint,
				constraint = SpeCon,
				agent_ids = IdAcc
			},
			genotype:write(Specie),
			SpecieID;
		create_specie(PopulationID,SpecieID,Agent_Index,IdAcc,SpeCon,Fingerprint)->
			AgentID = {genotype:generate_UniqueId(),agent},
			genotype:constructAgent(SpecieID,AgentID,SpeCon),
			create_specie(PopulationID,SpecieID,Agent_Index-1,[AgentID|IdAcc],SpeCon,Fingerprint).
%The create_Population/3 generates length(Specie_Constraints) number of specie, each composed of ?INIT_SPECIE_SIZE number of agents. The function uses the create_specie/4 to generate the species. The create_specie/3 and create_specie/4 functions are simplified versions which use default parameters to call the create_specie/6 function. The create_specie/6 function constructs the agents using the genotype:constructAgent/3 function, accumulating the AgentIDs in the IdAcc list. Once all the agents have been created, the function creates the specie record, fills in the required elements, writes the specie to database, and then finally returns the SpecieID to the caller.

continue(OpMode,Selection_Algorithm)->
	PopulationID = test,
	population_monitor:start({OpMode,PopulationID,Selection_Algorithm}).
continue(OpMode,Selection_Algorithm,PopulationID)->
	population_monitor:start({OpMode,PopulationID,Selection_Algorithm}).
%The function continue/2 and continue/3 are used to summon an already existing population with PopulationID, and continue with the experiment using the Selection_Algorithm.

mutate_population(PopulationID,KeepTotal,Fitness_Postprocessor,Selection_Algorithm)->
	NeuralEnergyCost = calculate_EnergyCost(PopulationID),
	F = fun()->
		P = genotype:read({population,PopulationID}),
		SpecieIDs = P#population.specie_ids,
		[mutate_Specie(SpecieID,KeepTotal,NeuralEnergyCost,Fitness_Postprocessor,Selection_Algorithm) || SpecieID <- SpecieIDs]
	end,
	{atomic,_} = mnesia:transaction(F).
%The function mutate_population/3 mutates the agents within every specie in its specie_ids list, maintianing each specie within the size of KeepTotal. The function first calculates the average cost of each neuron, and then calls each specie seperately with the Fitness_Postprocessor and Selection_Algorithm parameters, which are used to mutate the species.

	mutate_Specie(SpecieID,PopulationLimit,NeuralEnergyCost,Fitness_Postprocessor_Name,Selection_Algorithm_Name)->
		S = genotype:dirty_read({specie,SpecieID}),
		{AvgFitness,Std,MaxFitness,MinFitness} = calculate_SpecieFitness({specie,S}),
		AgentIDs = S#specie.agent_ids,
		Sorted_AgentSummaries = lists:reverse(lists:sort(constructAgentSummaries(AgentIDs,[]))),
		io:format("Using: Fitness Postprocessor:~p Selection Algorirthm:~p~n",[Fitness_Postprocessor_Name,Selection_Algorithm_Name]),
		ProperlySorted_AgentSummaries = fitness_postprocessor:Fitness_Postprocessor_Name(Sorted_AgentSummaries),
		{NewGenAgentIDs,TopAgentIDs} = selection_algorithm:Selection_Algorithm_Name(ProperlySorted_AgentSummaries,NeuralEnergyCost,PopulationLimit),
		{FList,_TNList,_AgentIds}=lists:unzip3(Sorted_AgentSummaries),
		[TopFitness|_] = FList,
		{Factor,Fitness}=S#specie.innovation_factor,
		U_InnovationFactor = case TopFitness > Fitness of
			true ->
				{0,TopFitness};
			false ->
				{S#specie.innovation_factor-1,Fitness}
		end,
		genotype:write(S#specie{
			agent_ids = NewGenAgentIDs,
			champion_ids = TopAgentIDs,
			fitness = {AvgFitness,Std,MaxFitness,MinFitness},
			innovation_factor = U_InnovationFactor}).
%The function mutate_Specie/5 calls the selection algorithm function to seperate the fit from the unfit organisms in the specie, and then mutates the fit organisms to produce offspring, maintaning the total specie size within PopulationLimit. The function first calls the fitness_postprocessor which sorts the agent summaries. Then, the resorted updated summaries are split into a valid (fit) and invalid (unfit) lists of agents by the selection algorithm. The invalid agents are deleted, and the valid agents are used to create offspring using the particular Selection_Algorithm_Name function. The agent ids belonging to the next generation (the valid agents and their offspring) are then produced by the selection function. Finally, the innovation factor (the last time the specie's top fitness improved) is updated, the ids of the 3 top agents within the species are noted, and the updated specie record is written to database.

	constructAgentSummaries([AgentID|AgentIDs],Acc)->
		A = genotype:dirty_read({agent,AgentID}),
		constructAgentSummaries(AgentIDs,[{A#agent.fitness,length((genotype:dirty_read({cortex,A#agent.cortex_id}))#cortex.neuron_ids),AgentID}|Acc]);
	constructAgentSummaries([],Acc)->
		Acc.
%The constructAgentSummaries/2 reads the agents in the AgentIDs list, and composes a list of tuples of the following format: [{AgentFitness,AgentTotalNeurons,AgentID}...]. This list of tuples is reffered to as AgentSummaries. Once the AgentSummaries list is composed, it is returned to the caller.

	create_MutantAgentCopy(AgentID)->
		AgentCloneID = genotype:cloneAgent(AgentID),
		io:format("AgentCloneID:~p~n",[AgentCloneID]),
		genome_mutator:mutate(AgentCloneID),
		AgentCloneID.
%The create_MutantAgentCopy/1 first creates a clone of the AgentID, and then uses the genome_mutator:mutate/1 function to mutate that clone, returning the id of the cloned agent to the caller.
				
	create_MutantAgentCopy(AgentID,safe)->%TODO
		A = genotype:dirty_read({agent,AgentID}),
		S = genotype:dirty_read({specie,A#agent.specie_id}),
		AgentCloneID = genotype:cloneAgent(AgentID),
		AgentIDs = S#specie.agent_ids,
		genotype:write(S#specie{agent_ids = [AgentCloneID|AgentIDs]}),
		io:format("AgentCloneID:~p~n",[AgentCloneID]),
		genome_mutator:mutate(AgentCloneID),
		AgentCloneID.
%The create_MutantAgentCopy/2 is similar to arity 1 function of the same name, but it also adds the id of the cloned mutant agent to the specie record to which the original belonged. The specie with its updated agent_ids is then written to database, and the id of the mutant clone is returned to the caller.

delete_population(PopulationID)->
	P = genotype:dirty_read({population,PopulationID}),
	SpecieIDs = P#population.specie_ids,
	[delete_specie(SpecieID) || SpecieID <- SpecieIDs],
	mnesia:delete({population,PopulationID}).
%The delete_population/1 function delets the entire population, by deleting the specie records belonging to the PopulationID, deleting the agent records belonging to those species, and then deleting the population record itself.
	
	delete_specie(SpecieID)->
		S = genotype:dirty_read({specie,SpecieID}),
		AgentIDs = S#specie.agent_ids,
		[genotype:deleteAgent(AgentID) || AgentID <- AgentIDs],
		mnesia:delete({specie,SpecieID}).
%The delete_specie/1 function delets the agents associated with the SpecieID, and then delets the specie record itself.
			
calculate_EnergyCost(PopulationID)->
	AgentIDs = extract_AgentIds(PopulationID,all),
	TotalEnergy = lists:sum([extract_AgentFitness(AgentID) || AgentID<-AgentIDs]),
	TotalNeurons = lists:sum([extract_AgentTotalNeurons(AgentID) || AgentID <- AgentIDs]),
	EnergyCost = TotalEnergy/TotalNeurons,
	EnergyCost.
%The calculate_EnergyCost/1 calculates the average cost of each neuron, based on the fitness of each agent in the population, and the total number of neurons in the population. The value is calcualted by first adding up all the fitness scores of the agents belonging to the population. Then adding up the total number of neurons composing each agent in the population. And then finally producing the EnergyCost value by dividing the TotalEnergy by TotalNeurons, returning the value to the caller.

	extract_AgentTotalNeurons(AgentID)->
		A = genotype:dirty_read({agent,AgentID}),
		Cortex = genotype:dirty_read({cortex,A#agent.cortex_id}),
		NeuronIDs = Cortex#cortex.neuron_ids,
		length(NeuronIDs).
	
	extract_AgentFitness(AgentID)->
		A = genotype:dirty_read({agent,AgentID}),
		A#agent.fitness.
%The function extract_AgentTotalNeurons simply extracts the neuron_ids list, and returns the length of that list, which is the total number of neurons belonging to the NN based system.

calculate_SpecieFitness({specie,S})->
	AgentIDs = S#specie.agent_ids,
	FitnessAcc = calculate_fitness(AgentIDs),
	Sorted_FitnessAcc=lists:sort(FitnessAcc),
	[MinFitness|_] = Sorted_FitnessAcc,
	[MaxFitness|_] = lists:reverse(Sorted_FitnessAcc),
	AvgFitness = functions:avg(FitnessAcc),
	Std = functions:std(FitnessAcc),
	{AvgFitness,Std,MaxFitness,MinFitness};
calculate_SpecieFitness(SpecieID)->
	S = genotype:dirty_read({specie,SpecieID}),
	calculate_SpecieFitness({specie,S}).
%The calculate_SpecieFitness/1 function calculates the general fitness statistic of the specie, the averate, max, min, and standard deviation of the specie's fitness. The function first composes a fitness list by accessing the fitness scores of each agent belonging to it, and then calculates the noted above statistics from that list, returning the tuple to the caller.
	
	calculate_fitness(AgentIDs)->
		calculate_fitness(AgentIDs,[]).
	calculate_fitness([AgentID|AgentIDs],FitnessAcc)->
		A = genotype:dirty_read({agent,AgentID}),
		case A#agent.fitness of
			undefined ->
				calculate_fitness(AgentIDs,FitnessAcc);
			Fitness ->
				calculate_fitness(AgentIDs,[Fitness|FitnessAcc])
		end;
	calculate_fitness([],FitnessAcc)->
		FitnessAcc.
%The calculate_fitness/1 composes a fitness list composed of the fitness values belonging to the agents in the AgentIDs list. If the agent does not yet have a fitness score, if for example it has just been created/mutated but not yet evaluated, it is skipped. The composed fitness list is returned to the caller.
