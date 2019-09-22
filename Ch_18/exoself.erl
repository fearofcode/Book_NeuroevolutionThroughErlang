%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(exoself).
-compile(export_all).
-include("records.hrl").
-record(state,{
	agent_id,
	morphology,
	generation,
	pm_pid,
	neuron_process_ids,
	cx_pid,
	specie_id,
	spids=[],
	npids=[],
	neuron_ids=[],
	apids=[],
	private_scape_pids=[],
	public_scape_pids=[],
	highest_fitness=-1,
	eval_acc=0,
	cycle_acc=0,
	time_acc=0,
	max_attempts=10,
	attempt=1,
	tuning_duration_f,
	tuning_selection_f,
	annealing_parameter,
	perturbation_range,
	substrate_pid,
	cpp_pids=[],
	cep_pids=[]
}).

start(Agent_Id)->
	case whereis(monitor) of
		undefined ->
			io:format("start(Agent_Id):: 'monitor' is not registered~n");
		ProcessID ->
			start(Agent_Id,ProcessID)
	end.
start(Agent_Id,PMProcess)->
	spawn(exoself,prep,[Agent_Id,PMProcess]).
%The start/2 function spawns a new Agent_Id exoself process, belonging to the population_monitor process with the pid PMProcess.

prep(Agent_Id,PMProcess)->io:format("Spawning:~p~n",[self()]),
	random:seed(now()),
	NeuronPIDs = ets:new(neuron_process_ids,[set,private]), 
	A = genotype:dirty_read({agent,Agent_Id}),
	HeredityType = A#agent.heredity_type,
	Cortex = genotype:dirty_read({cortex,A#agent.cortex_id}),
	SensorIDs = Cortex#cortex.sensor_ids,
	ActuatorIDs = Cortex#cortex.actuator_ids,
	NeuronIDs = Cortex#cortex.neuron_ids,
	{Private_ScapeProcessIDs,Public_ScapeProcessIDs} = spawn_Scapes(NeuronPIDs,SensorIDs,ActuatorIDs,Agent_Id),
	spawnCerebralUnits(NeuronPIDs,cortex,[Cortex#cortex.id]),
	spawnCerebralUnits(NeuronPIDs,sensor,SensorIDs),
	spawnCerebralUnits(NeuronPIDs,actuator,ActuatorIDs),
	spawnCerebralUnits(NeuronPIDs,neuron,NeuronIDs),
	case A#agent.encoding_type of
		substrate ->
			Substrate_Id=A#agent.substrate_id,
			Substrate = genotype:dirty_read({substrate,Substrate_Id}),
			CPP_Ids = Substrate#substrate.cpp_ids,
			CEP_Ids = Substrate#substrate.cep_ids,
			spawnCerebralUnits(NeuronPIDs,substrate_cpp,CPP_Ids),
			spawnCerebralUnits(NeuronPIDs,substrate_cep,CEP_Ids),
			spawnCerebralUnits(NeuronPIDs,substrate,[Substrate_Id]),
			
			SubstrateProcess=ets:lookup_element(NeuronPIDs,Substrate_Id,2),
			link_SubstrateCPPs(CPP_Ids,NeuronPIDs,SubstrateProcess),
			link_SubstrateCEPs(CEP_Ids,NeuronPIDs,SubstrateProcess),
			SDensities = Substrate#substrate.densities,
			SPlasticity = Substrate#substrate.plasticity,
			SLinkform = Substrate#substrate.linkform,
			Sensors=[genotype:dirty_read({sensor,SensorID})||SensorID <- SensorIDs],
			Actuators=[genotype:dirty_read({actuator,ActuatorID})||ActuatorID <- ActuatorIDs],		
			CPPProcess=[ets:lookup_element(NeuronPIDs,Id,2)||Id<-CPP_Ids],
			CEPProcess=[ets:lookup_element(NeuronPIDs,Id,2)||Id<-CEP_Ids],
			SubstrateProcess ! {self(),init,{Sensors,Actuators,[ets:lookup_element(NeuronPIDs,Id,2)||Id<-SensorIDs],[ets:lookup_element(NeuronPIDs,Id,2)||Id<-ActuatorIDs],CPPProcess,CEPProcess,SDensities,SPlasticity,SLinkform}};
		_ ->
			CPPProcess=[],
			CEPProcess=[],
			SubstrateProcess = undefined
	end,
	link_Sensors(SensorIDs,NeuronPIDs),
	link_Actuators(ActuatorIDs,NeuronPIDs),
	link_Neurons(NeuronIDs,NeuronPIDs,HeredityType),
	{SensorProcess,NeuronProcess,ActuatorProcess}=linkCortex(Cortex,NeuronPIDs),
	CortexProcess = ets:lookup_element(NeuronPIDs,Cortex#cortex.id,2),
	{TuningDurationFunction,Parameter} = A#agent.tuning_duration_f,
	Morphology = (A#agent.constraint)#constraint.morphology,
	S = #state{
		agent_id=Agent_Id,
		morphology=Morphology,
		generation=A#agent.generation,
		pm_pid=PMProcess,
		neuron_process_ids=NeuronPIDs,
		cx_pid=CortexProcess,
		specie_id=A#agent.specie_id,
		spids=SensorProcess,
		npids=NeuronProcess,
		neuron_ids=NeuronIDs,
		apids=ActuatorProcess,
		substrate_pid=SubstrateProcess,
		cpp_pids = CPPProcess,
		cep_pids = CEPProcess,
		private_scape_pids=Private_ScapeProcessIDs,
		public_scape_pids=Public_ScapeProcessIDs,
		max_attempts= tuning_duration:TuningDurationFunction(Parameter,NeuronIDs,A#agent.generation),
		tuning_selection_f=A#agent.tuning_selection_f,
		annealing_parameter=A#agent.annealing_parameter,
		tuning_duration_f=A#agent.tuning_duration_f,
		perturbation_range=A#agent.perturbation_range
	},
	loop(S).
%The prep/2 function prepares and sets up the exoself's state before dropping into the main loop. The function first reads the agent and cortex records belonging to the Agent_Id NN based system. The function then reads the sensor, actuator, and neuron ids, then spawns the private scapes using the spawn_Scapes/3 function, spawns the cortex, sensor, actuator, and neuron processes, and then finally links up all these processes together using the link_.../2 processes. Once the phenotype has been generated from the genotype, the exoself drops into its main loop.

loop(S)->
	receive
		{CortexProcess,evaluation_completed,Fitness,Cycles,Time,GoalReachedFlag}->
			%io:format("E Msg:~p~n E S:~p~n",[{CortexProcess,evaluation_completed,Fitness,Cycles,Time,GoalReachedFlag},S]),
			NeuronPIDs = S#state.neuron_process_ids,
			{U_HighestFitness,U_Attempt}=case Fitness > S#state.highest_fitness of
				true ->
					[NeuronProcess ! {self(),weight_backup} || NeuronProcess <- S#state.npids],
					{Fitness,0};
				false ->
					Perturbed_NeuronIDPs=get(perturbed),
					[ets:lookup_element(NeuronPIDs,NeuronID,2) ! {self(),weight_restore} || {NeuronID,_Spread} <- Perturbed_NeuronIDPs],
					{S#state.highest_fitness,S#state.attempt+1}
			end,
			[ProcessID ! {self(), reset_prep} || ProcessID <- S#state.npids],
			gather_acks(length(S#state.npids)),
			[ProcessID ! {self(), reset} || ProcessID <- S#state.npids],
			case S#state.substrate_pid of
				undefined ->
					ok;
				SubstrateProcess ->
					SubstrateProcess ! {self(),reset_substrate},
					receive
						{SubstrateProcess,ready}->
							ok
					end
			end,
			%io:format("HighestFitness:~p U_Attempt:~p~n",[U_HighestFitness,U_Attempt]),
			U_CycleAcc = S#state.cycle_acc+Cycles,
			U_TimeAcc = S#state.time_acc+Time,
			U_EvalAcc = S#state.eval_acc+1,
			gen_server:cast(S#state.pm_pid,{self(),evaluations,S#state.specie_id,1,Cycles,Time}),
			case (U_Attempt >= S#state.max_attempts) or (GoalReachedFlag == true) of
				true ->	%End training
					A=genotype:dirty_read({agent,S#state.agent_id}),
					genotype:write(A#agent{fitness=U_HighestFitness}),
					backup_genotype(S#state.neuron_process_ids,S#state.npids),
					terminate_phenotype(S#state.cx_pid,S#state.spids,S#state.npids,S#state.apids,S#state.private_scape_pids,S#state.cpp_pids,S#state.cep_pids,S#state.substrate_pid),
					io:format("Agent:~p terminating. Genotype has been backed up.~n Fitness:~p~n TotalEvaluations:~p~n TotalCycles:~p~n TimeAcc:~p~n",[self(),U_HighestFitness,U_EvalAcc,U_CycleAcc,U_TimeAcc]),
					case GoalReachedFlag of
						true ->
							gen_server:cast(S#state.pm_pid,{S#state.agent_id,goal_reached});
						_ ->
							ok
					end,
					gen_server:cast(S#state.pm_pid,{S#state.agent_id,terminated,U_HighestFitness});
				false -> %Continue training
					%io:format("exoself state:~p~n",[S]),
					reenter_PublicScape(S#state.public_scape_pids,[genotype:dirty_read({sensor,ets:lookup_element(NeuronPIDs,Id,2)})||Id<-S#state.spids],[genotype:dirty_read({actuator,ets:lookup_element(NeuronPIDs,Id,2)})||Id<-S#state.apids],S#state.specie_id,S#state.morphology,length(S#state.neuron_ids)),
					TuningSelectionFunction=S#state.tuning_selection_f,
					PerturbationRange = S#state.perturbation_range,
					AnnealingParameter = S#state.annealing_parameter,
					ChosenNeuronIDPs=tuning_selection:TuningSelectionFunction(S#state.neuron_ids,S#state.generation,PerturbationRange,AnnealingParameter),
					[ets:lookup_element(NeuronPIDs,NeuronID,2) ! {self(),weight_perturb,Spread} || {NeuronID,Spread} <- ChosenNeuronIDPs],
					%io:format("ChosenNeuronProcess:~p~n",[ChosenNeuronIDPs]),
					put(perturbed,ChosenNeuronIDPs),
					CortexProcess ! {self(),reactivate},
					U_S =S#state{
						cycle_acc=U_CycleAcc,
						time_acc=U_TimeAcc,
						eval_acc=U_EvalAcc,
						attempt=U_Attempt,
						highest_fitness=U_HighestFitness
					},
					exoself:loop(U_S)
			end
		%after 10000 ->
		%	io:format("exoself:~p stuck.~n",[S#state.agent_id])
	end.
%The exoself process' main loop awaits from its cortex proccess the evoluation_completed message. Once the message is received, based on the fitness achieved, exoself decides whether to continue tunning the weights or terminate the system. Exoself tries to improve the fitness by perturbing/tuning the weights of its neurons, after each tuning session, the Neural Network based system performs another evaluation by interacting with the scape until completion (the NN solves a problem, or dies within the scape or...). The order of events is important: When evaluation_completed message is received, the function first checks whether the newly achieved fitness is higher than the highest fitness achieved so far. If it is not, the function sends the neurons a message to restore their weights to previous state, during which it last acehived the highest fitness instead of their current state which yielded the current lower fitness score. If on the other hand the new fitness is higher than the previously highest achieved fitness, then the function tells the neurons to backup their current weights, as these weights represent the NN's best, most fit form yet. Exoself then tells all the neurons to prepare for a reset by sending each neuron the {self(),reset_prep} message. Since the NN can have recursive connections, and the manner in which initial recursive messages are sent, it is important for each neuron to flush their buffers to be reset into an initial fresh state, which is achieved after the neurons receive the reset_prep message. The function then sends the reset message to the neurons, which returns them into their main loop. Finally, the function checks whether exoself has already tried to improve the NN's fitness a maximum S#state.max_attempts number of times. If that is the case, the exoself process backs up the updated NN (the updated, tuned weights) to database using the backup_genotype/2 function, prints to screen that it is terminating, and sends to the population_monitor the acumulated statistics (highest fitness, evaluation count, cycle count...). On the other hand, if the exoself is not yet done tuning the neural weights, it has not yet reached its ending condition, it uses a tuning_selection_function to compose a list of tuples: [{NeuronID,Spread}...] of neuron ids and the perturbation spread values, where the spread is the range from which the perturbation is randomly chosen. The spread itself is based on the age of the slected neuron, using the annealing_factor value, which when set to 1 implies that there is no annealing, and when set to a value less than 1, decreases the Spread. Once this list of elements is composed, the exoself sends each of the neurons a message to perturb their synaptic weights using the Spread value. The exoself then reactivates the cortex, and drops back into its main loop.

	spawnCerebralUnits(NeuronPIDs,CerebralUnitType,[Id|Ids])-> 
		ProcessID = CerebralUnitType:gen(self(),node()),
		ets:insert(NeuronPIDs,{Id,ProcessID}), 
		ets:insert(NeuronPIDs,{ProcessID,Id}), 
		spawnCerebralUnits(NeuronPIDs,CerebralUnitType,Ids); 
	spawnCerebralUnits(NeuronPIDs,_CerebralUnitType,[])-> 
		ets:insert(NeuronPIDs,{bias,bias}).
%We spawn the process for each element based on its type: CerebralUnitType, and the gen function that belongs to the CerebralUnitType module. We then enter the {Id,ProcessID} tuple into our ETS table for later use.

	spawn_Scapes(NeuronPIDs,Sensor_Ids,ActuatorIDs,Agent_Id)->
		Sensor_Scapes = [(genotype:dirty_read({sensor,Id}))#sensor.scape || Id<-Sensor_Ids], 
		Actuator_Scapes = [(genotype:dirty_read({actuator,Id}))#actuator.scape || Id<-ActuatorIDs], 
		Unique_Scapes = Sensor_Scapes++(Actuator_Scapes--Sensor_Scapes), 
		Private_SN_Tuples=[{scape:gen(self(),node()),ScapeName} || {private,ScapeName}<-Unique_Scapes],
		[ets:insert(NeuronPIDs,{ScapeName,ProcessID}) || {ProcessID,ScapeName} <- Private_SN_Tuples], 
		[ets:insert(NeuronPIDs,{ProcessID,ScapeName}) || {ProcessID,ScapeName} <-Private_SN_Tuples],
		[ProcessID ! {self(),ScapeName} || {ProcessID,ScapeName} <- Private_SN_Tuples],
		PublicScapeProcessIDs=enter_PublicScape(NeuronPIDs,Sensor_Ids,ActuatorIDs,Agent_Id),
		PrivateScapeProcessIDs=[ProcessID || {ProcessID,_ScapeName} <-Private_SN_Tuples],
		{PrivateScapeProcessIDs,PublicScapeProcessIDs}.
%The spawn_Scapes/3 function first extracts all the scapes that the sensors and actuators interface with, it then creates a filtered scape list which only holds unique scape records, after which it further only selects those scapes that are private, and spawns them.

		enter_PublicScape(NeuronPIDs,Sensor_Ids,ActuatorIDs,Agent_Id)->
			A = genotype:dirty_read({agent,Agent_Id}),
			Sensors = [genotype:dirty_read({sensor,Id}) || Id<-Sensor_Ids],
			Actuators = [genotype:dirty_read({actuator,Id}) || Id<-ActuatorIDs],
			TotalNeurons = length((genotype:dirty_read({cortex,A#agent.cortex_id}))#cortex.neuron_ids),
			Morphology = (A#agent.constraint)#constraint.morphology,
			Sensor_Scapes = [Sensor#sensor.scape || Sensor<-Sensors], 
			Actuator_Scapes = [Actuator#actuator.scape || Actuator<-Actuators], 
			Unique_Scapes = Sensor_Scapes++(Actuator_Scapes--Sensor_Scapes), 
			Public_SN_Tuples=[{gen_server:call(polis,{get_scape,ScapeName}),ScapeName} || {public,ScapeName}<-Unique_Scapes],
			[ets:insert(NeuronPIDs,{ScapeName,ProcessID}) || {ProcessID,ScapeName} <- Public_SN_Tuples], 
			[ets:insert(NeuronPIDs,{ProcessID,ScapeName}) || {ProcessID,ScapeName} <-Public_SN_Tuples],
			[gen_server:call(ProcessID,{enter,Morphology,A#agent.specie_id,Sensors,Actuators,TotalNeurons,self()}) || {ProcessID,ScapeName} <- Public_SN_Tuples],
			[ProcessID || {ProcessID,_ScapeName} <-Public_SN_Tuples].
			
		reenter_PublicScape([PSProcess|PSProcess],Sensors,Actuators,Specie_Id,Morphology,TotalNeurons)->
			gen_server:call(PSProcess,{enter,Morphology,Specie_Id,Sensors,Actuators,TotalNeurons,self()}),
			reenter_PublicScape(PSProcess,Sensors,Actuators,Specie_Id,Morphology,TotalNeurons);
		reenter_PublicScape([],_Sensors,_Actuators,_Specie_Id,_Morphology,_TotalNeurons)->
			ok.
			
		leave_PublicScape([PSProcess|PSProcess])->
			gen_server:call(PSProcess,{leave,self()}),
			leave_PublicScape(PSProcess);
		leave_PublicScape([])->
			ok.

	link_Sensors([SensorID|Sensor_Ids],NeuronPIDs) ->
		S=genotype:dirty_read({sensor,SensorID}),
		SensorProcess = ets:lookup_element(NeuronPIDs,SensorID,2),
		CortexProcess = ets:lookup_element(NeuronPIDs,S#sensor.cortex_id,2),
		SensorName = S#sensor.name,
		Fanout_Ids = S#sensor.fanout_ids,
		FanoutProcess = [ets:lookup_element(NeuronPIDs,Id,2) || Id <- Fanout_Ids],
		Scape=case S#sensor.scape of
			{private,ScapeName}->
				ets:lookup_element(NeuronPIDs,ScapeName,2);
			{public,ScapeName}->
				ets:lookup_element(NeuronPIDs,ScapeName,2)
		end,
		SensorProcess ! {self(),{SensorID,CortexProcess,Scape,SensorName,S#sensor.vector_length,S#sensor.parameters,FanoutProcess}},
		link_Sensors(Sensor_Ids,NeuronPIDs);
	link_Sensors([],_NeuronPIDs)->
		ok.
%The link_Sensors/2 function sends to the already spawned and waiting sensors their states, composed of the ProcessID lists and other information which are needed by the sensors to link up and interface with other elements in the distributed phenotype.

	link_Actuators([ActuatorID|ActuatorIDs],NeuronPIDs) ->
		A=genotype:dirty_read({actuator,ActuatorID}),
		ActuatorProcess = ets:lookup_element(NeuronPIDs,ActuatorID,2),
		CortexProcess = ets:lookup_element(NeuronPIDs,A#actuator.cortex_id,2),
		ActuatorName = A#actuator.name,
		Fanin_Ids = A#actuator.fanin_ids,
		FaninProcess = [ets:lookup_element(NeuronPIDs,Id,2) || Id <- Fanin_Ids],
		Scape=case A#actuator.scape of
			{private,ScapeName}->
				ets:lookup_element(NeuronPIDs,ScapeName,2);
			{public,ScapeName}->
				ets:lookup_element(NeuronPIDs,ScapeName,2)
		end,
		ActuatorProcess ! {self(),{ActuatorID,CortexProcess,Scape,ActuatorName,A#actuator.vector_length,A#actuator.parameters,FaninProcess}},
		link_Actuators(ActuatorIDs,NeuronPIDs);
	link_Actuators([],_NeuronPIDs)->
		ok.
%The link_Actuators2 function sends to the already spawned and waiting actuators their states, composed of the ProcessID lists and other information which are needed by the actuators to link up and interface with other elements in the distributed phenotype.

	link_SubstrateCPPs([CPP_Id|CPP_Ids],NeuronPIDs,SubstrateProcess) ->
		CPP=genotype:dirty_read({sensor,CPP_Id}),
		CPPProcess = ets:lookup_element(NeuronPIDs,CPP_Id,2),
		CortexProcess = ets:lookup_element(NeuronPIDs,CPP#sensor.cortex_id,2),
		CPPName = CPP#sensor.name,
		Fanout_Ids = CPP#sensor.fanout_ids,
		FanoutProcess = [ets:lookup_element(NeuronPIDs,Id,2) || Id <- Fanout_Ids],
		CPPProcess ! {self(),{CPP_Id,CortexProcess,SubstrateProcess,CPPName,CPP#sensor.vector_length,CPP#sensor.parameters,FanoutProcess}},
		link_SubstrateCPPs(CPP_Ids,NeuronPIDs,SubstrateProcess);
	link_SubstrateCPPs([],_NeuronPIDs,_SubstrateProcess)->
		ok.
%The link_Sensors/2 function sends to the already spawned and waiting sensors their states, composed of the ProcessID lists and other information which are needed by the sensors to link up and interface with other elements in the distributed phenotype.

	link_SubstrateCEPs([CEP_Id|CEP_Ids],NeuronPIDs,SubstrateProcess) ->
		CEP=genotype:dirty_read({actuator,CEP_Id}),
		CEPProcess = ets:lookup_element(NeuronPIDs,CEP_Id,2),
		CortexProcess = ets:lookup_element(NeuronPIDs,CEP#actuator.cortex_id,2),
		CEPName = CEP#actuator.name,
		Fanin_Ids = CEP#actuator.fanin_ids,
		FaninProcess = [ets:lookup_element(NeuronPIDs,Id,2) || Id <- Fanin_Ids],
		CEPProcess ! {self(),{CEP_Id,CortexProcess,SubstrateProcess,CEPName,CEP#actuator.parameters,FaninProcess}},
		link_SubstrateCEPs(CEP_Ids,NeuronPIDs,SubstrateProcess);
	link_SubstrateCEPs([],_NeuronPIDs,_SubstrateProcess)->
		ok.
%The link_SubstrateCEPs/2 function sends to the already spawned and waiting substrate_ceps their states, composed of the ProcessID lists and other information which are needed by the substrate_ceps to link up and interface with other elements in the distributed phenotype.

	link_Neurons([NeuronID|Neuron_Ids],NeuronPIDs,HeredityType) ->
		N=genotype:dirty_read({neuron,NeuronID}),
		NeuronProcess = ets:lookup_element(NeuronPIDs,NeuronID,2),
		CortexProcess = ets:lookup_element(NeuronPIDs,N#neuron.cortex_id,2),
		ActivationFunctionName = N#neuron.af,
		PFName = N#neuron.pf,
		AggrFName = N#neuron.aggr_f,
		WeightedInputs = N#neuron.weighted_inputs,
		WeightedInputs_Modulation = N#neuron.weighted_inputs_modulation,
		OutputIDs = N#neuron.output_ids,
		RO_Ids = N#neuron.ro_ids,
		SIProcessPs = convertWeightedInputsToWeightedInputProcessIDs(NeuronPIDs,WeightedInputs,[]),
		MIProcessPs = convertWeightedInputsToWeightedInputProcessIDs(NeuronPIDs,WeightedInputs_Modulation,[]),
		OProcess = [ets:lookup_element(NeuronPIDs,Id,2) || Id <- OutputIDs],
		ROProcess = [ets:lookup_element(NeuronPIDs,Id,2) || Id <- RO_Ids],
		NeuronProcess ! {self(),{NeuronID,CortexProcess,ActivationFunctionName,PFName,AggrFName,HeredityType,SIProcessPs,MIProcessPs,OProcess,ROProcess}},
		link_Neurons(Neuron_Ids,NeuronPIDs,HeredityType);
	link_Neurons([],_NeuronPIDs,HeredityType)->
		ok.
%The link_Neurons/2 function sends to the already spawned and waiting neurons their states, composed of the ProcessID lists and other information needed by the neurons to link up and interface with other elements in the distributed phenotype.

		convertWeightedInputsToWeightedInputProcessIDs(NeuronPIDs,[{Id,WeightsP}|FaninWeightedInputs],Acc)->
			convertWeightedInputsToWeightedInputProcessIDs(NeuronPIDs,FaninWeightedInputs,[{ets:lookup_element(NeuronPIDs,Id,2),WeightsP}|Acc]);
		convertWeightedInputsToWeightedInputProcessIDs(_NeuronPIDs,[],Acc)->
			lists:reverse(Acc).
%The convertWeightedInputsToWeightedInputProcessIDs/3 converts the IdPs tuples into tuples that use ProcessIDs instead of Ids, such that the Neuron will know which weights are to be associated with which incoming vector signals. The last element is the bias, which is added to the list in a non tuple form. Afterwards, the list is reversed to take its proper order.

	linkCortex(Cortex,NeuronPIDs) ->
		CortexID = Cortex#cortex.id,
		CortexProcess = ets:lookup_element(NeuronPIDs,CortexID,2),
		SensorIDs = Cortex#cortex.sensor_ids,
		ActuatorIDs = Cortex#cortex.actuator_ids,
		NeuronIDs = Cortex#cortex.neuron_ids,
		SensorProcess = [ets:lookup_element(NeuronPIDs,SensorID,2) || SensorID <- SensorIDs],
		NeuronProcess = [ets:lookup_element(NeuronPIDs,NeuronID,2) || NeuronID <- NeuronIDs],
		ActuatorProcess = [ets:lookup_element(NeuronPIDs,ActuatorID,2) || ActuatorID <- ActuatorIDs],
		CortexProcess ! {self(),CortexID,SensorProcess,NeuronProcess,ActuatorProcess},
		{SensorProcess,NeuronProcess,ActuatorProcess}.
%The linkCortex/2 function sends to the already spawned and waiting cortex its state, composed of the ProcessID lists and other information which is needed by the cortex to link up and interface with other elements in the distributed phenotype.

backup_genotype(NeuronPIDs,NeuronProcess)->
	Neuron_IdsNWeights = get_backup(NeuronProcess,[]),
	updateGenotype(NeuronPIDs,Neuron_IdsNWeights),
	io:format("Finished updating genotype~n").

	get_backup([NeuronProcess|NeuronProcess],Acc)->
		NeuronProcess ! {self(),get_backup},
		receive
			{NeuronProcess,NeuronID,SWeightTuples,MWeightTuples}->
				get_backup(NeuronProcess,[{NeuronID,SWeightTuples,MWeightTuples}|Acc])
		end;
	get_backup([],Acc)->
		Acc.
%The backup_genotype/2 uses get_backup/2 to contact all the neurons in its NN and request for the neuron's Ids and their WeightedInputs. Once the updated WeightedInputs from all the neurons have been accumulated, they are passed through the updateGenotype/2 function to produce updated neurons, and write them to database.

	updateGenotype(NeuronPIDs,[{N_Id,SIProcessPs,MIProcessPs}|WeightPs])->
		Neuron = genotype:dirty_read({neuron,N_Id}),
		Updated_SIWeightedInputs = convertWeightedInputProcessesToWeightedInputs(NeuronPIDs,SIProcessPs,[]),
		Updated_MIWeightedInputs = convertWeightedInputProcessesToWeightedInputs(NeuronPIDs,MIProcessPs,[]),
		UpdatedNeuron = Neuron#neuron{weighted_inputs = Updated_SIWeightedInputs,weighted_inputs_modulation=Updated_MIWeightedInputs},
		genotype:write(UpdatedNeuron),
		%io:format("N:~p~n UpdatedNeuron:~p~n Genotype:~p~n UpdatedGenotype:~p~n",[N,UpdatedNeuron,Genotype,UpdatedGenotype]),
		updateGenotype(NeuronPIDs,WeightPs);
	updateGenotype(_NeuronPIDs,[])->
		ok.
%For every {N_Id,WeightedInputProcessIDs} tuple the updateGenotype/3 function extracts the neuron with the id: N_Id, updates the neuron's inputWeightedInputs, and writes the updated neuron to database.

		convertWeightedInputProcessesToWeightedInputs(NeuronPIDs,[{ProcessID,WeightsP}|WeightedInputProcess],Acc)->
			convertWeightedInputProcessesToWeightedInputs(NeuronPIDs,WeightedInputProcess,[{ets:lookup_element(NeuronPIDs,ProcessID,2),WeightsP}|Acc]);
		convertWeightedInputProcessesToWeightedInputs(_NeuronPIDs,[],Acc)->
			lists:reverse(Acc).
%The convertWeightedInputProcessesToWeightedInputs/3 performs the conversion from ProcessIDs to Ids of every {ProcessID,Weights} tuple in the WeightedInputProcess list. The updated WeightedInputs are then returned to the caller.
	
terminate_phenotype(CortexProcess,SensorProcess,NeuronProcess,ActuatorProcess,ScapeProcessIDs,CPPProcess,CEPProcess,SubstrateProcess)->
	%io:format("Terminating the phenotype:~nCortexProcess:~p~nSensorProcess:~p~nNeuronProcess:~p~nActuatorProcess:~p~nScapePids:~p~n",[CortexProcess,SensorProcess,NeuronProcess,ActuatorProcess,ScapeProcessIDs]),
	[ProcessID ! {self(),terminate} || ProcessID <- SensorProcess],
	[ProcessID ! {self(),terminate} || ProcessID <- ActuatorProcess],
	[ProcessID ! {self(),terminate} || ProcessID <- NeuronProcess],
	[ProcessID ! {self(),terminate} || ProcessID <- ScapeProcessIDs],
	case SubstrateProcess == undefined of
		true ->
			ok;
		false ->
			[ProcessID ! {self(),terminate} || ProcessID <- CPPProcess],
			[ProcessID ! {self(),terminate} || ProcessID <- CEPProcess],
			SubstrateProcess ! {self(),terminate}
	end,
	CortexProcess ! {self(),terminate}.
%The terminate_phenotype/5 function termiantes sensors, actuators, neurons, all private scapes, and the cortex which composes the NN based system.

gather_acks(0)->
	done;	
gather_acks(ProcessIDIndex)->
	receive
		{_From,ready}->
			gather_acks(ProcessIDIndex-1)
		after 100000 ->
			io:format("******** Not all acks received:~p~n",[ProcessIDIndex])
	end.
%gather_acks/1 ensures that the X number of {From,ready} messages are sent to it, before it returns with done. X is set by the caller of the function.
