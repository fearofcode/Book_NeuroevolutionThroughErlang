%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(neuron).
-compile(export_all).
-include("records.hrl").
-define(DELTA_MULTIPLIER,math:pi()*2).
-define(SAT_LIMIT,math:pi()*2).
-define(RO_SIGNAL,0).
-record(state,{
	id,
	cx_pid,
	af,
	pf,
	aggrf,
	heredity_type,
	si_pids=[],
	si_pidps_bl = [],
	si_pidps_current=[],
	si_pidps_backup=[],
	mi_pids=[],
	mi_pidps_current=[],
	mi_pidps_backup=[],
	output_pids=[],
	ro_pids=[]
}).
gen(ExoSelfProcess,Node)->
	spawn(Node,?MODULE,prep,[ExoSelfProcess]).

prep(ExoSelfProcess) ->
	random:seed(now()),
	receive 
		{ExoSelfProcess,{Id,CortexProcess,ActivationFunction,PF,AggrF,HeredityType,SIProcessPs,MIProcessPs,OutputProcess,ROProcess}} ->
			fanout(ROProcess,{self(),forward,[?RO_SIGNAL]}),
			SIProcess = lists:append([IProcessID || {IProcessID,_W} <- SIProcessPs, IProcessID =/= bias],[ok]),
			MIProcess = lists:append([IProcessID || {IProcessID,_W} <- MIProcessPs, IProcessID =/= bias],[ok]),
			S=#state{
				id=Id,
				cx_pid=CortexProcess,
				af=ActivationFunction,
				pf=PF,
				aggrf=AggrF,
				heredity_type = HeredityType,
				si_pids=SIProcess,
				si_pidps_bl = SIProcessPs,
				si_pidps_current=SIProcessPs,
				si_pidps_backup=SIProcessPs,
				mi_pids=MIProcess,
				mi_pidps_current=MIProcessPs,
				mi_pidps_backup=MIProcessPs,
				output_pids=OutputProcess,
				ro_pids=ROProcess
			},
			loop(S,ExoSelfProcess,SIProcess,MIProcess,[],[])
	end.
%When gen/2 is executed, it spawns the neuron element and immediately begins to wait for its initial state message from the exoself. Once the state message arrives, the neuron sends out the default forward signals to any elements in its ro_ids list, if any. Afterwards, prep drops into the neuron's main loop.

loop(S,ExoSelfProcess,[ok],[ok],SIAcc,MIAcc)->
	PF = S#state.pf,
	ActivationFunction = S#state.af,
	AggrF = S#state.aggrf,
	{PFName,PFParameters} = PF,
	Ordered_SIAcc = lists:reverse(SIAcc),
	SIProcessPs = S#state.si_pidps_current,
	SAggregation_Product = sat(signal_aggregator:AggrF(Ordered_SIAcc,SIProcessPs),?SAT_LIMIT),
	SOutput = functions:ActivationFunction(SAggregation_Product),
	
	OutputProcess = S#state.output_pids,
	[OutputProcess ! {self(),forward,[SOutput]} || OutputProcess <- OutputProcess],
	
	case PFName of
		none ->
			U_S=S;
		_ ->
			Ordered_MIAcc = lists:reverse(MIAcc),
			MIProcessPs = S#state.mi_pidps_current,
			MAggregation_Product = sat(signal_aggregator:dot_product(Ordered_MIAcc,MIProcessPs),?SAT_LIMIT),
			MOutput = functions:tanh(MAggregation_Product),
			U_SIProcessPs = plasticity:PFName([MOutput|PFParameters],Ordered_SIAcc,SIProcessPs,SOutput),
			U_S=S#state{
				si_pidps_current = U_SIProcessPs
			}
	end,
	SIProcess = S#state.si_pids,
	MIProcess = S#state.mi_pids,
	neuron:loop(U_S,ExoSelfProcess,SIProcess,MIProcess,[],[]);
loop(S,ExoSelfProcess,[SIProcess|SIProcess],[MIProcess|MIProcess],SIAcc,MIAcc)->
	receive
		{SIProcess,forward,Input}->
			loop(S,ExoSelfProcess,SIProcess,[MIProcess|MIProcess],[{SIProcess,Input}|SIAcc],MIAcc);
		{MIProcess,forward,Input}->
			loop(S,ExoSelfProcess,[SIProcess|SIProcess],MIProcess,SIAcc,[{MIProcess,Input}|MIAcc]);
		{ExoSelfProcess,weight_backup}->
			U_S=case S#state.heredity_type of
				darwinian ->
					S#state{
						si_pidps_backup=S#state.si_pidps_bl,
						mi_pidps_backup=S#state.mi_pidps_current
					};
				lamarckian ->
					S#state{
						si_pidps_backup=S#state.si_pidps_current,
						mi_pidps_backup=S#state.mi_pidps_current
					}
			end,
			loop(U_S,ExoSelfProcess,[SIProcess|SIProcess],[MIProcess|MIProcess],SIAcc,MIAcc);
		{ExoSelfProcess,weight_restore}->
			U_S = S#state{
				si_pidps_bl=S#state.si_pidps_backup,
				si_pidps_current=S#state.si_pidps_backup,
				mi_pidps_current=S#state.mi_pidps_backup
			},
			loop(U_S,ExoSelfProcess,[SIProcess|SIProcess],[MIProcess|MIProcess],SIAcc,MIAcc);
		{ExoSelfProcess,weight_perturb,Spread}->
			Perturbed_SIWeightedInputProcessIDs=perturb_IWeightedInputProcessIDs(Spread,S#state.si_pidps_backup),
			Perturbed_MIWeightedInputProcessIDs=perturb_IWeightedInputProcessIDs(Spread,S#state.mi_pidps_backup),
			U_S=S#state{
				si_pidps_bl=Perturbed_SIWeightedInputProcessIDs,
				si_pidps_current=Perturbed_SIWeightedInputProcessIDs,
				mi_pidps_current=Perturbed_MIWeightedInputProcessIDs
			},
			loop(U_S,ExoSelfProcess,[SIProcess|SIProcess],[MIProcess|MIProcess],SIAcc,MIAcc);
		{ExoSelfProcess,reset_prep}->
			neuron:flush_buffer(),
			ExoSelfProcess ! {self(),ready},
			ROProcess = S#state.ro_pids,
			receive 
				{ExoSelfProcess, reset}->
					fanout(ROProcess,{self(),forward,[?RO_SIGNAL]})
			end,
			loop(S,ExoSelfProcess,S#state.si_pids,S#state.mi_pids,[],[]);
		{ExoSelfProcess,get_backup}->
			NeuronID = S#state.id,
			ExoSelfProcess ! {self(),NeuronID,S#state.si_pidps_backup,S#state.mi_pidps_backup},
			loop(S,ExoSelfProcess,[SIProcess|SIProcess],[MIProcess|MIProcess],SIAcc,MIAcc);
		{ExoSelfProcess,terminate}->
			%io:format("Neuron:~p is terminating.~n",[self()])
			ok
		after 10000 ->
			io:format("neuron:~p stuck.~n",[S#state.id])
	end.
%The neuron process waits for vector signals from all the processes that it's connected from, taking the dot product of the input and weight vectors, and then adding it to the accumulator. Once all the signals from InputProcess are received, the accumulator contains the dot product to which the neuron then adds the bias and executes the activation function. After fanning out the output signal, the neuron again returns to waiting for incoming signals. When the neuron receives the {ExoSelfProcess,get_backup} message, it forwards to the exoself its full MWeightedInputProcess list, and its Id. The MWeightedInputProcess contains the modified, tuned and most effective version of the weighted_inputs. The neuron process is also accepts weight_backup signal, when receiving it the neuron saves to process dictionary the current MWeightedInputProcess. When the neuron receives the weight_restore signal, it reads back from the process dictionary the stored WeightedInputProcess, and switches over to using it as its active WeightedInputProcess list. When the neuron receives the weight_perturb signal from the exoself, it perturbs the weights by executing the perturb_Lipids/1 function, which returns the updated list. Finally, the neuron can also accept a reset_prep signal, which makes the neuron flush its buffer in the off chance that it has a recursively sent signal in its inbox. After flushing its buffer, the neuron waits for the exoself to send it the reset signal, at which point the neuron, now fully refreshed after the flush_buffer/0, outputs a default forward signal to its recursively connected elements, if any, and then drops back into the main loop.

	fanout([Pid|Pids],Msg)->
		Pid ! Msg,
		fanout(Pids,Msg);
	fanout([],_Msg)->
		true.
%The fanout/2 function fans out th Msg to all the ProcessIDs in its list.

	flush_buffer()->
		receive 
			_ ->
				flush_buffer()
		after 0 ->
			done
	end.
%The flush_buffer/0 cleans out the element's inbox.
perturb_IWeightedInputProcessIDs(Spread,[])->[];
perturb_IWeightedInputProcessIDs(Spread,WeightedInputProcess)->
	Total_Weights=lists:sum([length(WeightsP) || {_InputProcess,WeightsP}<-WeightedInputProcess]),
	MP = 1/math:sqrt(Total_Weights),
	perturb_IWeightedInputProcessIDs(Spread,MP,WeightedInputProcess,[]).
perturb_IWeightedInputProcessIDs(Spread,MP,[{InputProcess,WeightsP}|WeightedInputProcess],Acc)->
	U_WeightsP = perturb_weightsP(Spread,MP,WeightsP,[]),
	perturb_IWeightedInputProcessIDs(Spread,MP,WeightedInputProcess,[{InputProcess,U_WeightsP}|Acc]);
perturb_IWeightedInputProcessIDs(_Spread,_MP,[],Acc)->
	lists:reverse(Acc).
%The perturb_IWeightedInputProcessIDs/1 function calculates the probability with which each neuron in the WeightedInputProcess is chosen to be perturbed. The probablity is based on the total number of weights in the WeightedInputProcess list, with the actual mutation probablity equating to the inverse of square root of total number of weights. The perturb_IWeightedInputProcessIDs/3 function goes through each weights block and calls the perturb_weights/3 to perturb the weights.

	perturb_weightsP(Spread,MP,[{W,LPs}|Weights],Acc)->
		U_W = case rand:uniform() < MP of
			true->
				sat((rand:uniform()-0.5)*2*Spread+W,-?SAT_LIMIT,?SAT_LIMIT);
			false ->
				W
		end,
		perturb_weightsP(Spread,MP,Weights,[{U_W,LPs}|Acc]);
	perturb_weightsP(_Spread,_MP,[],Acc)->
		lists:reverse(Acc).
%The perturb_weights/3 function is the function that actually goes through each weight block, and perturbs each weight with a probablity of MP. If the weight is chosen to be perturbed, the perturbation intensity is chosen uniformly between -Spread and Spread.

		sat(Val,Limit)->
			sat(Val,-abs(Limit),abs(Limit)).
		sat(Val,Min,Max)->
			if
				Val < Min -> Min;
				Val > Max -> Max;
				true -> Val
			end.
%sat/3 function simply ensures that the Val is neither less than min or greater than max.
