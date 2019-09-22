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

gen(ExoSelfProcess,Node)->
	spawn(Node,?MODULE,prep,[ExoSelfProcess]).

prep(ExoSelfProcess) ->
	{V1,V2,V3} = now(),
	random:seed(V1,V2,V3),
	receive 
		{ExoSelfProcess,{Id,CortexProcess,ActivationFunction,WeightedInputProcess,OutputProcess,ROProcess}} ->
			fanout(ROProcess,{self(),forward,[?RO_SIGNAL]}),
			loop(Id,ExoSelfProcess,CortexProcess,ActivationFunction,{WeightedInputProcess,WeightedInputProcess},OutputProcess,ROProcess,0)
	end.
%When gen/2 is executed, it spawns the neuron element and immediately begins to wait for its initial state message from the exoself. Once the state message arrives, the neuron sends out the default forward signals to any elements in its ro_ids list, if any. Afterwards, prep drops into the neuron's main loop.

loop(Id,ExoSelfProcess,CortexProcess,ActivationFunction,{[{InputProcess,Weights}|WeightedInputProcess],MWeightedInputProcess},OutputProcess,ROProcess,Acc)->
	receive
		{InputProcess,forward,Input}->
			Result = dot(Input,Weights,0),
			loop(Id,ExoSelfProcess,CortexProcess,ActivationFunction,{WeightedInputProcess,MWeightedInputProcess},OutputProcess,ROProcess,Result+Acc);
		{ExoSelfProcess,weight_backup}->
			put(weights,MWeightedInputProcess),
			loop(Id,ExoSelfProcess,CortexProcess,ActivationFunction,{[{InputProcess,Weights}|WeightedInputProcess],MWeightedInputProcess},OutputProcess,ROProcess,Acc);
		{ExoSelfProcess,weight_restore}->
			RWeightedInputProcess = get(weights),
			loop(Id,ExoSelfProcess,CortexProcess,ActivationFunction,{RWeightedInputProcess,RWeightedInputProcess},OutputProcess,ROProcess,Acc);
		{ExoSelfProcess,weight_perturb}->
			PWeightedInputProcess=perturb_IWeightedInputProcessIDs(MWeightedInputProcess),
			loop(Id,ExoSelfProcess,CortexProcess,ActivationFunction,{[{InputProcess,Weights}|WeightedInputProcess],PWeightedInputProcess},OutputProcess,ROProcess,Acc);
		{ExoSelf,reset_prep}->
			neuron:flush_buffer(),
			ExoSelf ! {self(),ready},
			receive 
				{ExoSelf, reset}->
					fanout(ROProcess,{self(),forward,[?RO_SIGNAL]})
			end,
			loop(Id,ExoSelfProcess,CortexProcess,ActivationFunction,{MWeightedInputProcess,MWeightedInputProcess},OutputProcess,ROProcess,0);
		{ExoSelfProcess,get_backup}->
			ExoSelfProcess ! {self(),Id,MWeightedInputProcess},
			loop(Id,ExoSelfProcess,CortexProcess,ActivationFunction,{[{InputProcess,Weights}|WeightedInputProcess],MWeightedInputProcess},OutputProcess,ROProcess,Acc);
		{ExoSelfProcess,terminate}->
			io:format("Neuron:~p has termianted.~n",[self()]),
			ok
	end;
loop(Id,ExoSelfProcess,CortexProcess,ActivationFunction,{[Bias],MWeightedInputProcess},OutputProcess,ROProcess,Acc)->
	Output = functions:ActivationFunction(Acc+Bias),
	[OutputProcess ! {self(),forward,[Output]} || OutputProcess <- OutputProcess],
	loop(Id,ExoSelfProcess,CortexProcess,ActivationFunction,{MWeightedInputProcess,MWeightedInputProcess},OutputProcess,ROProcess,0);
loop(Id,ExoSelfProcess,CortexProcess,ActivationFunction,{[],MWeightedInputProcess},OutputProcess,ROProcess,Acc)->
	Output = functions:ActivationFunction(Acc),
	[OutputProcess ! {self(),forward,[Output]} || OutputProcess <- OutputProcess],
	loop(Id,ExoSelfProcess,CortexProcess,ActivationFunction,{MWeightedInputProcess,MWeightedInputProcess},OutputProcess,ROProcess,0).
%The neuron process waits for vector signals from all the processes that it’s connected from, taking the dot product of the input and weight vectors, and then adding it to the accumulator. Once all the signals from InputProcess are received, the accumulator contains the dot product to which the neuron then adds the bias and executes the activation function. After fanning out the output signal, the neuron again returns to waiting for incoming signals. When the neuron receives the {ExoSelfProcess,get_backup} message, it forwards to the exoself its full MWeightedInputProcess list, and its Id. The MWeightedInputProcess contains the modified, tuned and most effective version of the weighted_inputs. The neuron process also accepts the weight_backup signal, when receiving it, the neuron saves to process dictionary the current MWeightedInputProcess. When the neuron receives the weight_restore signal, it reads back from the process dictionary the stored WeightedInputProcess, and switches over to using it as its active WeightedInputProcess list. When the neuron receives the weight_perturb signal from the exoself, it perturbs the weights by executing the perturb_IWeightedInputProcessIDs/1 function, which returns the updated/perturbed weight list. Finally, the neuron can also accept a reset_prep signal, which makes the neuron flush its buffer in the off chance that it has a recursively sent to it signal in its inbox. After flushing its buffer, the neuron waits for the exoself to send it the reset signal, at which point the neuron, now fully refreshed after the flush_buffer/0, outputs a default forward signal to its recursively connected elements (ro_ids), if any, and then drops back into its main receive loop.


	dot([I|Input],[W|Weights],Acc) ->
		dot(Input,Weights,I*W+Acc);
	dot([],[],Acc)->
		Acc;
	dot([],[Bias],Acc)->
		Acc+Bias.
%The dot/3 function accepts an input vector and a weight list, and computes the dot product of the two vectors.

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

perturb_IWeightedInputProcessIDs(WeightedInputProcess)->
	Total_Weights=lists:sum([length(Weights) || {_InputProcess,Weights}<-WeightedInputProcess]),
	MP = 1/math:sqrt(Total_Weights),
	perturb_IWeightedInputProcessIDs(MP,WeightedInputProcess,[]).
perturb_IWeightedInputProcessIDs(MP,[{InputProcess,Weights}|WeightedInputProcess],Acc)->
	U_Weights = perturb_weights(MP,Weights,[]),
	perturb_IWeightedInputProcessIDs(MP,WeightedInputProcess,[{InputProcess,U_Weights}|Acc]);
perturb_IWeightedInputProcessIDs(MP,[Bias],Acc)->
	U_Bias = case rand:uniform() < MP of
		true-> sat((rand:uniform()-0.5)*?DELTA_MULTIPLIER+Bias,-?SAT_LIMIT,?SAT_LIMIT);
		false -> Bias
	end,
	lists:reverse([U_Bias|Acc]);
perturb_IWeightedInputProcessIDs(_MP,[],Acc)->
	lists:reverse(Acc).
%The perturb_IWeightedInputProcessIDs/1 function calculates the probablity with which each neuron in the WeightedInputProcess is chosen to be perturbed. The probablity is based on the total number of weights in the WeightedInputProcess list, with the actual mutation probablity equatling to the inverse of square root of total number of weights. The perturb_IWeightedInputProcessIDs/3 function goes through each weights block and calls the perturb_weights/3 to perturb the weights.

	perturb_weights(MP,[W|Weights],Acc)->
		U_W = case rand:uniform() < MP of
			true->
				sat((rand:uniform()-0.5)*?DELTA_MULTIPLIER+W,-?SAT_LIMIT,?SAT_LIMIT);
			false ->
				W
		end,
		perturb_weights(MP,Weights,[U_W|Acc]);
	perturb_weights(_MP,[],Acc)->
		lists:reverse(Acc).
%The perturb_weights/3 function is the function that actually goes through each weight block, and perturbs each weight with a probablity of MP. If the weight is chosen to be perturbed, the perturbation intensity is chosen uniformly between -Pi and Pi.

		sat(Val,Min,Max)->
			if
				Val < Min -> Min;
				Val > Max -> Max;
				true -> Val
			end.
%The sat/3 function simply ensures that the Val is neither less than min or greater than max. When used with synaptic weights (or other parameters), this function makes sure that the synaptic weights get saturated at the Min and Max values, rather than growing in magnitude without bound.
