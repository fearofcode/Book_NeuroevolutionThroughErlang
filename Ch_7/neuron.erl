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

gen(ExoSelfProcess,Node)->
	spawn(Node,?MODULE,prep,[ExoSelfProcess]).

prep(ExoSelfProcess) ->
	{V1,V2,V3} = now(),
	random:seed(V1,V2,V3),
	receive 
		{ExoSelfProcess,{Id,CortexProcess,ActivationFunction,WeightedInputProcess,OutputProcess}} ->
			loop(Id,ExoSelfProcess,CortexProcess,ActivationFunction,{WeightedInputProcess,WeightedInputProcess},OutputProcess,0)
	end.
%When gen/2 is executed it spawns the neuron element, which seeds the pseudo random number generator, and immediately begins to wait for its initial state message. It is essential that we seed the random number generator to make sure that every NN will have a different set of mutation probabilities and different combination of perturbation intensities. Once the initial state signal from the exoself is received, the neuron drops into its main loop.

loop(Id,ExoSelfProcess,CortexProcess,ActivationFunction,{[{InputProcess,Weights}|WeightedInputProcess],MWeightedInputProcess},OutputProcess,Acc)->
	receive
		{InputProcess,forward,Input}->
			Result = dot(Input,Weights,0),
			loop(Id,ExoSelfProcess,CortexProcess,ActivationFunction,{WeightedInputProcess,MWeightedInputProcess},OutputProcess,Result+Acc);
		{ExoSelfProcess,weight_backup}->
			put(weights,MWeightedInputProcess),
			loop(Id,ExoSelfProcess,CortexProcess,ActivationFunction,{[{InputProcess,Weights}|WeightedInputProcess],MWeightedInputProcess},OutputProcess,Acc);
		{ExoSelfProcess,weight_restore}->
			RWeightedInputProcess = get(weights),
			loop(Id,ExoSelfProcess,CortexProcess,ActivationFunction,{RWeightedInputProcess,RWeightedInputProcess},OutputProcess,Acc);
		{ExoSelfProcess,weight_perturb}->
			PWeightedInputProcess=perturb_IWeightedInputProcessIDs(MWeightedInputProcess),
			loop(Id,ExoSelfProcess,CortexProcess,ActivationFunction,{PWeightedInputProcess,PWeightedInputProcess},OutputProcess,Acc);
		{ExoSelfProcess,get_backup}->
			ExoSelfProcess ! {self(),Id,MWeightedInputProcess},
			loop(Id,ExoSelfProcess,CortexProcess,ActivationFunction,{[{InputProcess,Weights}|WeightedInputProcess],MWeightedInputProcess},OutputProcess,Acc);
		{ExoSelfProcess,terminate}->
			ok
	end;
loop(Id,ExoSelfProcess,CortexProcess,ActivationFunction,{[Bias],MWeightedInputProcess},OutputProcess,Acc)->
	Output = neuron:ActivationFunction(Acc+Bias),
	[OutputProcess ! {self(),forward,[Output]} || OutputProcess <- OutputProcess],
	loop(Id,ExoSelfProcess,CortexProcess,ActivationFunction,{MWeightedInputProcess,MWeightedInputProcess},OutputProcess,0);
loop(Id,ExoSelfProcess,CortexProcess,ActivationFunction,{[],MWeightedInputProcess},OutputProcess,Acc)->
	Output = neuron:ActivationFunction(Acc),
	[OutputProcess ! {self(),forward,[Output]} || OutputProcess <- OutputProcess],
	loop(Id,ExoSelfProcess,CortexProcess,ActivationFunction,{MWeightedInputProcess,MWeightedInputProcess},OutputProcess,0).
	
	dot([I|Input],[W|Weights],Acc) ->
		dot(Input,Weights,I*W+Acc);
	dot([],[],Acc)->
		Acc.
%The neuron process waits for vector signals from all the processes that it’s connected from. As the presynaptic signals fanin, the neuron takes the dot product of the input and their associated weight vectors, and then adds it to the accumulator. Once all the signals from InputProcess are received, the accumulator contains the dot product to which the neuron then adds the bias (if it exists) and executes the activation function. After fanning out the output signal, the neuron again returns to waiting for incoming signals. When the neuron receives the {ExoSelfProcess, get_backup} message, it forwards to the exoself its full MWeightedInputProcess list, and its Id. The MWeightedInputProcess contains the current version of the neural weights. When the neuron receives the {ExoSelfProcess,weight_perturb} message, it executes the perturb_IWeightedInputProcessIDs/1, after which the neuron drops back into the loop but with MWeightedInputProcess replaced by the new PWeightedInputProcess. It is important to note that the neuron expects to be synchronized, and expects that it has at this point not received any signals from the other elements it is connected from, because if it has and it then changes out the WeightedInputProcess with PWeightedInputProcess, it might start waiting for signals from the elements from which it has already received the signals. When the neuron receives the {ExoSelfProcess,weight_backup}, it stores its weights in its process dictionary. When the neuron receives the {ExoSelf,weight_restore}, it restores its weights to the state they were before being perturbed by restoring the saved synaptic weights from its process dictionary.

	tanh(Val)->
		math:tanh(Val).
%The activation function is a sigmoid function, tanh.

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
%perturb_IWeightedInputProcessIDs/1 first calculates the probability that a weight will be perturbed, the probability being the inverse square root of the total number of weights in the neuron. The function then drops into perturb_IWeightedInputProcessIDs/3, which executes perturb_weights/3 for every set of weights associated with a particular InputProcess in the WeightedInputProcess list. If bias is present in the weights list, it is reached last and perturbed just as any other weight, based on the probability. Afterwards, the perturbed and inverted version of the WeightedInputProcess is reversed back to the proper order and returned to the calling function.
	
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
		
		sat(Val,Min,Max)->
			if
				Val < Min -> Min;
				Val > Max -> Max;
				true -> Val
			end.
%perturb_weights/3 accepts a probability value, a list of weights, and an empty list to act as an accumulator. The function then goes through the weight list perturbing each weight with a probability of MP. The weights are constrained to be within the range of -?SAT_LIMIT and SAT_LIMIT through the use of the sat/3 function.
