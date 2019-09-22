%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(neuron).
-compile(export_all).
-include("records.hrl").

gen(ExoSelfProcess,Node)->
	spawn(Node,?MODULE,loop,[ExoSelfProcess]).

loop(ExoSelfProcess) ->
	receive 
		{ExoSelfProcess,{Id,CortexProcess,ActivationFunction,WeightedInputProcess,OutputProcess}} ->
			loop(Id,CortexProcess,ActivationFunction,{WeightedInputProcess,WeightedInputProcess},OutputProcess,0)
	end.
%When gen/2 is executed it spawns the neuron element and immediately begins to wait for its initial state message.

loop(Id,CortexProcess,ActivationFunction,{[{InputProcess,Weights}|WeightedInputProcess],MWeightedInputProcess},OutputProcess,Acc)->
	receive
		{InputProcess,forward,Input}->
			Result = dot(Input,Weights,0),
			loop(Id,CortexProcess,ActivationFunction,{WeightedInputProcess,MWeightedInputProcess},OutputProcess,Result+Acc);
		{CortexProcess,get_backup}->
			CortexProcess ! {self(),Id,MWeightedInputProcess},
			loop(Id,CortexProcess,ActivationFunction,{[{InputProcess,Weights}|WeightedInputProcess],MWeightedInputProcess},OutputProcess,Acc);
		{CortexProcess,terminate}->
			ok
	end;
loop(Id,CortexProcess,ActivationFunction,{[Bias],MWeightedInputProcess},OutputProcess,Acc)->
	Output = neuron:ActivationFunction(Acc+Bias),
	[OutputProcess ! {self(),forward,[Output]} || OutputProcess <- OutputProcess],
	loop(Id,CortexProcess,ActivationFunction,{MWeightedInputProcess,MWeightedInputProcess},OutputProcess,0);
loop(Id,CortexProcess,ActivationFunction,{[],MWeightedInputProcess},OutputProcess,Acc)->
	Output = neuron:ActivationFunction(Acc),
	[OutputProcess ! {self(),forward,[Output]} || OutputProcess <- OutputProcess],
	loop(Id,CortexProcess,ActivationFunction,{MWeightedInputProcess,MWeightedInputProcess},OutputProcess,0).
	
dot([I|Input],[W|Weights],Acc) ->
	dot(Input,Weights,I*W+Acc);
dot([],[],Acc)->
	Acc.
%The neuron process waits for vector signals from all the processes that it's connected from, taking the dot product of the input and weight vectors, and then adding it to the accumulator. Once all the signals from InputProcess are received, the accumulator contains the dot product to which the neuron then adds the bias and executes the activation function. After fanning out the output signal, the neuron again returns to waiting for incoming signals. When the neuron receives the {CortexProcess,get_backup} message, it forwards to the cortex its full MWeightedInputProcess list, and its Id. The MWeightedInputProcess will contain an modified version of the weights once the training/learning algorithm has been added to the system.

tanh(Val)->
	math:tanh(Val).
%Though in this current implementation the neuron has only the tanh/1 function available to it, we will later extend the system to allow different neurons to use different activation functions.
