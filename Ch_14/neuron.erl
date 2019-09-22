%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(neuron).

-compile(export_all).

-include("records.hrl").

-define(DELTA_MULTIPLIER, math:pi() * 2).

-define(SAT_LIMIT, math:pi() * 2).

-define(RO_SIGNAL, 0).

gen(ExoSelfProcess, Node) ->
    spawn(Node, ?MODULE, prep, [ExoSelfProcess]).

prep(ExoSelfProcess) ->
    random:seed(now()),
    receive
      {ExoSelfProcess,
       {Id, CortexProcess, ActivationFunction, PF, AggrF, WeightedInputProcess, OutputProcess,
	ROProcess}} ->
	  fanout(ROProcess, {self(), forward, [?RO_SIGNAL]}),
	  IProcessIDs = [IProcessID
		   || {IProcessID, _W} <- WeightedInputProcess, IProcessID =/= bias],
	  %io:format("IWeightedInputProcessIDs:~p~n",[WeightedInputProcess]),
	  loop(Id, ExoSelfProcess, CortexProcess, ActivationFunction, PF, AggrF,
	       {IProcessIDs, IProcessIDs}, [], {WeightedInputProcess, WeightedInputProcess},
	       OutputProcess, ROProcess)
    end.

%When gen/2 is executed, it spawns the neuron element and immediately begins to wait for its initial state message from the exoself. Once the state message arrives, the neuron sends out the default forward signals to any elements in its ro_ids list, if any. Afterwards, prep drops into the neuron's main loop.

loop(Id, ExoSelfProcess, CortexProcess, ActivationFunction, PF, AggrF,
     {[InputProcess | IProcessIDs], MIProcessIDs}, IAcc,
     {WeightedInputProcess, MWeightedInputProcess}, OutputProcess, ROProcess) ->
    receive
      {InputProcess, forward, Input} ->
	  loop(Id, ExoSelfProcess, CortexProcess, ActivationFunction, PF, AggrF,
	       {IProcessIDs, MIProcessIDs}, [{InputProcess, Input} | IAcc],
	       {WeightedInputProcess, MWeightedInputProcess}, OutputProcess, ROProcess);
      {ExoSelfProcess, weight_backup} ->
	  loop(Id, ExoSelfProcess, CortexProcess, ActivationFunction, PF, AggrF,
	       {[InputProcess | IProcessIDs], MIProcessIDs}, IAcc,
	       {WeightedInputProcess, WeightedInputProcess}, OutputProcess, ROProcess);
      {ExoSelfProcess, weight_restore} ->
	  loop(Id, ExoSelfProcess, CortexProcess, ActivationFunction, PF, AggrF,
	       {[InputProcess | IProcessIDs], MIProcessIDs}, IAcc,
	       {MWeightedInputProcess, MWeightedInputProcess}, OutputProcess, ROProcess);
      {ExoSelfProcess, weight_perturb, Spread} ->
	  Perturbed_IWeightedInputProcessIDs = perturb_IWeightedInputProcessIDs(Spread, MWeightedInputProcess),
	  loop(Id, ExoSelfProcess, CortexProcess, ActivationFunction, PF, AggrF,
	       {[InputProcess | IProcessIDs], MIProcessIDs}, IAcc,
	       {Perturbed_IWeightedInputProcessIDs, MWeightedInputProcess}, OutputProcess, ROProcess);
      {ExoSelf, reset_prep} ->
	  neuron:flush_buffer(),
	  ExoSelf ! {self(), ready},
	  receive
	    {ExoSelf, reset} ->
		fanout(ROProcess, {self(), forward, [?RO_SIGNAL]})
	  end,
	  loop(Id, ExoSelfProcess, CortexProcess, ActivationFunction, PF, AggrF,
	       {MIProcessIDs, MIProcessIDs}, [], {WeightedInputProcess, MWeightedInputProcess},
	       OutputProcess, ROProcess);
      {ExoSelfProcess, get_backup} ->
	  ExoSelfProcess ! {self(), Id, MWeightedInputProcess},
	  loop(Id, ExoSelfProcess, CortexProcess, ActivationFunction, PF, AggrF,
	       {[InputProcess | IProcessIDs], MIProcessIDs}, IAcc,
	       {WeightedInputProcess, MWeightedInputProcess}, OutputProcess, ROProcess);
      {ExoSelfProcess, terminate} ->
	  %io:format("Neuron:~p is terminating.~n",[self()]),
	  ok
      after 10000 -> io:format("neuron:~p stuck.~n", [Id])
    end;
loop(Id, ExoSelfProcess, CortexProcess, ActivationFunction, PF, AggrF,
     {[], MIProcessIDs}, IAcc, {WeightedInputProcess, MWeightedInputProcess},
     OutputProcess, ROProcess) ->
    Ordered_IAcc = lists:reverse(IAcc),
    Aggregation_Product =
	signal_aggregator:AggrF(Ordered_IAcc, WeightedInputProcess),
    Output = sat(functions:ActivationFunction(Aggregation_Product), -1, 1),
    U_IWeightedInputProcessIDs = plasticity:PF(Ordered_IAcc, WeightedInputProcess,
			     Output),
    [OutputProcess ! {self(), forward, [Output]}
     || OutputProcess <- OutputProcess],
    loop(Id, ExoSelfProcess, CortexProcess, ActivationFunction, PF, AggrF,
	 {MIProcessIDs, MIProcessIDs}, [], {U_IWeightedInputProcessIDs, MWeightedInputProcess},
	 OutputProcess, ROProcess).

%The neuron process waits for vector signals from all the processes that it's connected from, taking the dot product of the input and weight vectors, and then adding it to the accumulator. Once all the signals from InputProcess are received, the accumulator contains the dot product to which the neuron then adds the bias and executes the activation function. After fanning out the output signal, the neuron again returns to waiting for incoming signals. When the neuron receives the {ExoSelfProcess,get_backup} message, it forwards to the exoself its full MWeightedInputProcess list, and its Id. The MWeightedInputProcess contains the modified, tuned and most effective version of the weighted_inputs. The neuron process is also accepts weight_backup signal, when receiving it the neuon saves to process dictionary the current MWeightedInputProcess. When the neuron receives the weight_restore signal, it reads back from the process dictionary the stored INputProcessPs, and switches over to using it as its active WeightedInputProcess list. When the neuron receives the weight_perturb signal from the exoself, it perturbs the weights by executing the peturb_IWeightedInputProcessIDs/1 function, which returns the updated list. Finally, the neuron can also accept a reset_prep signal, which makes the neuron flush its buffer in the off chance that it has a recursivector_lengthy sent signal in its inbox. After flushing its buffer, the neuron waits for the exoself to send it the reset signal, at which point the neuron, now fully refreshed after the flush_buffer/0, outputs a default forward signal to its recursivector_lengthy connected elements, if any, and then drops back into the main loop.

fanout([Pid | Pids], Msg) ->
    Pid ! Msg, fanout(Pids, Msg);
fanout([], _Msg) -> true.

%The fanout/2 function fans out th Msg to all the ProcessIDs in its list.

flush_buffer() ->
    receive _ -> flush_buffer() after 0 -> done end.

%The flush_buffer/0 cleans out the element's inbox.

perturb_IWeightedInputProcessIDs(Spread, WeightedInputProcess) ->
    Total_Weights = lists:sum([length(Weights)
			     || {_InputProcess, Weights} <- WeightedInputProcess]),
    MP = 1 / math:sqrt(Total_Weights),
    perturb_IWeightedInputProcessIDs(Spread, MP, WeightedInputProcess, []).

perturb_IWeightedInputProcessIDs(Spread, MP,
	       [{InputProcess, Weights} | WeightedInputProcess], Acc) ->
    U_Weights = perturb_weights(Spread, MP, Weights, []),
    perturb_IWeightedInputProcessIDs(Spread, MP, WeightedInputProcess,
		   [{InputProcess, U_Weights} | Acc]);
perturb_IWeightedInputProcessIDs(_Spread, _MP, [], Acc) ->
    lists:reverse(Acc).

%The perturb_IWeightedInputProcessIDs/1 function calculates the probablity with which each neuron in the WeightedInputProcess is chosen to be perturbed. The probablity is based on the total number of weights in the WeightedInputProcess list, with the actual mutation probablity equatling to the inverse of square root of total number of weights. The perturb_IWeightedInputProcessIDs/3 function goes through each weights block and calls the perturb_weights/3 to perturb the weights.

perturb_weights(Spread, MP, [W | Weights], Acc) ->
    U_W = case rand:uniform() < MP of
	    true ->
		sat((rand:uniform() - 5.0e-1) * 2 * Spread + W,
		    -(?SAT_LIMIT), ?SAT_LIMIT);
	    false -> W
	  end,
    perturb_weights(Spread, MP, Weights, [U_W | Acc]);
perturb_weights(_Spread, _MP, [], Acc) ->
    lists:reverse(Acc).

%The perturb_weights/3 function is the function that actually goes through each weight block, and perturbs each weight with a probablity of MP. If the weight is chosen to be perturbed, the perturbation intensity is chosen uniformly between -Spread and Spread.

sat(Val, Limit) -> sat(Val, -abs(Limit), abs(Limit)).

sat(Val, Min, Max) ->
    if Val < Min -> Min;
       Val > Max -> Max;
       true -> Val
    end.

%sat/3 function simply ensures that the Val is neither less than min or greater than max.

