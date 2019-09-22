%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(plasticity).
-compile(export_all).
-include("records.hrl").
-define(SAT_LIMIT,math:pi()*2).

none({_NeuronID,mutate})->
	exit("Neuron does not support plasticity.");
none(neural_parameters)->
	[];
none(weight_parameters)->
	[].
%none/1 returns a set of learning parameters needed by the none/1 plasticity function. Since this function specifies that the neuron has no plasticity, the parameter lists are empty. When executed with the {NeuronID,mutate} parameter, the function exits, since there is nothing ot mutate. The exit allows for the neuroevolutionary system to try another mutation operator on the NN system.

none(_NeuralParameters,_IAcc,WeightedInputProcess,_Output)->
	WeightedInputProcess.
%none/3 returns the original WeightedInputProcess to the caller.

hebbian_w({NeuronID,mutate})->
	random:seed(now()),
	Neuron = genotype:read({neuron,NeuronID}),
	InputIdPs = N#neuron.weighted_inputs,
	UWeightedInput=perturb_parameters(InputIdPs,?SAT_LIMIT),
	N#neuron{weighted_inputs = UWeightedInput};
hebbian_w(neural_parameters)->
	[];
hebbian_w(weight_parameters)->
	[(rand:uniform()-0.5)].
%hebbian_w/1 function produces the necessary parameter list for the hebbian_w learning rule to operate. The parameter list for the simple hebbian_w learning rule is a parameter list composed of a single parameter H: [H], for every synaptic weight of the neuron. When hebbian_w/1 is called with the parameter neural_parameters, it returns []. When hebbian_w/1 is executed with the {NeuronID,mutate} parameter, the function goes through every parameter in the neuron's weighted_inputs, and perturbs the parameter value using the specified spread (?SAT_LIMIT).

	perturb_parameters(InputIdPs,Spread)->
		TotalParameters = lists:sum([ lists:sum([length(Ps) || {_W,Ps} <- WPs]) || {_InputID,WPs} <- InputIdPs]),
		MutationProb = 1/math:sqrt(TotalParameters),
		[{InputID,[{W,perturb(Ps,MutationProb,Spread,[])}|| {W,Ps} <- WPs]} || {InputID,WPs} <- InputIdPs].
	
		perturb([Val|Vals],MutationProb,Spread,Acc)->
			case rand:uniform() < MutationProb of
				true ->
					U_Val = sat((rand:uniform()-0.5)*2*Spread+Val,Spread,Spread),
					perturb(Vals,MutationProb,Spread,[U_Val|Acc]);
				false ->
					perturb(Vals,MutationProb,Spread,[Val|Acc])
			end;
		perturb([],_MutationProb,_Spread,Acc)->
			lists:reverse(Acc).

hebbian_w(_NeuralParameters,IAcc,WeightedInputProcess,Output)->
	hebbian_w1(IAcc,WeightedInputProcess,Output,[]).

	hebbian_w1([{IProcessID,Is}|IAcc],[{IProcessID,WPs}|WeightedInputProcess],Output,Acc)->
		Updated_WPs = hebbrule_w(Is,WPs,Output,[]),
		hebbian_w1(IAcc,WeightedInputProcess,Output,[{IProcessID,Updated_WPs}|Acc]);
	hebbian_w1([],[],_Output,Acc)->
		lists:reverse(Acc);
	hebbian_w1([],[{bias,WPs}],_Output,Acc)->
		lists:reverse([{bias,WPs}|Acc]).
%hebbian_w/4 function operates on each InputProcessP, calling the hebbian_w1/4 function which processes each of the complementary Is and WPs lists, producing the Updated_WPs list in return, with the updated/adapted weights based on the hebbian_w learning rule. 

	hebbrule_w([I|Is],[{W,[H]}|WPs],Output,Acc)->
		Updated_W = functions:saturation(W + H*I*Output,?SAT_LIMIT),
		hebbrule_w(Is,WPs,Output,[{Updated_W,[H]}|Acc]);
	hebbrule_w([],[],_Output,Acc)->
		lists:reverse(Acc).
%hebbrule_w/4 applies the hebbian learning rule to each weight, using the input value I, the neuron's calculated output Output, and its own distinct learning parameter H associated with each synaptic weight.

hebbian({NeuronID,mutate})->
	random:seed(now()),
	Neuron = genotype:read({neuron,NeuronID}),
	{PFName,ParameterList} = N#neuron.pf,
	Spread = ?SAT_LIMIT*10,
	MutationProb = 1/math:sqrt(length(ParameterList)),
	U_ParameterList = perturb(ParameterList,MutationProb,Spread,[]),
	U_PF = {PFName,U_ParameterList},
	N#neuron{pf=U_PF};
hebbian(neural_parameters)->
	[(rand:uniform()-0.5)];
hebbian(weight_parameters)->
	[].
%hebbian/1 function produces the necessary parameter list for the hebbian learning rule to operate. The parameter list for the standard hebbian learning rule is a parameter list composed of a single parameter H: [H], used by the neuron for all its synaptic weights. When hebbian/1 is called with the parameter weight_parameters, it returns []. When the function is executed with the {NeuronID,mutate} parameter, it uses the perturb/4 function to perturb the parameter list, which in this case is a list composed of a single floating point parameter.

hebbian([_M,H],IAcc,WeightedInputProcess,Output)->
	hebbian(H,IAcc,WeightedInputProcess,Output,[]).

	hebbian(H,[{IProcessID,Is}|IAcc],[{IProcessID,WPs}|WeightedInputProcess],Output,Acc)->
		Updated_WPs = hebbrule(H,Is,WPs,Output,[]),
		hebbian(H,IAcc,WeightedInputProcess,Output,[{IProcessID,Updated_WPs}|Acc]);
	hebbian(_H,[],[],_Output,Acc)->
		lists:reverse(Acc);
	hebbian(_H,[],[{bias,WPs}],_Output,Acc)->
		lists:reverse([{bias,WPs}|Acc]).
%hebbian/4 function operates on each InputProcessP, calling the hebbian/5 function which processes each of the complementary Is and WPs lists, producing the Updated_WPs list in return, with the updated/adapted weights based on the standard hebbian learning rule, using the neuron's single learning parameter H. 

	hebbrule(H,[I|Is],[{W,[]}|WPs],Output,Acc)->
		Updated_W = functions:saturation(W + H*I*Output,?SAT_LIMIT),
		hebbrule(H,Is,WPs,Output,[{Updated_W,[]}|Acc]);
	hebbrule(_H,[],[],_Output,Acc)->
		lists:reverse(Acc).
%hebbrule/5 applies the hebbian learning rule to each weight, using the input value I, the neuron's calculated output Output, and the neuron's leraning parameter H.

ojas_w({NeuronID,mutate})->
	random:seed(now()),
	Neuron = genotype:read({neuron,NeuronID}),
	InputIdPs = N#neuron.weighted_inputs,
	UWeightedInput=perturb_parameters(InputIdPs,?SAT_LIMIT),
	N#neuron{weighted_inputs = UWeightedInput};
ojas_w(neural_parameters)->
	[];
ojas_w(weight_parameters)->
	[(rand:uniform()-0.5)].
%oja/1 function produces the necessary parameter list for the oja's learning rule to operate. The parameter list for oja's learning rule is a list composed of a single parameter H: [H] per synaptic weight. If the learning parameter is positive, then the postsynaptic neuron's synaptic weight increases if the two connected neurons produce output signals of the same sign. If the learning parameter is negative, and the two connected neurons produce output signals of the same sign, then the synaptic weight of the postsynaptic neuron, decreases in magnitude. Otherwise it increases.

ojas_w(_Neural_Parameters,IAcc,WeightedInputProcess,Output)->
	ojas_w1(IAcc,WeightedInputProcess,Output,[]).
ojas_w1([{IProcessID,Is}|IAcc],[{IProcessID,WPs}|WeightedInputProcess],Output,Acc)->
	Updated_WPs = ojas_rule_w(Is,WPs,Output,[]),
	ojas_w1(IAcc,WeightedInputProcess,Output,[{IProcessID,Updated_WPs}|Acc]);
ojas_w1([],[],_Output,Acc)->
	lists:reverse(Acc);
ojas_w1([],[{bias,WPs}],_Output,Acc)->
	lists:reverse([{bias,WPs}|Acc]).
%ojas_w/4 function operates on each InputProcessP, calling the ojas_rule_w/4 function which processes each of the complementary Is and WPs lists, producing the Updated_WPs list in return, with the updated/adapted weights based on the oja's learning rule, using each synaptic weight's distinct learning parameter. 

	ojas_rule_w([I|Is],[{W,[H]}|WPs],Output,Acc)->
		Updated_W =functions:saturation(W + H*Output*(I - Output*W),?SAT_LIMIT),
		ojas_rule_w(Is,WPs,Output,[{Updated_W,[H]}|Acc]);
	ojas_rule_w([],[],_Output,Acc)->
		lists:reverse(Acc).
%ojas_weights/4 applies the ojas learning rule to each weight, using the input value I, the neuron's calculated output Output, and each weight's learning parameter H.
tt(W,H,Output,I)->
W + H*Output*(I - Output*W).

ojas({NeuronID,mutate})->
	random:seed(now()),
	Neuron = genotype:read({neuron,NeuronID}),
	{PFName,ParameterList} = N#neuron.pf,
	Spread = ?SAT_LIMIT*10,
	MutationProb = 1/math:sqrt(length(ParameterList)),
	U_ParameterList = perturb(ParameterList,MutationProb,Spread,[]),
	U_PF = {PFName,U_ParameterList},
	N#neuron{pf=U_PF};
ojas(neural_parameters)->
	[(rand:uniform()-0.5)];
ojas(weight_parameters)->
	[].
%oja/1 function produces the necessary parameter list for the oja's learning rule to operate. The parameter list for oja's learning rule is a list composed of a single parameter H: [H], used by the neuron for all its synaptic weights. If the learning parameter is positive, and the two connected neurons produce output signals of the same sign, then the postsynaptic neuron's synaptic weight increases. Otherwise it decreases.

ojas([_M,H],IAcc,WeightedInputProcess,Output)->
	ojas(H,IAcc,WeightedInputProcess,Output,[]).
ojas(H,[{IProcessID,Is}|IAcc],[{IProcessID,WPs}|WeightedInputProcess],Output,Acc)->
	Updated_WPs = ojas_rule(H,Is,WPs,Output,[]),
	ojas(H,IAcc,WeightedInputProcess,Output,[{IProcessID,Updated_WPs}|Acc]);
ojas(_H,[],[],_Output,Acc)->
	lists:reverse(Acc);
ojas(_H,[],[{bias,WPs}],_Output,Acc)->
	lists:reverse([{bias,WPs}|Acc]).
%ojas/5 function operates on each InputProcessP, calling the ojas_rule/5 function which processes each of the complementary Is and WPs lists, producing the Updated_WPs list in return, with the updated/adapted weights based on the standard oja's learning rule. 

	ojas_rule(H,[I|Is],[{W,[]}|WPs],Output,Acc)->
		Updated_W = functions:saturation(W + H*Output*(I - Output*W),?SAT_LIMIT),
		ojas_rule(H,Is,WPs,Output,[{Updated_W,[]}|Acc]);
	ojas_rule(_H,[],[],_Output,Acc)->
		lists:reverse(Acc).
%ojas_rule/5 updates every synaptic weight using Oja's learning rule.

self_modulationV1({NeuronID,mutate})->
	random:seed(now()),
	Neuron = genotype:read({neuron,NeuronID}),
	InputIdPs = N#neuron.weighted_inputs,
	UWeightedInput=perturb_parameters(InputIdPs,?SAT_LIMIT),
	N#neuron{weighted_inputs=UWeightedInput};
self_modulationV1(neural_parameters)->
	A=0.1,
	B=0,
	C=0,
	D=0,
	[A,B,C,D];
self_modulationV1(weight_parameters)->
	[(rand:uniform()-0.5)].
	
self_modulationV1([_M,A,B,C,D],IAcc,WeightedInputProcess,Output)->
	H = math:tanh(dot_productV1(IAcc,WeightedInputProcess)),
	neuromodulation([H,A,B,C,D],IAcc,WeightedInputProcess,Output,[]).
	
	dot_productV1(IAcc,IWeightedInputProcessIDs)->
		dot_productV1(IAcc,IWeightedInputProcessIDs,0).
	dot_productV1([{IProcessID,Input}|IAcc],[{IProcessID,WeightsP}|IWeightedInputProcessIDs],Acc)->
		Dot = dotV1(Input,WeightsP,0),
		dot_productV1(IAcc,IWeightedInputProcessIDs,Dot+Acc);
	dot_productV1([],[{bias,[{_Bias,[H_Bias]}]}],Acc)->
		Acc + H_Bias;
	dot_productV1([],[],Acc)->
		Acc.
	
		dotV1([I|Input],[{_W,[H_W]}|Weights],Acc) ->
			dotV1(Input,Weights,I*H_W+Acc);
		dotV1([],[],Acc)->
			Acc.
	
neuromodulation([H,A,B,C,D],[{IProcessID,Is}|IAcc],[{IProcessID,WPs}|WeightedInputProcess],Output,Acc)->
	%io:format("Neuromod:~p~n",[{[H,A,B,C,D],[{IProcessID,Is}|IAcc],[{IProcessID,WPs}|WeightedInputProcess],Output,Acc}]),
	Updated_WPs = genheb_rule([H,A,B,C,D],Is,WPs,Output,[]),
	neuromodulation([H,A,B,C,D],IAcc,WeightedInputProcess,Output,[{IProcessID,Updated_WPs}|Acc]);
neuromodulation(_NeuralParameters,[],[],_Output,Acc)->
	lists:reverse(Acc);
neuromodulation([H,A,B,C,D],[],[{bias,WPs}],Output,Acc)->
	Updated_WPs = genheb_rule([H,A,B,C,D],[1],WPs,Output,[]),
	lists:reverse([{bias,Updated_WPs}|Acc]).

	genheb_rule([H,A,B,C,D],[I|Is],[{W,Ps}|WPs],Output,Acc)->
		%io:format("genheb_rule:~p~n",[{[H,A,B,C,D],[I|Is],[{W,Ps}|WPs],Output,Acc}]),
		Updated_W = functions:saturation(W + H*(A*I*Output + B*I + C*Output + D),?SAT_LIMIT),
		genheb_rule([H,A,B,C,D],Is,WPs,Output,[{Updated_W,Ps}|Acc]);
	genheb_rule(_,[],[],_Output,Acc)->
		lists:reverse(Acc).
%Updated_W(i)= W(i) + H*(A*I(i)*Output + B*I(i) + C*Output + D)

self_modulationV2({NeuronID,mutate})->
	random:seed(now()),
	Neuron = genotype:read({neuron,NeuronID}),
	{PFName,[A|ParameterList]} = N#neuron.pf,
	[UpdatedActuator] = perturb([A],0.5,?SAT_LIMIT*10,[]),
	U_PF = {PFName,[UpdatedActuator|ParameterList]},
	InputIdPs = N#neuron.weighted_inputs,
	UWeightedInput=perturb_parameters(InputIdPs,?SAT_LIMIT),
	N#neuron{pf=U_PF,weighted_inputs=UWeightedInput};
self_modulationV2(neural_parameters)->
	A=(rand:uniform()-0.5),
	B=0,
	C=0,
	D=0,
	[A,B,C,D];
self_modulationV2(weight_parameters)->
	[(rand:uniform()-0.5)].
	
self_modulationV2([_M,A,B,C,D],IAcc,WeightedInputProcess,Output)->
	H = math:tanh(dot_productV1(IAcc,WeightedInputProcess)),
	neuromodulation([H,A,B,C,D],IAcc,WeightedInputProcess,Output,[]).

self_modulationV3({NeuronID,mutate})->
	random:seed(now()),
	Neuron = genotype:read({neuron,NeuronID}),
	{PFName,ParameterList} = N#neuron.pf,
	MSpread = ?SAT_LIMIT*10,
	MutationProb = 1/math:sqrt(length(ParameterList)),
	U_ParameterList = perturb(ParameterList,MutationProb,MSpread,[]),
	U_PF = {PFName,U_ParameterList},
	InputIdPs = N#neuron.weighted_inputs,
	UWeightedInput=perturb_parameters(InputIdPs,?SAT_LIMIT),
	N#neuron{pf=U_PF,weighted_inputs=UWeightedInput};
self_modulationV3(neural_parameters)->
	A=(rand:uniform()-0.5),
	B=(rand:uniform()-0.5),
	C=(rand:uniform()-0.5),
	D=(rand:uniform()-0.5),
	[A,B,C,D];
self_modulationV3(weight_parameters)->
	[(rand:uniform()-0.5)].

self_modulationV3([_M,A,B,C,D],IAcc,WeightedInputProcess,Output)->
	H = math:tanh(dot_productV1(IAcc,WeightedInputProcess)),
	neuromodulation([H,A,B,C,D],IAcc,WeightedInputProcess,Output,[]).

self_modulationV4({NeuronID,mutate})->
	random:seed(now()),
	Neuron = genotype:read({neuron,NeuronID}),
	InputIdPs = N#neuron.weighted_inputs,
	UWeightedInput=perturb_parameters(InputIdPs,?SAT_LIMIT),
	N#neuron{weighted_inputs=UWeightedInput};
self_modulationV4(neural_parameters)->
	B=0,
	C=0,
	D=0,
	[B,C,D];
self_modulationV4(weight_parameters)->
	[(rand:uniform()-0.5),(rand:uniform()-0.5)].

self_modulationV4([_M,B,C,D],IAcc,WeightedInputProcess,Output)->
	{AccH,AccA} = dot_productV4(IAcc,WeightedInputProcess),
	H = math:tanh(AccH),
	A = math:tanh(AccA),
	neuromodulation([H,A,B,C,D],IAcc,WeightedInputProcess,Output,[]).
	
	dot_productV4(IAcc,IWeightedInputProcessIDs)->
		dot_productV4(IAcc,IWeightedInputProcessIDs,0,0).
	dot_productV4([{IProcessID,Input}|IAcc],[{IProcessID,WeightsP}|IWeightedInputProcessIDs],AccH,AccA)->
		{DotH,DotA} = dotV4(Input,WeightsP,0,0),
		dot_productV4(IAcc,IWeightedInputProcessIDs,DotH+AccH,DotA+AccA);
	dot_productV4([],[{bias,[{_Bias,[H_Bias,A_Bias]}]}],AccH,AccA)->
		{AccH + H_Bias,AccA+A_Bias};
	dot_productV4([],[],AccH,AccA)->
		{AccH,AccA}.
	
		dotV4([I|Input],[{_W,[H_W,A_W]}|Weights],AccH,AccA) ->
			dotV4(Input,Weights,I*H_W+AccH,I*A_W+AccA);
		dotV4([],[],AccH,AccA)->
			{AccH,AccA}.

self_modulationV5({NeuronID,mutate})->
	random:seed(now()),
	Neuron = genotype:read({neuron,NeuronID}),
	{PFName,ParameterList} = N#neuron.pf,
	MSpread = ?SAT_LIMIT*10,
	MutationProb = 1/math:sqrt(length(ParameterList)),
	U_ParameterList = perturb(ParameterList,MutationProb,MSpread,[]),
	U_PF = {PFName,U_ParameterList},
	InputIdPs = N#neuron.weighted_inputs,
	UWeightedInput=perturb_parameters(InputIdPs,?SAT_LIMIT),
	N#neuron{pf=U_PF,weighted_inputs=UWeightedInput};
self_modulationV5(neural_parameters)->
	B=(rand:uniform()-0.5),
	C=(rand:uniform()-0.5),
	D=(rand:uniform()-0.5),
	[B,C,D];
self_modulationV5(weight_parameters)->
	[(rand:uniform()-0.5),(rand:uniform()-0.5)].

self_modulationV5([_M,B,C,D],IAcc,WeightedInputProcess,Output)->
	{AccH,AccA} = dot_productV4(IAcc,WeightedInputProcess),
	H = math:tanh(AccH),
	A = math:tanh(AccA),
	neuromodulation([H,A,B,C,D],IAcc,WeightedInputProcess,Output,[]).

self_modulationV6({NeuronID,mutate})->
	random:seed(now()),
	Neuron = genotype:read({neuron,NeuronID}),
	InputIdPs = N#neuron.weighted_inputs,
	UWeightedInput=perturb_parameters(InputIdPs,?SAT_LIMIT),
	N#neuron{weighted_inputs=UWeightedInput};
self_modulationV6(neural_parameters)->
	[];
self_modulationV6(weight_parameters)->
	H = (rand:uniform()-0.5),
	A = (rand:uniform()-0.5),
	B = (rand:uniform()-0.5),
	C = (rand:uniform()-0.5),
	D = (rand:uniform()-0.5),
	[H,A,B,C,D].

self_modulationV6(_Neural_Parameters,IAcc,WeightedInputProcess,Output)->
	{AccH,AccA,AccB,AccC,AccD} = dot_productV6(IAcc,WeightedInputProcess),
	H = math:tanh(AccH),
	A = math:tanh(AccA),
	B = math:tanh(AccB),
	C = math:tanh(AccC),
	D = math:tanh(AccD),
	neuromodulation([H,A,B,C,D],IAcc,WeightedInputProcess,Output,[]).
	
	dot_productV6(IAcc,IWeightedInputProcessIDs)->
		dot_productV6(IAcc,IWeightedInputProcessIDs,0,0,0,0,0).
	dot_productV6([{IProcessID,Input}|IAcc],[{IProcessID,WeightsP}|IWeightedInputProcessIDs],AccH,AccA,AccB,AccC,AccD)->
		{DotH,DotA,DotB,DotC,DotD} = dotV6(Input,WeightsP,0,0,0,0,0),
		dot_productV6(IAcc,IWeightedInputProcessIDs,DotH+AccH,DotA+AccA,DotB+AccB,DotC+AccC,DotD+AccD);
	dot_productV6([],[{bias,[{_Bias,[H_Bias,A_Bias,B_Bias,C_Bias,D_Bias]}]}],AccH,AccA,AccB,AccC,AccD)->
		{AccH + H_Bias,AccA+A_Bias,AccB+B_Bias,AccC+C_Bias,AccD+D_Bias};
	dot_productV6([],[],AccH,AccA,AccB,AccC,AccD)->
		{AccH,AccA,AccB,AccC,AccD}.
	
		dotV6([I|Input],[{_W,[H_W,A_W,B_W,C_W,D_W]}|Weights],AccH,AccA,AccB,AccC,AccD) ->
			dotV6(Input,Weights,I*H_W+AccH,I*A_W+AccA,I*B_W+AccB,I*C_W+AccC,I*D_W+AccD);
		dotV6([],[],AccH,AccA,AccB,AccC,AccD)->
			{AccH,AccA,AccB,AccC,AccD}.

neuromodulation({NeuronID,mutate})->
	random:seed(now()),
	Neuron = genotype:read({neuron,NeuronID}),
	{PFName,ParameterList} = N#neuron.pf,
	MSpread = ?SAT_LIMIT*10,
	MutationProb = 1/math:sqrt(length(ParameterList)),
	U_ParameterList = perturb(ParameterList,MutationProb,MSpread,[]),
	U_PF = {PFName,U_ParameterList},
	N#neuron{pf=U_PF};	
neuromodulation(neural_parameters)->
	H = (rand:uniform()-0.5),
	A = (rand:uniform()-0.5),
	B = (rand:uniform()-0.5),
	C = (rand:uniform()-0.5),
	D = (rand:uniform()-0.5),
	[H,A,B,C,D];
neuromodulation(weight_parameters)->
	[].
	
neuromodulation([M,H,A,B,C,D],IAcc,WeightedInputProcess,Output)->
	Modulator = scale_dzone(M,0.33,?SAT_LIMIT),
	neuromodulation([Modulator*H,A,B,C,D],IAcc,WeightedInputProcess,Output,[]).
	
scale_dzone(Val,Threshold,MaxMagnitude)->
	if 
		Val > Threshold ->
			(functions:scale(Val,MaxMagnitude,Threshold)+1)*MaxMagnitude/2;
		Val < -Threshold ->
			(functions:scale(Val,-Threshold,-MaxMagnitude)-1)*MaxMagnitude/2;
		true ->
			0
	end.
								
	scale(Val,Max,Min)-> %Nm = (Y*2 - (Max + Min))/(Max-Min)
		case Max == Min of
			true ->
				0;
			false ->
				(Val*2 - (Max+Min))/(Max-Min)
		end.

sat_dzone(Val,Max,Min,DZMax,DZMin)->
	case (Val < DZMax) and (Val > DZMin) of
		true ->
			0;
		false ->
			sat(Val,Max,Min)
	end.
	
	sat(Val,Max,Min)->
		case Val > Max of
			true ->
				Max;
			false ->
				case Val < Min of
					true ->
						Min;
					false ->
						Val
				end
		end.
