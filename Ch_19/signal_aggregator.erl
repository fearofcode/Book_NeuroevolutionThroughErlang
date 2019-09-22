%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(signal_aggregator).
-compile(export_all).
-include("records.hrl").

dot_product(IAcc,IWeightedInputProcessIDs)->
	dot_product(IAcc,IWeightedInputProcessIDs,0).
dot_product([{IProcessID,Input}|IAcc],[{IProcessID,WeightsP}|IWeightedInputProcessIDs],Acc)->
	Dot = dot(Input,WeightsP,0),
	dot_product(IAcc,IWeightedInputProcessIDs,Dot+Acc);
dot_product([],[{bias,[{Bias,_LPs}]}],Acc)->
	Acc + Bias;
dot_product([],[],Acc)->
	Acc.
		
		dot([I|Input],[{W,_LPs}|WeightsP],Acc) ->
			dot(Input,WeightsP,I*W+Acc);
		dot([],[],Acc)->
			Acc.
%The dot/3 function accepts an input vector and a weight list, and computes the dot product of the two vectors.

diff_product(IAcc,IWeightedInputProcessIDs)->
	case get(diff_product) of
		undefined ->
			put(diff_product,IAcc),
			dot_product(IAcc,IWeightedInputProcessIDs,0);
		Prev_IAcc ->
			put(diff_product,IAcc),
			Diff_IAcc = input_diff(IAcc,Prev_IAcc,[]),
			dot_product(Diff_IAcc,IWeightedInputProcessIDs,0)
	end.
	
	input_diff([{IProcessID,Input}|IAcc],[{IProcessID,Prev_Input}|Prev_IAcc],Acc)->
		Vector_Diff = diff(Input,Prev_Input,[]),
		input_diff(IAcc,Prev_IAcc,[{IProcessID,Vector_Diff}|Acc]);
	input_diff([],[],Acc)->
		lists:reverse(Acc).
	
		diff([A|Input],[B|Prev_Input],Acc)->
			diff(Input,Prev_Input,[A-B|Acc]);
		diff([],[],Acc)->
			lists:reverse(Acc).
			
mult_product(IAcc,IWeightedInputProcessIDs)->
	mult_product(IAcc,IWeightedInputProcessIDs,1).
mult_product([{IProcessID,Input}|IAcc],[{IProcessID,WeightsP}|IWeightedInputProcessIDs],Acc)->
	Dot = mult(Input,WeightsP,1),
	mult_product(IAcc,IWeightedInputProcessIDs,Dot*Acc);
mult_product([],[{bias,[{Bias,_LPs}]}],Acc)->
	Acc * Bias;
mult_product([],[],Acc)->
	Acc.

	mult([I|Input],[{W,_LPs}|Weights],Acc) ->
		mult(Input,Weights,I*W*Acc);
	mult([],[],Acc)->
		Acc.
