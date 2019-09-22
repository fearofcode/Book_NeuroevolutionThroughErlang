%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(scape).
-compile(export_all).
-include("records.hrl").

gen(ExoSelfProcess,Node)->
	spawn(Node,?MODULE,prep,[ExoSelfProcess]).

prep(ExoSelfProcess) ->
	receive 
		{ExoSelfProcess,Name} ->
			scape:Name(ExoSelfProcess)
	end.

xor_sim(ExoSelfProcess)->
	XOR = [{[-1,-1],[-1]},{[1,-1],[1]},{[-1,1],[1]},{[1,1],[-1]}],
	xor_sim(ExoSelfProcess,{XOR,XOR},0).
	
xor_sim(ExoSelfProcess,{[{Input,CorrectOutput}|XOR],MXOR},ErrAcc) ->
	receive 
		{From,sense} ->
			From ! {self(),percept,Input},
			xor_sim(ExoSelfProcess,{[{Input,CorrectOutput}|XOR],MXOR},ErrAcc);
		{From,action,Output}->
			Error = sse(Output,CorrectOutput,0),
			%io:format("{Output,TargetOutput}:~p~n",[{Output,CorrectOutput}]),
			case XOR of
				[] ->
					SSE = ErrAcc+Error,
					Fitness = 1/(SSE+0.000001),
					From ! {self(),Fitness,1},
					xor_sim(ExoSelfProcess,{MXOR,MXOR},0);
				_ ->
					From ! {self(),0,0},
					xor_sim(ExoSelfProcess,{XOR,MXOR},ErrAcc+Error)
			end;
		{ExoSelfProcess,terminate}->
			ok
	end.

		
	sse([T|Target],[O|Output],SSEAcc)->
		SSE = math:pow(T-O,2),
		sse(Target,Output,SSE+SSEAcc);
	sse([],[],SSEAcc)->
		SSEAcc.
