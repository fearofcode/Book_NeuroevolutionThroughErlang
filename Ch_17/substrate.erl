%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(substrate).
-compile(export_all).
-include("records.hrl").
-define(SAT_LIMIT,math:pi()).
-record(state,{
	type,
	plasticity=none,
	morphology,
	specie_id,
	sensors,
	actuators,
	spids=[],
	apids=[],
	cpp_pids=[],
	cep_pids=[],
	densities,
	substrate_state_flag,
	old_substrate,
	cur_substrate,
	link_form
}).

gen(ExoSelfProcess,Node)->
	spawn(Node,?MODULE,prep,[ExoSelfProcess]).

prep(ExoSelf)->
	random:seed(now()),
	receive
		{ExoSelf,init,InitState}->
			{Sensors,Actuators,SensorProcess,ActuatorProcess,CPPProcess,CEPProcess,Densities,Plasticity,LinkForm}=InitState,
			%io:format("InitState:~p~n",[InitState]),
			S = #state{
				sensors=Sensors,
				actuators=Actuators,
				spids=SensorProcess,
				apids=ActuatorProcess,
				cpp_pids=CPPProcess,
				cep_pids=CEPProcess,
				densities = Densities,
				substrate_state_flag=reset,
				old_substrate=void,
				cur_substrate=init,
				plasticity=Plasticity,
				link_form = LinkForm
			},
			substrate:loop(ExoSelf,S,SensorProcess,[])
	end.

loop(ExoSelf,S,[SensorProcess|SensorProcess],SAcc)->
	receive
		{SensorProcess,forward,Sensory_Signal}->
			loop(ExoSelf,S,SensorProcess,[Sensory_Signal|SAcc]);
		{ExoSelf,reset_substrate}->
			U_S = S#state{
				old_substrate=S#state.cur_substrate,
				substrate_state_flag=reset
			},
			ExoSelf ! {self(),ready},
			loop(ExoSelf,U_S,[SensorProcess|SensorProcess],SAcc);
		{ExoSelf,backup_substrate} ->
%			io:format("reseting:~n"),
			U_S = S#state{
				old_substrate=S#state.cur_substrate,
				substrate_state_flag=reset
			},
			ExoSelf ! {self(),ready},
			loop(ExoSelf,U_S,[SensorProcess|SensorProcess],SAcc);
		{ExoSelf,revert_substrate} ->
%			io:format("reverting:~n"),
			U_S = S#state{
				cur_substrate = S#state.old_substrate,
				substrate_state_flag=reset
			},
			ExoSelf ! {self(),ready},
			loop(ExoSelf,U_S,[SensorProcess|SensorProcess],SAcc);
		{ExoSelf,terminate}->
%			io:format("Resulting substrate:~p~n",[Substrate]),
			void
%		after 20000 ->
%			io:format("********ERROR: Substrate Crashed:~p~n",[S])
	end;
loop(ExoSelf,S,[],SAcc)->%All sensory signals received
	{U_Substrate,U_SMode,OAcc} = reason(SAcc,S),
	advanced_fanout(OAcc,S#state.actuators,S#state.apids),
	U_S = S#state{
		cur_substrate=U_Substrate,
		substrate_state_flag=U_SMode
	},
	loop(ExoSelf,U_S,S#state.spids,[]).

reason(Input,S)->
	Densities = S#state.densities,
	Substrate = S#state.cur_substrate,
	SMode = S#state.substrate_state_flag,
	CPPProcess = S#state.cpp_pids,
	CEPProcess = S#state.cep_pids,
	Plasticity = S#state.plasticity,
	case SMode of
		reset ->%io:format("reset~n"),
			Sensors=S#state.sensors,
			Actuators=S#state.actuators,
			New_Substrate = create_substrate(Sensors,Densities,Actuators,S#state.link_form),
			%io:format("New_Substrate:~p~n Output:~p~n Populated_Substrate:~p~n",[New_Substrate,Output,Populated_Substrate]),
			U_SMode=case Plasticity of
				iterative ->
					{Output,Populated_Substrate} = calculate_ResetOutput(Densities,New_Substrate,Input,CPPProcess,CEPProcess,Plasticity,S#state.link_form),
					iterative;
				_ ->
					{Output,Populated_Substrate} = calculate_ResetOutput(Densities,New_Substrate,Input,CPPProcess,CEPProcess,Plasticity,S#state.link_form),
					hold
			end,
			{Populated_Substrate,U_SMode,Output};
		iterative ->%io:format("Iterative~n"),
			{Output,U_Substrate} = calculate_ResetOutput(Densities,Substrate,Input,CPPProcess,CEPProcess,Plasticity,S#state.link_form),
%			io:format("Output:~p~n Densities:~p~n Substrate:~p~n U_Substrate:~p~n CT:~p~n CF:~p~n",[Output,Densities,Substrate,U_Substrate,CT,CF]),
			{U_Substrate,SMode,Output};
		hold ->%io:format("hold~n"),
			{Output,U_Substrate} = calculate_HoldOutput(Densities,Substrate,Input,S#state.link_form,Plasticity,CPPProcess,CEPProcess),
			%io:format("Substrate:~p~n U_Substrate:~p~n",[Substrate,U_Substrate]),
			%io:format("Output1:~p Output:~p~n",[Output,Output]),
			{U_Substrate,SMode,Output}
	end.

advanced_fanout(OAcc,[Actuator|Actuators],[ActuatorProcess|ActuatorProcess])->
	{Output,OAccRem}=lists:split(Actuator#actuator.vector_length,OAcc),
	ActuatorProcess ! {self(),forward,Output},
	advanced_fanout(OAccRem,Actuators,ActuatorProcess);
advanced_fanout([],[],[])->
	ok.
%%==================================================================== Internal Functions
fanout([Pid|Pids],Msg)->
	Pid ! Msg,
	fanout(Pids,Msg);
fanout([],_Msg)->
	true.

flush_buffer()->
	receive 
		ANY -> %io:format("ANY:~p~n",[ANY]),
		flush_buffer()
	after 0 ->
		done
end.

%	no_geo
%	{symetric,[R1,R2...Rk],[Val1...Valn]} where n == R1*R2*...Dk and k = dimension
%	{asymetric,[[R1..Rp],[R1..Rt]],[Val1...Valn]} where lists:sum(lists:flatten([[R1...Rp],[R1..Rt]])) == n, and depth = Dimension.
%	coorded, every val comes with its own coord tuple: {Coord,Val}. The coord is a list, thus specifying the dimensionality.
test_cs()->
	Sensors = [
		#sensor{format=no_geo,vector_length=3},
		#sensor{format={symetric,lists:reverse([2,3])},vector_length=6}
	],
	Actuators = [
		#actuator{format=no_geo,vector_length=2},
		#actuator{format={symetric,lists:reverse([3,2])},vector_length=6}
	],
	create_substrate(Sensors,[3,2,3,2],Actuators,l2l_feedforward).
	
test_IS(SubstrateDimension)->
	Sensors = [
		#sensor{format=no_geo,vector_length=10},
		#sensor{format={symetric,lists:reverse([3,4])},vector_length=[
		1,-1,-1,-1,
		1,-1,-1,-1,
		1,1,1,1]}
	],
	compose_ISubstrate(Sensors,SubstrateDimension).

test_OS(SubstrateDimension)->
	Actuators = [
		#actuator{format=no_geo,vector_length=10},
		#actuator{format={symetric,lists:reverse([3,4])},vector_length=[
		1,-1,-1,-1,
		1,-1,-1,-1,
		1,1,1,1]}
	],
	compose_OSubstrate(Actuators,SubstrateDimension,[w1,w2,w3]).
	
create_substrate(Sensors,Densities,Actuators,LinkForm)->
	[Depth|SubDensities] = Densities,
	Substrate_I = compose_ISubstrate(Sensors,length(Densities)),
	IVL = length(Substrate_I),
	case LinkForm of
		l2l_feedforward ->
			Weight = 0,
			H = mult(SubDensities),
			IWeights = lists:duplicate(IVL,Weight),
			HWeights = lists:duplicate(H,Weight);
		fully_interconnected ->
			Output_Neurodes = tot_ONeurodes(Actuators,0),
			Weight = 0,
			Total_HiddenNeurodes = mult([Depth-1|SubDensities]),
			Total_Weights = Total_HiddenNeurodes + IVL + Output_Neurodes,
			IWeights = lists:duplicate(Total_Weights,Weight),
			HWeights = lists:duplicate(Total_Weights,Weight);
		jordan_recurrent ->
			Output_Neurodes = tot_ONeurodes(Actuators,0),
			Weight = 0,
			H = mult(SubDensities),
			IWeights = lists:duplicate(IVL+Output_Neurodes,Weight),
			HWeights = lists:duplicate(H,Weight);
		neuronself_recurrent ->
			Weight = 0,
			H = mult(SubDensities),
			IWeights = lists:duplicate(IVL+1,Weight),
			HWeights = lists:duplicate(H+1,Weight)
	end,	
	case Depth of
		0 ->
			Substrate_O=compose_OSubstrate(Actuators,length(Densities),IWeights),
			[Substrate_I,Substrate_O];
		1 ->
			Substrate_R = cs(SubDensities,IWeights),
			Substrate_O=compose_OSubstrate(Actuators,length(Densities),HWeights),
%			io:format("Substrate_I:~n~p~n Substrate_R:~n~p~n Substrate_O:~n~p~n",[Substrate_I,Substrate_R,Substrate_O]),
			[Substrate_I,extrude(0,Substrate_R),Substrate_O];
		_ ->
			Substrate_R = cs(SubDensities,IWeights),
			Substrate_H = cs(SubDensities,HWeights),
			Substrate_O=compose_OSubstrate(Actuators,length(Densities),HWeights),
			%io:format("OResolutions:~p Substrate_O:~p~n",[OResolutions,Substrate_O]),
			[_,RCoord|C1] = build_CoordList(Depth+1),
			[_|C2] = lists:reverse(C1),
			HCoords = lists:reverse(C2),
			%io:format("RCoord:~p HCoord:~p~n",[RCoord,HCoords]),
			ESubstrate_R = extrude(RCoord,Substrate_R),
			ESubstrates_H = [extrude(HCoord,Substrate_H) || HCoord<-HCoords],
			%io:format("ESubstrate_R:~p ESubstrates_H:~p~n",[ESubstrate_R,ESubstrates_H]),
			lists:append([[Substrate_I,ESubstrate_R],ESubstrates_H,[Substrate_O]])
	end.

	compose_ISubstrate(Sensors,SubstrateDimension)->
		compose_ISubstrate(Sensors,[],1,SubstrateDimension-2).
	compose_ISubstrate([S|Sensors],Acc,Max_Dim,Required_Dim)->
		case S#sensor.format of
			undefined ->
				Dim=1,
				CoordLists = create_CoordLists([S#sensor.vector_length]),
				ISubstrate_Part=[{Coord,0,void}|| Coord<-CoordLists],
				{Dim,ISubstrate_Part};
			no_geo ->
				Dim=1,
				CoordLists = create_CoordLists([S#sensor.vector_length]),
				ISubstrate_Part=[{Coord,0,void}|| Coord<-CoordLists],
				{Dim,ISubstrate_Part};
			{symetric,Resolutions}->
				Dim = length(Resolutions),
				Signal_Length = mult(Resolutions),
				CoordLists = create_CoordLists(Resolutions),
				ISubstrate_Part=[{Coord,0,void}|| Coord<-CoordLists],
				{Dim,ISubstrate_Part};
			{coorded,Dim,Resolutions,ISubstrate_Part} ->
				{Dim,ISubstrate_Part}
		end,
		U_Dim = case Max_Dim > Dim of
			true ->
				Max_Dim;
			false ->
				Dim
		end,
		compose_ISubstrate(Sensors,[ISubstrate_Part|Acc],U_Dim,Required_Dim);
	compose_ISubstrate([],Acc,ISubstratePart_MaxDim,Required_Dim)->
		case Required_Dim >= ISubstratePart_MaxDim of
			true ->
				ISubstrate_Depth = length(Acc),
				ISubstrate_DepthCoords = build_CoordList(ISubstrate_Depth),
				adv_extrude(Acc,Required_Dim,lists:reverse(ISubstrate_DepthCoords),-1,[]);%Passed in inverted,reversed inside adv_extrude, same for depth coords.
			false ->
				exit("Error in adv_extrude, Required_Depth < ISubstratePart_MaxDepth~n")
		end.

		adv_extrude([ISubstrate_Part|ISubstrate],Required_Dim,[IDepthCoord|ISubstrate_DepthCoords],LeadCoord,Acc)->
			Extruded_ISP = [{[LeadCoord,IDepthCoord|lists:append(lists:duplicate(Required_Dim - length(Coord),0),Coord)],O,W} || {Coord,O,W}<-ISubstrate_Part],
			extrude(ISubstrate_Part,Required_Dim,IDepthCoord,[]),
			adv_extrude(ISubstrate,Required_Dim,ISubstrate_DepthCoords,LeadCoord,lists:append(Extruded_ISP,Acc));
		adv_extrude([],_Required_Dim,[],_LeadCoord,Acc)->
			Acc.
			
			extrude([{Coord,O,W}|ISubstrate_Part],Required_Dim,DepthCoord,Acc)->
				Dim_Dif = Required_Dim - length(Coord),
				U_Coord= [1,DepthCoord|lists:append(lists:duplicate(Dim_Dif,0),Coord)],
				extrude(ISubstrate_Part,Required_Dim,DepthCoord,[{U_Coord,O,W}|Acc]);
			extrude([],_Required_Dim,_DepthCoord,Acc)->
				Acc.

	compose_OSubstrate(Actuators,SubstrateDimension,Weights)->
		compose_OSubstrate(Actuators,[],1,SubstrateDimension-2,Weights).
	compose_OSubstrate([A|Actuators],Acc,Max_Dim,Required_Dim,Weights)->
		case A#actuator.format of
			undefined ->%Dim=void,OSubstrate_Part=void,
				Dim=1,
				CoordLists = create_CoordLists([A#actuator.vector_length]),
				OSubstrate_Part=[{Coord,0,Weights}|| Coord<-CoordLists],
				{Dim,OSubstrate_Part};
			no_geo ->%Dim=void,OSubstrate_Part=void,
				Dim=1,
				CoordLists = create_CoordLists([A#actuator.vector_length]),
				OSubstrate_Part=[{Coord,0,Weights}|| Coord<-CoordLists],
				{Dim,OSubstrate_Part};
			{symetric,Resolutions}->%Dim=void,OSubstrate_Part=void,
				Dim = length(Resolutions),
				Signal_Length = mult(Resolutions),
				CoordLists = create_CoordLists(Resolutions),
				OSubstrate_Part=[{Coord,0,Weights}|| Coord<-CoordLists],
				{Dim,OSubstrate_Part};
			{coorded,Dim,Resolutions,Unadjusted_OSubstrate_Part} ->
				OSubstrate_Part=[{Coord,O,Weights}|| {Coord,O,_}<-Unadjusted_OSubstrate_Part],
				{Dim,OSubstrate_Part}
		end,
		U_Dim = case Max_Dim > Dim of
			true ->
				Max_Dim;
			false ->
				Dim
		end,
		compose_OSubstrate(Actuators,[OSubstrate_Part|Acc],U_Dim,Required_Dim,Weights);
	compose_OSubstrate([],Acc,OSubstratePart_MaxDim,Required_Dim,_Weights)->
		case Required_Dim >= OSubstratePart_MaxDim of
			true ->%done;
				ISubstrate_Depth = length(Acc),
				ISubstrate_DepthCoords = build_CoordList(ISubstrate_Depth),
				adv_extrude(Acc,Required_Dim,lists:reverse(ISubstrate_DepthCoords),1,[]);%Passed in inverted,reversed inside adv_extrude, same for depth coord
			false ->
				exit("Error in adv_extrude, Required_Depth < OSubstratePart_MaxDepth~n")
		end.

		find_depth(Resolutions)->find_depth(Resolutions,0).
		find_depth(Resolutions,Acc)->
			case is_list(Resolutions) of
				true ->
					[_Head|Tail] = Resolutions,
					find_depth(Tail,Acc+1);
				false ->
					Acc
			end.

%Substrate encoding: X density = n, Y density = k, Z density = p, T density = l
%Weights = [W1,W2...WI],
%[[{[Z1,Y,X],o,[W1...Wn]}...{[Z1,Yn,Xk],o,[W1...Wn]}]...[{[Zs,Y,X],o,[W1...Wn]}...]],
		build_CoordList(Density)->
			case Density == 1 of
				true ->
					[0.0];
				false ->
					DensityDividers = Density - 1,
					Resolution = 2/DensityDividers,
					build_CoordList(Resolution,DensityDividers,1,[])
			end.

			extend(I,DI,D,Substrate)->
				void.
				
			mult(List)->
				mult(List,1).
			mult([Val|List],Acc)->
				mult(List,Val*Acc);
			mult([],Acc)->
				Acc.

tot_ONeurodes([A|Actuators],Acc)->
	Total_ANeurodes=case A#actuator.format of
		undefined ->
			A#actuator.vector_length;
		no_geo ->
			A#actuator.vector_length;
		{symetric,Resolutions}->
			mult(Resolutions);
		{coorded,Dim,Resolutions,Unadjusted_OSubstrate_Part} ->
			length(Unadjusted_OSubstrate_Part)
	end,
	tot_ONeurodes(Actuators,Total_ANeurodes+Acc);
tot_ONeurodes([],Acc)->
	Acc.


%[{[D3,D2,D1],o,[W1,W2,W3...]}...]
	cs(Densities,Weights)->
		RDensities = lists:reverse(Densities),
		Substrate = create_CoordLists(RDensities,[]),
		attach(Substrate,0,Weights).
	
		create_CoordLists(Densities)->
			create_CoordLists(Densities,[]).	
		create_CoordLists([Density|RDensities],[])->
			CoordList = build_CoordList(Density),
			XtendedCoordList = [[Coord]||Coord <- CoordList],
			create_CoordLists(RDensities,XtendedCoordList);
		create_CoordLists([Density|RDensities],Acc)->
			CoordList = build_CoordList(Density),
			XtendedCoordList = [[Coord|Sub_Coord]||Coord <- CoordList,Sub_Coord <- Acc],
			create_CoordLists(RDensities,XtendedCoordList);
		create_CoordLists([],Acc)->
			Acc.
			
			build_CoordList(Resolution,0,Coord,Acc)->
				[-1|Acc];
			build_CoordList(Resolution,DensityDividers,Coord,Acc)->
				build_CoordList(Resolution,DensityDividers-1,Coord-Resolution,[Coord|Acc]).
		
attach(List,E1,E2)->
	attach(List,E1,E2,[]).
attach([Val|List],E1,E2,Acc)->
	attach(List,E1,E2,[{Val,E1,E2}|Acc]);
attach([],_E1,_E2,Acc)->
	lists:reverse(Acc).
	
extrude(NewDimension_Coord,Substrate)->
	extrude(NewDimension_Coord,Substrate,[]).
extrude(NewDimension_Coord,[{Coord,O,W}|Substrate],Acc)->
	extrude(NewDimension_Coord,Substrate,[{[NewDimension_Coord|Coord],O,W}|Acc]);
extrude(_Coord,[],Acc)->
	lists:reverse(Acc).

calculate_HoldOutput(Densities,Substrate,Input,LinkForm,Plasticity,CPPProcess,CEPProcess)->
	[IHyperlayer|Populated_PHyperlayers] = Substrate,
	Populated_IHyperlayer = populate_InputHyperlayer(IHyperlayer,lists:flatten(Input),[]),
	{Output,U_PHyperlayers}=calculate_substrate_output(Populated_IHyperlayer,Populated_PHyperlayers,LinkForm,Plasticity,CPPProcess,CEPProcess),
	{Output,[IHyperlayer|U_PHyperlayers]}.

calculate_ResetOutput(Densities,Substrate,Input,CPPProcess,CEPProcess,Plasticity,LinkForm)->
	[IHyperlayer|PHyperlayers] = Substrate,
	%io:format("IHyperlayer:~p~n PHyperlayers:~p~n",[IHyperlayer,PHyperlayers]),
	Populated_IHyperlayer = populate_InputHyperlayer(IHyperlayer,lists:flatten(Input),[]),
	case Plasticity of
		iterative ->
			{Output,U_PHyperlayers}=calculate_substrate_output(Populated_IHyperlayer,PHyperlayers,LinkForm,Plasticity,CPPProcess,CEPProcess),
			{Output,[IHyperlayer|U_PHyperlayers]};
		_->
			Populated_PHyperlayers = populate_PHyperlayers(Substrate,CPPProcess,CEPProcess,LinkForm,Plasticity),
			{Output,U_PHyperlayers}=calculate_substrate_output(Populated_IHyperlayer,Populated_PHyperlayers,LinkForm,Plasticity,CPPProcess,CEPProcess),
			{Output,[IHyperlayer|U_PHyperlayers]}
	end.

	populate_InputHyperlayer([{Coord,PrevO,void}|Substrate],[I|Input],Acc)->
		populate_InputHyperlayer(Substrate,Input,[{Coord,I,void}|Acc]);
	populate_InputHyperlayer([],[],Acc)->
		lists:reverse(Acc).
		
	populate_PHyperlayers(Substrate,CPPProcess,CEPProcess,LinkForm,Plasticity)->
		case LinkForm of
			l2l_feedforward ->
				[IHyperlayer,PHyperlayer|RemSubstrate] = Substrate,
				%io:format("Substrate:~p~n",[Substrate]),
				populate_PHyperlayers_l2l(IHyperlayer,PHyperlayer,RemSubstrate,CPPProcess,CEPProcess,Plasticity,[],[]);
			fully_interconnected ->
				[_IHyperlayer,PHyperlayer|RemSubstrate] = Substrate,
				I_Neurodes = lists:flatten(Substrate),
				populate_PHyperlayers_fi(I_Neurodes,PHyperlayer,RemSubstrate,CPPProcess,CEPProcess,Plasticity,[],[]);
			jordan_recurrent ->
				[IHyperlayer,PHyperlayer|RemSubstrate] = Substrate,
				[OHyperlayer|_]=lists:reverse(Substrate),
				I_Neurodes=lists:flatten([IHyperlayer,OHyperlayer]),
				populate_PHyperlayers_l2l(I_Neurodes,PHyperlayer,RemSubstrate,CPPProcess,CEPProcess,Plasticity,[],[]);
			neuronself_recurrent ->
				[IHyperlayer,PHyperlayer|RemSubstrate] = Substrate,
				populate_PHyperlayers_nsr(IHyperlayer,PHyperlayer,RemSubstrate,CPPProcess,CEPProcess,Plasticity,[],[])
		end.
	
		populate_PHyperlayers_l2l(PrevHyperlayer,[{Coord,PrevO,PrevWeights}|CurHyperlayer],Substrate,CPPProcess,CEPProcess,Plasticity,Acc1,Acc2)->
			NewWeights = case Plasticity of
				none -> 
					get_weights(PrevHyperlayer,Coord,CPPProcess,CEPProcess,[]);
				_ ->
					get_weights(PrevHyperlayer,Coord,CPPProcess,CEPProcess,[],PrevWeights,PrevO)
			end,
			populate_PHyperlayers_l2l(PrevHyperlayer,CurHyperlayer,Substrate,CPPProcess,CEPProcess,Plasticity,[{Coord,PrevO,NewWeights}|Acc1],Acc2);
		populate_PHyperlayers_l2l(_PrevHyperlayer,[],[CurHyperlayer|Substrate],CPPProcess,CEPProcess,Plasticity,Acc1,Acc2)->
			PrevHyperlayer = lists:reverse(Acc1),
			populate_PHyperlayers_l2l(PrevHyperlayer,CurHyperlayer,Substrate,CPPProcess,CEPProcess,Plasticity,[],[PrevHyperlayer|Acc2]);
		populate_PHyperlayers_l2l(_PrevHyperlayer,[],[],CPPProcess,CEPProcess,Plasticity,Acc1,Acc2)->
			lists:reverse([lists:reverse(Acc1)|Acc2]).

		populate_PHyperlayers_fi(FlatSubstrate,[{Coord,PrevO,PrevWeights}|CurHyperlayer],Substrate,CPPProcess,CEPProcess,Plasticity,Acc1,Acc2)->
			NewWeights = case Plasticity of
				none -> 
					get_weights(FlatSubstrate,Coord,CPPProcess,CEPProcess,[]);
				_ ->
					get_weights(FlatSubstrate,Coord,CPPProcess,CEPProcess,[],PrevWeights,PrevO)
			end,
			populate_PHyperlayers_fi(FlatSubstrate,CurHyperlayer,Substrate,CPPProcess,CEPProcess,Plasticity,[{Coord,PrevO,NewWeights}|Acc1],Acc2);
		populate_PHyperlayers_fi(FlatSubstrate,[],[CurHyperlayer|Substrate],CPPProcess,CEPProcess,Plasticity,Acc1,Acc2)->
			populate_PHyperlayers_fi(FlatSubstrate,CurHyperlayer,Substrate,CPPProcess,CEPProcess,Plasticity,[],[lists:reverse(Acc1)|Acc2]);
		populate_PHyperlayers_fi(_FlatSubstrate,[],[],CPPProcess,CEPProcess,Plasticity,Acc1,Acc2)->
			lists:reverse([lists:reverse(Acc1)|Acc2]).

		populate_PHyperlayers_nsr(PrevHyperlayer,[{Coord,PrevO,PrevWeights}|CurHyperlayer],Substrate,CPPProcess,CEPProcess,Plasticity,Acc1,Acc2)->
			NewWeights = case Plasticity of
				none -> 
					get_weights([{Coord,PrevO,PrevWeights}|PrevHyperlayer],Coord,CPPProcess,CEPProcess,[]);
				_ ->
					get_weights([{Coord,PrevO,PrevWeights}|PrevHyperlayer],Coord,CPPProcess,CEPProcess,[],PrevWeights,PrevO)
			end,
			populate_PHyperlayers_nsr(PrevHyperlayer,CurHyperlayer,Substrate,CPPProcess,CEPProcess,Plasticity,[{Coord,PrevO,NewWeights}|Acc1],Acc2);
		populate_PHyperlayers_nsr(_PrevHyperlayer,[],[CurHyperlayer|Substrate],CPPProcess,CEPProcess,Plasticity,Acc1,Acc2)->
			PrevHyperlayer = lists:reverse(Acc1),
			populate_PHyperlayers_nsr(PrevHyperlayer,CurHyperlayer,Substrate,CPPProcess,CEPProcess,Plasticity,[],[PrevHyperlayer|Acc2]);
		populate_PHyperlayers_nsr(_PrevHyperlayer,[],[],CPPProcess,CEPProcess,Plasticity,Acc1,Acc2)->
			lists:reverse([lists:reverse(Acc1)|Acc2]).
						
			get_weights([{I_Coord,I,_I_Weights}|I_Neurodes],Coord,CPPProcess,CEPProcess,Acc)->
				static_fanout(CPPProcess,I_Coord,Coord),
				U_W=fanin(CEPProcess,void),
				get_weights(I_Neurodes,Coord,CPPProcess,CEPProcess,[U_W|Acc]);
			get_weights([],_Coord,_CPPProcess,_CEPProcess,Acc)->
				lists:reverse(Acc).

				static_fanout([CPPProcess|CPPProcess],I_Coord,Coord)->
					%io:format("CPPProcess:~p~n",[CPPProcess]),
					CPPProcess ! {self(),I_Coord,Coord},
					static_fanout(CPPProcess,I_Coord,Coord);
				static_fanout([],_I_Coord,_Coord)->
					done.
					
				fanin([CEPProcess|CEPProcess],W)->
					receive
						{CEPProcess,Command,Signal}->
							U_W=substrate:Command(Signal,W)
					end,
					fanin(CEPProcess,U_W);
				fanin([],W)->
					W.
				
			get_weights([{I_Coord,I,_I_Weights}|I_Neurodes],Coord,CPPProcess,CEPProcess,Acc,[W|Weights],O)->
				plasticity_fanout(CPPProcess,I_Coord,Coord,[I,O,W]),
				U_W=fanin(CEPProcess,W),
				get_weights(I_Neurodes,Coord,CPPProcess,CEPProcess,[U_W|Acc],Weights,O);
			get_weights([],_Coord,CPPProcess,CEPProcess,Acc,[],_O)->
				lists:reverse(Acc).

				plasticity_fanout([CPPProcess|CPPProcess],I_Coord,Coord,IOW)->
					CPPProcess ! {self(),I_Coord,Coord,IOW},
					plasticity_fanout(CPPProcess,I_Coord,Coord,IOW);
				plasticity_fanout([],_I_Coord,_Coord,_IOW)->
					done.
							
					set_weight(Signal,_WP)->
						[U_W] = Signal,
						functions:sat(U_W,3.1415,-3.1415).
						
					weight_expression(Signal,_WP) ->
						[U_W,Expression]=Signal,
						case Expression > 0 of
							true ->
								functions:sat(U_W,3.1415,-3.1415);
							false ->
								0
						end.
					
					set_abcn(Signal,_WP)->
						[U_W,A,B,C,N] = Signal,
						{functions:sat(U_W,3.1415,-3.1415),abcn,[A,B,C,N]}.
						
					set_iterative(Signal,W)->
						[Delta_Weight] = Signal,
						functions:sat(W + Delta_Weight,3.1415,-3.1415).
						
		calculate_substrate_output(IHyperlayer,PHyperlayer,LinkForm,Plasticity,CPPProcess,CEPProcess)->
			case LinkForm of
				l2l_feedforward ->
					calculate_output_std(IHyperlayer,PHyperlayer,Plasticity,CPPProcess,CEPProcess,[]);
				fully_interconnected ->
					calculate_output_fi(lists:flatten([IHyperlayer|PHyperlayer]),PHyperlayer,Plasticity,CPPProcess,CEPProcess,[]);
				jordan_recurrent ->
					[OHyperlayer|_] = lists:reverse(PHyperlayer,Plasticity),
					calculate_output_std(lists:flatten([IHyperlayer|OHyperlayer]),PHyperlayer,Plasticity,CPPProcess,CEPProcess,[]);
				neuronself_recurrent ->
					calculate_output_nsr(IHyperlayer,PHyperlayer,Plasticity,CPPProcess,CEPProcess,[])
			end.
			
		calculate_output_std(I_Neurodes,[Cur_Hyperlayer|Substrate],Plasticity,CPPProcess,CEPProcess,Acc)->
			U_CurHyperlayer = [calculate_output(I_Neurodes,Neurode,Plasticity,CPPProcess,CEPProcess) || Neurode <- Cur_Hyperlayer],
			calculate_output_std(U_CurHyperlayer,Substrate,Plasticity,CPPProcess,CEPProcess,[U_CurHyperlayer|Acc]);
		calculate_output_std(Output_Hyperlayer,[],_Plasticity,CPPProcess,CEPProcess,Acc)->
			{[Output || {_Coord,Output,_Weights} <- Output_Hyperlayer],lists:reverse(Acc)}.
			
			calculate_output(I_Neurodes,Neurode,Plasticity,CPPProcess,CEPProcess)->
				{Coord,_Prev_O,Weights} = Neurode,
				case Plasticity of
					none ->
						Output=calculate_neurode_output_std(I_Neurodes,Neurode,0),
						{Coord,Output,Weights};
					iterative ->
						Output=calculate_neurode_output_std(I_Neurodes,Neurode,0),
						U_Weights = get_weights(I_Neurodes,Coord,CPPProcess,CEPProcess,[],Weights,Output),
						{Coord,Output,U_Weights};
					abcn ->
						Output=calculate_neurode_output_plast(I_Neurodes,Neurode,0),
						update_neurode(I_Neurodes,{Coord,Output,Weights},[])
				end.
			
					calculate_neurode_output_std([{_I_Coord,O,_I_Weights}|I_Neurodes],{Coord,Prev_O,[Weight|Weights]},Acc)->
						calculate_neurode_output_std(I_Neurodes,{Coord,Prev_O,Weights},O*Weight+Acc);
					calculate_neurode_output_std([],{Coord,Prev_O,[]},Acc)->
						functions:tanh(Acc).
					
					calculate_neurode_output_plast([{_I_Coord,O,_I_Weights}|I_Neurodes],{Coord,Prev_O,[{W,_LF,_Parameters}|WPs]},Acc)->
						calculate_neurode_output_plast(I_Neurodes,{Coord,Prev_O,WPs},O*W+Acc);
					calculate_neurode_output_plast([],{Coord,Prev_O,[]},Acc)->
						functions:tanh(Acc).
						
					update_neurode([{_I_Coord,I_O,_I_Weights}|I_Neurodes],{Coord,O,[{W,LF,Parameters}|WPs]},Acc)->
						U_W = substrate:LF(I_O,O,W,Parameters),
						update_neurode(I_Neurodes,{Coord,O,WPs},[{U_W,LF,Parameters}|Acc]);
					update_neurode([],{Coord,O,[]},Acc)->
						{Coord,O,lists:reverse(Acc)}.
			
						abcn(Input,Output,W,[A,B,C,N])->
							Delta_Weight = N*(A*Input*Output + B*Input + C*Output),
							W+Delta_Weight.
			
		calculate_output_fi(I_Neurodes,[Cur_Hyperlayer|Substrate],Plasticity,CPPProcess,CEPProcess,Acc)->
			U_CurHyperlayer = [calculate_output(I_Neurodes,Neurode,Plasticity,CPPProcess,CEPProcess) || Neurode <- Cur_Hyperlayer],
			calculate_output_fi([I_Neurodes|U_CurHyperlayer],Substrate,Plasticity,CPPProcess,CEPProcess,[U_CurHyperlayer|Acc]);
		calculate_output_fi(Output_Hyperlayer,[],_Plasticity,CPPProcess,CEPProcess,Acc)->
			{[Output || {_Coord,Output,_Weights} <- Output_Hyperlayer],lists:reverse(Acc)}.
			
		calculate_output_nsr(I_Neurodes,[Cur_Hyperlayer|Substrate],Plasticity,CPPProcess,CEPProcess,Acc)->
			U_CurHyperlayer = [calculate_output([Neurode|I_Neurodes],Neurode,Plasticity,CPPProcess,CEPProcess) || Neurode <- Cur_Hyperlayer],
			calculate_output_nsr(U_CurHyperlayer,Substrate,Plasticity,CPPProcess,CEPProcess,[U_CurHyperlayer|Acc]);
		calculate_output_nsr(Output_Hyperlayer,[],_Plasticity,CPPProcess,CEPProcess,Acc)->
			{[Output || {_Coord,Output,_Weights} <- Output_Hyperlayer],lists:reverse(Acc)}.
