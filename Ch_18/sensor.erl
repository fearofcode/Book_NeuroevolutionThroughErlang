%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(sensor).
-compile(export_all).
-include("records.hrl").

gen(ExoSelfProcess,Node)->
	spawn(Node,?MODULE,prep,[ExoSelfProcess]).

prep(ExoSelfProcess) ->
	receive 
		{ExoSelfProcess,{Id,CortexProcess,Scape,SensorName,VL,Parameters,FanoutProcess}} ->
			loop(Id,ExoSelfProcess,CortexProcess,Scape,SensorName,VL,Parameters,FanoutProcess)
	end.
%When gen/2 is executed it spawns the sensor element and immediately begins to wait for its initial state message.

loop(Id,ExoSelfProcess,CortexProcess,Scape,SensorName,VL,Parameters,FanoutProcess)->
	receive
		{CortexProcess,sync}->
			SensoryVector = sensor:SensorName(ExoSelfProcess,VL,Parameters,Scape),
			[Pid ! {self(),forward,SensoryVector} || Pid <- FanoutProcess],
			loop(Id,ExoSelfProcess,CortexProcess,Scape,SensorName,VL,Parameters,FanoutProcess);
		{ExoSelfProcess,terminate} ->
			%io:format("Sensor:~p is terminating.~n",[Id]),
			ok
	end.
%The sensor process accepts only 2 types of messages, both from the cortex. The sensor can either be triggered to begin gathering sensory data based on its sensory role, or terminate if the cortex requests so.

rng(Exoself,VL,_Scape)->
	rng1(VL,[]).
rng1(0,Acc)->
	Acc;
rng1(VL,Acc)-> 
	rng1(VL-1,[rand:uniform()|Acc]).
%rng/2 is a simple random number generator that produces a vector of random values, each between 0 and 1. The length of the vector is defined by the VL, which itself is specified within the sensor record.

xor_GetInput(Exoself,VL,_Parameters,Scape)->
	Scape ! {self(),sense},
	receive
		{Scape,percept,SensoryVector}->
			case length(SensoryVector)==VL of
				true ->
					SensoryVector;
				false ->
					io:format("Error in sensor:xor_sim/3, VL:~p SensoryVector:~p~n",[VL,SensoryVector]),
					lists:duplicate(VL,0)
			end
	end.
%xor_GetInput/2 contacts the XOR simulator and requests the sensory vector, which in this case should be a binary vector of length 2. The sensor checks that the incoming sensory signal, the percept, is indeed of length 2. If the vector length differs, then this is printed to the console and a dummy vector of appropriate length is constructed.

pb_GetInput(Exoself,VL,Parameters,Scape)->
	Scape ! {self(),sense,Parameters},
	receive
		{Scape,percept,SensoryVector}->
			case length(SensoryVector)==VL of
				true ->
					SensoryVector;
				false ->
					io:format("Error in sensor:pb_GetInput/3, VL:~p SensoryVector:~p~n",[VL,SensoryVector]),
					lists:duplicate(VL,0)
			end
	end.
	
dtm_GetInput(Exoself,VL,Parameters,Scape)->
	Scape ! {self(),sense,Parameters},
	receive
		{Scape,percept,SensoryVector}->
			%io:format("self():~p SensoryVector:~p~n",[self(),SensoryVector]),
			case length(SensoryVector)==VL of
				true ->
					SensoryVector;
				false ->
					io:format("Error in sensor:dtm_GetInput/3, VL:~p SensoryVector:~p~n",[VL,SensoryVector]),
					lists:duplicate(VL,0)
			end
	end.
	
distance_scanner(ExoselfProcess,VL,[Spread,Density,RadialOffset],Scape)->
	case gen_server:call(Scape,{get_all,avatars,ExoselfProcess}) of
		destroyed->
			lists:duplicate(VL,-1);
		Avatars ->
			Self = lists:keyfind(ExoselfProcess,2,Avatars),
			Loc = Self#avatar.loc,
			Direction = Self#avatar.direction,
			Result=distance_scanner(silent,{1,0,0},Density,Spread,Loc,Direction,lists:keydelete(ExoselfProcess, 2, Avatars)),
			Result
	end.

color_scanner(ExoselfProcess,VL,[Spread,Density,RadialOffset],Scape)->
	case gen_server:call(Scape,{get_all,avatars,ExoselfProcess}) of
		destroyed->
			lists:duplicate(VL,-1);
		Avatars ->%io:format("Avatars:~p~n",[Avatars]),
			Self = lists:keyfind(ExoselfProcess,2,Avatars),
			%io:format("Self:~p~n",[Self]),
			Loc = Self#avatar.loc,
			Direction = Self#avatar.direction,
			Result=color_scanner(silent,{1,0,0},Density,Spread,Loc,Direction,lists:keydelete(ExoselfProcess, 2, Avatars)),
			Result
	end.
	
%Input: ViewAngle= Radian, Density= n, Gaze direction= {SensorLoc,Direction}.
%Output: List of ranges 1/Distance no intersection = -1, with angle starting with Gaze + (ViewAngle/2), and ending with (Gaze - ViewAngle/2), [Dist1...DistDensity].
	distance_scanner(Op,{Zoom,PanX,PanY},Density,Spread,Loc,Direction,Avatars)->
		case is_even(Density) of
			true ->
				Resolution = Spread/Density,
				SAngle = (Density/2)*Resolution,
				StartAngle = -SAngle+Resolution/2;
			false ->
				Resolution = Spread/Density,
				SAngle=trunc(Density/2)*Resolution,
				StartAngle = -SAngle
		end,
		UnitRays = create_UnitRays(Direction,Density,Resolution,StartAngle,[]),
		RangeScanList = compose_RangeScanList(Loc,UnitRays,Avatars,[]),
		%io:format("RangeScanList:~p~n",[RangeScanList]),
		case {Op,get(canvas)} of
			{silent,_} ->
				done;
			{draw,undefined} ->
				Canvas = gen_server:call(get(scape),get_canvas),
				put(canvas,Canvas);
			{draw,Canvas}->
				{X,Y} = Loc,
				FLoc = {X*Zoom+PanX,Y*Zoom+PanY},
				ScanListP=lists:zip(UnitRays,RangeScanList),
				Ids = [gs:create(line,Canvas,[{coords,[FLoc,{(X+Xr*Scale)*Zoom+PanX,(Y+Yr*Scale)*Zoom+PanY}]}])||{{Xr,Yr},Scale}<-ScanListP, Scale =/= -1],
				timer:sleep(2),
				[gs:destroy(Id) || Id<- Ids]
		end,
		RangeScanList.
		
		compose_RangeScanList(Loc,[Ray|UnitRays],Avatars,Acc)->
			{Distance,_Color}=shortest_intrLine({Loc,Ray},Avatars,{inf,void}),
			compose_RangeScanList(Loc,UnitRays,Avatars,[Distance|Acc]);
		compose_RangeScanList(_Loc,[],_Avatars,Acc)->
			lists:reverse(Acc).

	color_scanner(Op,{Zoom,PanX,PanY},Density,Spread,Loc,Direction,Avatars)->
		case is_even(Density) of
			true ->
				Resolution = Spread/Density,
				SAngle = (Density/2)*Resolution,
				StartAngle = -SAngle+Resolution/2;
			false ->
				Resolution = Spread/Density,
				SAngle=trunc(Density/2)*Resolution,
				StartAngle = -SAngle
		end,
		UnitRays = create_UnitRays(Direction,Density,Resolution,StartAngle,[]),
		ColorScanList = compose_ColorScanList(Loc,UnitRays,Avatars,[]),
		%io:format("ColorScanList:~p~n",[ColorScanList]),
		case {Op,get(canvas)} of
			{silent,_} ->
				done;
			{draw,undefined} ->
				Canvas = gen_server:call(get(scape),get_canvas),
				put(canvas,Canvas);
			{draw,Canvas}->
				{X,Y} = Loc,
				FLoc = {X*Zoom+PanX,Y*Zoom+PanY},
				ScanListP=lists:zip(UnitRays,ColorScanList),
				Ids = [gs:create(line,Canvas,[{coords,[FLoc,{(X+Xr*25)*Zoom+PanX,(Y+Yr*25)*Zoom+PanY}]},{fg,val2clr(Color)}])||{{Xr,Yr},Color}<-ScanListP],
				timer:sleep(2),
				[gs:destroy(Id) || Id<- Ids]
		end,
		ColorScanList.
		
		compose_ColorScanList(Loc,[Ray|UnitRays],Avatars,Acc)->
			{_Distance,Color}=shortest_intrLine({Loc,Ray},Avatars,{inf,void}),
			compose_ColorScanList(Loc,UnitRays,Avatars,[Color|Acc]);
		compose_ColorScanList(_Loc,[],_Avatars,Acc)->
			lists:reverse(Acc).

	energy_scaner(Op,{Zoom,PanX,PanY},Density,Spread,Loc,Direction,Avatars)->
		case is_even(Density) of
			true ->
				Resolution = Spread/Density,
				SAngle = (Density/2)*Resolution,
				StartAngle = -SAngle+Resolution/2;
			false ->
				Resolution = Spread/Density,
				SAngle=trunc(Density/2)*Resolution,
				StartAngle = -SAngle
		end,
		UnitRays = create_UnitRays(Direction,Density,Resolution,StartAngle,[]),
		EnergyScanList = compose_EnergyScanList(Loc,UnitRays,Avatars,[]),
		%io:format("RangeScanList:~p~n",[RangeScanList]),
		case Op of
			silent ->
				done;
			draw ->
				io:format("EnergyScanList:~p~n",[EnergyScanList])
		end,
		EnergyScanList.
		
		compose_EnergyScanList(Loc,[Ray|UnitRays],Avatars,Acc)->
			{_Distance,_Color,Energy}=shortest_intrLine2({Loc,Ray},Avatars,{inf,void},0),
			%io:format("compose_EnergyScanList:~p~n",[Energy]),
			compose_EnergyScanList(Loc,UnitRays,Avatars,[Energy/100|Acc]);
		compose_EnergyScanList(_Loc,[],_Avatars,Acc)->
			lists:reverse(Acc).

		shortest_intrLine2(Gaze,[Avatar|Avatars],Val,Energy)->
			{D,_} = Val,
			{U_D,U_C} = intr(Gaze,Avatar#avatar.objects,Val),
			U_Energy = case D == U_D of
				true ->
					Energy;
				false ->
					Avatar#avatar.energy
			end,
			shortest_intrLine2(Gaze,Avatars,{U_D,U_C},U_Energy);
		shortest_intrLine2(_Gaze,[],{Distance,Color},Energy)->
			case Distance of
				inf ->%TODO, perhaps absence of color should be -1, not 1.
					{-1,1,Energy};
				0.0 ->
					{-1,1,Energy};
				_ ->
					{Distance,clr2val(Color),Energy}
			end.

		create_UnitRays(_,0,_,_,Acc)->
			Acc;
		create_UnitRays({X,Y},Density,Resolution,Angle,Acc)->
			%io:format("Angle:~p~n",[Angle*180/math:pi()]),
			UnitRay = {X*math:cos(Angle) - Y*math:sin(Angle), X*math:sin(Angle) + Y*math:cos(Angle)},
			create_UnitRays({X,Y},Density-1,Resolution,Angle+Resolution,[UnitRay|Acc]).

		shortest_intrLine(Gaze,[Avatar|Avatars],Val)->
			shortest_intrLine(Gaze,Avatars,intr(Gaze,Avatar#avatar.objects,Val));
		shortest_intrLine(_Gaze,[],{Distance,Color})->
			case Distance of
				inf ->%TODO, perhaps absence of color should be -1, not 1.
					{-1,1};
				0.0 ->
					{-1,1};
				_ ->
					{Distance,clr2val(Color)}
			end.

		intr(Gaze,[{circle,_Id,Color,_Pivot,C,R}|Objects],{Min,MinColor})->
			{S,D} = Gaze,
			[{Xc,Yc}] = C,
			{Xs,Ys} = S,
			{Xd,Yd} = D,
			{Xv,Yv} = {Xs-Xc,Ys-Yc},
			VdotD = Xv*Xd + Yv*Yd,
			Dis = math:pow(VdotD,2) - (Xv*Xv + Yv*Yv - R*R),
			%io:format("S:~p D:~p C:~p V:~p R:~p VdotD:~p Dis:~p~n",[S,D,C,{Xv,Yv},R,VdotD,Dis]),
			Result=case Dis > 0 of
				false ->
					inf;
				true ->
					SqrtDis = math:sqrt(Dis),
					I1 = -VdotD - SqrtDis,
					I2 = -VdotD + SqrtDis,
					case (I1 > 0) and (I2 >0) of
						true ->
							erlang:min(I1,I2);
						false ->
							inf
					end
			end,
			{UMin,UMinColor}=case Result < Min of
				true ->
					{Result,Color};
				false ->
					{Min,MinColor}
			end,
			intr(Gaze,Objects,{UMin,UMinColor});
		intr(Gaze,[{line,_Id,Color,_Pivot,[{X3,Y3},{X4,Y4}],_Parameter}|Objects],{Min,MinColor})->
			{S,D} = Gaze,
			{X1,Y1} = S,
			{XD0,YD0} = D,
			PerpXD1 = Y4-Y3,
			PerpYD1 = -(X4-X3),
			PerpXD0 = YD0,
			PerpYD0 = -XD0,
			Result=case PerpXD1*XD0 + PerpYD1*YD0 of
				0.0 ->
					inf;
				Denom ->
					RayLength = ((PerpXD1*(X3-X1)) + (PerpYD1*(Y3-Y1)))/Denom,
					T = ((PerpXD0*(X3-X1)) + (PerpYD0*(Y3-Y1)))/Denom,			
					case (RayLength >= 0) and (T >= 0) and (T =< 1) of
						true ->
							RayLength;
						false ->
							inf
					end
			end,
			{UMin,UMinColor}=case Result < Min of
				true ->
					{Result,Color};
				false ->
					{Min,MinColor}
			end,
			intr(Gaze,Objects,{UMin,UMinColor});
		intr(_Gaze,[],{Min,MinColor})->
			{Min,MinColor}.

	shortest_distance(OperatorAvatar,Avatars)->
		Loc = OperatorAvatar#avatar.loc,
		shortest_distance(Loc,Avatars,inf).
		
		shortest_distance({X,Y},[Avatar|Avatars],SD)->
			{LX,LY} = Avatar#avatar.loc,
			Distance = math:sqrt(math:pow(X-LX,2)+math:pow(Y-LY,2)),
			shortest_distance({X,Y},Avatars,erlang:min(SD,Distance));
		shortest_distance({_X,_Y},[],SD)->
			case SD of
				inf ->
					-1;
				_ ->
					SD
			end.
			
clr2val(Color)->
	case Color of
		black -> -1; %poison
		cyan -> -0.75;
		green -> -0.5; %plant
		yellow -> -0.25;
		blue -> 0; %prey
		gret -> 0.25;
		red -> 0.5; %predator
		brown -> 0.75; % wall
		_ -> 1%io:format("transducers:clr2val(Color): Color = ~p~n",[Color]), 1 %emptiness
	end.
	
val2clr(Val)->
	case Val of
		-1 -> black;
		-0.75 -> cyan;
		-0.5 -> green;
		-0.25 -> yellow;
		0 -> blue;
		0.25 -> grey;
		0.5 ->	red;
		0.75 -> brown;
		_ -> white
	end.

is_even(Val)->
	case (Val rem 2) of
		0 ->
			true;
		_ ->
			false
	end.
