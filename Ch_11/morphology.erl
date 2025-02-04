%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(morphology).
-compile(export_all).
-include("records.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Get Init Actuators/Sensors %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_InitSensors(Morphology)->
	Sensors = morphology:Morphology(sensors),
	[lists:nth(1,Sensors)].

get_InitActuators(Morphology)->
	Actuators = morphology:Morphology(actuators),
	[lists:nth(1,Actuators)].

get_Sensors(Morphology)->
	morphology:Morphology(sensors).

get_Actuators(Morphology)->
	morphology:Morphology(actuators).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MORPHOLOGIES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
xor_mimic(sensors)->
	[
		#sensor{name=xor_GetInput,scape={private,xor_sim},vector_length=2}
	];
xor_mimic(actuators)->
	[
		#actuator{name=xor_SendOutput,scape={private,xor_sim},vector_length=1}
	].
%*Every sensor and actuator uses some kind of function associated with it. A function that either polls the environment for sensory signals (in the case of a sensor) or acts upon the environment (in the case of an actuator). It is a function that we need to define and program before it is used, and the name of the function is the same as the name of the sensor or actuator it self. For example, the createSensor/1 has specified only the rng sensor, because that is the only sensor function we've finished developing. The rng function has its own vector_length specification, which will determine the number of weights that a neuron will need to allocate if it is to accept this sensor's output vector. The same principles apply to the createActuator function. Both, createSensor and createActuator function, given the name of the sensor or actuator, will return a record with all the specifications of that element, each with its own unique Id.

generate_id() ->
	{MegaSeconds,Seconds,MicroSeconds} = now(), 
	1/(MegaSeconds*1000000 + Seconds + MicroSeconds/1000000).
