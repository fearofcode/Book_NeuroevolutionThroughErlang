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

pole_balancing(sensors)->
	[
		#sensor{name=pb_GetInput,scape={private,pb_sim},vector_length=3,parameters=[3]}
	];
pole_balancing(actuators)->
	[
		#actuator{name=pb_SendOutput,scape={private,pb_sim},vector_length=1,parameters=[with_damping,1]}
	].
%Both, the pole balancing sensor and actuator, interface with the pole balancing simulation, a private scape. The type of problem the pole balancing simulation is used as depends on the sensor and acutuator parameters. The sensor's vector_length and parameters specify that the sensor will request the private scape for the cart's and pole's position and angular position respectively. The actuator's parameters specify that the scape should use without_damping type of fitness, and that since only a single pole is being used, that the termination condition associated with the second pole will be zeroed out, by being multiplied by the specified 0 value. When instead of using 0, we use 1, the private scape would use the angular position of the second pole as an element in calculating the fitness score of the interfacing agent, and using that angular position for the purpose of calculating whether termination condition has been reached by the problem.

discrete_tmaze(sensors)->
	[
		#sensor{name=dtm_GetInput,scape={private,dtm_sim},vector_length=4,parameters=[all]}
	];
discrete_tmaze(actuators)->
	[
		#actuator{name=dtm_SendOutput,scape={private,dtm_sim},vector_length=1,parameters=[]}
	].

generate_id() ->
	{MegaSeconds,Seconds,MicroSeconds} = now(), 
	1/(MegaSeconds*1000000 + Seconds + MicroSeconds/1000000).
