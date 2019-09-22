%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-record(sensor,{id,name,cortex_id,scape,vector_length,fanout_ids}).
-record(actuator,{id,name,cortex_id,scape,vector_length,fanin_ids}).
-record(neuron, {id, cortex_id, af, weighted_inputs, output_ids}).
-record(cortex, {id, sensor_ids, actuator_ids, neuron_ids}).
