-record(sensor,{id,name,cortex_id,scape,vector_length,fanout_ids=[],generation}). 
-record(actuator,{id,name,cortex_id,scape,vector_length,fanin_ids=[],generation}). 
-record(neuron, {id, generation, cortex_id, af, weighted_inputs=[], output_ids=[], ro_ids=[]}). 
-record(cortex, {id, agent_id, neuron_ids=[], sensor_ids=[], actuator_ids=[]}). 
-record(agent,{id, generation, population_id, specie_id, cortex_id, fingerprint, constraint, evo_hist=[], fitness, innovation_factor=0, pattern=[]}). 
-record(specie,{id, population_id, fingerprint, constraint, agent_ids=[], dead_pool=[], champion_ids=[], fitness, innovation_factor=0}). 
-record(population,{id,polis_id,specie_ids=[],morphologies=[],innovation_factor}).
-record(constraint,{
	morphology=xor_mimic,
	neural_afs=[tanh,cos,gauss,abs]
	}).
