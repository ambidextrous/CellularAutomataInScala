Cellular automata is the name given to a family of approaches that model agents on a grid. 

Imagine you have two species of creature that live in a one dimensional grid world with R rows and C columns. 

Each space in the grid can either be unoccupied or occupied by one creature from species 1 or one creature from species 2. 

Each creature when born is given a lifespan which should be generated randomly  between 0 seconds and a maximum time dependent on the species type. 

Each creature when born is given a fitness value between 0 and 1 which is, again, species dependent. 

Maximum lifespan and fitness are fixed for a species at the start of the simulation and do not change thereafter. Suggested values:  

species 1: max lifespan = 10 seconds, fitness = 0.8 

species 2: max lifespan = 5 seconds, fitness = 0.4 

At the end of its lifespan, a creature reproduces and then dies. 
When reproducing, the creature can potentially place a child in any of the 9 grid squares in its direct neighbourhood 

For each neighbouring square, the reproducing behaviour depends on whether it is already occupied. If unoccupied, a child is placed with probability equal to the fitness of the parent. If occupied, a child is placed with probability equal to the fitness of the parent minus the fitness of the occupier (when searching neighbours the parent's square (i.e. 'x') should be considered empty). 

If a parent places a child in an occupied square, the previous occupier should be murdered. If a creature is murdered, it doesn't reproduce. 
Each of the creatures in your simulation should run on its own thread.  
