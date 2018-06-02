'''
Implements a pseudo-lifecycle version of the model perpetual youth model where simulated agents 
don't survive beyond a certain age T_age.

The exercise is done for two cases:
Case 1. Agents update their consumption rule when growth changes
    Case 1a. T_age == 400 (this is the default value and corresponds to a maximum age of 24+400/4=124 yrs)
    Case 1b. T_age == 200 (this corresponds to a maximum age of 24+200/4=74 yrs)
    Case 1c. T_age == 120 (this corresponds to a maximum age of 24+120/4=54 yrs)
Case 2. Agents DO NOT update their consumption rule when growth changes
    Case 2a. T_age == 400 (this is the default value and corresponds to a maximum age of 24+400/4=124 yrs)
    Case 2b. T_age == 200 (this corresponds to a maximum age of 24+200/4=74 yrs)
    Case 2c. T_age == 120 (this corresponds to a maximum age of 24+120/4=54 yrs)

This experiment should shed light on why inequality in wealth levels is not monotonic in the PY model.
It is possible that inequality goes back up after a certain growth level because of the agents that
have been around for many years.

Figures are saved in ./Figures/Baseline/PseudoLC/ and ./Figures/HighEstimationGrowth/PseudoLC/

Assumes that estimates of center and spread have already been computed and stored in ./ParamsEstimates/
This is done by FindEstimates.py

Assumes the model has been solved for Case 1a. and results stored in ./Results/
This is done by VaryGrowth.py 

Assumes the model has been simulated for Case 2a. and simulation results stored in 
./Results/Baseline/Decompose_inequality/ and ./Results/HighEstimationGrowth/Decompose_inequality/
This is done by Decompose_inequality.py
'''

import pdb
import numpy as np
from copy import copy, deepcopy
from time import clock
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import pickle
import pandas as pd
import SetupParams as Params
from cstwGrowth import cstwMPCagent, calcStationaryAgeDstn, cstwMPCmarket, getGini

Params.do_param_dist = False    # Do param-dist version if True, param-point if False
Params.do_lifecycle = False     # Use lifecycle model if True, perpetual youth if False
Params.do_simulation = True     # Run simulation if True, load existing simulation results if False
which_estimation_growth = 1.0   # Pick estimates obtained under a specific growth factor 
                                # 1.0 for Baseline, >1 for HighEstimationGrowth
path_estimation_growth = 'Baseline/' if which_estimation_growth == 1 else 'HighEstimationGrowth/'

# Update spec_name
Params.spec_name = 'Dist' if Params.do_param_dist else 'Point'
Params.spec_name += 'LC' if Params.do_lifecycle else 'PY'

# Load previously computed estimates for spread and center
with open('./ParamsEstimates/' + path_estimation_growth + Params.spec_name + '.pkl') as f:
    center_estimate, spread_estimate, estimation_growth = pickle.load(f)

#----------------------------------------------------------------------------------
# Case 1a. Agents update their consumption rule when growth changes. T_age==400
#----------------------------------------------------------------------------------

# Load previously computed inequality data
with open('./Results/' + path_estimation_growth + Params.spec_name + '.pkl') as f:
    annual_growthFactors,\
    growthFactors,\
    LorenzLongLvlSim,\
    LorenzLongNrmSim,\
    aLvlGiniSim,\
    aNrmGiniSim,\
    aLvlMeanToMedianSim,\
    aNrmMeanToMedianSim,\
    aLvlPercentilesSim,\
    aNrmPercentilesSim = pickle.load(f)
    
#---------------------------------------------------------------------------------------
# Case 2a. Agents DO NOT update their consumption rule when growth changes. T_age==400
#--------------------------------------------------------------------------------------
    
# Load previously computed inequality data
with open('./Results/' + path_estimation_growth + 'Decompose_inequality/' + Params.spec_name + '.pkl') as f:
    annual_growthFactors,\
    growthFactors,\
    LorenzLongLvlSim_no_update,\
    LorenzLongNrmSim_no_update,\
    aLvlGiniSim_no_update,\
    aNrmGiniSim_no_update,\
    aLvlMeanToMedianSim_no_update,\
    aNrmMeanToMedianSim_no_update,\
    aLvlPercentilesSim_no_update,\
    aNrmPercentilesSim_no_update = pickle.load(f)
    
#-------------------------------------------------------------------------------------
# Case 1b,c. Agents update their consumption rule when growth changes. T_age==200,120
#-------------------------------------------------------------------------------------

# Set number of beta types
if Params.do_param_dist:
    Params.pref_type_count = 7       # Number of discrete beta types in beta-dist
else:
    Params.pref_type_count = 1       # Just one beta type in beta-point

# Make AgentTypes
if Params.do_lifecycle:
    DropoutType = cstwMPCagent(**Params.init_dropout)
    DropoutType.PermGroFac = [estimation_growth]    # Update growth factor
    DropoutType.AgeDstn = calcStationaryAgeDstn(DropoutType.LivPrb,True)
    HighschoolType = deepcopy(DropoutType)
    HighschoolType(**Params.adj_highschool)
    HighschoolType.AgeDstn = calcStationaryAgeDstn(HighschoolType.LivPrb,True)
    CollegeType = deepcopy(DropoutType)
    CollegeType(**Params.adj_college)
    CollegeType.AgeDstn = calcStationaryAgeDstn(CollegeType.LivPrb,True)
    DropoutType.update()
    HighschoolType.update()
    CollegeType.update()
    AgentList = []
    for n in range(Params.pref_type_count):
        AgentList.append(deepcopy(DropoutType))
        AgentList.append(deepcopy(HighschoolType))
        AgentList.append(deepcopy(CollegeType))
else:
    if Params.do_agg_shocks:
        PerpetualYouthType = cstwMPCagent(**Params.init_agg_shocks)
        PerpetualYouthType.PermGroFac = [estimation_growth]     # Update growth factor
    else:
        PerpetualYouthType = cstwMPCagent(**Params.init_infinite)
        PerpetualYouthType.PermGroFac = [estimation_growth]     # Update growth factor
    PerpetualYouthType.AgeDstn = np.array(1.0)
    AgentList = []
    for n in range(Params.pref_type_count):
        AgentList.append(deepcopy(PerpetualYouthType))
        
# Give all the AgentTypes different seeds
for j in range(len(AgentList)):
    AgentList[j].seed = j
    
# Make an economy for the consumers to live in
Economy = cstwMPCmarket(**Params.init_market)
if Params.do_param_dist:    # Update simulation parameters
    if Params.do_agg_shocks:
        Economy.Population = 16800
    else:
        Economy.Population = 14000
else:
    if Params.do_agg_shocks:
        Economy.Population = 9600
    else:
        Economy.Population = 10000    # Total number of simulated agents in the population
Economy.agents = AgentList
if Params.do_lifecycle:
    Economy.PopGroFac = Params.PopGroFac
    Economy.TypeWeight = Params.TypeWeight_lifecycle
    Economy.T_retire = Params.working_T-1
    Economy.act_T = Params.T_sim_LC
    Economy.ignore_periods = Params.ignore_periods_LC
else:
    Economy.PopGroFac = 1.0
    Economy.TypeWeight = [1.0]
    Economy.act_T = Params.T_sim_PY
    Economy.ignore_periods = Params.ignore_periods_PY
if Params.do_agg_shocks:
    Economy(**Params.aggregate_params)
    Economy.update()
    Economy.makeAggShkHist()
        
# Solve the model for different growth factors and different T_age
Economy.distributeParams(Params.param_name,Params.pref_type_count,center_estimate,
                         spread_estimate,Params.dist_type)
annual_growthFactors = np.arange(1.0, 1.07, 0.01)
growthFactors = np.power(annual_growthFactors, 0.25)
T_age_list = [200, 120]

NewEconomies = []   # Make list of economies for different combinations of growth and T_age

for k in range(len(T_age_list)):
    for i in range(len(growthFactors)):
        annual_g = annual_growthFactors[i]
        g = growthFactors[i]
        T_age = T_age_list[k]
        NewEconomy = deepcopy(Economy)
        for j in range(len(NewEconomy.agents)):
            if Params.do_lifecycle:
                NewEconomy.agents[j].PermGroFac = [i*g for i in Economy.agents[j].PermGroFac]
                NewEconomy.agents[j].PermGroFacAgg = 1.0  # Turn off technological growth
            else:
                NewEconomy.agents[j].PermGroFac = [g]
                NewEconomy.agents[j].PermGroFacAgg = 1.0  # Turn off technological growth
                NewEconomy.agents[j].T_age = T_age
        
        print('Now solving model for T_age = ' + str(T_age) + ', annual growth = '+ str(annual_g))    
        t_start = clock()
        NewEconomy.solve()
        t_end = clock()
        
        NewEconomy.showManyStats(Params.spec_name)
        NewEconomies.append(NewEconomy)     # Add the new economy to the list
        
        print('Solving model took ' + str(t_end-t_start) + ' seconds.')






























