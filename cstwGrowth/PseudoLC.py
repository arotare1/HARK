'''
Implements a pseudo-lifecycle version of the model perpetual youth model where simulated agents 
don't survive beyond a certain age T_age.

The exercise is done for two cases:
Case 1. Agents update their consumption rule when growth changes
    Case 1a. T_age == 400 (this is the default value and corresponds to a maximum age of 24+400/4=124 yrs)
    Case 1b. T_age == 200 (this corresponds to a maximum age of 24+200/4=74 yrs)
    Case 1c. T_age == 100 (this corresponds to a maximum age of 24+100/4=49 yrs)
Case 2. Agents DO NOT update their consumption rule when growth changes
    Case 2a. T_age == 400 (this is the default value and corresponds to a maximum age of 24+400/4=124 yrs)
    Case 2b. T_age == 200 (this corresponds to a maximum age of 24+200/4=74 yrs)
    Case 2c. T_age == 100 (this corresponds to a maximum age of 24+100/4=49 yrs)

This experiment should shed light on why inequality in wealth levels is not monotonic in the PY model.
It is possible that inequality goes back up after a certain growth level because of the agents that
have been around for many years.

Figures are saved in ./Figures/Baseline/PseudoLC/ and ./Figures/HighEstimationGrowth/PseudoLC/

Assumes that estimates of center and spread have already been computed and stored in ./ParamsEstimates/
This is done by FindEstimates.py

Assumes that the model has been solved for Case 1a. and results stored in ./Results/
This is done by VaryGrowth.py 

Assumes that the model has been simulated for Case 2a. and simulation results stored in 
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

Params.do_param_dist = True    # Do param-dist version if True, param-point if False
Params.do_lifecycle = False     # Use lifecycle model if True, perpetual youth if False
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
    
#--------------------------------------------------------------------------------------
# Case 2a. Agents DO NOT update their consumption rule when growth changes. T_age=400
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
# Cases 1b,c and 2b,c. T_age=200,120
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
Economy.center_estimate = center_estimate # Use previously computed estimates for center
Economy.spread_estimate = spread_estimate # Use previously computed estimates for spread
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
    
# Distribute preference parameters
Economy.distributeParams(Params.param_name,Params.pref_type_count,
                         center_estimate,spread_estimate,Params.dist_type)
        
# Create list of growth factors and terminal ages to loop over
annual_growthFactors = np.arange(1.0, 1.07, 0.01)
growthFactors = np.power(annual_growthFactors, 0.25)
T_age_list = [200, 100]

aLvlGinis = []  # Make list of wealth level Ginis where agents update the consumption rule
                # Each element corresponds to a combination of growth factor and terminal age
aLvlGinis_no_update = []  # Make list of wealth ratio Ginis where agents DON'T update the consumption rule
aNrmGinis = []  # Make list of wealth level Ginis where agents update the consumption rule
aNrmGinis_no_update = []  # Make list of wealth ratio Ginis where agents DON'T update the consumption rule

for i in range(len(T_age_list)):
    T_age = T_age_list[i]
    
    # Create a new "update" economy
    NewEconomy = deepcopy(Economy)
    NewEconomy.LorenzBool = True
    NewEconomy.ManyStatsBool = True
    
    # Create a new "no update" economy
    NewEconomy_no_update = deepcopy(Economy)
    NewEconomy_no_update.LorenzBool = True
    NewEconomy_no_update.ManyStatsBool = True

    for j in range(len(Economy.agents)):
        # Give agents the current T_age
        NewEconomy.agents[j].T_age = T_age
        NewEconomy_no_update.agents[j].T_age = T_age
        
    # Solve the "no update" economy for this T_age. This consumption rule will be used when we loop
    # over growth factors
    NewEconomy_no_update.solveAgents()
    
    for k in range(len(growthFactors)):
        annual_g = annual_growthFactors[k]
        g = growthFactors[k]
        
        # Give agents the current growthFactor
        for j in range(len(Economy.agents)):
            if Params.do_lifecycle:
                NewEconomy.agents[j].PermGroFac = [i*g for i in Economy.agents[j].PermGroFac]
                NewEconomy.agents[j].PermGroFacAgg = 1.0  # Turn off technological growth
                NewEconomy_no_update.agents[j].PermGroFac = [i*g for i in Economy.agents[j].PermGroFac]
                NewEconomy_no_update.agents[j].PermGroFacAgg = 1.0  # Turn off technological growth
            else:
                NewEconomy.agents[j].PermGroFac = [g]
                NewEconomy.agents[j].PermGroFacAgg = 1.0  # Turn off technological growth
                NewEconomy_no_update.agents[j].PermGroFac = [g]
                NewEconomy_no_update.agents[j].PermGroFacAgg = 1.0  # Turn off technological growth
        
        # Solve and simulate the "update" economy
        print('Now solving the "update" economy for T_age = ' + str(T_age) + 
              ' and annual growth = ' + (str(annual_g)))
        NewEconomy.solve()
        NewEconomy.showManyStats(Params.spec_name)

        # Just simulate the "no update" economy using previously computed consumption rule
        print('Now simulating the "no update" economy for T_age = ' + str(T_age) + 
              ' and annual growth = ' + (str(annual_g)))
        NewEconomy_no_update.makeHistory()
        NewEconomy_no_update.showManyStats(Params.spec_name)
        
        # Add Ginis to list
        aLvlGinis.append(getGini(NewEconomy.LorenzLongLvlSim))
        aLvlGinis_no_update.append(getGini(NewEconomy_no_update.LorenzLongLvlSim))
        aNrmGinis.append(getGini(NewEconomy.LorenzLongNrmSim))
        aNrmGinis_no_update.append(getGini(NewEconomy_no_update.LorenzLongNrmSim))

#------------------------------------------------------------------------------------------
# Make figures
#------------------------------------------------------------------------------------------

# Get Ginis of baseline economy
aLvlGini = [getGini(item) for item in LorenzLongLvlSim]   # Gini coefficient of average Lorenz curve
aLvlGini_no_update = [getGini(item) for item in LorenzLongLvlSim_no_update]

aNrmGini = [getGini(item) for item in LorenzLongNrmSim]   # Gini coefficient of average Lorenz curve
aNrmGini_no_update = [getGini(item) for item in LorenzLongNrmSim_no_update]

# For the economies with different T_age, split Gini list into chunks by T_age
aLvlGinis = np.array(aLvlGinis)
aLvlGinis = np.array_split(aLvlGinis, len(T_age_list))
aLvlGinis_no_update = np.array(aLvlGinis_no_update)
aLvlGinis_no_update = np.array_split(aLvlGinis_no_update, len(T_age_list))

aNrmGinis = np.array(aNrmGinis)
aNrmGinis = np.array_split(aNrmGinis, len(T_age_list))
aNrmGinis_no_update = np.array(aNrmGinis_no_update)
aNrmGinis_no_update = np.array_split(aNrmGinis_no_update, len(T_age_list))

# Plot wealth level Ginis for "update" economies
fig = plt.figure()
plt.plot(annual_growthFactors, aLvlGini, '-ko', label='T_age=400 (124yrs)')
colors = iter(cm.rainbow(np.linspace(0, 1, len(T_age_list))))
for j in range(len(T_age_list)):
    plt.plot(annual_growthFactors, aLvlGinis[j], '-o', color=next(colors),
             label='T_age=' + str(T_age_list[j]))
plt.axvline(x=estimation_growth**4)
plt.title('Gini coefficient for wealth levels')
plt.xlabel('Growth factor',fontsize=12)
plt.ylabel('Gini coefficient',fontsize=12)
plt.legend(loc='upper left')
plt.show()
fig.savefig('./Figures/' + path_estimation_growth + 'PseudoLC/Gini_Lvl_'
            + Params.spec_name + '.pdf')

# Plot wealth level Ginis for "no update" economies
fig = plt.figure()
plt.plot(annual_growthFactors, aLvlGini_no_update, '-ko', label='T_age=400 (124yrs)')
colors = iter(cm.rainbow(np.linspace(0, 1, len(T_age_list))))
for j in range(len(T_age_list)):
    plt.plot(annual_growthFactors, aLvlGinis_no_update[j], '-o', color=next(colors),
             label='T_age=' + str(T_age_list[j]))
plt.axvline(x=estimation_growth**4)
plt.title('Gini coefficient for wealth levels (no update)')
plt.xlabel('Growth factor',fontsize=12)
plt.ylabel('Gini coefficient',fontsize=12)
plt.legend(loc='upper left')
plt.show()
fig.savefig('./Figures/' + path_estimation_growth + 'PseudoLC/Gini_Lvl_no_update'
            + Params.spec_name + '.pdf')

# Plot wealth ratio Ginis for "update" economies
fig = plt.figure()
plt.plot(annual_growthFactors, aNrmGini, '-ko', label='T_age=400 (124yrs)')
colors = iter(cm.rainbow(np.linspace(0, 1, len(T_age_list))))
for j in range(len(T_age_list)):
    plt.plot(annual_growthFactors, aNrmGinis[j], '-o', color=next(colors),
             label='T_age=' + str(T_age_list[j]))
plt.axvline(x=estimation_growth**4)
plt.title('Gini coefficient for wealth-to-income ratios')
plt.xlabel('Growth factor',fontsize=12)
plt.ylabel('Gini coefficient',fontsize=12)
plt.legend(loc='upper right')
plt.show()
fig.savefig('./Figures/' + path_estimation_growth + 'PseudoLC/Gini_Nrm_'
            + Params.spec_name + '.pdf')

# Plot wealth ratio Ginis for "no update" economies
fig = plt.figure()
plt.plot(annual_growthFactors, aNrmGini_no_update, '-ko', label='T_age=400 (124yrs)')
colors = iter(cm.rainbow(np.linspace(0, 1, len(T_age_list))))
for j in range(len(T_age_list)):
    plt.plot(annual_growthFactors, aNrmGinis_no_update[j], '-o', color=next(colors),
             label='T_age=' + str(T_age_list[j]))
plt.axvline(x=estimation_growth**4)
plt.title('Gini coefficient for wealth-to-income ratios (no update)')
plt.xlabel('Growth factor',fontsize=12)
plt.ylabel('Gini coefficient',fontsize=12)
plt.legend(loc='upper right')
plt.show()
fig.savefig('./Figures/' + path_estimation_growth + 'PseudoLC/Gini_Nrm_no_update'
            + Params.spec_name + '.pdf')



