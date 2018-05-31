'''
Decomposes the change in wealth inequality using the following experiment:
Step 1. Compute inequality measures for when people update their consumption rule when growth changes
Step 2. Compute inequality measures for when people DO NOT update their consumption rule

The difference between these two cases comes from the erosion of the wealth-to-income ratio due to
the change in income growth.

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

Params.do_param_dist = True     # Do param-dist version if True, param-point if False
Params.do_lifecycle = False     # Use lifecycle model if True, perpetual youth if False
Params.do_simulation = True     # Runs simulation if True
initial_growth = 1.0       # 1.0 for baseline, 1.03**0.25 for high initial growth

Params.spec_name = 'Dist' if Params.do_param_dist else 'Point'
Params.spec_name += 'LC' if Params.do_lifecycle else 'PY'

path_initial_growth = 'Baseline/' if initial_growth == 1 else 'High_initial_growth/'

#------------------------------------------------------------------------------------------
# Step 1. Load inequality data for the case when agents update their consumption rule
#------------------------------------------------------------------------------------------

# Load previously computed estimates for spread and center
with open('./ParamsEstimates/' + path_initial_growth + Params.spec_name + '.pkl') as f:
    center_estimate, spread_estimate, estimation_growth = pickle.load(f)

# Load previously computed inequality data
with open('./Results/' + path_initial_growth + Params.spec_name + '.pkl') as f:
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
    
#------------------------------------------------------------------------------------------
# Step 2. Simulate economy for different growth factors WITHOUT updating consumption rule
#------------------------------------------------------------------------------------------

if Params.do_simulation: # Run simulation if True, o.w. load previously computed simmulation results
    # Set number of beta types
    if Params.do_param_dist:
        Params.pref_type_count = 7       # Number of discrete beta types in beta-dist
    else:
        Params.pref_type_count = 1       # Just one beta type in beta-point
        
    # Set simulation parameters
    if Params.do_param_dist:
        if Params.do_agg_shocks:
            Params.Population = 16800
        else:
            Params.Population = 14000
    else:
        if Params.do_agg_shocks:
            Params.Population = 9600
        else:
            Params.Population = 10000    # Total number of simulated agents in the population
    
    # Make AgentTypes
    if Params.do_lifecycle:
        DropoutType = cstwMPCagent(**Params.init_dropout)
        DropoutType.PermGroFac = [initial_growth]
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
        EstimationAgentList = []
        for n in range(Params.pref_type_count):
            EstimationAgentList.append(deepcopy(DropoutType))
            EstimationAgentList.append(deepcopy(HighschoolType))
            EstimationAgentList.append(deepcopy(CollegeType))
    else:
        if Params.do_agg_shocks:
            PerpetualYouthType = cstwMPCagent(**Params.init_agg_shocks)
            PerpetualYouthType.PermGroFac = [initial_growth]
        else:
            PerpetualYouthType = cstwMPCagent(**Params.init_infinite)
            PerpetualYouthType.PermGroFac = [initial_growth]
        PerpetualYouthType.AgeDstn = np.array(1.0)
        EstimationAgentList = []
        for n in range(Params.pref_type_count):
            EstimationAgentList.append(deepcopy(PerpetualYouthType))
            
    # Give all the AgentTypes different seeds
    for j in range(len(EstimationAgentList)):
        EstimationAgentList[j].seed = j
    
    # Make an economy for the consumers to live in
    EstimationEconomy = cstwMPCmarket(**Params.init_market)
    EstimationEconomy.agents = EstimationAgentList
    EstimationEconomy.center_estimate = center_estimate # Use previously computed estimates for center
    EstimationEconomy.spread_estimate = spread_estimate # Use previously computed estimates for spread

    if Params.do_lifecycle:
        EstimationEconomy.PopGroFac = Params.PopGroFac
        EstimationEconomy.TypeWeight = Params.TypeWeight_lifecycle
        EstimationEconomy.T_retire = Params.working_T-1
        EstimationEconomy.act_T = Params.T_sim_LC
        EstimationEconomy.ignore_periods = Params.ignore_periods_LC
    else:
        EstimationEconomy.PopGroFac = 1.0
        EstimationEconomy.TypeWeight = [1.0]
        EstimationEconomy.act_T = Params.T_sim_PY
        EstimationEconomy.ignore_periods = Params.ignore_periods_PY
    if Params.do_agg_shocks:
        EstimationEconomy(**Params.aggregate_params)
        EstimationEconomy.update()
        EstimationEconomy.makeAggShkHist()
            
    # Solve economy for initial growth rate. This consumption rule will be used throughout the simulation
    EstimationEconomy.LorenzBool = True
    EstimationEconomy.ManyStatsBool = True
    EstimationEconomy.distributeParams(Params.param_name,Params.pref_type_count,
                                       center_estimate,spread_estimate,Params.dist_type)
    
    EstimationEconomy.solveAgents()
    
    # Initialize inequality data to be filled during simulation
    LorenzLongLvlSim_no_update = []
    LorenzLongNrmSim_no_update = []
    aLvlGiniSim_no_update = []
    aNrmGiniSim_no_update = []
    aLvlMeanToMedianSim_no_update = []
    aNrmMeanToMedianSim_no_update = []
    aLvlPercentilesSim_no_update = []
    aNrmPercentilesSim_no_update = []    
    
    # Simulate model for different growth rates without updating the consumption rule    
    for i in range(len(growthFactors)):
        annual_g = annual_growthFactors[i]
        g = growthFactors[i]
        
        print('Now simulating model for annual growth = ' + str(annual_g))
        for j in range(len(EstimationEconomy.agents)):
            if Params.do_lifecycle:
                EstimationEconomy.agents[j].PermGroFac = [i*g for i in EstimationEconomy.agents[j].PermGroFac]
            else:
                EstimationEconomy.agents[j].PermGroFac = [g]
        
        EstimationEconomy.makeHistory()
        EstimationEconomy.showManyStats(Params.spec_name)
        
        LorenzLongLvlSim_no_update.append(EstimationEconomy.LorenzLongLvlSim)
        LorenzLongNrmSim_no_update.append(EstimationEconomy.LorenzLongNrmSim)
        aLvlGiniSim_no_update.append(EstimationEconomy.aLvlGiniSim)
        aNrmGiniSim_no_update.append(EstimationEconomy.aNrmGiniSim)
        aLvlMeanToMedianSim_no_update.append(EstimationEconomy.aLvlMeanToMedianSim)
        aNrmMeanToMedianSim_no_update.append(EstimationEconomy.aNrmMeanToMedianSim)
        aLvlPercentilesSim_no_update.append(EstimationEconomy.aLvlPercentilesSim)
        aNrmPercentilesSim_no_update.append(EstimationEconomy.aNrmPercentilesSim)
        
    # Save growth factors and corresponding results as .pkl and .csv
    with open('./Results/' + path_initial_growth + 'Decompose_inequality/' 
              + Params.spec_name + '.pkl', 'w') as f:
        pickle.dump([annual_growthFactors,
                     growthFactors,
                     LorenzLongLvlSim_no_update,
                     LorenzLongNrmSim_no_update,
                     aLvlGiniSim_no_update,
                     aNrmGiniSim_no_update,
                     aLvlMeanToMedianSim_no_update,
                     aNrmMeanToMedianSim_no_update,
                     aLvlPercentilesSim_no_update,
                     aNrmPercentilesSim_no_update], f)
    
    csvdict = {'annual_growthFactors': annual_growthFactors,
               'growthFactors' : growthFactors,
               'aLvlGiniSim' : aLvlGiniSim_no_update,
               'aNrmGiniSim' : aNrmGiniSim_no_update,
               'aLvlMeanToMedianSim' : aLvlMeanToMedianSim_no_update,
               'aNrmMeanToMedianSim' : aNrmMeanToMedianSim_no_update}
    
    df = pd.DataFrame.from_dict(csvdict)
    df.to_csv('./Results/' + path_initial_growth + 'Decompose_inequality/' 
              + Params.spec_name + '.csv')
    
else: # Load previously computed simulation results
    with open('./Results/' + path_initial_growth + 'Decompose_inequality/' 
              + Params.spec_name + '.pkl') as f:
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

#------------------------------------------------------------------------------------------
# Make figures
#------------------------------------------------------------------------------------------

fig = plt.figure()
aLvlGini = [getGini(item) for item in LorenzLongLvlSim]   # Gini coefficient of average Lorenz curve
aLvlGini_no_update = [getGini(item) for item in LorenzLongLvlSim_no_update]
fig = plt.figure()
plt.plot(annual_growthFactors, aLvlGiniSim, '-bo', label='update avg(gini)')
plt.plot(annual_growthFactors, aLvlGiniSim_no_update, '-ro', label='no update avg(gini)')
plt.plot(annual_growthFactors, aLvlGini, '-go', label='update gini(avg)')
plt.plot(annual_growthFactors, aLvlGini_no_update, '-ko', label='no update gini(avg)')
plt.axvline(x=estimation_growth**4)
plt.title('Gini coefficient for wealth levels')
plt.xlabel('Growth factor',fontsize=12)
plt.ylabel('Gini coefficient',fontsize=12)
plt.legend(loc='lower right')
plt.show()
fig.savefig('./Figures/' + path_initial_growth + 'Decompose_inequality/Gini_Lvl_'
            + Params.spec_name + '.pdf')

fig = plt.figure()
aNrmGini = [getGini(item) for item in LorenzLongNrmSim]   # Gini coefficient of average Lorenz curve
aNrmGini_no_update = [getGini(item) for item in LorenzLongNrmSim_no_update]
fig = plt.figure()
plt.plot(annual_growthFactors, aNrmGiniSim, '-bo', label='update avg(gini)')
plt.plot(annual_growthFactors, aNrmGiniSim_no_update, '-ro', label='no update avg(gini)')
plt.plot(annual_growthFactors, aNrmGini, '-go', label='update gini(avg)')
plt.plot(annual_growthFactors, aNrmGini_no_update, '-ko', label='no update gini(avg)')
plt.axvline(x=estimation_growth**4)
plt.title('Gini coefficient for wealth-to-income ratios')
plt.xlabel('Growth factor',fontsize=12)
plt.ylabel('Gini coefficient',fontsize=12)
plt.legend(loc='lower right')
plt.show()
fig.savefig('./Figures/' + path_initial_growth + 'Decompose_inequality/Gini_Nrm_'
            + Params.spec_name + '.pdf')





    





















