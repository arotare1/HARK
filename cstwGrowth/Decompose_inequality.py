'''
Decomposes the change in wealth inequality using the following experiment:
Step 1. Plot inequality measures for when people update their consumption rule when growth changes
Step 2. Plot inequality measures for when people DO NOT update their consumption rule

The difference between these two cases comes from the erosion of the wealth-to-income ratio due to
the change in income growth.

This exercise uses the beta dist perpetual youth version of the model
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
import cstwGrowth as Model

Params.do_param_dist = True     # Do param-dist version if True, param-point if False
Params.do_lifecycle = False     # Use lifecycle model if True, perpetual youth if False
Params.PermGroFac_i = 1.0       # 1.0 for baseline, 1.03**0.25 for high initial growth

Params.spec_name = 'Dist' if Params.do_param_dist else 'Point'
Params.spec_name += 'LC' if Params.do_lifecycle else 'PY'

path_initial_growth = 'Baseline/' if Params.PermGroFac_i == 1 else 'High_initial_growth/'

#------------------------------------------------------------------------------------------
# Step 1. Load inequality stats for the case when agents update their consumption rule
#------------------------------------------------------------------------------------------

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
    DropoutType = Model.cstwMPCagent(**Params.init_dropout)
    DropoutType.AgeDstn = Model.calcStationaryAgeDstn(DropoutType.LivPrb,True)
    HighschoolType = deepcopy(DropoutType)
    HighschoolType(**Params.adj_highschool)
    HighschoolType.AgeDstn = Model.calcStationaryAgeDstn(HighschoolType.LivPrb,True)
    CollegeType = deepcopy(DropoutType)
    CollegeType(**Params.adj_college)
    CollegeType.AgeDstn = Model.calcStationaryAgeDstn(CollegeType.LivPrb,True)
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
        PerpetualYouthType = Model.cstwMPCagent(**Params.init_agg_shocks)
    else:
        PerpetualYouthType = Model.cstwMPCagent(**Params.init_infinite)
    PerpetualYouthType.AgeDstn = np.array(1.0)
    EstimationAgentList = []
    for n in range(Params.pref_type_count):
        EstimationAgentList.append(deepcopy(PerpetualYouthType))
        
# Give all the AgentTypes different seeds
for j in range(len(EstimationAgentList)):
    EstimationAgentList[j].seed = j

# Make an economy for the consumers to live in
EstimationEconomy = Model.cstwMPCmarket(**Params.init_market)
EstimationEconomy.agents = EstimationAgentList
#EstimationEconomy.KYratioTarget = KY_target
#EstimationEconomy.LorenzTarget = lorenz_target
#EstimationEconomy.LorenzData = lorenz_long_data
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
        
# Load previously computed estimates
with open('./ParamsEstimates/' + path_initial_growth + Params.spec_name + '.pkl') as f:
    center_estimate, spread_estimate, estimation_growth = pickle.load(f)
EstimationEconomy.center_estimate = center_estimate
EstimationEconomy.spread_estimate = spread_estimate

# Solve economy
EstimationEconomy.LorenzBool = True
EstimationEconomy.ManyStatsBool = True
EstimationEconomy.distributeParams(Params.param_name,Params.pref_type_count,center_estimate,spread_estimate,Params.dist_type)
EstimationEconomy.solveAgents()


LorenzLongLvlSim_new = []
LorenzLongNrmSim_new = []
aLvlGiniSim_new = []
aNrmGiniSim_new = []
aLvlMeanToMedianSim_new = []
aNrmMeanToMedianSim_new = []
aLvlPercentilesSim_new = []
aNrmPercentilesSim_new = []    
    
for i in range(len(growthFactors)):
    annual_g = annual_growthFactors[i]
    g = growthFactors[i]
    
    print('Now simulating model for annual growth = ' + str(annual_g))
    for j in range(len(EstimationEconomy.agents)):
        if Params.do_lifecycle:
            EstimationEconomy.agents[j].PermGroFac = [i*g for i in EstimationEconomy.agents[j].PermGroFac]
        else:
            EstimationEconomy.agents[j].PermGroFac = [g]
    
    t_start = clock()
    EstimationEconomy.makeHistory()
    t_end = clock()
    
    EstimationEconomy.showManyStats(Params.spec_name)
    
    LorenzLongLvlSim_new.append(EstimationEconomy.LorenzLongLvlSim)
    LorenzLongNrmSim_new.append(EstimationEconomy.LorenzLongNrmSim)
    aLvlGiniSim_new.append(EstimationEconomy.aLvlGiniSim)
    aNrmGiniSim_new.append(EstimationEconomy.aNrmGiniSim)
    aLvlMeanToMedianSim_new.append(EstimationEconomy.aLvlMeanToMedianSim)
    aNrmMeanToMedianSim_new.append(EstimationEconomy.aNrmMeanToMedianSim)
    aLvlPercentilesSim_new.append(EstimationEconomy.aLvlPercentilesSim)
    aNrmPercentilesSim_new.append(EstimationEconomy.aNrmPercentilesSim)
    
# Save growth factors and corresponding results as .pkl and .csv
with open('./Results/' + path_initial_growth + 'Decompose_inequality/' 
          + Params.spec_name + '.pkl', 'w') as f:
    pickle.dump([annual_growthFactors,
                 growthFactors,
                 LorenzLongLvlSim_new,
                 LorenzLongNrmSim_new,
                 aLvlGiniSim_new,
                 aNrmGiniSim_new,
                 aLvlMeanToMedianSim_new,
                 aNrmMeanToMedianSim_new,
                 aLvlPercentilesSim_new,
                 aNrmPercentilesSim_new], f)

csvdict = {'annual_growthFactors': annual_growthFactors,
           'growthFactors' : growthFactors,
           'aLvlGiniSim' : aLvlGiniSim_new,
           'aNrmGiniSim' : aNrmGiniSim_new,
           'aLvlMeanToMedianSim' : aLvlMeanToMedianSim_new,
           'aNrmMeanToMedianSim' : aNrmMeanToMedianSim_new}

df = pd.DataFrame.from_dict(csvdict)
df.to_csv('./Results/' + path_initial_growth + 'Decompose_inequality/' 
          + Params.spec_name + '.csv')

fig = plt.figure()
plt.plot(annual_growthFactors, aLvlGiniSim, '-bo', label='update')
plt.plot(annual_growthFactors, aLvlGiniSim_new, '-ro', label='no update')
plt.axvline(x=estimation_growth**4)
plt.title('Gini coefficient for wealth levels')
plt.xlabel('Growth factor',fontsize=12)
plt.ylabel('Gini coefficient',fontsize=12)
plt.legend(loc='lower right')
plt.show()


fig = plt.figure()
plt.plot(annual_growthFactors, aNrmGiniSim, '-bo', label='update')
plt.plot(annual_growthFactors, aNrmGiniSim_new, '-ro', label='no update')
plt.axvline(x=estimation_growth**4)
plt.title('Gini coefficient for wealth levels')
plt.xlabel('Growth factor',fontsize=12)
plt.ylabel('Gini coefficient',fontsize=12)
plt.legend(loc='lower right')
plt.show()
            





    





















