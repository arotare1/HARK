'''
Solves model for different growth factors and saves results in ./Results/

Assumes that estimates of center and spread have already been computed and stored in ./ParamsEstimates/
This is done by FindEstimates.py
'''

import pdb
import sys 
import os
sys.path.insert(0, os.path.abspath('../'))
sys.path.insert(0, os.path.abspath('../ConsumptionSaving'))
import numpy as np
from copy import copy, deepcopy
from time import clock
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import pickle
import pandas as pd
from HARKutilities import approxMeanOneLognormal, approxUniform, getPercentiles, getLorenzShares
import SetupParams as Params
from cstwGrowth import cstwMPCagent, cstwMPCmarket, calcStationaryAgeDstn, \
                        findLorenzDistanceAtTargetKY, getKYratioDifference

Params.do_param_dist = True    # Do param-dist version if True, param-point if False
Params.do_lifecycle = True     # Use lifecycle model if True, perpetual youth if False
which_estimation_growth = 1.0   # Pick estimates obtained under a specific growth factor 
                                # 1.0 for Baseline, >1 for HighEstimationGrowth
path_estimation_growth = 'Baseline/' if which_estimation_growth == 1 else 'HighEstimationGrowth/'

# Update spec_name
Params.spec_name = 'Dist' if Params.do_param_dist else 'Point'
Params.spec_name += 'LC' if Params.do_lifecycle else 'PY'

# Load previously computed estimates
# For now, use "fake" parameters which were estimated for PY with PermGroFac 1.0 and T_age=160
with open('./ParamsEstimates/' + path_estimation_growth + 'DistPY' + '.pkl') as f:
    center_estimate, spread_estimate, estimation_growth = pickle.load(f)

# Set number of beta types
if Params.do_param_dist:
    Params.pref_type_count = 7       # Number of discrete beta types in beta-dist
else:
    Params.pref_type_count = 1       # Just one beta type in beta-point
    
# Set targets for K/Y and the Lorenz curve based on the data
if Params.do_liquid:
    lorenz_target = np.array([0.0, 0.004, 0.025,0.117])
    KY_target = 6.60
else: # This is hacky until I can find the liquid wealth data and import it
    lorenz_target = getLorenzShares(Params.SCF_wealth,weights=Params.SCF_weights,percentiles=Params.percentiles_to_match)
    lorenz_long_data = np.hstack((np.array(0.0),getLorenzShares(Params.SCF_wealth,weights=Params.SCF_weights,percentiles=np.arange(0.01,1.0,0.01).tolist()),np.array(1.0)))
    #lorenz_target = np.array([-0.002, 0.01, 0.053,0.171])
    KY_target = 10.26

# Make AgentTypes
if Params.do_lifecycle:
    DropoutType = cstwMPCagent(**Params.init_dropout)
    DropoutType.PermGroFac = [g*estimation_growth for g in DropoutType.PermGroFac]   # Update growth factor
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
Economy.KYratioTarget = KY_target
Economy.LorenzTarget = lorenz_target
Economy.LorenzData = lorenz_long_data
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

# Solve the model for different growth factors and save results
Economy.LorenzBool = True
Economy.ManyStatsBool = True
Economy.center_estimate = center_estimate
Economy.spread_estimate = spread_estimate
Economy.distributeParams(Params.param_name,Params.pref_type_count,center_estimate,
                         spread_estimate,Params.dist_type)

annual_growthFactors = np.arange(1.0, 1.03, 0.01)
growthFactors = np.power(annual_growthFactors, 0.25)
LorenzLongLvlSim = []
LorenzLongNrmSim = []
aLvlGiniSim = []
aNrmGiniSim = []
aLvlMeanToMedianSim = []
aNrmMeanToMedianSim = []
aLvlPercentilesSim = []
aNrmPercentilesSim = []

for i in range(len(growthFactors)):
    annual_g = annual_growthFactors[i]
    g = growthFactors[i]
    print('Now solving model for annual growth = ' + str(annual_g))
    NewEconomy = deepcopy(Economy)
    for j in range(len(NewEconomy.agents)):
        if Params.do_lifecycle:
            #NewEconomy.agents[j].PermGroFac = [i*g for i in Economy.agents[j].PermGroFac]
            NewEconomy.agents[j].PermGroFacAgg = g
        else:
            NewEconomy.agents[j].PermGroFac = [g]
            NewEconomy.agents[j].PermGroFacAgg = 1.0  # Turn off technological growth
    #pdb.set_trace()
    t_start = clock()
    NewEconomy.solve()
    t_end = clock()
    
    NewEconomy.calcLorenzDistance()
    NewEconomy.showManyStats(Params.spec_name)
    
    LorenzLongLvlSim.append(NewEconomy.LorenzLongLvlSim)
    LorenzLongNrmSim.append(NewEconomy.LorenzLongNrmSim)
    aLvlGiniSim.append(NewEconomy.aLvlGiniSim)
    aNrmGiniSim.append(NewEconomy.aNrmGiniSim)
    aLvlMeanToMedianSim.append(NewEconomy.aLvlMeanToMedianSim)
    aNrmMeanToMedianSim.append(NewEconomy.aNrmMeanToMedianSim)
    aLvlPercentilesSim.append(NewEconomy.aLvlPercentilesSim)
    aNrmPercentilesSim.append(NewEconomy.aNrmPercentilesSim)
    
    print('Solving model for annual growth = ' + str(annual_g) + ' took ' + str(t_end-t_start) + ' seconds.')
    
# Save growth factors and corresponding results as .pkl and .csv
with open('./Results/' + path_estimation_growth + Params.spec_name + '.pkl', 'w') as f:
    pickle.dump([annual_growthFactors,
                 growthFactors,
                 LorenzLongLvlSim,
                 LorenzLongNrmSim,
                 aLvlGiniSim,
                 aNrmGiniSim,
                 aLvlMeanToMedianSim,
                 aNrmMeanToMedianSim,
                 aLvlPercentilesSim,
                 aNrmPercentilesSim], f)

csvdict = {'annual_growthFactors': annual_growthFactors,
           'growthFactors' : growthFactors,
           'aLvlGiniSim' : aLvlGiniSim,
           'aNrmGiniSim' : aNrmGiniSim,
           'aLvlMeanToMedianSim' : aLvlMeanToMedianSim,
           'aNrmMeanToMedianSim' : aNrmMeanToMedianSim}

df = pd.DataFrame.from_dict(csvdict)
df.to_csv('./Results/' + path_estimation_growth + Params.spec_name + '.csv')
    
    
    
    
    
    
    
