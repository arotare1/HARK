'''
Load various estimates and see how well the model fits the empirical Lorenz curve
'''

import pdb
import numpy as np
import matplotlib.pyplot as plt
import pickle
import pandas as pd
import SetupParams as Params
from copy import copy, deepcopy
from time import clock
import matplotlib.pyplot as plt
import matplotlib.cm as cm

from HARKutilities import approxMeanOneLognormal, approxUniform, getPercentiles, getLorenzShares

from cstwGrowth import cstwMPCagent, cstwMPCmarket, calcStationaryAgeDstn, \
                        findLorenzDistanceAtTargetKY, getKYratioDifference, getLorenzShares, getGini

LorenzAxis = np.arange(101,dtype=float)

Params.do_param_dist = True    # Do param-dist version if True, param-point if False
Params.do_lifecycle = False     # Use lifecycle model if True, perpetual youth if False

# Update spec_name
Params.spec_name = 'Dist' if Params.do_param_dist else 'Point'
Params.spec_name += 'LC' if Params.do_lifecycle else 'PY'

# Choose country
country = ''
# Load previously computed estimates
with open('./ParamsEstimates_from_thinkpad/' + country + '/' + Params.spec_name + '.pkl') as f:
    center_estimate, \
    spread_estimate, \
    estimation_growth, \
    estimation_T_age, \
    estimation_Rfree, \
    estimation_CRRA = pickle.load(f)
    
# Set number of beta types
if Params.do_param_dist:
    Params.pref_type_count = 7       # Number of discrete beta types in beta-dist
else:
    Params.pref_type_count = 1       # Just one beta type in beta-point
    
path_to_lorenz = 'wealthData_' + country + '_1988.csv'
lorenz_long_data = pd.read_csv(path_to_lorenz)['botsh'].values
lorenz_long_data = np.hstack((np.array(0.0), pd.read_csv(path_to_lorenz)['botsh'].values, np.array(1.0)))
lorenz_target = lorenz_long_data[np.array([int(100*p) for p in Params.percentiles_to_match])]
KY_target = 10.26

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

# Solve the model
Economy.LorenzBool = True
Economy.ManyStatsBool = True
Economy.center_estimate = center_estimate
Economy.spread_estimate = spread_estimate
Economy.distributeParams(Params.param_name,Params.pref_type_count,center_estimate,
                         spread_estimate,Params.dist_type)
Economy.solve()
Economy.calcLorenzDistance()
Economy.showManyStats(Params.spec_name)

# Plot Lorenz curve fit
fig = plt.figure()
plt.plot(LorenzAxis, lorenz_long_data, '-k', linewidth=1.5, label = country + ' data')
plt.plot(LorenzAxis, Economy.LorenzLongLvlSim, '--', label = 'model')
plt.xlabel('Wealth percentile',fontsize=12)
plt.ylabel('Cumulative wealth share',fontsize=12)
plt.title(country)
plt.ylim([-0.02,1.0])
plt.legend(loc='upper left')
plt.show()

































