'''
Finds estimates for the center and spread of the beta distribution for 2014 US data.

Reads wealth data from ../output/CountryWealth/

Saves output in ../output/BaselineEstimates/
'''

import pdb
import numpy as np
from time import clock
import matplotlib.pyplot as plt
import pickle
import pandas as pd
from scipy.optimize import golden, brentq
import SetupParams as Params
from cstwGrowth import cstwMPCagent, cstwMPCmarket, findLorenzDistanceAtTargetKY, getKYratioDifference


Params.do_param_dist = False     # Do param-dist version if True, param-point if False
do_actual_KY = False            # Use actual K/Y ratio from WID.world if True, 10.26 o.w.
estimation_growth = 1.015**(0.25)     # Set growth rate to be used when estimating parameters 
                            # If equal to 1 estimates are saved in ../output/BaselineEstimates/NoGrowth/
                            # If > 1 estimates are saved in ../output/BaselineEstimates/HighGrowth
#path_estimation_growth = 'NoGrowth/' if estimation_growth == 1 else ''

pdb.set_trace()

# Update spec_name
Params.spec_name = '/NoGrowth' if estimation_growth == 1 else '/HighGrowth'
Params.spec_name += '/ActualKY' if do_actual_KY else '/BaselineKY' 
Params.spec_name += '/Dist' if Params.do_param_dist else '/Point'

# Set number of beta types
if Params.do_param_dist:
    Params.pref_type_count = 7       # Number of discrete beta types in beta-dist
else:
    Params.pref_type_count = 1       # Just one beta type in beta-point
    
# Set targets for K/Y and the Lorenz curve based on 2015 US data
path_to_lorenz = '../../output/CountryWealth/WealthData_US_20.csv'
lorenz_long_data = pd.read_csv(path_to_lorenz)['botsh_after'].values
lorenz_long_data = np.hstack((np.array(0.0), lorenz_long_data, np.array(1.0)))
lorenz_target = lorenz_long_data[np.array([int(100*p) for p in Params.percentiles_to_match])]
if do_actual_KY:
    KY_target = pd.read_csv(path_to_lorenz)['KY_after'].values[0]
else:
    KY_target = 10.26

# Make AgentTypes for estimation
PerpetualYouthType = cstwMPCagent(**Params.init_infinite)
PerpetualYouthType.PermGroFac = [estimation_growth]     # Update growth factor
PerpetualYouthType.AgeDstn = np.array(1.0)
EstimationAgentList = []
for n in range(Params.pref_type_count):
    EstimationAgentList.append(deepcopy(PerpetualYouthType))
        

# Give all the AgentTypes different seeds
for j in range(len(EstimationAgentList)):
    EstimationAgentList[j].seed = j
    

# Make an economy for the consumers to live in
EstimationEconomy = cstwMPCmarket(**Params.init_market)
if Params.do_param_dist:    # Update simulation parameters
    EstimationEconomy.Population = 14000
else:
    EstimationEconomy.Population = 10000    # Total number of simulated agents in the population
EstimationEconomy.agents = EstimationAgentList
EstimationEconomy.KYratioTarget = KY_target
EstimationEconomy.LorenzTarget = lorenz_target
EstimationEconomy.LorenzData = lorenz_long_data
EstimationEconomy.PopGroFac = 1.0
EstimationEconomy.TypeWeight = [1.0]
EstimationEconomy.act_T = Params.T_sim_PY
EstimationEconomy.ignore_periods = Params.ignore_periods_PY

    
# Find estimates of center and spread
    
# Choose the bounding region for the parameter search
if Params.param_name == 'CRRA':
    param_range = [0.2,70.0]
    spread_range = [0.00001,1.0]
elif Params.param_name == 'DiscFac':
    param_range = [0.95,0.995]
    spread_range = [0.006,0.008]     

if Params.do_param_dist:
    # Run the param-dist estimation
    paramDistObjective = lambda spread : findLorenzDistanceAtTargetKY(Economy = EstimationEconomy,
                                                                      param_name = Params.param_name,
                                                                      param_count = Params.pref_type_count,
                                                                      center_range = param_range,
                                                                      spread = spread,
                                                                      dist_type = Params.dist_type)
    t_start = clock()
    spread_estimate = golden(paramDistObjective,brack=spread_range,tol=1e-4)
    center_estimate = EstimationEconomy.center_save
    t_end = clock()
else:
    # Run the param-point estimation only
    paramPointObjective = lambda center : getKYratioDifference(Economy = EstimationEconomy,
                                                               param_name = Params.param_name,
                                                               param_count = Params.pref_type_count,
                                                               center = center,
                                                               spread = 0.0,
                                                               dist_type = Params.dist_type)
    t_start = clock()
    center_estimate = brentq(paramPointObjective,param_range[0],param_range[1],xtol=1e-6)
    spread_estimate = 0.0
    t_end = clock()

print('Estimate is center=' + str(center_estimate) + ', spread=' + str(spread_estimate) + ', took ' + str(t_end-t_start) + ' seconds.')


# Solve the economy for the estimated parameters
EstimationEconomy.LorenzBool = True
EstimationEconomy.ManyStatsBool = True
EstimationEconomy.center_estimate = center_estimate
EstimationEconomy.spread_estimate = spread_estimate
EstimationEconomy.distributeParams(Params.param_name,Params.pref_type_count,center_estimate,
                                   spread_estimate,Params.dist_type)
EstimationEconomy.solve()
EstimationEconomy.calcLorenzDistance()
EstimationEconomy.showManyStats()


# Make figure of Lorenz fit
LorenzAxis = np.arange(101,dtype=float)
fig = plt.figure()
plt.plot(LorenzAxis, EstimationEconomy.LorenzData, '-k', linewidth=1.5, label = 'data')
plt.plot(LorenzAxis, EstimationEconomy.LorenzLongLvlSim, '--', label = 'model')
plt.xlabel('Wealth percentile',fontsize=12)
plt.ylabel('Cumulative wealth share',fontsize=12)
plt.ylim([-0.04,1.0])
plt.legend(loc='upper left')
plt.show()
fig.savefig('../../output/BaselineEstimates' + Params.spec_name + '.pdf')

# Save estimates and a bunch of parameters used in estimation
with open('../../output/BaselineEstimates' + Params.spec_name + '.pkl', 'w') as f:
    pickle.dump([center_estimate, spread_estimate], f)
with open('../../output/BaselineEstimates' + Params.spec_name + '.txt','w') as f:
    f.write('center_estimate = %s \
            \nspread_estimate = %s \
            \ngrowth factor used for estimation is %s \
            \nT_age used for estimation is %s \
            \nRfree used for estimation is %s \
            \nCRRA used for estimation is %s \
            \nK/Y ratio to match is %s \
            \nLorenz percentiles to match are %s \
            \nLorenz distance is %s \
            \nSeconds it took to estimate: %s'
            % (center_estimate,
               spread_estimate,
               EstimationEconomy.agents[0].PermGroFac[0],
               EstimationEconomy.agents[0].T_age,
               EstimationEconomy.agents[0].Rfree,
               EstimationEconomy.agents[0].CRRA,
               EstimationEconomy.KYratioTarget,
               EstimationEconomy.LorenzTarget,
               EstimationEconomy.LorenzDistance,
               t_end-t_start))
    
# Save EstimationEconomy
with open('../../output/BaselineEstimates' + Params.spec_name + '_EstimationEconomy.pkl', 'wb') as f:
    pickle.dump(EstimationEconomy, f, pickle.HIGHEST_PROTOCOL)

    
