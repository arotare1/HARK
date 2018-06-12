'''
For each of the countries France, Spain, US, UK, finds estimates for center and spread to match the 
wealth distribution in 1988 using the growth rate that was observed over the 25-year period 1963-1988.

Saves estimates in ./ParamsEstimates/Country

Assumes the empirical Lorenz curves are available in .csv format
'''

import pdb
import numpy as np
from copy import copy, deepcopy
from time import clock
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import pickle
import pandas as pd
from scipy.optimize import golden, brentq
from HARKutilities import approxMeanOneLognormal, approxUniform, getPercentiles, getLorenzShares
import SetupParams as Params
from cstwGrowth import cstwMPCagent, cstwMPCmarket, calcStationaryAgeDstn, \
                        findLorenzDistanceAtTargetKY, getKYratioDifference
                        
Params.do_param_dist = True     # Do param-dist version if True, param-point if False
Params.do_lifecycle = False     # Use lifecycle model if True, perpetual youth if False

# Update spec_name
Params.spec_name = 'Dist' if Params.do_param_dist else 'Point'
Params.spec_name += 'LC' if Params.do_lifecycle else 'PY'

# Set number of beta types
if Params.do_param_dist:
    Params.pref_type_count = 7       # Number of discrete beta types in beta-dist
else:
    Params.pref_type_count = 1       # Just one beta type in beta-point
    
#country_list = ['US', 'FR', 'GB', 'ES']
country_list = ['US']

for country in country_list:
    # Read Lorenz curve and growth to be used in estimation from the corresponding .csv file
    path_to_lorenz = 'wealthData_' + country + '_1988.csv'
    lorenz_long_data = pd.read_csv(path_to_lorenz)['botsh'].values
    lorenz_long_data = np.hstack((np.array(0.0), pd.read_csv(path_to_lorenz)['botsh'].values, np.array(1.0)))
    lorenz_target = lorenz_long_data[np.array([int(100*p) for p in Params.percentiles_to_match])]
    KY_target = 10.26
    estimation_growth = pd.read_csv(path_to_lorenz)['growth_factor1'].values[0]**0.25
    
    # Make AgentTypes for estimation
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
        EstimationAgentList = []
        for n in range(Params.pref_type_count):
            EstimationAgentList.append(deepcopy(DropoutType))
            EstimationAgentList.append(deepcopy(HighschoolType))
            EstimationAgentList.append(deepcopy(CollegeType))
    else:
        if Params.do_agg_shocks:
            PerpetualYouthType = cstwMPCagent(**Params.init_agg_shocks)
            PerpetualYouthType.PermGroFac = [estimation_growth]     # Update growth factor
        else:
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
        if Params.do_agg_shocks:
            EstimationEconomy.Population = 16800
        else:
            EstimationEconomy.Population = 14000
    else:
        if Params.do_agg_shocks:
            EstimationEconomy.Population = 9600
        else:
            EstimationEconomy.Population = 10000    # Total number of simulated agents in the population
    EstimationEconomy.agents = EstimationAgentList
    EstimationEconomy.KYratioTarget = KY_target
    EstimationEconomy.LorenzTarget = lorenz_target
    EstimationEconomy.LorenzData = lorenz_long_data
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
        
    # Find estimates of center and spread
        
    # Choose the bounding region for the parameter search
    if Params.param_name == 'CRRA':
        param_range = [0.2,70.0]
        spread_range = [0.00001,1.0]
    elif Params.param_name == 'DiscFac':
#        if estimation_growth == 1.0:
        param_range = [0.95, 0.999]
        spread_range = [0.006,0.008]
#        else:
#            param_range = [0.95,0.998]
#            spread_range = [0.006,0.008]
            
    else:
        print('Parameter range for ' + Params.param_name + ' has not been defined!')
        
    # Only run estimation if getKYratioDifference has different signs for param_range
    # o.w. the loop breaks and we waste time
    x = getKYratioDifference(Economy = EstimationEconomy,
                                       param_name = Params.param_name,
                                       param_count = Params.pref_type_count,
                                       center = 0.95,
                                       spread = 0.006,
                                       dist_type = Params.dist_type)
    y = getKYratioDifference(Economy = EstimationEconomy,
                                       param_name = Params.param_name,
                                       param_count = Params.pref_type_count,
                                       center = 0.999,
                                       spread = 0.006,
                                       dist_type = Params.dist_type)
    if x*y > 0:
        print('Estimation failed for ' + country)
    else:
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
        
        print('For country ' + country +' center=' + str(center_estimate) + ', spread=' + str(spread_estimate) + ', took ' + str(t_end-t_start) + ' seconds.')
        
        # Save estimates and a bunch of parameters used in estimation
        with open('./ParamsEstimates/' + country +'/' + Params.spec_name + '.pkl', 'w') as f:
            pickle.dump([center_estimate,
                         spread_estimate, 
                         EstimationEconomy.agents[0].PermGroFac[0],
                         EstimationEconomy.agents[0].T_age,
                         EstimationEconomy.agents[0].Rfree,
                         EstimationEconomy.agents[0].CRRA], f)
        with open('./ParamsEstimates/' + country +'/' + Params.spec_name + '.txt','w') as f:
            f.write('center_estimate = %s \
                    \nspread_estimate = %s \
                    \ngrowth factor used for estimation is %s \
                    \nT_age used for estimation is %s \
                    \nRfree used for estimation is %s \
                    \nCRRA is %s'
                    % (center_estimate,
                       spread_estimate,
                       EstimationEconomy.agents[0].PermGroFac[0],
                       EstimationEconomy.agents[0].T_age,
                       EstimationEconomy.agents[0].Rfree,
                       EstimationEconomy.agents[0].CRRA))
            
        # Save EstimationEconomy as .pkl
        with open('./ParamsEstimates/' + country +'/' + Params.spec_name + '_EstimationEconomy.pkl', 'wb') as f:
            pickle.dump(EstimationEconomy, f, pickle.HIGHEST_PROTOCOL)




#y = []
#for x in np.arange(0.999, 1.05, 0.01):
#    y.append(getKYratioDifference(Economy = EstimationEconomy,
#                         param_name = Params.param_name,
#                         param_count = Params.pref_type_count,
#                         center = x,
#                         spread = 0.0,
#                         dist_type = Params.dist_type))
#    
#getKYratioDifference(Economy = EstimationEconomy,
#                         param_name = Params.param_name,
#                         param_count = Params.pref_type_count,
#                         center = 1.03,
#                         spread = 0.0,
#                         dist_type = Params.dist_type)
#    
#
#EstimationEconomy.agents[0].checkConditions()













