'''
Takes parameters estimated by ./FindEstimatesByCountry.py and solves model for the growth rate
observed over the following 25 years. Then it compares the resulting wealth distribution to the
realized wealth distribution.

Assumes parameter estimates for each country exist in ../../output/countryEstimates/

Saves results in ../../output/modelFit
'''

import pdb
import numpy as np
from time import clock
import matplotlib.pyplot as plt
import pickle
from copy import copy, deepcopy
import pandas as pd
from scipy.optimize import golden, brentq
import SetupParams as Params
from cstwGrowth import cstwMPCagent, cstwMPCmarket, calcStationaryAgeDstn, \
                        findLorenzDistanceAtTargetKY, getKYratioDifference, getLorenzShares, getGini
                        
Params.do_param_dist = True     # Do param-dist version if True, param-point if False
Params.do_lifecycle = False     # Use lifecycle model if True, perpetual youth if False

do_more_targets = False  # Set percentiles_to_match=[0.1,0.2,..,0.9] instead of [0.2,0.4,0.6,0.8] if True
do_actual_KY = True      # Set K/Y ratio from data instead of 10.26 if True
do_low_T_age = True      # Set the maximum age in simulation to 200 (=74 yrs) intead of 400 if True
do_high_Rfree = False    # Set quarterly interest rate to 1.02 instead of 1.01 if True
do_high_CRRA = True     # Set CRRA coefficient to be 1.25 instead of 1 if True
do_baseline = not do_actual_KY and not do_more_targets and not do_low_T_age \
                and not do_high_Rfree and not do_high_CRRA

Params.spec_name = '/lower_T_age_actual_KY_high_CRRA/'
## Update spec_name
#if do_baseline:
#    Params.spec_name = '/baseline/'
#
#if do_more_targets:
#    Params.percentiles_to_match = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]
#    if do_actual_KY and not do_low_T_age:
#        Params.spec_name = '/more_targets/actual_KY/'
#    if do_low_T_age and not do_actual_KY:
#        Params.spec_name = '/more_targets/low_T_age/'
#    if do_low_T_age and do_actual_KY:
#        if do_high_CRRA:
#            Params.spec_name = '/more_targets/low_T_age_actual_KY_high_CRRA/'
#        else:
#            Params.spec_name = '/more_targets/low_T_age_actual_KY/'
#    
#else:
#    if do_actual_KY and not do_low_T_age:
#        Params.spec_name = '/actual_KY/'
#    if do_low_T_age and not do_actual_KY:
#        Params.spec_name = '/low_T_age/'
#    if do_low_T_age and do_actual_KY:
#        if do_high_CRRA:
#            Params.spec_name = '/low_T_age_actual_KY_high_CRRA/'
#        else:
#            Params.spec_name = '/low_T_age_actual_KY/'
#    
#if do_high_Rfree:
#    Params.spec_name = '/high_Rfree/'
    
Params.spec_name += 'Dist' if Params.do_param_dist else 'Point'
Params.spec_name += 'LC' if Params.do_lifecycle else 'PY'

# Set number of beta types
if Params.do_param_dist:
    Params.pref_type_count = 7       # Number of discrete beta types in beta-dist
else:
    Params.pref_type_count = 1       # Just one beta type in beta-point

#country_list = ['ES', 'FR', 'GB', 'US']
country_list = ['FR', 'US']

for country in country_list:      
    print('Now solving economy of ' + country + '\n')       
    
    path_to_lorenz = '../../output/countryWealth/wealthData_' + country + '.csv'
    
    if Params.do_lifecycle:    # Since we don't have estimates for LC, use the ones for PY to create EconomyNow
        # Load growth rate observed from 1963 to 1988
        growth_before = pd.read_csv(path_to_lorenz)['growth_before_penn'].values[0]**0.25
        
        # Make AgentTypes for estimation
        DropoutType = cstwMPCagent(**Params.init_dropout)
        DropoutType.PermGroFac = [growth_before*g for g in DropoutType.PermGroFac]
        DropoutType.PermGroFacAgg = 1.0      # Turn off technological growth
        DropoutType.AgeDstn = calcStationaryAgeDstn(DropoutType.LivPrb,True)
        HighschoolType = deepcopy(DropoutType)
        HighschoolType(**Params.adj_highschool)
        HighschoolType.PermGroFac = [growth_before*g for g in HighschoolType.PermGroFac]
        HighschoolType.AgeDstn = calcStationaryAgeDstn(HighschoolType.LivPrb,True)
        CollegeType = deepcopy(DropoutType)
        CollegeType(**Params.adj_college)
        CollegeType.PermGroFac = [growth_before*g for g in CollegeType.PermGroFac]
        CollegeType.AgeDstn = calcStationaryAgeDstn(CollegeType.LivPrb,True)
        DropoutType.update()
        HighschoolType.update()
        CollegeType.update()
        AgentList = []
        for n in range(Params.pref_type_count):
            AgentList.append(deepcopy(DropoutType))
            AgentList.append(deepcopy(HighschoolType))
            AgentList.append(deepcopy(CollegeType))
        
        # Give all the AgentTypes different seeds
        for j in range(len(AgentList)):
            AgentList[j].seed = j
            
        # Make an economy for the consumers to live in
        EconomyNow = cstwMPCmarket(**Params.init_market)
        EconomyNow.LorenzPercentiles = Params.percentiles_to_match # Update percentiles to match
        if Params.do_param_dist:    # Update simulation parameters
            if Params.do_agg_shocks:
                EconomyNow.Population = 16800
            else:
                EconomyNow.Population = 14000
        else:
            if Params.do_agg_shocks:
                EconomyNow.Population = 9600
            else:
                EconomyNow.Population = 10000    # Total number of simulated agents in the population
        EconomyNow.agents = AgentList
        
        # Set KY target
        KY_now = pd.read_csv(path_to_lorenz)['KY_now'].values[0]
        EconomyNow.KYratioTarget = KY_now if do_actual_KY else 10.26
        
        # Set Lorenz target and data
        lorenz_long_now = pd.read_csv(path_to_lorenz)['botsh_now'].values
        lorenz_long_now = np.hstack((np.array(0.0), lorenz_long_now, np.array(1.0)))
        lorenz_target_now = lorenz_long_now[np.array([int(100*p) for p in EconomyNow.LorenzPercentiles])]
        EconomyNow.LorenzTarget = lorenz_target_now
        EconomyNow.LorenzData = lorenz_long_now
        
        # Set macro parameters
        EconomyNow.PopGroFac = Params.PopGroFac
        EconomyNow.TypeWeight = Params.TypeWeight_lifecycle
        EconomyNow.T_retire = Params.working_T-1
        EconomyNow.act_T = Params.T_sim_LC
        EconomyNow.ignore_periods = Params.ignore_periods_LC
        
        # Solve the economy for the PY parameters
        path_to_estimates = '../../output/countryEstimates_longer_horizon/' + country + Params.spec_name + '.pkl'
        head, replace, tail = path_to_estimates.rpartition('LC')
        path_to_estimates = head + 'PY' + tail
        with open(path_to_estimates, 'r') as f:
            center_estimate, spread_estimate = pickle.load(f)
        pdb.set_trace()
        EconomyNow.LorenzBool = True
        EconomyNow.ManyStatsBool = True
        EconomyNow.center_estimate = center_estimate
        EconomyNow.spread_estimate = spread_estimate
        EconomyNow.distributeParams(Params.param_name,Params.pref_type_count,center_estimate,
                                           spread_estimate,Params.dist_type)
        EconomyNow.solve()
        EconomyNow.calcLorenzDistance()
        EconomyNow.showManyStats()
        
    else: # If we're in the PY case load the already existing economy
        # Load correpsonding economy from ../../output/countryEstimates/
        with open('../../output/countryEstimates_longer_horizon/' + country + Params.spec_name + '_EstimationEconomy.pkl', 'rb') as f:
            EconomyNow = pickle.load(f)
    #pdb.set_trace()
    # Load growth rate observed from 1988 to 2013
    growth_after = pd.read_csv(path_to_lorenz)['growth_after_penn'].values[0]**0.25
    
    # Create new economy
    EconomyAfter  = deepcopy(EconomyNow)
    if Params.do_lifecycle:
        for j in range(len(EconomyAfter.agents)):
            EconomyAfter.agents[j].PermGroFac = [growth_after/growth_before * g for g in EconomyAfter.agents[j].PermGroFac]
    
    # Update Lorenz data of new economy
    lorenz_long_after = pd.read_csv(path_to_lorenz)['botsh_after'].values
    lorenz_long_after = np.hstack((np.array(0.0), lorenz_long_after, np.array(1.0)))
    lorenz_target_after = lorenz_long_after[np.array([int(100*p) for p in EconomyAfter.LorenzPercentiles])]
    if do_actual_KY:
        KY_after = pd.read_csv(path_to_lorenz)['KY_after'].values[0]
        EconomyAfter.KYratioTarget = KY_after
    EconomyAfter.LorenzData = lorenz_long_after
    EconomyAfter.LorenzTarget = lorenz_target_after
    
    # Solve new economy for growth rate observed between 1988 and 2013
    for j in range(len(EconomyAfter.agents)):
        if Params.do_lifecycle:
            EconomyAfter.agents[j].PermGroFac = [growth_after] * Params.T_cycle
        else:
            EconomyAfter.agents[j].PermGroFac = [growth_after]
    EconomyAfter.solve()
    EconomyAfter.calcLorenzDistance()
    EconomyAfter.calcKYratioDifference()
    EconomyAfter.showManyStats()
    
    # Make figure of Lorenz fit
    LorenzAxis = np.arange(101,dtype=float)
    if country == 'ES':     # Must deal with missing values
        fig = plt.figure()
        plt.plot(LorenzAxis, EconomyAfter.LorenzData.astype(np.double), '.r', linewidth=1.5, label = '2013 data')
        plt.plot(LorenzAxis, EconomyAfter.LorenzLongLvlSim, '--r', label = '2013 model')
        plt.plot(LorenzAxis, EconomyNow.LorenzData.astype(np.double), '.k', linewidth=1.5, label = '1988 data')
        plt.plot(LorenzAxis, EconomyNow.LorenzLongLvlSim, '--k', label = '1988 model')
        plt.xlabel('Wealth percentile',fontsize=12)
        plt.ylabel('Cumulative wealth share',fontsize=12)
        plt.title(country + ' wealth distribution 1988 to 2013')
        plt.ylim([-0.03,1.0])
        plt.legend(loc='upper left')
        plt.show()
        fig.savefig('../../output/modelFit_longer_horizon/' + country + Params.spec_name + '.pdf')
    else:
        fig = plt.figure()
        plt.plot(LorenzAxis, EconomyAfter.LorenzData, '-r', linewidth=1.5, label = '2013 data')
        plt.plot(LorenzAxis, EconomyAfter.LorenzLongLvlSim, '--r', label = '2013 model')
        plt.plot(LorenzAxis, EconomyNow.LorenzData, '-k', linewidth=1.5, label = '1988 data')
        plt.plot(LorenzAxis, EconomyNow.LorenzLongLvlSim, '--k', label = '1988 model')
        plt.xlabel('Wealth percentile',fontsize=12)
        plt.ylabel('Cumulative wealth share',fontsize=12)
        plt.title(country + ' wealth distribution 1988 to 2013')
        plt.ylim([-0.03,1.0])
        plt.legend(loc='upper left')
        plt.show()
        fig.savefig('../../output/modelFit_longer_horizon/' + country + Params.spec_name + '.pdf')
        
    # Save Lorenz and KY ratio distance
    with open('../../output/modelFit_longer_horizon/' + country + Params.spec_name + '.txt','w') as f:
        f.write('center_estimate = %s \
                \nspread_estimate = %s \
                \ngrowth factor for new economy is %s \
                \nT_age used in simulation is %s \
                \nRfree used in simulation is %s \
                \nCRRA used fin simulation is %s \
                \nK/Y ratio for new economy is %s \
                \nLorenz distance is %s \
                \nKY ratio difference is %s'
                % (EconomyAfter.center_estimate,
                   EconomyAfter.spread_estimate,
                   EconomyAfter.agents[0].PermGroFac[0],
                   EconomyAfter.agents[0].T_age,
                   EconomyAfter.agents[0].Rfree,
                   EconomyAfter.agents[0].CRRA,
                   EconomyAfter.KYratioTarget,
                   EconomyAfter.LorenzDistance,
                   EconomyAfter.KYratioDiff))
    
    # Save EconomyAfter
    with open('../../output/modelFit_longer_horizon/' + country + Params.spec_name + '_EconomyAfter.pkl', 'wb') as f:
            pickle.dump(EconomyAfter, f, pickle.HIGHEST_PROTOCOL)








