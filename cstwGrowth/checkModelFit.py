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
do_low_T_age = False      # Set the maximum age in simulation to 200 (=74 yrs) intead of 400 if True
do_high_Rfree = False    # Set quarterly interest rate to 1.02 instead of 1.01 if True
do_high_CRRA = False     # Set CRRA coefficient to be 1.25 instead of 1 if True
do_baseline = not do_actual_KY and not do_more_targets and not do_low_T_age \
                and not do_high_Rfree and not do_high_CRRA


# Update spec_name
if do_baseline:
    Params.spec_name = '/baseline/'
if do_more_targets:
    Params.spec_name = '/more_targets/'
    Params.percentiles_to_match = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]
if do_actual_KY and not do_low_T_age:
    Params.spec_name = '/actual_KY/'
if do_low_T_age and not do_actual_KY:
    Params.spec_name = '/low_T_age/'
if do_low_T_age and do_actual_KY:
    Params.spec_name = '/low_T_age_actual_KY/'
if do_high_Rfree:
    Params.spec_name = '/high_Rfree/'
if do_high_CRRA:
    Params.spec_name = '/high_CRRA/'
    
Params.spec_name += 'Dist' if Params.do_param_dist else 'Point'
Params.spec_name += 'LC' if Params.do_lifecycle else 'PY'

#country_list = ['ES', 'FR', 'GB', 'US']
country_list = ['ES']

for country in country_list:      
    print('Now solving economy of ' + country + '\n')       
          
    # Load correpsonding economy from ../../output/countryEstimates/
    with open('../../output/countryEstimates/' + country + Params.spec_name + '_EstimationEconomy.pkl', 'rb') as f:
        EconomyNow = pickle.load(f)
    
    # Load growth rate observed from 1988 to 2013
    path_to_lorenz = '../../output/countryWealth/wealthData_' + country + '_1988.csv'
    growth_after = pd.read_csv(path_to_lorenz)['growth_after_1'].values[0]**0.25
    
    # Create new economy
    EconomyAfter  = deepcopy(EconomyNow)
    
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
        fig.savefig('../../output/modelFit/' + country + Params.spec_name + '.pdf')
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
        fig.savefig('../../output/modelFit/' + country + Params.spec_name + '.pdf')
        
    # Save Lorenz and KY ratio distance
    with open('../../output/modelFit/' + country + Params.spec_name + '.txt','w') as f:
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
    with open('../../output/modelFit/' + country + Params.spec_name + '_EconomyAfter.pkl', 'wb') as f:
            pickle.dump(EconomyAfter, f, pickle.HIGHEST_PROTOCOL)








