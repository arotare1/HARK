'''
Takes parameters estimated by FindEstimatesByCountry.py and solves model for the growth rate
observed over the following 'horizon' years. Then it compares the resulting wealth distribution to the
realized wealth distribution.

Assumes parameter estimates for each country exist in ../../output/CountryEstimates/

Saves results in ../../output/ModelFit/
'''

import pdb
import numpy as np
from time import clock
import matplotlib.pyplot as plt
import pickle
from copy import copy, deepcopy
import pandas as pd
import SetupParams as Params
from cstwGrowth import getGiniPrc
                        
Params.do_param_dist = True     # Do param-dist version if True, param-point if False
horizon = '20'      # Set interval over which we analyze changes in the wealth distribution
                    # Can be 20, 25, 30
do_more_targets = False  # Set percentiles_to_match=[0.1,0.2,..,0.9] instead of [0.2,0.4,0.6,0.8] if True
do_actual_KY = True      # Set K/Y ratio from data instead of 10.26 if True
do_low_T_age = True      # Set the maximum age in simulation to 200 (=74 yrs) intead of 400 if True
do_high_Rfree = False    # Set quarterly interest rate to 1.02 instead of 1.01 if True
do_high_CRRA = False     # Set CRRA coefficient to be 2.0 instead of 1 if True


# Update spec_name
if do_more_targets:
    Params.spec_name += '/more_targets'
    Params.percentiles_to_match = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]
if do_actual_KY and not do_low_T_age:
    Params.spec_name += '/actual_KY'
if do_low_T_age and not do_actual_KY:
    Params.spec_name += '/low_T_age'
if do_low_T_age and do_actual_KY:
    Params.spec_name += '/low_T_age_actual_KY'
Params.spec_name += '/Dist' if Params.do_param_dist else '/Point'
Params.spec_name += '_' + horizon

# Set number of beta types
if Params.do_param_dist:
    Params.pref_type_count = 7       # Number of discrete beta types in beta-dist
else:
    Params.pref_type_count = 1       # Just one beta type in beta-point

country_list = ['ES', 'FR', 'GB', 'US']
#country_list = ['ES']

for country in country_list:      
    print('Now solving economy of ' + country + '\n')    
    
    # Make country label for plot title    
    if country=='FR':
        label = '(a) France'
    
    if country=='ES':
        label = '(b) Spain'
        
    if country=='GB':
        label = '(c) United Kingdom'
        
    if country=='US':
        label = '(d) United States'
    
    # Load estimation economy
    with open('../../output/CountryEstimates/' + country + Params.spec_name + '_EstimationEconomy.pkl', 'rb') as f:
        EconomyNow = pickle.load(f)
    
    # Create new economy
    EconomyAfter  = deepcopy(EconomyNow)
    
    # Update Lorenz data and growth of new economy
    path_to_lorenz = '../../output/CountryWealth/WealthData_' + country + '_' + horizon + '.csv'
    lorenz_long_after = pd.read_csv(path_to_lorenz)['botsh_after'].values
    lorenz_long_after = np.hstack((np.array(0.0), lorenz_long_after, np.array(1.0)))
    lorenz_target_after = lorenz_long_after[np.array([int(100*p) for p in EconomyAfter.LorenzPercentiles])]
    growth_after = pd.read_csv(path_to_lorenz)['growth_after_wb'].values[0]**0.25
    now = pd.read_csv(path_to_lorenz)['now'].values[0]
    after = pd.read_csv(path_to_lorenz)['after'].values[0]
    if do_actual_KY:
        KY_after = pd.read_csv(path_to_lorenz)['KY_after'].values[0]
        EconomyAfter.KYratioTarget = KY_after
    EconomyAfter.LorenzData = lorenz_long_after
    EconomyAfter.LorenzTarget = lorenz_target_after
    for j in range(len(EconomyAfter.agents)):
        EconomyAfter.agents[j].PermGroFac = [growth_after]
    
    # Solve new economy
    EconomyAfter.solve()
    EconomyAfter.calcLorenzDistance()
    EconomyAfter.calcKYratioDifference()
    EconomyAfter.showManyStats()
    
    # Make figure of Lorenz fit
    LorenzAxis = np.arange(101,dtype=float)
    idx = ~np.isnan(EconomyAfter.LorenzData)
    fig = plt.figure()
    plt.plot(LorenzAxis[idx], EconomyNow.LorenzData[idx], linestyle = '-', linewidth = 0.75,
             color = 'grey', label = str(now) + ' data')
    plt.plot(LorenzAxis[idx], EconomyAfter.LorenzData[idx], linestyle = '-', linewidth = 0.75, 
             color = 'salmon', label = str(after) + ' data')
    plt.plot(LorenzAxis, EconomyNow.LorenzLongLvlSim, linestyle = '--', linewidth = 1, color = 'black',
             label = str(now) + ' model')
    plt.plot(LorenzAxis, EconomyAfter.LorenzLongLvlSim, linestyle = '--', linewidth = 1, color = 'red', 
             label = str(after) + ' model')
    plt.xlabel('Wealth percentile')
    plt.ylabel('Cumulative wealth share')
    plt.title(label)
    plt.ylim([-0.04,1.0])
    plt.legend(loc='upper left')
    plt.show()
    fig.savefig('../../output/ModelFit/' + country + Params.spec_name + '.pdf')
    fig.savefig('../../tex/model_fit_' + country + '.pdf')
    
    # Compute gini coefficient now and after for model and data
    gini_now_model = getGiniPrc(EconomyNow.LorenzLongLvlSim[idx])
    gini_after_model = getGiniPrc(EconomyAfter.LorenzLongLvlSim[idx])
    
    gini_now_data = getGiniPrc(EconomyNow.LorenzData[idx])
    gini_after_data = getGiniPrc(EconomyAfter.LorenzData[idx])
    
    # Compute mean to median ratio now and after for model and data
    mean_now_model = EconomyNow.aLvlMean
    mean_after_model = EconomyAfter.aLvlMean
    median_now_model = EconomyNow.aLvlMedian
    median_after_model = EconomyAfter.aLvlMedian
    mean_to_median_now_model = mean_now_model / median_now_model
    mean_to_median_after_model = mean_after_model / median_after_model
    
    mean_now_data = pd.read_csv(path_to_lorenz)['mean_now'].values[9]
    median_now_data = pd.read_csv(path_to_lorenz)['median_now'].values[9]
    mean_after_data = pd.read_csv(path_to_lorenz)['mean_after'].values[9]
    median_after_data = pd.read_csv(path_to_lorenz)['median_after'].values[9]
    mean_to_median_now_data = mean_now_data / median_now_data
    mean_to_median_after_data = mean_after_data / median_after_data
    
    # Compute wealth shares now and after for model and data
    top1_share_now_model = 1 - EconomyNow.LorenzLongLvlSim[99]
    top5_share_now_model = 1 - EconomyNow.LorenzLongLvlSim[95]
    top10_share_now_model = 1 - EconomyNow.LorenzLongLvlSim[90]
    bot40_share_now_model = EconomyNow.LorenzLongLvlSim[40]
    
    top1_share_after_model = 1 - EconomyAfter.LorenzLongLvlSim[99]
    top5_share_after_model = 1 - EconomyAfter.LorenzLongLvlSim[95]
    top10_share_after_model = 1 - EconomyAfter.LorenzLongLvlSim[90]
    bot40_share_after_model = EconomyAfter.LorenzLongLvlSim[40]
    
    top1_share_now_data = 1 - EconomyNow.LorenzData[99]
    top5_share_now_data = 1 - EconomyNow.LorenzData[95]
    top10_share_now_data = 1 - EconomyNow.LorenzData[90]
    bot40_share_now_data = EconomyNow.LorenzData[40]
    
    top1_share_after_data = 1 - EconomyAfter.LorenzData[99]
    top5_share_after_data = 1 - EconomyAfter.LorenzData[95]
    top10_share_after_data = 1 - EconomyAfter.LorenzData[90]
    bot40_share_after_data = EconomyAfter.LorenzData[40]
    
    # Compute absolute deviation from median now and after for model and data
    # E.g. (avg wealth of top 5% - median)/median  or  (median - avg wealth of bottom 40%)/median
    top1_to_median_now_model = top1_share_now_model * mean_now_model / 0.01 / median_now_model - 1
    top5_to_median_now_model = top5_share_now_model * mean_now_model / 0.05 / median_now_model - 1
    top10_to_median_now_model = top10_share_now_model * mean_now_model / 0.1 / median_now_model - 1
    bot40_to_median_now_model = 1 - bot40_share_now_model * mean_now_model / 0.4 / median_now_model
    
    top1_to_median_after_model = top1_share_after_model * mean_after_model / 0.01 / median_after_model - 1
    top5_to_median_after_model = top5_share_after_model * mean_after_model / 0.05 / median_after_model - 1
    top10_to_median_after_model = top10_share_after_model * mean_after_model / 0.1 / median_after_model - 1
    bot40_to_median_after_model = 1 - bot40_share_after_model * mean_after_model / 0.4 / median_after_model
    
    top1_to_median_now_data = top1_share_now_data * mean_now_data / 0.01 / median_now_data - 1
    top5_to_median_now_data = top5_share_now_data * mean_now_data / 0.05 / median_now_data - 1
    top10_to_median_now_data = top10_share_now_data * mean_now_data / 0.1 / median_now_data - 1
    bot40_to_median_now_data = 1 - bot40_share_now_data * mean_now_data / 0.4 / median_now_data
    
    top1_to_median_after_data = top1_share_after_data * mean_after_data / 0.01 / median_after_data - 1
    top5_to_median_after_data = top5_share_after_data * mean_after_data / 0.05 / median_after_data - 1
    top10_to_median_after_data = top10_share_after_data * mean_after_data / 0.1 / median_after_data - 1
    bot40_to_median_after_data = 1 - bot40_share_after_data * mean_after_data / 0.4 / median_after_data
    
    # Load annual growth
    annual_growth_before = pd.read_csv(path_to_lorenz)['growth_before_wb'].values[0]-1
    annual_growth_after = pd.read_csv(path_to_lorenz)['growth_after_wb'].values[0]-1
    
    # Make .csv with comparison now vs. after for model and data
    csvdict = {'iso' : [country] * 3,
               'center' : [EconomyNow.center_estimate] * 3,
               'spread' : [EconomyNow.spread_estimate] * 3,
               'year' : [int(now), int(after), np.nan],
               'KY_diff' : [EconomyNow.KYratioDiff, EconomyAfter.KYratioDiff, np.nan],
               'Lorenz_dist' : [EconomyNow.LorenzDistance, EconomyAfter.LorenzDistance, np.nan],
               'annual_growth' : [annual_growth_before,
                                  annual_growth_after,
                                  (annual_growth_after/annual_growth_before-1)*100],
                                  
                'gini_data' : [gini_now_data,
                               gini_after_data,
                               (gini_after_data/gini_now_data-1)*100],
                               
                'gini_model' : [gini_now_model,
                                gini_after_model,
                                (gini_after_model/gini_now_model-1)*100],
                                
                'mean_to_median_data' : [mean_to_median_now_data,
                                         mean_to_median_after_data,
                                         (mean_to_median_after_data/mean_to_median_now_data-1)*100],
                                         
                'mean_to_median_model' : [mean_to_median_now_model,
                                          mean_to_median_after_model,
                                          (mean_to_median_after_model/mean_to_median_now_model-1)*100],
                                          
                'top1_to_median_data' : [top1_to_median_now_data,
                                         top1_to_median_after_data,
                                         (top1_to_median_after_data/top1_to_median_now_data-1)*100],
                                         
                'top1_to_median_model' : [top1_to_median_now_model,
                                         top1_to_median_after_model,
                                         (top1_to_median_after_model/top1_to_median_now_model-1)*100],
                                          
                'top5_to_median_data' : [top5_to_median_now_data,
                                         top5_to_median_after_data,
                                         (top5_to_median_after_data/top5_to_median_now_data-1)*100],
                                         
                'top5_to_median_model' : [top5_to_median_now_model,
                                         top5_to_median_after_model,
                                         (top5_to_median_after_model/top5_to_median_now_model-1)*100],
                                          
                'top10_to_median_data' : [top10_to_median_now_data,
                                          top10_to_median_after_data,
                                          (top10_to_median_after_data/top10_to_median_now_data-1)*100],
                                          
                'top10_to_median_model' : [top10_to_median_now_model,
                                           top10_to_median_after_model,
                                           (top10_to_median_after_model/top10_to_median_now_model-1)*100],
                                           
                'bot40_to_median_data' : [bot40_to_median_now_data,
                                          bot40_to_median_after_data,
                                          (bot40_to_median_after_data/bot40_to_median_now_data-1)*100],
                                          
                'bot40_to_median_model' : [bot40_to_median_now_model,
                                           bot40_to_median_after_model,
                                           (bot40_to_median_after_model/bot40_to_median_now_model-1)*100]}

    df = pd.DataFrame.from_dict(csvdict)
    df.to_csv('../../output/ModelFit/' + country + Params.spec_name + '.csv', float_format='%.3f')
    
    # Save some info
    with open('../../output/ModelFit/' + country + Params.spec_name + '.txt','w') as f:
        f.write('center_estimate is %s \
                \nspread_estimate is %s \
                \ngrowth factor for new economy is %s \
                \nT_age used in simulation is %s \
                \nRfree used in simulation is %s \
                \nCRRA used in simulation is %s \
                \nK/Y ratio for new economy is %s \
                \nLorenz distance is %s \
                \nKY ratio difference is %s \
                \nGini now (data) is %s \
                \nGini after (data) is %s \
                \nGini now (model) is %s \
                \nGini after (model) is %s \
                \nMean to median ratio now (data) is %s \
                \nMean to median ratio after (data) is %s \
                \nMean to median ratio now (model) is %s \
                \nMean to median ratio after (model) is %s'
                % (EconomyAfter.center_estimate,
                   EconomyAfter.spread_estimate,
                   EconomyAfter.agents[0].PermGroFac[0],
                   EconomyAfter.agents[0].T_age,
                   EconomyAfter.agents[0].Rfree,
                   EconomyAfter.agents[0].CRRA,
                   EconomyAfter.KYratioTarget,
                   EconomyAfter.LorenzDistance,
                   EconomyAfter.KYratioDiff,
                   getGiniPrc(EconomyNow.LorenzData),
                   getGiniPrc(EconomyAfter.LorenzData),
                   getGiniPrc(EconomyNow.LorenzLongLvlSim),
                   getGiniPrc(EconomyAfter.LorenzLongLvlSim),
                   mean_to_median_now_data,
                   mean_to_median_after_data,
                   mean_to_median_now_model,
                   mean_to_median_after_model))
    
    # Save EconomyAfter
    with open('../../output/ModelFit/' + country + Params.spec_name + '_EconomyAfter.pkl', 'wb') as f:
            pickle.dump(EconomyAfter, f, pickle.HIGHEST_PROTOCOL)

