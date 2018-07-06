'''
Compare wealth inequality for the case when agents update their consumption rule as growth changes
to the case when they don't. The difference between these two cases comes from the erosion of the 
wealth-to-income ratio due to the change in income growth.


'''

import pdb
import numpy as np
import matplotlib.pyplot as plt
from copy import copy, deepcopy
from time import clock
import pickle
import pandas as pd
import SetupParams as Params
from cstwGrowth import getGiniPrc

Params.do_param_dist = True     # Do param-dist version if True, param-point if False
do_actual_KY = False            # Use actual K/Y ratio from WID.world if True, 10.26 o.w.
do_low_T_age = False      # Set the maximum age in simulation to 200 (=74 yrs) intead of 400 if True
estimation_growth = 1.015**(0.25)     # Set growth rate to be used when estimating parameters 
                            # If equal to 1 estimates are saved in ../output/BaselineEstimates/NoGrowth/
                            # If > 1 estimates are saved in ../output/BaselineEstimates/HighGrowth
                            
# Update spec_name
Params.spec_name = '/NoGrowth' if estimation_growth == 1 else '/HighGrowth'
if do_actual_KY and not do_low_T_age:
    Params.spec_name += '/actual_KY'
if do_low_T_age and not do_actual_KY:
    Params.spec_name += '/lower_T_age'
if do_low_T_age and do_actual_KY:
    Params.spec_name += '/lower_T_age_actual_KY'
Params.spec_name += '/Dist' if Params.do_param_dist else '/Point'

# Load economy where agents update
with open('../../output/BaselineEstimates' + Params.spec_name + '_EstimationEconomy.pkl', 'rb') as f:
    Economy = pickle.load(f)
    
# Load inequality stats for when agents update
with open('../../output/VaryGrowth/' + Params.spec_name + '.pkl') as f:
    T_ageSim,\
    annual_growthFactors,\
    growthFactors,\
    LorenzLongLvlSim,\
    LorenzLongNrmSim,\
    LorenzLongIncSim,\
    aLvlGiniSim,\
    aNrmGiniSim,\
    IncGiniSim,\
    aLvlMeanSim,\
    aNrmMeanSim,\
    aLvlMedianSim,\
    aNrmMedianSim,\
    aLvlMeanToMedianSim,\
    aNrmMeanToMedianSim,\
    aLvlPercentilesSim,\
    aNrmPercentilesSim,\
    DiscFacByWealthSim,\
    AgeByWealthSim, \
    DiscFacByIncSim, \
    AgeByIncSim = pickle.load(f)

# For selected inequality measures, pick just the ones corresponding to T_age used in estiamtion
aLvlMeanToMedianSim = np.array(aLvlMeanToMedianSim)[np.array(T_ageSim)==Economy.agents[0].T_age]
aNrmMeanToMedianSim = np.array(aNrmMeanToMedianSim)[np.array(T_ageSim)==Economy.agents[0].T_age]
aLvlGiniSim = np.array(aLvlGiniSim)[np.array(T_ageSim)==Economy.agents[0].T_age]
aNrmGiniSim = np.array(aNrmGiniSim)[np.array(T_ageSim)==Economy.agents[0].T_age]
annual_growthFactors = np.array(annual_growthFactors)[np.array(T_ageSim)==Economy.agents[0].T_age]
growthFactors = np.array(growthFactors)[np.array(T_ageSim)==Economy.agents[0].T_age]

aLvlMeanToMedianSim_NoUpdate = []
aNrmMeanToMedianSim_NoUpdate = []
aLvlGiniSim_NoUpdate = []
aNrmGiniSim_NoUpdate = []

for i in range(len(growthFactors)):
    annual_g = annual_growthFactors[i]
    g = growthFactors[i]
    NoUpdateEconomy = deepcopy(Economy)
    
    print('Now simulating model for annual growth = ' + str(annual_g))
    for j in range(len(NoUpdateEconomy.agents)):
        NoUpdateEconomy.agents[j].PermGroFac = [g]
    
    t_start = clock()
    NoUpdateEconomy.makeHistory()
    NoUpdateEconomy.calcLorenzDistance()
    NoUpdateEconomy.showManyStats()
    t_end = clock()
    
    aLvlMeanToMedianSim_NoUpdate.append(NoUpdateEconomy.aLvlMeanSim / NoUpdateEconomy.aLvlMedianSim)
    aNrmMeanToMedianSim_NoUpdate.append(NoUpdateEconomy.aNrmMeanSim / NoUpdateEconomy.aNrmMedianSim)
    aLvlGiniSim_NoUpdate.append(getGiniPrc(NoUpdateEconomy.LorenzLongLvlSim))
    aNrmGiniSim_NoUpdate.append(getGiniPrc(NoUpdateEconomy.LorenzLongNrmSim))
    
    print('Simulation took ' + str(t_end-t_start) + ' seconds.\n')
    
# Save inequality measures as .csv
csvdict = {'annual_growth': annual_growthFactors,
           'Lvl_gini' : aLvlGiniSim,
           'Lvl_gini_no_update' : aLvlGiniSim_NoUpdate,
           'Lvl_mean_to_median' : aLvlMeanToMedianSim,
           'Lvl_mean_to_median_no_update' : aLvlMeanToMedianSim_NoUpdate,
           'Nrm_gini' : aNrmGiniSim,
           'Nrm_gini_no_update' : aNrmGiniSim_NoUpdate,
           'Nrm_mean_to_median' : aNrmMeanToMedianSim,
           'Nrm_mean_to_median_no_update' : aNrmMeanToMedianSim_NoUpdate}

df = pd.DataFrame.from_dict(csvdict)
df.to_csv('../../output/NoUpdate/' + Params.spec_name + '.csv')
    
    