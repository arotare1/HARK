'''
Solves model for different growth factors and terminal ages and saves results in ../../output/VaryGrowth/

Assumes that estimates of center and spread have already been computed and stored 
in ../../output/BaselineEstimates/

This is done by FindEstimates.py
'''

import pdb
import numpy as np
from copy import copy, deepcopy
from time import clock
import pickle
import pandas as pd
import SetupParams as Params
from cstwGrowth import getGiniPrc

Params.do_param_dist = True     # Do param-dist version if True, param-point if False
do_actual_KY = True            # Use actual K/Y ratio from WID.world if True, 10.26 o.w.
do_low_T_age = True      # Set the maximum age in simulation to 200 (=74 yrs) intead of 400 if True
estimation_growth = 1.0     # Set growth rate to be used when estimating parameters 
                            # If equal to 1 estimates are saved in ../output/BaselineEstimates/NoGrowth/
                            # If > 1 estimates are saved in ../output/BaselineEstimates/HighGrowth

# Update spec_name
Params.spec_name = '/NoGrowth' if estimation_growth == 1 else '/HighGrowth'
if do_actual_KY and not do_low_T_age:
    Params.spec_name += '/actual_KY'
if do_low_T_age and not do_actual_KY:
    Params.spec_name += '/low_T_age'
if do_low_T_age and do_actual_KY:
    Params.spec_name += '/low_T_age_actual_KY'
Params.spec_name += '/Dist' if Params.do_param_dist else '/Point'


# Load economy
with open('../../output/BaselineEstimates' + Params.spec_name + '_EstimationEconomy.pkl', 'rb') as f:
    Economy = pickle.load(f)


# Solve the model for different growth factors and save results
annual_growthFactors = np.arange(1.0, 1.1, 0.01)
growthFactors = np.power(annual_growthFactors, 0.25)
T_ageSim = []
LorenzLongLvlSim = []
LorenzLongNrmSim = []
LorenzLongIncSim = []
aLvlGiniSim = []
aNrmGiniSim = []
IncGiniSim = []
aLvlMeanSim = []
aNrmMeanSim = []
aLvlMedianSim = []
aNrmMedianSim = []
aLvlMeanToMedianSim = []
aNrmMeanToMedianSim = []
aLvlPercentilesSim = []
aNrmPercentilesSim = []

T_age_list = [400, 200, 160]

pdb.set_trace()

for T_age in T_age_list:
    for i in range(len(growthFactors)):
        annual_g = annual_growthFactors[i]
        g = growthFactors[i]
        NewEconomy = deepcopy(Economy)
        
        print('Now solving model for T_age = ' + str(T_age) + ' and annual growth = ' + str(annual_g))
        for j in range(len(NewEconomy.agents)):
            NewEconomy.agents[j].PermGroFac = [g]
            NewEconomy.agents[j].T_age = T_age
        
        t_start = clock()
        NewEconomy.solve()
        NewEconomy.calcLorenzDistance()
        NewEconomy.showManyStats()
        t_end = clock()
        
        T_ageSim.append(T_age)
        LorenzLongLvlSim.append(NewEconomy.LorenzLongLvlSim)
        LorenzLongNrmSim.append(NewEconomy.LorenzLongNrmSim)
        LorenzLongIncSim.append(NewEconomy.LorenzLongIncSim)
        aLvlGiniSim.append(getGiniPrc(NewEconomy.LorenzLongLvlSim))
        aNrmGiniSim.append(getGiniPrc(NewEconomy.LorenzLongNrmSim))
        IncGiniSim.append(getGiniPrc(NewEconomy.LorenzLongIncSim))
        aLvlMeanSim.append(NewEconomy.aLvlMeanSim)
        aNrmMeanSim.append(NewEconomy.aNrmMeanSim)
        aLvlMedianSim.append(NewEconomy.aLvlMedianSim)
        aNrmMedianSim.append(NewEconomy.aNrmMedianSim)
        aLvlMeanToMedianSim.append(NewEconomy.aLvlMeanSim / NewEconomy.aLvlMedianSim)
        aNrmMeanToMedianSim.append(NewEconomy.aNrmMeanSim / NewEconomy.aNrmMedianSim)
        aLvlPercentilesSim.append(NewEconomy.aLvlPercentilesSim)
        aNrmPercentilesSim.append(NewEconomy.aNrmPercentilesSim)
        
        print('Solving took ' + str(t_end-t_start) + ' seconds.\n')


# Save growth factors and corresponding results
with open('../../output/VaryGrowth/' + Params.spec_name + '.pkl', 'w') as f:
    pickle.dump([T_ageSim,
                 np.tile(annual_growthFactors, len(T_age_list)),
                 np.tile(growthFactors, len(T_age_list)),
                 LorenzLongLvlSim,
                 LorenzLongNrmSim,
                 LorenzLongIncSim,
                 aLvlGiniSim,
                 aNrmGiniSim,
                 IncGiniSim,
                 aLvlMeanSim,
                 aNrmMeanSim,
                 aLvlMedianSim,
                 aNrmMedianSim,
                 aLvlMeanToMedianSim,
                 aNrmMeanToMedianSim,
                 aLvlPercentilesSim,
                 aNrmPercentilesSim], f)

