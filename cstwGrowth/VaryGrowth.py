'''
Solves model for different growth factors and saves results in ../../output/VaryGrowth/

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

Params.do_param_dist = False     # Do param-dist version if True, param-point if False
do_actual_KY = False            # Use actual K/Y ratio from WID.world if True, 10.26 o.w.
estimation_growth = 1.0     # Set growth rate to be used when estimating parameters 
                            # If equal to 1 estimates are saved in ../output/BaselineEstimates/NoGrowth/
                            # If > 1 estimates are saved in ../output/BaselineEstimates/HighGrowth


# Update spec_name
Params.spec_name = '/NoGrowth' if estimation_growth == 1 else '/HighGrowth'
Params.spec_name += '/ActualKY' if do_actual_KY else '/BaselineKY' 
Params.spec_name += '/Dist' if Params.do_param_dist else '/Point'


# Load economy
with open('../../output/BaselineEstimates' + Params.spec_name + '_EstimationEconomy.pkl', 'rb') as f:
    Economy = pickle.load(f)


# Solve the model for different growth factors and save results
annual_growthFactors = np.arange(1.0, 1.07, 0.01)
growthFactors = np.power(annual_growthFactors, 0.25)
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

for i in range(len(growthFactors)):
    annual_g = annual_growthFactors[i]
    g = growthFactors[i]
    NewEconomy = deepcopy(Economy)
    
    print('Now solving model for annual growth = ' + str(annual_g))
    for j in range(len(NewEconomy.agents)):
        NewEconomy.agents[j].PermGroFac = [g]
    
    t_start = clock()
    NewEconomy.solve()
    NewEconomy.calcLorenzDistance()
    NewEconomy.showManyStats()
    t_end = clock()
    
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
    pickle.dump([annual_growthFactors,
                 growthFactors,
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

