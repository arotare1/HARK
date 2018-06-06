'''
Plots inequality measures for different growth factors and saves them in ./Figures/

Assumes that estimates of center and spread have already been computed and stored in ./ParamsEstimates/
This is done by FindEstimates.py

Assumes that the model has been solved and results stored in ./Results/
This is done by VaryGrowth.py
'''
import pdb
import numpy as np
import matplotlib.pyplot as plt
import pickle
import pandas as pd
import SetupParams as Params
from cstwGrowth import getLorenzShares, getGini

Params.do_param_dist = True    # Do param-dist version if True, param-point if False
Params.do_lifecycle = True     # Use lifecycle model if True, perpetual youth if False
which_estimation_growth = 1.0   # Pick estimates obtained under a specific growth factor 
                                # 1.0 for Baseline, >1 for HighEstimationGrowth
path_estimation_growth = 'Baseline/' if which_estimation_growth == 1 else 'HighEstimationGrowth/'

# Update spec_name
Params.spec_name = 'Dist' if Params.do_param_dist else 'Point'
Params.spec_name += 'LC' if Params.do_lifecycle else 'PY'

# Load US wealth data
lorenz_long_data = np.hstack((np.array(0.0),getLorenzShares(Params.SCF_wealth,weights=Params.SCF_weights,percentiles=np.arange(0.01,1.0,0.01).tolist()),np.array(1.0)))

# Load estimates and growth factor used when finding the estimates
# For now, use "fake" parameters which were estimated for PY with PermGroFac 1.0 and T_age=160
with open('./ParamsEstimates/' + path_estimation_growth + 'DistPY' + '.pkl') as f:
    center_estimate, spread_estimate, estimation_growth = pickle.load(f)

# Load inequality stats
with open('./Results/' + path_estimation_growth + Params.spec_name + '.pkl') as f:
    annual_growthFactors,\
    growthFactors,\
    LorenzLongLvlSim,\
    LorenzLongNrmSim,\
    aLvlGiniSim,\
    aNrmGiniSim,\
    aLvlMeanToMedianSim,\
    aNrmMeanToMedianSim,\
    aLvlPercentilesSim,\
    aNrmPercentilesSim = pickle.load(f)

# Compute some useful wealth shares and percentile ratios (for wealth levels and wealth ratios)
aLvlTop1share = [1-item[99] for item in LorenzLongLvlSim]
aNrmTop1share = [1-item[99] for item in LorenzLongNrmSim]

aLvlTop5share = [1-item[95] for item in LorenzLongLvlSim]
aNrmTop5share = [1-item[95] for item in LorenzLongNrmSim]

aLvlTop10share = [1-item[90] for item in LorenzLongLvlSim]
aNrmTop10share = [1-item[90] for item in LorenzLongNrmSim]

aLvl_99to50 = [item[98]/item[49] for item in aLvlPercentilesSim]
aNrm_99to50 = [item[98]/item[49] for item in aNrmPercentilesSim]

aLvl_95to50 = [item[94]/item[49] for item in aLvlPercentilesSim]
aNrm_95to50 = [item[94]/item[49] for item in aNrmPercentilesSim]

aLvl_90to50 = [item[89]/item[49] for item in aLvlPercentilesSim]
aNrm_90to50 = [item[89]/item[49] for item in aNrmPercentilesSim]

aLvl_80to50 = [item[79]/item[49] for item in aLvlPercentilesSim]
aNrm_80to50 = [item[79]/item[49] for item in aNrmPercentilesSim]

aLvl_95to5 = [item[94]/item[4] for item in aLvlPercentilesSim]
aNrm_95to5 = [item[94]/item[4] for item in aNrmPercentilesSim]

aLvl_90to10 = [item[89]/item[9] for item in aLvlPercentilesSim]
aNrm_90to10 = [item[89]/item[9] for item in aNrmPercentilesSim]

    
# Plot average Lorenz curve for wealth levels
which_growth = np.where(growthFactors==estimation_growth)[0][0] # index of growth for which estimates were computed
LorenzAxis = np.arange(101,dtype=float)
fig = plt.figure()
plt.plot(LorenzAxis, lorenz_long_data, '-k', linewidth=1.5, label='data')
plt.plot(LorenzAxis, LorenzLongLvlSim[which_growth], '--',
         label='g=' + str(annual_growthFactors[which_growth]))
plt.xlabel('Wealth percentile',fontsize=12)
plt.ylabel('Cumulative wealth share',fontsize=12)
plt.ylim([-0.02,1.0])
plt.legend(loc='upper left')
plt.show()
fig.savefig('./Figures/' + path_estimation_growth + 'Lorenz_' + Params.spec_name + '.pdf')


# Plot Gini for wealth levels
aLvlGini = [getGini(item) for item in LorenzLongLvlSim]   # Gini coefficient of average Lorenz curve
fig = plt.figure()
plt.plot(annual_growthFactors, aLvlGiniSim, '-bo', label='avg(gini)')
plt.plot(annual_growthFactors, aLvlGini, '-ro', label='gini(avg)')
plt.axvline(x=estimation_growth**4)
plt.title('Gini coefficient for wealth levels')
plt.xlabel('Growth factor',fontsize=12)
plt.ylabel('Gini coefficient',fontsize=12)
plt.legend(loc='lower right')
plt.show()
fig.savefig('./Figures/' + path_estimation_growth + 'Gini_Lvl_' + Params.spec_name + '.pdf')


# Plot Gini for wealth-to-income ratios
aNrmGini = [getGini(item) for item in LorenzLongNrmSim]   # Gini coefficient of average Lorenz curve
fig = plt.figure()
plt.plot(annual_growthFactors, aNrmGiniSim, '-bo', label='avg(gini)')
plt.plot(annual_growthFactors, aNrmGini, '-ro', label='gini(avg)')
plt.axvline(x=estimation_growth**4)
plt.title('Gini coefficient for wealth-to-income ratios')
plt.xlabel('Growth factor',fontsize=12)
plt.ylabel('Gini coefficient',fontsize=12)
plt.legend(loc='upper right')
plt.show()
fig.savefig('./Figures/' + path_estimation_growth + 'Gini_Nrm_' + Params.spec_name + '.pdf')


# Plot mean-to-median ratio for wealth levels
fig = plt.figure()
plt.plot(annual_growthFactors, aLvlMeanToMedianSim, '-bo')
plt.axvline(x=estimation_growth**4)
plt.title('Mean-to-median ratio for wealth levels')
plt.xlabel('Growth factor',fontsize=12)
plt.ylabel('Mean-to-median ratio',fontsize=12)
plt.legend(loc='lower right')
plt.show()
fig.savefig('./Figures/' + path_estimation_growth + 'MeanToMedian_Lvl_' + Params.spec_name + '.pdf')


# Plot mean-to-median ratio for wealth-to-income ratios
fig = plt.figure()
plt.plot(annual_growthFactors, aNrmMeanToMedianSim, '-bo')
plt.axvline(x=estimation_growth**4)
plt.title('Mean-to-median ratio for wealth-to-income ratios')
plt.xlabel('Growth factor',fontsize=12)
plt.ylabel('Mean-to-median ratio',fontsize=12)
plt.legend(loc='lower right')
plt.show()
fig.savefig('./Figures/' + path_estimation_growth + 'MeanToMedian_Nrm_' + Params.spec_name + '.pdf')


# Plot wealth shares of different percentiles of the wealth level distribution
fig = plt.figure()
plt.plot(annual_growthFactors, aLvlTop1share, '-bo', label='top 1%')
plt.plot(annual_growthFactors, aLvlTop5share, '-ro', label='top 5%')
plt.plot(annual_growthFactors, aLvlTop10share, '-go', label='top 10%')
plt.axvline(x=estimation_growth**4)
plt.title('Wealth level shares')
plt.xlabel('Growth factor',fontsize=12)
plt.ylabel('Wealth share',fontsize=12)
plt.legend(loc='lower right')
plt.show()
fig.savefig('./Figures/' + path_estimation_growth + 'Top_Lvl_' + Params.spec_name + '.pdf')


# Plot wealth shares of different percentiles of the wealth-to-income ratio distribution
fig = plt.figure()
plt.plot(annual_growthFactors, aNrmTop1share, '-bo', label='top 1%')
plt.plot(annual_growthFactors, aNrmTop5share, '-ro', label='top 5%')
plt.plot(annual_growthFactors, aNrmTop10share, '-go', label='top 10%')
plt.axvline(x=estimation_growth**4)
plt.title('Wealth-to-income ratio shares')
plt.xlabel('Growth factor',fontsize=12)
plt.ylabel('Wealth share',fontsize=12)
plt.legend(loc='upper right')
plt.show()
fig.savefig('./Figures/' + path_estimation_growth + 'Top_Nrm_' + Params.spec_name + '.pdf')


# Plot percentile ratios for wealth levels
fig = plt.figure()
plt.plot(annual_growthFactors, aLvl_99to50, '-bo', label='99/50')
plt.plot(annual_growthFactors, aLvl_95to50, '-ro', label='95/50')
plt.plot(annual_growthFactors, aLvl_90to50, '-go', label='90/50')
plt.plot(annual_growthFactors, aLvl_95to5, '-yo', label='95/5')
plt.plot(annual_growthFactors, aLvl_90to10, '-ko', label='90/10')
plt.plot(annual_growthFactors, aLvl_80to50, '-mo', label='80/50')
plt.axvline(x=estimation_growth**4)
plt.title('Wealth levels percentile ratios')
plt.xlabel('Growth factor',fontsize=12)
plt.ylabel('Ratio',fontsize=12)
plt.legend(loc='upper left')
plt.show()
fig.savefig('./Figures/' + path_estimation_growth + 'PrcRatios_Lvl_' + Params.spec_name + '.pdf')


# Plot percentile ratios for wealth-to-income ratios
fig = plt.figure()
plt.plot(annual_growthFactors, aNrm_99to50, '-bo', label='99/50')
plt.plot(annual_growthFactors, aNrm_95to50, '-ro', label='95/50')
plt.plot(annual_growthFactors, aNrm_90to50, '-go', label='90/50')
plt.plot(annual_growthFactors, aNrm_95to5, '-yo', label='95/5')
plt.plot(annual_growthFactors, aNrm_90to10, '-ko', label='90/10')
plt.plot(annual_growthFactors, aNrm_80to50, '-mo', label='80/50')
plt.axvline(x=estimation_growth**4)
plt.title('Wealth-to-income percentile ratios')
plt.xlabel('Growth factor',fontsize=12)
plt.ylabel('Ratio',fontsize=12)
plt.legend(loc='upper right')
plt.show()
fig.savefig('./Figures/' + path_estimation_growth + 'PrcRatios_Nrm_' + Params.spec_name + '.pdf')

