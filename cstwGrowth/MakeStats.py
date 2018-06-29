'''
Takes simulation results from VaryGrowth.py and computes various inequality stats for different
combinations of growth factors and terminal ages.

Saves inequality measures as .csv in ../../output/VaryGrowth/

Assumes that estimates of center and spread have already been computed and stored in 
../../output/BaselineEstimates/
This is done by FindEstimates.py

Assumes that the model has been solved and results stored in ../../output/VaryGrowth/
This is done by VaryGrowth.py
'''

import pdb
import numpy as np
import matplotlib.pyplot as plt
import pickle
import pandas as pd
import SetupParams as Params
from cstwGrowth import getGiniPrc

Params.do_param_dist = True     # Do param-dist version if True, param-point if False
do_actual_KY = False            # Use actual K/Y ratio from WID.world if True, 10.26 o.w.
do_lower_T_age = False      # Set the maximum age in simulation to 160 (=74 yrs) intead of 400 if True
estimation_growth = 1.015**(0.25)     # Set growth rate to be used when estimating parameters 
                            # If equal to 1 estimates are saved in ../output/BaselineEstimates/NoGrowth/
                            # If > 1 estimates are saved in ../output/BaselineEstimates/HighGrowth

#pdb.set_trace()

# Update spec_name
Params.spec_name = '/NoGrowth' if estimation_growth == 1 else '/HighGrowth'
if do_actual_KY and not do_lower_T_age:
    Params.spec_name += '/actual_KY'
if do_lower_T_age and not do_actual_KY:
    Params.spec_name += '/lower_T_age'
if do_lower_T_age and do_actual_KY:
    Params.spec_name += '/lower_T_age_actual_KY'
Params.spec_name += '/Dist' if Params.do_param_dist else '/Point'

# Load economy
with open('../../output/BaselineEstimates' + Params.spec_name + '_EstimationEconomy.pkl', 'rb') as f:
    Economy = pickle.load(f)

# Load inequality stats
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
    aNrmPercentilesSim = pickle.load(f)


# Compute top wealth shares
aLvl_top1_share = [1-item[99] for item in LorenzLongLvlSim]
aNrm_top1_share = [1-item[99] for item in LorenzLongNrmSim]

aLvl_top5_share = [1-item[95] for item in LorenzLongLvlSim]
aNrm_top5_share = [1-item[95] for item in LorenzLongNrmSim]

aLvl_top10_share = [1-item[90] for item in LorenzLongLvlSim]
aNrm_top10_share = [1-item[90] for item in LorenzLongNrmSim]

aLvl_bot40_share = [item[40] for item in LorenzLongLvlSim]


# Compute absolute deviation from median
# E.g. (avg wealth of top 5% - median)/median  or  (median - avg wealth of bottom 40%)/median
aLvl_top1_to_median = []
aLvl_top5_to_median = []
aLvl_top10_to_median = []
aLvl_bot40_to_median = []
for i in range(len(aLvl_top1_share)):
    mean = aLvlMeanSim[i]
    median = aLvlMedianSim[i]
    aLvl_top1_to_median.append((aLvl_top1_share[i]*mean/0.01 - median)/median)
    aLvl_top5_to_median.append((aLvl_top5_share[i]*mean/0.05 - median)/median)
    aLvl_top10_to_median.append((aLvl_top10_share[i]*mean/0.1 - median)/median)
    aLvl_bot40_to_median.append((median - aLvl_bot40_share[i]*mean/0.4)/median)
    
    
# Save stats on wealth levels to .csv    
csvdict = {'T_age' : T_ageSim,
           'annual_growth': annual_growthFactors,
           'gini' : aLvlGiniSim,
           'mean_to_median' : aLvlMeanToMedianSim,
           'top1_share' : aLvl_top1_share,
           'top5_share' : aLvl_top5_share,
           'top10_share' : aLvl_top10_share,
           'top1_to_median' : aLvl_top1_to_median,
           'top5_to_median' : aLvl_top5_to_median,
           'top10_to_median' : aLvl_top10_to_median,
           'bot40_to_median' : aLvl_bot40_to_median}

df = pd.DataFrame.from_dict(csvdict)
df.to_csv('../../output/VaryGrowth/' + Params.spec_name + '.csv')
    
# Plot Gini coefficient
fig = plt.figure()
plt.plot(annual_growthFactors[0:11], aLvlGiniSim[0:11], '-ro', label='wealth levels')
plt.plot(annual_growthFactors[0:11], aNrmGiniSim[0:11], '-go', label='wealth to income ratios')
plt.plot(annual_growthFactors[0:11], IncGiniSim[0:11], '-bo', label='income')
plt.axvline(x = Economy.agents[0].PermGroFac[0]**4)
plt.xlabel('Annual growth')
plt.title('Gini coefficient')
plt.legend(bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)
plt.show()
fig.savefig('../../output/Figures_model' + Params.spec_name + '_gini.pdf', bbox_inches='tight')
fig.savefig('../../tex/model_ginis_no_growth.pdf' if estimation_growth==1 else '../../tex/model_ginis_high_growth.pdf', bbox_inches='tight')


## Plot mean-to-median ratio
#fig = plt.figure()
#plt.plot(annual_growthFactors, aLvlMeanToMedianSim, '-ro', label='wealth levels')
#plt.plot(annual_growthFactors, aNrmMeanToMedianSim, '-go', label='wealth ratios')
#plt.axvline(x = Economy.agents[0].PermGroFac[0]**4)
#plt.xlabel('Annual growth')
#plt.title('Mean to median ratio')
#plt.legend(loc='upper right')
#plt.show()
#fig.savefig('../../output/Figures_model' + Params.spec_name + '_mean_to_median.pdf')
#
#
## Plot top wealth shares (levels)
#fig = plt.figure()
#plt.plot(annual_growthFactors, aLvl_top1_share, '-ro', label='top 1%')
#plt.plot(annual_growthFactors, aLvl_top5_share, '-go', label='top 5%')
#plt.plot(annual_growthFactors, aLvl_top10_share, '-bo', label='top 10%')
#plt.axvline(x = Economy.agents[0].PermGroFac[0]**4)
#plt.title('Top wealth shares (levels)')
#plt.xlabel('Annual growth')
#plt.legend(loc='upper right')
#plt.show()
#fig.savefig('../../output/Figures_model' + Params.spec_name + '_top_shares_levels.pdf')
#
#
## Plot top wealth shares (ratios)
#fig = plt.figure()
#plt.plot(annual_growthFactors, aNrm_top1_share, '-ro', label='top 1%')
#plt.plot(annual_growthFactors, aNrm_top5_share, '-go', label='top 5%')
#plt.plot(annual_growthFactors, aNrm_top10_share, '-bo', label='top 10%')
#plt.axvline(x = Economy.agents[0].PermGroFac[0]**4)
#plt.title('Top wealth shares (ratios)')
#plt.xlabel('Annual growth')
#plt.legend(loc='upper right')
#plt.show()
#fig.savefig('../../output/Figures_model' + Params.spec_name + '_top_shares_ratios.pdf')
#
#
## Plot absolute deviations from median
#fig = plt.figure()
#plt.plot(annual_growthFactors, aLvl_top1_to_median, '-ro', label='top 1%')
#plt.plot(annual_growthFactors, aLvl_top5_to_median, '-go', label='top 5%')
#plt.plot(annual_growthFactors, aLvl_top10_to_median, '-bo', label='top 10%')
#plt.plot(annual_growthFactors, aLvl_bot40_to_median, '-yo', label='bottom 40%')
#plt.axvline(x = Economy.agents[0].PermGroFac[0]**4)
#plt.title('Average wealth relative to median')
#plt.xlabel('Annual growth')
#plt.legend(loc='upper right')
#plt.show()
#fig.savefig('../../output/Figures_model' + Params.spec_name + '_dev_from_median.pdf')
#
