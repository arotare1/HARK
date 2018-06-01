'''
Implements a pseudo-lifecycle version of the model perpetual youth model where simulated agents 
don't survive beyond a certain age T_age.

The exercise is done for two cases:
Case 1. Agents update their consumption rule when growth changes
    Case 1a. T_age == 400 (this is the default value and corresponds to a maximum age of 24+400/4=124 yrs)
    Case 1b. T_age == 200 (this corresponds to a maximum age of 24+200/4=74 yrs)
    Case 1c. T_age == 120 (this corresponds to a maximum age of 24+120/4=54 yrs)
Case 2. Agents DO NOT update their consumption rule when growth changes
    Case 2a. T_age == 400 (this is the default value and corresponds to a maximum age of 24+400/4=124 yrs)
    Case 2b. T_age == 200 (this corresponds to a maximum age of 24+200/4=74 yrs)
    Case 2c. T_age == 120 (this corresponds to a maximum age of 24+120/4=54 yrs)

This experiment should shed light on why inequality in wealth levels is not monotonic in the PY model.
It is possible that inequality goes back up after a certain growth level because of the agents that
have been around for many years.

Figures are saved in ./Figures/Baseline/PseudoLC/ and ./Figures/HighEstimationGrowth/PseudoLC/

Assumes that estimates of center and spread have already been computed and stored in ./ParamsEstimates/
This is done by FindEstimates.py

Assumes the model has been solved for Case 1a. and results stored in ./Results/
This is done by VaryGrowth.py 

Assumes the model has been simulated for Case 2a. and simulation results stored in 
./Results/Baseline/Decompose_inequality/ and ./Results/HighEstimationGrowth/Decompose_inequality/
This is done by Decompose_inequality.py
'''

#----------------------------------------------------------------------------------
# Case 1. Agents update their consumption rule when growth changes
#----------------------------------------------------------------------------------


