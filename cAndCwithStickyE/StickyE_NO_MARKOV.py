'''
This module runs the exercises and regressions for the cAndCwithStickyE paper,
but in models without the Markov growth process.  The results generated by these
models do not appear in the paper.  Without the growth process, there are not
strong enough instruments for the IV regressions to recover the serial corre-
lation of aggregate consumption growth after introducing measurement error in C_t.
User can choose which among the three models are actually run.  Descriptive
statistics and regression results are both output to screen and saved in a log
file in the ./Results directory.  TeX code for tables in the paper are saved in
the ./Tables directory.  See StickyEparams for calibrated model parameters.
'''

import sys 
import os
sys.path.insert(0, os.path.abspath('../'))
sys.path.insert(0, os.path.abspath('../ConsumptionSaving'))

import numpy as np
import csv
from time import clock
from copy import deepcopy
from StickyEmodel import StickyEconsumerType, StickyErepAgent, StickyCobbDouglasEconomy
from ConsAggShockModel import SmallOpenEconomy
from HARKutilities import plotFuncs
import matplotlib.pyplot as plt
import StickyEparams as Params
from StickyEtools import makeStickyEdataFile, runStickyEregressions, makeResultsTable,\
                  runStickyEregressionsInStata, makeParameterTable, makeEquilibriumTable,\
                  makeMicroRegressionTable, extractSampleMicroData, makeuCostVsPiFig

# Choose which models to do work for
do_SOE  = True
do_DSGE = True
do_RA   = True

# Choose what kind of work to do for each model
run_models = True       # Whether to solve models and generate new simulated data
calc_micro_stats = False # Whether to calculate microeconomic statistics (only matters when run_models is True)
make_tables = True      # Whether to make LaTeX tables in the /Tables folder
use_stata = False        # Whether to use Stata to run regressions
save_data = True        # Whether to save data for use in Stata (as a tab-delimited text file)
run_ucost_vs_pi = True  # Whether to run an exercise that finds the cost of stickiness as it varies with update probability

ignore_periods = Params.ignore_periods # Number of simulated periods to ignore as a "burn-in" phase
interval_size = Params.interval_size   # Number of periods in each non-overlapping subsample
total_periods = Params.periods_to_sim  # Total number of periods in simulation
interval_count = (total_periods-ignore_periods)/interval_size # Number of intervals in the macro regressions
periods_to_sim_micro = Params.periods_to_sim_micro # To save memory, micro regressions are run on a smaller sample
AgentCount_micro = Params.AgentCount_micro # To save memory, micro regressions are run on a smaller sample
my_counts = [interval_size,interval_count]
mystr = lambda number : "{:.3f}".format(number)

# Define the function to run macroeconomic regressions, depending on whether Stata is used
if use_stata:
    runRegressions = lambda a,b,c,d,e : runStickyEregressionsInStata(a,b,c,d,e,Params.stata_exe)
else:
    runRegressions = lambda a,b,c,d,e : runStickyEregressions(a,b,c,d,e)



# Run models and save output if this module is called from main
if __name__ == '__main__':
    ###############################################################################
    ################# SMALL OPEN ECONOMY ##########################################
    ###############################################################################
    
    if do_SOE:
        if run_models:
            # Make a small open economy and the consumers who live in it
            StickySOEbaseType = StickyEconsumerType(**Params.init_SOE_consumer)
            StickySOEbaseType.track_vars = ['aLvlNow','cLvlNow','yLvlNow','pLvlTrue','t_age','TranShkNow']
            StickySOEconsumers = []
            for n in range(Params.TypeCount):
                StickySOEconsumers.append(deepcopy(StickySOEbaseType))
                StickySOEconsumers[-1].seed = n
                StickySOEconsumers[-1].DiscFac = Params.DiscFacSetSOE[n]
            StickySOEconomy = SmallOpenEconomy(agents=StickySOEconsumers, **Params.init_SOE_market)
            StickySOEconomy.makeAggShkHist()
            for n in range(Params.TypeCount):
                StickySOEconsumers[n].getEconomyData(StickySOEconomy)
            
            # Solve the small open economy and display some output
            t_start = clock()
            StickySOEconomy.solveAgents()
            t_end = clock()
            print('Solving the small open economy took ' + str(t_end-t_start) + ' seconds.')
            
            print('Consumption function for one type in the small open economy:')
            cFunc = lambda m : StickySOEconsumers[0].solution[0].cFunc(m,np.ones_like(m))
            plotFuncs(cFunc,0.0,20.0)
            
            # Simulate the frictionless small open economy
            t_start = clock()
            for agent in StickySOEconomy.agents:
                agent(UpdatePrb = 1.0)
            StickySOEconomy.makeHistory()
            t_end = clock()
            print('Simulating the frictionless small open economy took ' + mystr(t_end-t_start) + ' seconds.')
            
            # Make results for the frictionless representative agent economy
            desc = 'Results for the frictionless small open economy (update probability 1.0)'
            name = 'SOEsimpleFrictionless'
            makeStickyEdataFile(StickySOEconomy,ignore_periods,description=desc,filename=name,save_data=save_data,calc_micro_stats=calc_micro_stats)
            
            # Simulate the sticky small open economy
            t_start = clock()
            for agent in StickySOEconomy.agents:
                agent(UpdatePrb = Params.UpdatePrb)
            StickySOEconomy.makeHistory()
            t_end = clock()
            print('Simulating the sticky small open economy took ' + mystr(t_end-t_start) + ' seconds.')
            
            # Make results for the sticky small open economy
            desc = 'Results for the sticky small open economy with update probability ' + mystr(Params.UpdatePrb)
            name = 'SOEsimpleSticky'
            makeStickyEdataFile(StickySOEconomy,ignore_periods,description=desc,filename=name,save_data=save_data,calc_micro_stats=calc_micro_stats)
        
        if make_tables:
            # Process the coefficients, standard errors, etc into a LaTeX table
            t_start = clock()
            frictionless_panel = runRegressions('SOEsimpleFrictionlessData',interval_size,False,False,False)
            frictionless_me_panel = runRegressions('SOEsimpleFrictionlessData',interval_size,True,False,False)
            sticky_panel = runRegressions('SOEsimpleStickyData',interval_size,False,True,False)
            sticky_me_panel = runRegressions('SOEsimpleStickyData',interval_size,True,True,False)
            makeResultsTable('Aggregate Consumption Dynamics in SOE Model',[frictionless_me_panel,sticky_panel,sticky_me_panel],my_counts,'SOEsimReg','tPESOEsim')
            t_end = clock()
            print('Running time series regressions for the small open economy took ' + mystr(t_end-t_start) + ' seconds.')
    
    
    
    
    ###############################################################################
    ################# COBB-DOUGLAS ECONOMY ########################################
    ###############################################################################
    
    if do_DSGE:
        if run_models:
            # Make consumers who will live in a Cobb-Douglas economy
            StickyDSGEbaseType = StickyEconsumerType(**Params.init_DSGE_consumer)
            StickyDSGEbaseType.track_vars = ['aLvlNow','cLvlNow','yLvlNow','pLvlTrue','pLvlNow','t_age','TranShkNow']
            StickyDSGEconsumers = []
            for n in range(Params.TypeCount):
                StickyDSGEconsumers.append(deepcopy(StickyDSGEbaseType))
                StickyDSGEconsumers[-1].seed = n
                StickyDSGEconsumers[-1].DiscFac = Params.DiscFacSetDSGE[n]
                
            # Make a Cobb-Douglas economy and put the agents in it
            StickyDSGEeconomy = StickyCobbDouglasEconomy(agents=StickyDSGEconsumers,**Params.init_DSGE_market)
            StickyDSGEeconomy.makeAggShkHist()
            for n in range(Params.TypeCount):
                StickyDSGEconsumers[n].getEconomyData(StickyDSGEeconomy)
                StickyDSGEconsumers[n](UpdatePrb = 1.0)
                
            # Solve the frictionless HA-DSGE model
            t_start = clock()
            StickyDSGEeconomy.solve()
            t_end = clock()
            print('Solving the frictionless Cobb-Douglas economy took ' + mystr(t_end-t_start) + ' seconds.')
            
            # Plot the consumption function
            print('Consumption function for the frictionless Cobb-Douglas economy:')
            m = np.linspace(0.,20.,300)
            for M in StickyDSGEconsumers[0].Mgrid:
                c = StickyDSGEconsumers[0].solution[0].cFunc(m,M*np.ones_like(m))
                plt.plot(m,c)
            plt.show()
            
            # Make results for the frictionless Cobb-Douglas economy
            desc = 'Results for the frictionless Cobb-Douglas economy (update probability 1.0)'
            name = 'DSGEsimpleFrictionless'
            makeStickyEdataFile(StickyDSGEeconomy,ignore_periods,description=desc,filename=name,save_data=save_data,calc_micro_stats=calc_micro_stats)
            
            # Solve the sticky HA-DSGE model
            for agent in StickyDSGEeconomy.agents:
                agent(UpdatePrb = Params.UpdatePrb)
            t_start = clock()
            StickyDSGEeconomy.solve()
            t_end = clock()
            print('Solving the sticky Cobb-Douglas economy took ' + mystr(t_end-t_start) + ' seconds.')
            
            # Plot the consumption function
            print('Consumption function for the sticky Cobb-Douglas economy:')
            m = np.linspace(0.,20.,300)
            for M in StickyDSGEconsumers[0].Mgrid:
                c = StickyDSGEconsumers[0].solution[0].cFunc(m,M*np.ones_like(m))
                plt.plot(m,c)
            plt.show()
            
            # Make results for the sticky Cobb-Douglas economy
            desc = 'Results for the sticky Cobb-Douglas economy with update probability ' + mystr(Params.UpdatePrb)
            name = 'DSGEsimpleSticky'
            makeStickyEdataFile(StickyDSGEeconomy,ignore_periods,description=desc,filename=name,save_data=save_data,calc_micro_stats=calc_micro_stats)
        
        # Process the coefficients, standard errors, etc into a LaTeX table
        if make_tables:
            t_start = clock()
            frictionless_panel = runRegressions('DSGEsimpleFrictionlessData',interval_size,False,False,False)
            frictionless_me_panel = runRegressions('DSGEsimpleFrictionlessData',interval_size,True,False,False)
            sticky_panel = runRegressions('DSGEsimpleStickyData',interval_size,False,True,False)
            sticky_me_panel = runRegressions('DSGEsimpleStickyData',interval_size,True,True,False)
            makeResultsTable('Aggregate Consumption Dynamics in HA-DSGE Model',[frictionless_me_panel,sticky_panel,sticky_me_panel],my_counts,'DSGEsimReg','tDSGEsim')
            t_end = clock()
            print('Running time series regressions for the Cobb-Douglas economy took ' + mystr(t_end-t_start) + ' seconds.')
    
    
    
    ###############################################################################
    ################# REPRESENTATIVE AGENT ECONOMY ################################
    ###############################################################################
    
    if do_RA:
        if run_models:
            # Make a representative agent consumer, then solve and simulate the model
            StickyRAconsumer = StickyErepAgent(**Params.init_RA_consumer)
            StickyRAconsumer.track_vars = ['cLvlNow','yNrmTrue','aLvlNow','pLvlTrue','TranShkNow']
            
            # Solve the representative agent's problem        
            t_start = clock()
            StickyRAconsumer.solve()
            t_end = clock()
            print('Solving the representative agent economy took ' + mystr(t_end-t_start) + ' seconds.')
            
            print('Consumption function for the representative agent:')
            plotFuncs(StickyRAconsumer.solution[0].cFunc,0,50)
            
            # Simulate the representative agent with frictionless expectations
            t_start = clock()
            StickyRAconsumer(UpdatePrb = 1.0)
            StickyRAconsumer.initializeSim()
            StickyRAconsumer.simulate()
            t_end = clock()
            print('Simulating the frictionless representative agent economy took ' + mystr(t_end-t_start) + ' seconds.')
            
            # Make results for the frictionless representative agent economy
            desc = 'Results for the frictionless representative agent economy (update probability 1.0)'
            name = 'RAsimpleFrictionless'
            makeStickyEdataFile(StickyRAconsumer,ignore_periods,description=desc,filename=name,save_data=save_data,calc_micro_stats=calc_micro_stats)
            
            # Simulate the representative agent with sticky expectations
            t_start = clock()
            StickyRAconsumer(UpdatePrb = Params.UpdatePrb)
            StickyRAconsumer.initializeSim()
            StickyRAconsumer.simulate()
            t_end = clock()
            print('Simulating the sticky representative agent economy took ' + mystr(t_end-t_start) + ' seconds.')
            
            # Make results for the sticky representative agent economy
            desc = 'Results for the sticky representative agent economy with update probability ' + mystr(Params.UpdatePrb)
            name = 'RAsimpleSticky'
            makeStickyEdataFile(StickyRAconsumer,ignore_periods,description=desc,filename=name,save_data=save_data,calc_micro_stats=calc_micro_stats)
        
        if make_tables:
            # Process the coefficients, standard errors, etc into a LaTeX table
            t_start = clock()
            frictionless_panel = runRegressions('RAsimpleFrictionlessData',interval_size,False,False,False)
            frictionless_me_panel = runRegressions('RAsimpleFrictionlessData',interval_size,True,False,False)
            sticky_panel = runRegressions('RAsimpleStickyData',interval_size,False,True,False)
            sticky_me_panel = runRegressions('RAsimpleStickyData',interval_size,True,True,False)
            makeResultsTable('Aggregate Consumption Dynamics in RA Model',[frictionless_me_panel,sticky_panel,sticky_me_panel],my_counts,'RepAgentSimReg','tRAsim')
            t_end = clock()
            print('Running time series regressions for the representative agent economy took ' + mystr(t_end-t_start) + ' seconds.')
    