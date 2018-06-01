'''
Adapts the cstwMPC model and examines what it implies about the relationship between
wealth inequality and growth.
'''

# Import the HARK library.  The assumption is that this code is in a folder
# contained in the HARK folder. Also import ConsumptionSavingModel
import pdb
import sys 
import os
sys.path.insert(0, os.path.abspath('../'))
sys.path.insert(0, os.path.abspath('../ConsumptionSaving'))

import numpy as np
from copy import copy, deepcopy
from time import clock
from HARKutilities import approxMeanOneLognormal, combineIndepDstns, approxUniform, \
                          getPercentiles, getLorenzShares, calcSubpopAvg, approxLognormal
from HARKsimulation import drawDiscrete
from HARKcore import Market
#from HARKparallel import multiThreadCommandsFake
import SetupParams as Params
import ConsIndShockModel as Model
from ConsAggShockModel import CobbDouglasEconomy, AggShockConsumerType
from scipy.optimize import golden, brentq
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import pickle
import pandas as pd

mystr = lambda number : "{:.3f}".format(number)

if Params.do_agg_shocks:
    EstimationAgentClass = AggShockConsumerType
    EstimationMarketClass = CobbDouglasEconomy
else:
    EstimationAgentClass = Model.IndShockConsumerType
    EstimationMarketClass = Market

class cstwMPCagent(EstimationAgentClass):
    '''
    A slight extension of the idiosyncratic consumer type for the cstwMPC model.
    '''
    def reset(self):
        self.initializeSim()
        self.t_age = drawDiscrete(self.AgentCount,P=self.AgeDstn,X=np.arange(self.AgeDstn.size),exact_match=False,seed=self.RNG.randint(0,2**31-1)).astype(int)
        self.t_cycle = copy(self.t_age)
        if hasattr(self,'kGrid'):
            self.aLvlNow = self.kInit*np.ones(self.AgentCount) # Start simulation near SS
            self.aNrmNow = self.aLvlNow/self.pLvlNow
        
    def marketAction(self):
        if hasattr(self,'kGrid'):
            self.pLvl = self.pLvlNow/np.mean(self.pLvlNow)
        self.simulate(1)
        
    def updateIncomeProcess(self):
        '''
        An alternative method for constructing the income process in the infinite horizon model.
        
        Parameters
        ----------
        none
            
        Returns
        -------
        none
        '''
        if self.cycles == 0:
            tax_rate = (self.IncUnemp*self.UnempPrb)/((1.0-self.UnempPrb)*self.IndL)
            TranShkDstn     = deepcopy(approxMeanOneLognormal(self.TranShkCount,sigma=self.TranShkStd[0],tail_N=0))
            TranShkDstn[0]  = np.insert(TranShkDstn[0]*(1.0-self.UnempPrb),0,self.UnempPrb)
            TranShkDstn[1]  = np.insert(TranShkDstn[1]*(1.0-tax_rate)*self.IndL,0,self.IncUnemp)
            PermShkDstn     = approxMeanOneLognormal(self.PermShkCount,sigma=self.PermShkStd[0],tail_N=0)
            self.IncomeDstn = [combineIndepDstns(PermShkDstn,TranShkDstn)]
            self.TranShkDstn = TranShkDstn
            self.PermShkDstn = PermShkDstn
            self.addToTimeVary('IncomeDstn')
        else: # Do the usual method if this is the lifecycle model
            EstimationAgentClass.updateIncomeProcess(self)

class cstwMPCmarket(EstimationMarketClass):
    '''
    A class for representing the economy in the cstwMPC model.
    '''
    reap_vars = ['aLvlNow','pLvlNow','MPCnow','TranShkNow','EmpNow','t_age']
    sow_vars  = [] # Nothing needs to be sent back to agents in the idiosyncratic shocks version
    const_vars = ['LorenzBool','ManyStatsBool']
    track_vars = ['MaggNow','AaggNow','KtoYnow',
                  'LorenzLvl','LorenzLongLvl', 'LorenzLongNrm',
                  'aLvlGini', 'aNrmGini',
                  'aLvlMeanToMedian', 'aNrmMeanToMedian',
                  'aLvlPercentiles', 'aNrmPercentiles']
    dyn_vars = [] # No dynamics in the idiosyncratic shocks version
    
    def __init__(self,**kwds):
        '''
        Make a new instance of cstwMPCmarket.
        '''
        self.assignParameters(**kwds)
        if self.AggShockBool:
            self.sow_vars=['MaggNow','AaggNow','RfreeNow','wRteNow','PermShkAggNow','TranShkAggNow','KtoLnow']
            self.dyn_vars=['AFunc']
            self.max_loops = 20
        
    def solve(self):
        '''
        Solves the cstwMPCmarket.
        '''
        if self.AggShockBool:
            for agent in self.agents:
                agent.getEconomyData(self)
            Market.solve(self)
        else:
            self.solveAgents()
            self.makeHistory()
        
    def millRule(self,aLvlNow,pLvlNow,MPCnow,TranShkNow,EmpNow,t_age,LorenzBool,ManyStatsBool):
        '''
        The millRule for this class simply calls the method calcStats.
        '''
        self.calcStats(aLvlNow,pLvlNow,MPCnow,TranShkNow,EmpNow,t_age,LorenzBool,ManyStatsBool)
        if self.AggShockBool:
            return self.calcRandW(aLvlNow,pLvlNow)
        else: # These variables are tracked but not created in no-agg-shocks specifications
            self.MaggNow = 0.0
            self.AaggNow = 0.0
        
    def calcStats(self,aLvlNow,pLvlNow,MPCnow,TranShkNow,EmpNow,t_age,LorenzBool,ManyStatsBool):
        '''
        Calculate various statistics about the current population in the economy.
        
        Parameters
        ----------
        aLvlNow : [np.array]
            Arrays with end-of-period assets, listed by each ConsumerType in self.agents.
        pLvlNow : [np.array]
            Arrays with permanent income levels, listed by each ConsumerType in self.agents.
        MPCnow : [np.array]
            Arrays with marginal propensity to consume, listed by each ConsumerType in self.agents.
        TranShkNow : [np.array]
            Arrays with transitory income shocks, listed by each ConsumerType in self.agents.
        EmpNow : [np.array]
            Arrays with employment states: True if employed, False otherwise.
        t_age : [np.array]
            Arrays with periods elapsed since model entry, listed by each ConsumerType in self.agents.
        LorenzBool: bool
            Indicator for whether the Lorenz target points should be calculated.  Usually False,
            only True when DiscFac has been identified for a particular nabla.
        ManyStatsBool: bool
            Indicator for whether a lot of statistics for tables should be calculated. Usually False,
            only True when parameters have been estimated and we want values for tables.
            
        Returns
        -------
        None
        '''
        # Combine inputs into single arrays
        aLvl = np.hstack(aLvlNow)
        pLvl = np.hstack(pLvlNow)
        TranShk = np.hstack(TranShkNow)
        IncLvl = TranShk*pLvl # Labor income this period
        aNrm = aLvl/pLvl # wealth-to-income ratio
        age  = np.hstack(t_age)
        
        # Calculate the capital to income ratio in the economy
        CohortWeight = self.PopGroFac**(-age)
        CapAgg = np.sum(aLvl*CohortWeight)
        IncAgg = np.sum(pLvl*TranShk*CohortWeight)
        KtoYnow = CapAgg/IncAgg
        self.KtoYnow = KtoYnow
        
        # Store Lorenz data if requested
        self.LorenzLongLvl = np.nan
        self.LorenzLongNrm = np.nan
        if LorenzBool:
            wealth_shares = getLorenzShares(aLvl,weights=CohortWeight,
                                            percentiles=self.LorenzPercentiles,
                                            presorted=False)
            self.LorenzLvl = wealth_shares
            
            if ManyStatsBool:
                self.LorenzLongLvl = getLorenzShares(aLvl,weights=CohortWeight,
                                                     percentiles=np.arange(0.01,1.0,0.01),presorted=False)
                self.LorenzLongNrm = getLorenzShares(aNrm,weights=CohortWeight,
                                                     percentiles=np.arange(0.01,1.0,0.01),presorted=False)                
        else: # Store nothing if we don't want Lorenz data
            self.LorenzLvl = np.nan
            
        # Calculate a whole bunch of statistics if requested
        if ManyStatsBool:
            # Compute statistics for wealth levels
            self.aLvlGini = getGini(aLvl,weights=CohortWeight,presorted=False)
            self.aLvlMeanToMedian = np.mean(aLvl)/np.median(aLvl)
            self.aLvlPercentiles = getPercentiles(aLvl,weights=CohortWeight,
                                                  percentiles=np.arange(0.01,1.0,0.01),presorted=False)
            
            # Compute statistics for wealth-to-income ratios
            self.aNrmGini = getGini(aNrm,weights=CohortWeight,presorted=False)
            self.aNrmMeanToMedian = np.mean(aNrm)/np.median(aNrm)
            self.aNrmPercentiles = getPercentiles(aNrm,weights=CohortWeight,
                                                  percentiles=np.arange(0.01,1.0,0.01),presorted=False)

        else: # If we don't want these stats, just put empty values in history
            self.aLvlGini = np.nan
            self.aNrmGini = np.nan
            self.aLvlMeanToMedian = np.nan
            self.aNrmMeanToMedian = np.nan
            self.aLvlPercentiles = np.nan
            self.aNrmPercentiles = np.nan
            
            
    def distributeParams(self,param_name,param_count,center,spread,dist_type):
        '''
        Distributes heterogeneous values of one parameter to the AgentTypes in self.agents.
        
        Parameters
        ----------
        param_name : string
            Name of the parameter to be assigned.
        param_count : int
            Number of different values the parameter will take on.
        center : float
            A measure of centrality for the distribution of the parameter.
        spread : float
            A measure of spread or diffusion for the distribution of the parameter.
        dist_type : string
            The type of distribution to be used.  Can be "lognormal" or "uniform" (can expand).
            
        Returns
        -------
        None
        '''
        # Get a list of discrete values for the parameter
        if dist_type == 'uniform':
            # If uniform, center is middle of distribution, spread is distance to either edge
            param_dist = approxUniform(N=param_count,bot=center-spread,top=center+spread)
        elif dist_type == 'lognormal':
            # If lognormal, center is the mean and spread is the standard deviation (in log)
            tail_N = 3
            param_dist = approxLognormal(N=param_count-tail_N,mu=np.log(center)-0.5*spread**2,sigma=spread,tail_N=tail_N,tail_bound=[0.0,0.9], tail_order=np.e)
            
        # Distribute the parameters to the various types, assigning consecutive types the same
        # value if there are more types than values
        replication_factor = len(self.agents)/param_count
        j = 0
        b = 0
        while j < len(self.agents):
            for n in range(replication_factor):
                self.agents[j](AgentCount = int(self.Population*param_dist[0][b]*self.TypeWeight[n]))
                exec('self.agents[j](' + param_name + '= param_dist[1][b])')
                j += 1
            b += 1
            
    def calcKYratioDifference(self):
        '''
        Returns the difference between the simulated capital to income ratio and the target ratio.
        Can only be run after solving all AgentTypes and running makeHistory.
        
        Parameters
        ----------
        None
        
        Returns
        -------
        diff : float
            Difference between simulated and target capital to income ratio.
        '''
        # Ignore the first X periods to allow economy to stabilize from initial conditions
        KYratioSim = np.mean(np.array(self.KtoYnow_hist)[self.ignore_periods:])
        diff = KYratioSim - self.KYratioTarget
        return diff
        
    def calcLorenzDistance(self):
        '''
        Returns the sum of squared differences between simulated and target Lorenz points.
        
        Parameters
        ----------
        None
        
        Returns
        -------
        dist : float
            Sum of squared distances between simulated and target Lorenz points (sqrt)
        '''
        LorenzLvlSim = np.mean(np.array(self.LorenzLvl_hist)[self.ignore_periods:,:],axis=0)
        dist = np.sqrt(np.sum((100*(LorenzLvlSim - self.LorenzTarget))**2))
        self.LorenzDistance = dist        
        return dist
        
    def showManyStats(self,spec_name=None):
        '''
        Calculates the "many statistics" by averaging histories across simulated periods.  Displays
        the results as text and saves them to files if spec_name is not None.
        
        Parameters
        ----------
        spec_name : string
            A name or label for the current specification.
            
        Returns
        -------
        None
        '''
        
        # Compute and store Lorenz curves for wealth levels and wealth-to-income ratios
        self.LorenzLongLvlSim = np.hstack((np.array(0.0),np.mean(np.array(self.LorenzLongLvl_hist)[self.ignore_periods:,:],axis=0),np.array(1.0)))
        self.LorenzLongNrmSim = np.hstack((np.array(0.0),np.mean(np.array(self.LorenzLongNrm_hist)[self.ignore_periods:,:],axis=0),np.array(1.0)))
        
        # Compute and store average inequality data across histories
        self.aLvlGiniSim = np.mean(self.aLvlGini_hist[self.ignore_periods:])
        self.aNrmGiniSim = np.mean(self.aNrmGini_hist[self.ignore_periods:])
        self.aLvlMeanToMedianSim = np.mean(self.aLvlMeanToMedian_hist[self.ignore_periods:])
        self.aNrmMeanToMedianSim = np.mean(self.aNrmMeanToMedian_hist[self.ignore_periods:])
        self.aLvlPercentilesSim = np.hstack(np.mean(np.array(self.aLvlPercentiles_hist)[self.ignore_periods:,:],axis=0))
        self.aNrmPercentilesSim = np.hstack(np.mean(np.array(self.aNrmPercentiles_hist)[self.ignore_periods:,:],axis=0))
                     
        # Make a string of results to display
        results_string = 'Estimate is center=' + str(self.center_estimate) + ', spread=' + str(self.spread_estimate) + '\n'
        if hasattr(self, 'LorenzDistance'): # This attribute only exists if we run the estimation
            results_string += 'Lorenz distance is ' + str(self.LorenzDistance) + '\n'
        results_string += 'Gini coefficient for wealth levels is ' + str(self.aLvlGiniSim) + '\n'
        results_string += 'Gini coefficient for wealth-to-income ratios is ' + str(self.aNrmGiniSim) + '\n'
        results_string += 'Mean to median ratio for wealth levels is ' + str(self.aLvlMeanToMedianSim) + '\n'
        results_string += 'Mean to median ratio for wealth-to-income ratios is ' + str(self.aNrmMeanToMedianSim) + '\n'
        print(results_string)
        
        
def getKYratioDifference(Economy,param_name,param_count,center,spread,dist_type):
    '''
    Finds the difference between simulated and target capital to income ratio in an economy when
    a given parameter has heterogeneity according to some distribution.
    
    Parameters
    ----------
    Economy : cstwMPCmarket
        An object representing the entire economy, containing the various AgentTypes as an attribute.
    param_name : string
        The name of the parameter of interest that varies across the population.
    param_count : int
        The number of different values the parameter of interest will take on.
    center : float
        A measure of centrality for the distribution of the parameter of interest.
    spread : float
        A measure of spread or diffusion for the distribution of the parameter of interest.
    dist_type : string
        The type of distribution to be used.  Can be "lognormal" or "uniform" (can expand).
        
    Returns
    -------
    diff : float
        Difference between simulated and target capital to income ratio for this economy.
    '''
    Economy(LorenzBool = False, ManyStatsBool = False) # Make sure we're not wasting time calculating stuff
    Economy.distributeParams(param_name,param_count,center,spread,dist_type) # Distribute parameters
    Economy.solve()
    diff = Economy.calcKYratioDifference()
    print('getKYratioDifference tried center = ' + str(center) + ' and got ' + str(diff))
    return diff
    
    
def findLorenzDistanceAtTargetKY(Economy,param_name,param_count,center_range,spread,dist_type):
    '''
    Finds the sum of squared distances between simulated and target Lorenz points in an economy when
    a given parameter has heterogeneity according to some distribution.  The class of distribution
    and a measure of spread are given as inputs, but the measure of centrality such that the capital
    to income ratio matches the target ratio must be found.
    
    Parameters
    ----------
    Economy : cstwMPCmarket
        An object representing the entire economy, containing the various AgentTypes as an attribute.
    param_name : string
        The name of the parameter of interest that varies across the population.
    param_count : int
        The number of different values the parameter of interest will take on.
    center_range : [float,float]
        Bounding values for a measure of centrality for the distribution of the parameter of interest.
    spread : float
        A measure of spread or diffusion for the distribution of the parameter of interest.
    dist_type : string
        The type of distribution to be used.  Can be "lognormal" or "uniform" (can expand).
        
    Returns
    -------
    dist : float
        Sum of squared distances between simulated and target Lorenz points for this economy (sqrt).
    '''
    # Define the function to search for the correct value of center, then find its zero
    intermediateObjective = lambda center : getKYratioDifference(Economy = Economy,
                                                                 param_name = param_name,
                                                                 param_count = param_count,
                                                                 center = center,
                                                                 spread = spread,
                                                                 dist_type = dist_type)
    optimal_center = brentq(intermediateObjective,center_range[0],center_range[1],xtol=10**(-6))
    Economy.center_save = optimal_center
    
    # Get the sum of squared Lorenz distances given the correct distribution of the parameter
    Economy(LorenzBool = True) # Make sure we actually calculate simulated Lorenz points
    Economy.distributeParams(param_name,param_count,optimal_center,spread,dist_type) # Distribute parameters
    Economy.solveAgents()
    Economy.makeHistory()
    dist = Economy.calcLorenzDistance()
    Economy(LorenzBool = False)
    print ('findLorenzDistanceAtTargetKY tried spread = ' + str(spread) + ' and got ' + str(dist))
    return dist
    
def calcStationaryAgeDstn(LivPrb,terminal_period):
    '''
    Calculates the steady state proportions of each age given survival probability sequence LivPrb.
    Assumes that agents who die are replaced by a newborn agent with t_age=0.
    
    Parameters
    ----------
    LivPrb : [float]
        Sequence of survival probabilities in ordinary chronological order.  Has length T_cycle.
    terminal_period : bool
        Indicator for whether a terminal period follows the last period in the cycle (with LivPrb=0).
        
    Returns
    -------
    AgeDstn : np.array
        Stationary distribution of age.  Stochastic vector with frequencies of each age.
    '''
    T = len(LivPrb)
    if terminal_period:
        MrkvArray = np.zeros((T+1,T+1))
        top = T
    else:
        MrkvArray = np.zeros((T,T))
        top = T-1
        
    for t in range(top):
        MrkvArray[t,0] = 1.0 - LivPrb[t]
        MrkvArray[t,t+1] = LivPrb[t]
    MrkvArray[t+1,0] = 1.0
    
    w, v = np.linalg.eig(np.transpose(MrkvArray))
    idx = (np.abs(w-1.0)).argmin()
    x = v[:,idx].astype(float)
    AgeDstn = (x/np.sum(x))
    return AgeDstn

def getGini(data,weights=None,presorted=False):
    '''
    Computes the Gini coefficient from (weighted) wealth data
    
    Parameters
    ----------
    data : numpy.array
        A 1D array of float data.
    weights : numpy.array
        A weighting vector for the data.
    presorted : boolean
        Indicator for whether data has already been sorted.
        
    Returns
    -------
    Gini : float
        The Gini coefficient of the data.
    '''
    n = data.size
    if weights is None: # Set equiprobable weights if none were given
        weights = np.ones(n)
    
    if presorted: # Sort the data if it is not already
        data_sorted = data
        weights_sorted = weights
    else:
        order = np.argsort(data)
        data_sorted = data[order]
        weights_sorted = weights[order]
    
    wealth_sorted = data_sorted*weights_sorted
    index = np.arange(1, n+1, 1)
    Gini = np.sum((2*index-n-1)*wealth_sorted)/(n*np.sum(wealth_sorted))
    return Gini

