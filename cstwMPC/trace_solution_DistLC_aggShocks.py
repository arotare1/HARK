#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Thu May 24 12:17:57 2018

@author: andreea
"""

EstimationEconomy.solve() #---------->
    # this method is defined in cstwMPC.py
    if self.AggShockBool:
            for agent in self.agents:
                agent.getEconomyData(self) #---------->
                    # this method is defined in ConsAggShockModel.py
                    '''
                    Imports economy-determined objects into self from a Market.
                    Instances of AggShockConsumerType "live" in some macroeconomy that has
                    attributes relevant to their microeconomic model, like the relationship
                    between the capital-to-labor ratio and the interest and wage rates; this
                    method imports those attributes from an "economy" object and makes them
                    attributes of the ConsumerType.
                    
                    Parameters
                    ----------
                    Economy : Market
                        The "macroeconomy" in which this instance "lives".  Might be of the
                        subclass CobbDouglasEconomy, which has methods to generate the
                        relevant attributes.
                        
                    Returns
                    -------
                    None
                    '''
                    self.T_sim = Economy.act_T                          # Need to be able to track as many periods as economy runs
                    self.kInit = Economy.kSS                            # Initialize simulation assets to steady state
                    self.aNrmInitMean = np.log(0.00000001)              # Initialize newborn assets to nearly zero
                    self.Mgrid = Economy.MSS*self.MgridBase             # Aggregate market resources grid adjusted around SS capital ratio
                    self.AFunc = Economy.AFunc                          # Next period's aggregate savings function
                    self.Rfunc = Economy.Rfunc                          # Interest factor as function of capital ratio
                    self.wFunc = Economy.wFunc                          # Wage rate as function of capital ratio
                    self.DeprFac = Economy.DeprFac                      # Rate of capital depreciation
                    self.PermGroFacAgg = Economy.PermGroFacAgg          # Aggregate permanent productivity growth
                    self.addAggShkDstn(Economy.AggShkDstn)              # Combine idiosyncratic and aggregate shocks into one dstn 
                    self.addToTimeInv('Mgrid','AFunc','Rfunc', 'wFunc','DeprFac','PermGroFacAgg')
                    
                    
            Market.solve(self) #---------->
                # this method is defined in HARKcore.py
                '''
                "Solves" the market by finding a "dynamic rule" that governs the aggregate
                market state such that when agents believe in these dynamics, their actions
                collectively generate the same dynamic rule.
                
                Parameters
                ----------
                None
                
                Returns
                -------
                None
                '''
                go              = True
                max_loops       = self.max_loops # Failsafe against infinite solution loop
                completed_loops = 0
                old_dynamics    = None
        
                while go: # Loop until the dynamic process converges or we hit the loop cap
                    self.solveAgents()     # Solve each AgentType's micro problem #---------->
                        # this method is defined in HARKcore.py
                        # self refers to market
                        for this_type in self.agents:
                            this_type.solve() #---------->
                                # this method is defined in HARKcore.py
                                # self refers to agent
                                self.preSolve() #---------->
                                    # this method is defined for IndShockConsumerType which inherits PerfForesightConsumerType
                                    # it creates the terminal solution for each agent type
                                    PerfForesightConsumerType.preSolve(self) #---------->
                                        # this method is defined for AgentType in HARKcore
                                        self.checkElementsOfTimeVaryAreLists()
                                        return None
                                    
                                    self.updateSolutionTerminal() #---------->
                                        # this method is defined for AggShockConsumerType
                                        cFunc_terminal  = BilinearInterp(np.array([[0.0,0.0],[1.0,1.0]]),np.array([0.0,1.0]),np.array([0.0,1.0]))
                                        vPfunc_terminal = MargValueFunc2D(cFunc_terminal,self.CRRA)
                                        mNrmMin_terminal = ConstantFunction(0)
                                        self.solution_terminal = ConsumerSolution(cFunc=cFunc_terminal,vPfunc=vPfunc_terminal,mNrmMin=mNrmMin_terminal)
                                        
                                self.solution = solveAgent(self,verbose) #---------->
                                    # This method is defined in HARKcore.py
                                    # Solve the model by backward induction
                                    '''
                                    Solve the dynamic model for one agent type.  This function iterates on "cycles"
                                    of an agent's model either a given number of times or until solution convergence
                                    if an infinite horizon model is used (with agent.cycles = 0).
                                    
                                    Returns
                                    -------
                                    solution : [Solution]
                                        A list of solutions to the one period problems that the agent will
                                        encounter in his "lifetime".  Returns in reverse chronological order.
                                    '''
                                    # Record the flow of time when the Agent began the process, and make sure time is flowing backwards
                                    original_time_flow = agent.time_flow
                                    agent.timeRev()
                                
                                    # Check to see whether this is an (in)finite horizon problem
                                    cycles_left      = agent.cycles
                                    infinite_horizon = cycles_left == 0
                                    
                                    # Initialize the solution, which includes the terminal solution if it's not a pseudo-terminal period
                                    solution = []
                                    if not agent.pseudo_terminal:
                                        solution.append(deepcopy(agent.solution_terminal))
                                
                                    # Initialize the process, then loop over cycles
                                    solution_last    = agent.solution_terminal
                                    go               = True
                                    completed_cycles = 0
                                    max_cycles       = 5000 # escape clause
                                    if verbose:
                                        t_last = clock()
                                    while go:
                                        # Solve a cycle of the model, recording it if horizon is finite
                                        solution_cycle = solveOneCycle(agent,solution_last) #---------->
                                            # Calculate number of periods per cycle, defaults to 1 if all variables are time invariant
                                            if len(agent.time_vary) > 0:
                                                name = agent.time_vary[0]
                                                T    = len(eval('agent.' + name))
                                            else:
                                                T = 1
                                        
                                            # Check whether the same solution method is used in all periods
                                            always_same_solver = 'solveOnePeriod' not in agent.time_vary
                                            if always_same_solver:
                                                solveOnePeriod = agent.solveOnePeriod
                                                these_args     = getArgNames(solveOnePeriod)
                                        
                                            # Construct a dictionary to be passed to the solver
                                            time_inv_string = ''
                                            for name in agent.time_inv:
                                                time_inv_string += ' \'' + name + '\' : agent.' +name + ','
                                            time_vary_string = ''
                                            for name in agent.time_vary:
                                                time_vary_string += ' \'' + name + '\' : None,'
                                            solve_dict = eval('{' + time_inv_string + time_vary_string + '}')
                                        
                                            # Initialize the solution for this cycle, then iterate on periods
                                            solution_cycle = []
                                            solution_next  = solution_last
                                            for t in range(T):
                                                # Update which single period solver to use (if it depends on time)
                                                if not always_same_solver:
                                                    solveOnePeriod = agent.solveOnePeriod[t]
                                                    these_args = getArgNames(solveOnePeriod)
                                        
                                                # Update time-varying single period inputs
                                                for name in agent.time_vary:
                                                    if name in these_args:
                                                        solve_dict[name] = eval('agent.' + name + '[t]')
                                                solve_dict['solution_next'] = solution_next
                                                
                                                # Make a temporary dictionary for this period
                                                temp_dict = {name: solve_dict[name] for name in these_args}
                                                
                                                # Arguments passed to solver are: ('solution_next', 'IncomeDstn', 'LivPrb', 'DiscFac', 'CRRA', 'PermGroFac', 'PermGroFacAgg', 'aXtraGrid', 'BoroCnstArt', 'Mgrid', 'AFunc', 'Rfunc', 'wFunc', 'DeprFac')
                                                # Solver is: <function solveConsAggShock at 0x1a21036e60> 
                                                # Solve one period, add it to the solution, and move to the next period
                                                solution_t = solveOnePeriod(**temp_dict)
                                                solution_cycle.append(solution_t)
                                                solution_next = solution_t
                                        
                                            # Return the list of per-period solutions
                                            return solution_cycle
                                        
                                        
                                        if not infinite_horizon:
                                            solution += solution_cycle
                                
                                        # Check for termination: identical solutions across cycle iterations or run out of cycles     
                                        solution_now = solution_cycle[-1]
                                        if infinite_horizon:
                                            if completed_cycles > 0:
                                                solution_distance = solution_now.distance(solution_last)
                                                go = (solution_distance > agent.tolerance and completed_cycles < max_cycles)
                                            else: # Assume solution does not converge after only one cycle
                                                solution_distance = 100.0
                                                go = True
                                        else:
                                            cycles_left += -1
                                            go = cycles_left > 0
                                
                                        # Update the "last period solution"
                                        solution_last = solution_now
                                        completed_cycles += 1
                                        
                                        # Display progress if requested
                                        if verbose:
                                            t_now = clock()
                                            if infinite_horizon:
                                                print('Finished cycle #' + str(completed_cycles) + ' in ' + str(t_now-t_last) +\
                                                ' seconds, solution distance = ' + str(solution_distance))
                                            else:
                                                print('Finished cycle #' + str(completed_cycles) + ' of ' + str(agent.cycles) +\
                                                ' in ' + str(t_now-t_last) + ' seconds.')
                                            t_last = t_now
                                
                                    # Record the last cycle if horizon is infinite (solution is still empty!)
                                    if infinite_horizon:
                                        solution = solution_cycle # PseudoTerminal=False impossible for infinite horizon
                                
                                    # Restore the direction of time to its original orientation, then return the solution
                                    if original_time_flow:
                                        agent.timeFwd()
                                    return solution
                                    
                                    
                                    
                                if self.time_flow: # Put the solution in chronological order if this instance's time flow runs that way
                                    self.solution.reverse()
                                self.addToTimeVary('solution') # Add solution to the list of time-varying attributes
                                self.postSolve() # Do post-solution stuff
                                
                                
                    self.makeHistory()     # "Run" the model while tracking aggregate variables
                    new_dynamics = self.updateDynamics() # Find a new aggregate dynamic rule
                    
                    # Check to see if the dynamic rule has converged (if this is not the first loop)
                    if completed_loops > 0:
                        distance = new_dynamics.distance(old_dynamics)
                    else:
                        distance = 1000000.0
                    
                    # Move to the next loop if the terminal conditions are not met
                    old_dynamics     = new_dynamics
                    completed_loops += 1
                    go               = distance >= self.tolerance and completed_loops < max_loops
                    
                self.dynamics = new_dynamics # Store the final dynamic rule in self















