"""
Author: Ranjit Deshmukh
"""

## THIS SCRIPT CALCULATES THE NET LOAD AS LOAD MINUS ALL THE MUSTRUN AND MIN STABLE GENERATION AND THEN DISPATCHES
## AVAILABLE HYDRO ENERGY ACROSS EACH HOUR SUBJECT TO HYDRO CAPACITY CONSTRAINTS. 
## THE SCRIPT OUTPUTS NET LOAD AFTER HYDRO DISPATCH, AND HYDRO DISPATCH INTO A CSV


#from pyomo.environ import *
import numpy as np
import pandas as pd
import scipy.stats as stats
import math
import time
import datetime
import os
import csv
#from pyomo.opt import SolverFactory
# import pyomo.environ
#opt = SolverFactory("cplex")
# opt.options["mipgap"] = 4
start_time = time.time()
print start_time

'''
#############################################
## IMPORTING DATA - CSVS AND PATH ##
#############################################
'''
### IMPORTING DATA

myPath = "C:\\Users\\akjohnson\\Desktop\\Ranjit\\"
inputPath = myPath + "india_ED\\india_ED_input\\"
inputPathVRE_gen_profiles = myPath + "india_ED\\india_ED_input\\REvalue_gen_profiles\\"
inputPathVRE_capacity = myPath + "india_ED\\india_ED_input\\REvalue_capacity\\"

# Ana note: for Mac, will probably work on Windows
# inputPath = os.path.join(os.getcwd(), "india_ED_input/")
# inputPathVRE = os.path.join(os.getcwd(), "india_ED_input/")

#inputPathVRE = "G:\\IndiaREZ\\OUTPUTS\\india\\01052016_in\\REvalue_gen_profiles\\"
yearAnalysis = 2030
yearBase = 2014
load_csv = "load" + str(yearAnalysis) + "_19EPS" + ".csv" # Load CSV
genALL_input_csv = "gen_all_input.csv" # generator csv with var cost and max capacity for all generators
#genVRE_csv = "vre_gen.csv" # variable RE generator csv with dispatch capacity factors
genHYDRO_minGen_csv = "hydro_min_gen.csv"
genHYDRO_maxEnergy_csv = "hydro_max_energy.csv"
genMUSTRUN_csv = "mustrun_gen.csv" # must run generators like nuclear and run-of-river hydro that have a constant output through the timeseries (e.g. day)
#VoLL = 100000 # Value of lost load
genNEWCOAL_csv = "gen_new_coal_input.csv" # List of new coal plants
genNEWGAS_csv = "gen_new_gas_input.csv" # List of new gas plants
ratio_new_coal = 1 # Fraction of new conventional capacity from coal
ratio_new_gas = 1-ratio_new_coal # Fraction of new conventional capacity from gas
outage_rate_coal = 0.2 # These outage rates need to match the ones from the ED algorithm
outage_rate_gas = 0.2 # These outage rates need to match the ones from the ED algorithm
outage_rate_other = 0.3 # These outage rates need to match the ones from the ED algorithm
reserve_margin_over_peak = 0.15 # This is the reserve margin for "available" generation capacity above peak demand
days_to_run = 365 # Number of days to run. Set to 365 for 1 year run

'''
#############################################
## INPUTS ##
#############################################
'''
scenarios = ["S0W300", "S75W225", "S150W150", "S225W75", "S300W0", "S0W400", "S100W300", "S200W200", "S300W100", "S400W0"] #"S0W400", # List of VRE scenarios ["S0W0", "S0W200", "S50W150", "S100W100", "S150W50", "S200W0", "S0W400", "S100W300", "S200W200", "S300W100", "S400W0", "S0W600", "S150W450", "S300W300", "S450W150", "S600W0"]

## Load
load = pd.read_csv(inputPath + load_csv, sep=',')

## Read the VRE capacities for all scenarios
#genVRE_allScenarios = pd.read_csv(inputPathVRE + str(yearBase) + "_RE_capacity_all_scenarios" + ".csv", sep=',')

'''
#############################################
## OUTPUTS ##
#############################################
'''

# Path for output files for conventional generator builout
outputConvBuildoutPath = myPath + "india_ED\\india_ED_input\\new_conventional_capacity\\"

'''
#############################################
## SCENARIOS IN A LOOP ##
#############################################
'''

for sc in range(len(scenarios)):
    ## VRE generation
    genVRE_all = pd.read_csv(inputPathVRE_gen_profiles + str(yearBase) + "_RE_gen_" + scenarios[sc] + ".csv", sep=',')
    ## Add VRE generators and their capacities to the gen
    #genVRE_toAdd = genVRE_allScenarios.loc[genVRE_allScenarios['scenario'] == sc]
    genVRE_toAdd = pd.read_csv(inputPathVRE_capacity + str(yearBase) + "_RE_capacity_" + scenarios[sc] + ".csv", sep=',')
    genVRE_toAdd.drop(['scenario'], axis=1, inplace=True)
    genVRE_toAdd_melt = pd.melt(genVRE_toAdd, value_vars=list(genVRE_toAdd.columns.values))
    genVRE_toAdd_melt.columns = ['generator', 'gen_capacity']
    genVRE_toAdd_melt['var_cost'] = 0
    genVRE_toAdd_melt['type'] = 'vre'
    
    
    ## All generators
    genALL_input = pd.read_csv(inputPath + genALL_input_csv, sep=',')
    # Add the VRE generators for the current scenario
    genALL_input = genALL_input.append(genVRE_toAdd_melt)
    
    #genALL  = genALL_input['generator'].tolist() #Initialize the generator list. This is used for the generator index.
    ## Variable cost
    #genALL_varCost = genALL_input[['generator', 'var_cost']]
    #genALL_varCost.set_index('generator', inplace=True)
    #genALL_varCost = genALL_varCost.to_dict()['var_cost']
    # Capacity - # All generators
    genALL_genCapacity = genALL_input[['generator', 'gen_capacity']]
    genALL_genCapacity.set_index('generator', inplace=True)
    genALL_genCapacity = genALL_genCapacity.to_dict()['gen_capacity']
    # Capacity - # hydro generators
    genHYDRO_genCapacity = genALL_input[genALL_input['type'].isin(['hydro'])][['generator', 'gen_capacity']]
    genHYDRO_genCapacity.set_index('generator', inplace=True)
    genHYDRO_genCapacity = genHYDRO_genCapacity.to_dict()['gen_capacity']
    # Capacity - # VRE generators
    genVRE_genCapacity = genALL_input[genALL_input['type'].isin(['vre'])][['generator', 'gen_capacity']]
    genVRE_genCapacity.set_index('generator', inplace=True)
    genVRE_genCapacity = genVRE_genCapacity.to_dict()['gen_capacity']
    ## Capacity - # coal generators
    genCOAL_genCapacity = genALL_input[genALL_input['type'].isin(['coal'])][['generator', 'gen_capacity']]
    #genCOAL_genCapacity.set_index('generator', inplace=True)
    #genCOAL_genCapacity = genCOAL_genCapacity.to_dict()['gen_capacity']
    ## Capacity - # gas generators
    genGAS_genCapacity = genALL_input[genALL_input['type'].isin(['gas'])][['generator', 'gen_capacity']]
    #genGAS_genCapacity.set_index('generator', inplace=True)
    #genGAS_genCapacity = genGAS_genCapacity.to_dict()['gen_capacity']
    ## Capacity - # other generators - These include biomass cogeneration and diesel
    genOTHER_genCapacity = genALL_input[genALL_input['type'].isin(['other'])][['generator', 'gen_capacity']]
    #genOTHER_genCapacity.set_index('generator', inplace=True)
    #genOTHER_genCapacity = genOTHER_genCapacity.to_dict()['gen_capacity']
    # Capacity - # must run generators
    genMUSTRUN_genCapacity = genALL_input[genALL_input['type'].isin(['mustrun'])][['generator', 'gen_capacity']]
    genMUSTRUN_genCapacity.set_index('generator', inplace=True)
    genMUSTRUN_genCapacity = genMUSTRUN_genCapacity.to_dict()['gen_capacity']
    
    ## Hydro generation
    genHYDRO_minGen_all = pd.read_csv(inputPath + genHYDRO_minGen_csv, sep=',')
    genHYDRO_maxEnergy_all = pd.read_csv(inputPath + genHYDRO_maxEnergy_csv, sep=',')
    
    ## Must Run generation
    genMUSTRUN_all = pd.read_csv(inputPath + genMUSTRUN_csv, sep=',')
    
    ### Thermal and other dispatchable generators (other than hydro)
    #genCOAL = genALL_input[genALL_input['type'].isin(['coal'])]['generator'].tolist()
    #genGAS = genALL_input[genALL_input['type'].isin(['gas'])]['generator'].tolist()
    #genOTHER = genALL_input[genALL_input['type'].isin(['other'])]['generator'].tolist()
    
    
    '''
    #############################################
    ## INPUT DATA IN A LOOP ##
    #############################################
    '''
    
    ## Output dataframe
    net_load_annual = pd.DataFrame()
    dispatch_hydro_ts_annual = pd.DataFrame()
    
    
    for d in range(1,days_to_run + 1):
        print "day " + str(d)
        ## Net load
        load_ts = load.loc[load['Day'] == d]
        timepoints = load_ts['Timepoint'].tolist() # Initialize the timepoints from the load data frame. This is used for the Timepoint index
        load_ts.drop(['Day', 'dateTime'], axis=1, inplace=True) # Keep only the timepoint and load columns
        genVRE_cf_ts = genVRE_all.loc[genVRE_all['Day'] == d] # Select all the rows for a particular day in the loop
        genVRE_cf_ts.drop(['Day', 'dateTime'], axis=1, inplace=True) # Drop all columns except timepoints and VRE generators
        net_load_ts = load_ts.merge(genVRE_cf_ts) # Merge load and RE data
        
        genMUSTRUN_cf_ts = pd.DataFrame(genMUSTRUN_all.loc[genMUSTRUN_all['Day'] == d])
        genMUSTRUN_cf_ts.drop(['Day'], axis=1, inplace=True) 
        genMUSTRUN_cf_ts = pd.concat([genMUSTRUN_cf_ts]*24, ignore_index=True)
        net_load_ts = net_load_ts.merge(genMUSTRUN_cf_ts, left_index=True, right_index=True) # Merge net load and must run plant dispatch
        
        genHYDRO_minGen_cf_ts = pd.DataFrame(genHYDRO_minGen_all.loc[genHYDRO_minGen_all['Day'] == d])
        genHYDRO_minGen_cf_ts.drop(['Day'], axis=1, inplace=True)
        genHYDRO_minGen_cf_ts = pd.concat([genHYDRO_minGen_cf_ts]*24, ignore_index=True)
        net_load_ts = net_load_ts.merge(genHYDRO_minGen_cf_ts, left_index=True, right_index=True) # Merge net load and hydro min plant dispatch
        
        # Multiply all the capacity factors by the total capacities
        net_load_ts['solarPV'] = net_load_ts['solarPV'] * genALL_input.loc[genALL_input['generator']=='solarPV'].iloc[0]['gen_capacity']
        net_load_ts['wind'] = net_load_ts['wind'] * genALL_input.loc[genALL_input['generator']=='wind'].iloc[0]['gen_capacity']
        net_load_ts['HYDRO-PONDAGE'] = net_load_ts['HYDRO-PONDAGE'] * genALL_input.loc[genALL_input['generator']=='HYDRO-PONDAGE'].iloc[0]['gen_capacity']
        net_load_ts['HYDRO-ROR'] = net_load_ts['HYDRO-ROR'] * genALL_input.loc[genALL_input['generator']=='HYDRO-ROR'].iloc[0]['gen_capacity']
        net_load_ts['NUCLEAR'] = net_load_ts['NUCLEAR'] * genALL_input.loc[genALL_input['generator']=='NUCLEAR'].iloc[0]['gen_capacity']
        net_load_ts['HYDRO-STORAGE'] = net_load_ts['HYDRO-STORAGE'] * genALL_input.loc[genALL_input['generator']=='HYDRO-STORAGE'].iloc[0]['gen_capacity']
        
        # Calculate the hydro energy required to meet daily hydro min generation limit
        genHYDRO_minGenEnergy_ts = net_load_ts[['HYDRO-STORAGE']].sum()
        # Min hydro capacity to generate minimum hydro 
        genHYDRO_minGenCapacity_ts = genHYDRO_minGenEnergy_ts / 24 # For hourly data set
        # Hydro capacity available for dispatch
        genHYDRO_avCapacity_ts = genHYDRO_genCapacity['HYDRO-STORAGE'] - genHYDRO_minGenCapacity_ts
        # Max Hydro Energy
        genHYDRO_maxEnergy_ts = genHYDRO_maxEnergy_all.loc[genHYDRO_maxEnergy_all['Day'] == d]
        genHYDRO_dispatchableEnergy_ts = genHYDRO_maxEnergy_ts.iloc[0]['HYDRO-STORAGE'] - genHYDRO_minGenEnergy_ts.iloc[0] # Substracting the min gen energy from daily max energy
        
        
        # Calculate the net load
        net_load_ts['net_load'] = net_load_ts['load'] - net_load_ts['solarPV'] - net_load_ts['wind'] - net_load_ts['HYDRO-PONDAGE'] - net_load_ts['HYDRO-ROR'] - net_load_ts['NUCLEAR'] - net_load_ts['HYDRO-STORAGE']
        # Convert negatives to zeros
        net_load_ts.loc[net_load_ts['net_load'] < 0, 'net_load'] = 0
        # Keeping only net load and Timepoint columns
        col_list = ['Timepoint', 'net_load']
        net_load_ts = net_load_ts[col_list]
        net_load_ts.set_index('Timepoint', inplace=True)
        net_load_ts = net_load_ts.round(0)
        net_load_ts_dict = net_load_ts.to_dict()['net_load']
        
        ## HYDRO DISPATCH IN A LOOP
        
        net_load_ts_sorted = net_load_ts.sort_values(by='net_load', ascending=False)
        # Add a column for sorted index
        net_load_ts_sorted['sorted_index'] = range(len(net_load_ts_sorted))
        # Add column for hydro dispatch. Note, this dispatch does not have min gen
        net_load_ts_sorted['hydro_dispatch'] = np.nan
        # Make a copy of the sorted net_load to modify during looping
        net_load_ts_sorted_looped = net_load_ts_sorted.copy()
        # Initiate available hydro capacity and energy. Energy changes through  the loops.
        available_hydro_energy = genHYDRO_dispatchableEnergy_ts
        available_hydro_capacity = genHYDRO_avCapacity_ts.iloc[0] 
        
        for i in range(len(net_load_ts_sorted)):
            print i
            
            if available_hydro_energy <= 0:
                break
            #net_load_ts_sorted_looped = net_load_ts_sorted
            # Add a column for sorted index for looped net_load
            net_load_ts_sorted_looped['sorted_index_looped'] = range(len(net_load_ts_sorted_looped))
            
            
            # Difference between rows
            net_load_ts_sorted_looped['net_load_diff'] = net_load_ts_sorted_looped['net_load'] - net_load_ts_sorted_looped['net_load'].shift(-1) # Move outside the loop
            # Set the difference in last row (base row) as the net load itself. This is useful to calculate the last horizontal area under the net load
            net_load_ts_sorted_looped['net_load_diff'].iloc[-1] = net_load_ts_sorted_looped['net_load'].iloc[-1]
            # Horizontal Area Stripes under the net laod curve
            net_load_ts_sorted_looped['horizontal_net_load'] = net_load_ts_sorted_looped['net_load_diff'] * (net_load_ts_sorted_looped['sorted_index_looped'] + 1)  
            # Cumulative sum of area
            net_load_ts_sorted_looped['cum_horizontal_net_load'] = net_load_ts_sorted_looped['horizontal_net_load'].cumsum()
            # Calculate the reverse net load for using to compare the hydro capacity
            net_load_ts_sorted_looped['net_load_reversed'] = net_load_ts_sorted_looped['net_load_diff'].cumsum()
            
            for j in range(len(net_load_ts_sorted_looped)):
                #print "second loop " + str(j)
                # Check if the reversed net load is greater than the available hydro capacity [total hydro capacity - min gen]

                if net_load_ts_sorted_looped['cum_horizontal_net_load'].iloc[j] > available_hydro_energy: 
                    flag = 1
                    # Calculate the overshoot
                    overshoot = (net_load_ts_sorted_looped['cum_horizontal_net_load'].iloc[j] - available_hydro_energy)/(j+1)
                    # Set the dispatch for that hour
                    hydro_dispatch_interval = net_load_ts_sorted_looped['net_load_reversed'].iloc[j] - overshoot
                    # Check whether hydro dispacth is greater than available hydro capacity. If it is, then set it to available hydro capacity
                    if hydro_dispatch_interval > available_hydro_capacity:
                        net_load_ts_sorted['hydro_dispatch'].iloc[i] = available_hydro_capacity 
                        # Subtract the hydro dispatch from the available hydro energy for the next loop
                        available_hydro_energy = available_hydro_energy - available_hydro_capacity
                    else:
                        net_load_ts_sorted['hydro_dispatch'].iloc[i] = hydro_dispatch_interval 
                        # Subtract the hydro dispatch from the available hydro energy for the next loop
                        available_hydro_energy = available_hydro_energy - hydro_dispatch_interval                     
                    break
                if net_load_ts_sorted_looped['net_load_reversed'].iloc[j] > available_hydro_capacity:
                    #print "yes"
                    net_load_ts_sorted['hydro_dispatch'].iloc[i] = available_hydro_capacity
                    # Subtract the full hydro capacity from the available hydro energy for the next loop
                    available_hydro_energy = available_hydro_energy - available_hydro_capacity
                    break
             # Drop the first row of the looped data frame
            net_load_ts_sorted_looped = net_load_ts_sorted_looped.iloc[1:]

        # Set the dispatch for the remaining hours to zero
        net_load_ts_sorted['hydro_dispatch'].fillna(0, inplace=True)
        # Calculate the final net load column
        net_load_ts_sorted['net_load_final'] = net_load_ts_sorted['net_load'] - net_load_ts_sorted['hydro_dispatch']
        # Calculate the total hydro dispatch (including min gen) for comparison with ED
        net_load_ts_sorted['hydro_dispatch_final'] = net_load_ts_sorted['hydro_dispatch'] + genHYDRO_minGenCapacity_ts[0]
        # Resort the net load by index
        net_load_ts = net_load_ts_sorted.sort_index()
    
   
        
        '''
        #############################################
        ## RESULTS PROCESSING ##
        #############################################
        '''
        # Add to the annual net load
        net_load_annual = net_load_annual.append(pd.DataFrame(net_load_ts['net_load_final']))
        
        ## DISPATCH OF Hydro GENERATORS
        #dispatch_all_ts = pd.DataFrame({'Day': d}, index = timepoints)
        dispatch_hydro_ts_annual = dispatch_hydro_ts_annual.append(pd.DataFrame(net_load_ts['hydro_dispatch_final']))
        
        
    ## Write out the net load and hydro dispatch for the buildout scenarios
    net_load_hydro_ts_annual = pd.concat([net_load_annual, dispatch_hydro_ts_annual], axis=1)
    net_load_hydro_ts_annual.to_csv(inputPath + "net_load\\" + "net_load_hydro_" + scenarios[sc] + ".csv", sep=',', index = True)
    

elapsed_time = (time.time() - start_time)/(60)
print str(elapsed_time) + " minutes"


