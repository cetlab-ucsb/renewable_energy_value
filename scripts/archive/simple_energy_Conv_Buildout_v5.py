"""
Author: Ranjit Deshmukh
"""

from pyomo.environ import *
import numpy as np
import pandas as pd
import scipy.stats as stats
import math
import time
import datetime
import os
import csv
from pyomo.opt import SolverFactory
# import pyomo.environ
opt = SolverFactory("cplex")
# opt.options["mipgap"] = 4


'''
#############################################
## IMPORTING DATA - CSVS AND PATH ##
#############################################
'''
### IMPORTING DATA

myPath = "G:\\SWITCH\\"
inputPath = myPath + "india_ED\\india_ED_input\\"
inputPathVRE_gen_profiles = myPath + "india_ED\\india_ED_input\\REvalue_gen_profiles\\"
inputPathVRE_capacity = myPath + "india_ED\\india_ED_input\\REvalue_capacity\\"

# Ana note: for Mac, will probably work on Windows
# inputPath = os.path.join(os.getcwd(), "india_ED_input/")
# inputPathVRE = os.path.join(os.getcwd(), "india_ED_input/")

#inputPathVRE = "G:\\IndiaREZ\\OUTPUTS\\india\\01052016_in\\REvalue_gen_profiles\\"
yearAnalysis = 2030
yearBase = 2014
load_csv = "load" + str(yearAnalysis) + ".csv" # Load CSV
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
outage_rate_gas = 0.3 # These outage rates need to match the ones from the ED algorithm
outage_rate_other = 0.3 # These outage rates need to match the ones from the ED algorithm
reserve_margin_over_peak = 0.15 # This is the reserve margin for "available" generation capacity above peak demand
days_to_run = 2 # Number of days to run. Set to 365 for 1 year run

'''
#############################################
## INPUTS ##
#############################################
'''
scenarios = ["S0W400"]#, "S100W300", "S200W200", "S300W100", "S400W0"] # List of VRE scenarios

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
    net_peak_df_annual = pd.DataFrame()
    
    d = 1
    for d in range(1,days_to_run + 1):
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
        net_load_ts = net_load_ts.merge(genHYDRO_minGen_cf_ts, left_index=True, right_index=True) # Merge net load and must run plant dispatch
        
        # Multiply all the capacity factors by the total capacities
        net_load_ts['solarPV'] = net_load_ts['solarPV'] * genALL_input.loc[genALL_input['generator']=='solarPV'].iloc[0]['gen_capacity']
        net_load_ts['wind'] = net_load_ts['wind'] * genALL_input.loc[genALL_input['generator']=='wind'].iloc[0]['gen_capacity']
        net_load_ts['HYDRO-PONDAGE'] = net_load_ts['HYDRO-PONDAGE'] * genALL_input.loc[genALL_input['generator']=='HYDRO-PONDAGE'].iloc[0]['gen_capacity']
        net_load_ts['HYDRO-ROR'] = net_load_ts['HYDRO-ROR'] * genALL_input.loc[genALL_input['generator']=='HYDRO-ROR'].iloc[0]['gen_capacity']
        net_load_ts['NUCLEAR'] = net_load_ts['NUCLEAR'] * genALL_input.loc[genALL_input['generator']=='NUCLEAR'].iloc[0]['gen_capacity']
        net_load_ts['HYDRO-STORAGE'] = net_load_ts['HYDRO-STORAGE'] * genALL_input.loc[genALL_input['generator']=='HYDRO-STORAGE'].iloc[0]['gen_capacity']
        
        # Calculate the hydro energy required to meet daily hydro min generation limit
        genHYDRO_minGenEnergy_ts = net_load_ts[['HYDRO-STORAGE']].sum()
        
        # Calculate the net load
        net_load_ts['net_load'] = net_load_ts['load'] - net_load_ts['solarPV'] - net_load_ts['wind'] - net_load_ts['HYDRO-PONDAGE'] - net_load_ts['HYDRO-ROR'] - net_load_ts['NUCLEAR'] - net_load_ts['HYDRO-STORAGE']
        # Convert negatives to zeros
        net_load_ts.loc[net_load_ts['net_load'] < 0, 'net_load'] = 0
        # Keeping only net load and Timepoint columns
        col_list = ['Timepoint', 'net_load']
        net_load_ts = net_load_ts[col_list]
        net_load_ts.set_index('Timepoint', inplace=True)
        net_load_ts = net_load_ts.round(0)
        net_load_ts = net_load_ts.to_dict()['net_load']
        
        # Max Hydro Energy
        genHYDRO_maxEnergy_ts = genHYDRO_maxEnergy_all.loc[genHYDRO_maxEnergy_all['Day'] == d]
        genHYDRO_dispatchableEnergy_ts = genHYDRO_maxEnergy_ts.iloc[0]['HYDRO-STORAGE'] - genHYDRO_minGenEnergy_ts.iloc[0] # Substracting the min gen energy from daily max energy
        
        
        #start_time = time.time()
        #print start_time
        '''
        #############################################
        ## DEFINE MODEL ##
        #############################################
        '''
        model = AbstractModel()
        
        '''
        #############################################
        ## SETS ##
        #############################################
        '''
        # These will be indexes to some parameters, variables and constraints
        model.TIMEPOINTS = Set(initialize=timepoints)
        
        
        '''
        #############################################
        ## PARAMETERS ##
        #############################################
        '''
        
        model.net_load = Param(model.TIMEPOINTS, default=net_load_ts)
        model.gen_energy_hydro = Param(default=genHYDRO_dispatchableEnergy_ts) # Dispatchable energy hydro
        
        '''
        #############################################
        ## DECISION VARIABLES ##
        #############################################
        '''
        
        
        model.hydro_dispatch = Var(model.TIMEPOINTS, within=NonNegativeReals)
        model.net_peak = Var()
        
        '''
        #############################################
        ## OBJECTIVE FUNCTION ##
        #############################################
        '''
        
        
        def obj_rule(mod):
            return mod.net_peak
        model.obj = Objective(rule=obj_rule)
        
        
        
        '''
        #############################################
        ## CONSTRAINTS ##
        #############################################
        '''
        def net_peak_rule(mod, t):
            return (mod.hydro_dispatch[t] + mod.net_peak) >= mod.net_load[t]
        model.netPeak = Constraint(model.TIMEPOINTS, rule=net_peak_rule)
        
        def total_hydro_energy_rule(mod):
            return sum(mod.hydro_dispatch[t] for t in mod.TIMEPOINTS) == mod.gen_energy_hydro
        model.totalHydro = Constraint(model.TIMEPOINTS, rule=total_hydro_energy_rule)
        
        
        '''
        #############################################
        ## CREATE MODEL INSTANCE AND RUN THE MODEL ##
        #############################################
        '''
        
        print("Compiling...")
        model_instance = model.create_instance()
        print("Solving...")
        results = opt.solve(model_instance, tee=True)
        
        print("Writing results...")
        results.write()
        
        #print "Total dispatch cost: USD " + str(model_instance.DispatchCost.expr())
        
        # model_instance.solutions.load_from(results)
        
        '''
        #############################################
        ## RESULTS PROCESSING ##
        #############################################
        '''
        net_peak_daily_load = model_instance.obj.expr()
        
        net_peak_df = pd.DataFrame([[d, net_peak_daily_load]], columns = ['Day', 'Net_Peak'])
        net_peak_df_annual = net_peak_df_annual.append(net_peak_df, ignore_index=True)

        ## DISPATCH OF ALL GENERATORS
        #dispatch_all_ts = pd.DataFrame({'Day': d}, index = timepoints)
        dispatch_hydro_ts = pd.DataFrame(index = timepoints)
        for p in genALL:
            dispatch_ts_gen = [model_instance.hydro_dispatch[t].value for t in timepoints]
            dispatch_ts_gen_df = pd.DataFrame({p : dispatch_ts_gen}, index = timepoints)
            # dispatch_ts = dispatch_ts.concat(dispatch_ts_gen_df, axis = 1)
            dispatch_all_ts[p] = dispatch_ts_gen_df
        # dispatch_all_ts_annual = dispatch_all_ts_annual.append(dispatch_all_ts, ignore_index=True) # Add the day's dispatch to the annual result
        dispatch_annual_gen_all = np.around(dispatch_annual_gen_all + dispatch_all_ts.sum(axis=1).sum(axis=0), decimals = 2) # Total annual generation
    
    
    ## CREATE NEW COAL AND GAS BUILDOUT
    
    # max net peak to be met by coal and gas
    max_annual_net_peak = net_peak_df_annual[['Net_Peak']].max().iloc[0]
    
    # peak load
    load_peak = load['load'].max()
    
    # new capacity required from coal and gas
    # [new capacity required = max annual net peak + reserve margin based on peak load - existing available fossil dispatchable capacity]
    new_conv_dispatchable_capacity_required = max_annual_net_peak + reserve_margin_over_peak * load_peak - (genCOAL_genCapacity['gen_capacity'].sum() * (1-outage_rate_coal) + genGAS_genCapacity['gen_capacity'].sum() * (1-outage_rate_gas) + genOTHER_genCapacity['gen_capacity'].sum() * (1-outage_rate_other))
    
    # Read the new coal and gas lists
    genNEWCOAL_input = pd.read_csv(inputPath + genNEWCOAL_csv, sep=',')
    genNEWGAS_input = pd.read_csv(inputPath + genNEWGAS_csv, sep=',')
    
    # Select new coal and gas capacity
    genNEWCOAL_sel = pd.DataFrame()
    genNEWGAS_sel = pd.DataFrame()
    
    available_capacity_sel = 0
    for g in range(len(genNEWCOAL_input)):
        if available_capacity_sel >= new_conv_dispatchable_capacity_required * ratio_new_coal:
            print "Total new coal capacity selected: " + str(available_capacity_sel / (1-outage_rate_coal)) + " MW"
            break
        genNEWCOAL_sel = genNEWCOAL_sel.append(genNEWCOAL_input.iloc[g]) # Add new capacity to the selected list
        available_capacity_sel = available_capacity_sel + genNEWCOAL_input.iloc[g]['gen_capacity'] * (1-outage_rate_coal) # include outage rates
    
    available_capacity_sel = 0
    for g in range(len(genNEWGAS_input)):
        if available_capacity_sel >= new_conv_dispatchable_capacity_required * ratio_new_gas:
            print "Total new gas capacity selected: " + str(available_capacity_sel / (1-outage_rate_gas)) + " MW"
            break
        genNEWGAS_sel = genNEWGAS_sel.append(genNEWGAS_input.iloc[g]) # Add new capacity to the selected list
        available_capacity_sel = available_capacity_sel + genNEWGAS_input.iloc[g]['gen_capacity'] * (1-outage_rate_gas) # include outage rates
    
    genNEWGEN_all_sel = pd.concat([genNEWCOAL_sel, genNEWGAS_sel])
    
    csvOutputGenSel = outputConvBuildoutPath + str(yearAnalysis) + "_conventional_capacity_" + scenarios[sc] + ".csv"
    genNEWGEN_all_sel.to_csv(csvOutputGenSel, sep=',', index = False)




'''
## DISPATCH OF ALL GENERATORS
dispatch_all_ts = pd.DataFrame({'Day': d}, index = timepoints)
for p in genALL:
    dispatch_ts_gen = [model_instance.DispatchMW[t,p].value for t in timepoints]
    dispatch_ts_gen_df = pd.DataFrame({p : dispatch_ts_gen}, index = timepoints)
    # dispatch_ts = dispatch_ts.concat(dispatch_ts_gen_df, axis = 1)
    dispatch_all_ts[p] = dispatch_ts_gen_df

## DISPATCH OF VRE GENERATORS
dispatch_vre_ts = pd.DataFrame({'Day': d}, index = timepoints)
for p in genVRE:
    dispatch_ts_gen = [model_instance.DispatchMW[t,p].value for t in timepoints]
    dispatch_ts_gen_df = pd.DataFrame({p : dispatch_ts_gen}, index = timepoints)
    # dispatch_ts = dispatch_ts.concat(dispatch_ts_gen_df, axis = 1)
    dispatch_vre_ts[p] = dispatch_ts_gen_df

'''

'''
# Extract the results of the solution 
results_list = []

model_instance.solutions.load_from(results)

# See https://projects.coin-or.org/Coopr/export/9162/coopr.doc/tags/1.3/GettingStarted/current/scripts.txt
for v in model_instance.component_objects(Var, active=True):
    print ("Variable",v)
    varobject = getattr(model_instance, str(v))
    for index in varobject:
        print ("   ",index, varobject[index].value)
        results_list.append([index, varobject[index].value])

results_df = pd.DataFrame(results_list, columns = ["ind", "dispatch"]) # Convert the lsit of tuples into a dataframe
results_df['timepoint'], results_df['generator'] = zip(*df.ind) # Convert the index tuple into separate columns
results_df = results_df.drop(["ind"], 1)

### Rough Code
# defining a dictionary
#{x: x**2 for x in (2,4,6)}
#timeIndex = pd.date_range('1-1-'+ '2014', periods=24, freq = 'H')


csvVREgenxls = inputs + "simple_energy_ED_inputs" + ".xlsx" 
test = pd.read_excel(csvVREgenxls, sheetname = None)
test2 = test['vre_gen']
'''

'''
## ACCESSING PARAMETER VALUES
from pyomo.core import Param
for p in model_instance.component_objects(Param, active=True):
    print ("Parameter "+str(p))
    parmobject = getattr(instance, p)
    for index in parmobject:
        print ("   ",index, parmobject[index].value)
'''