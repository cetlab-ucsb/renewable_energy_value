# -*- coding: utf-8 -*-
"""
Created on Mon Nov 07 10:32:29 2016

@author: Ranjit
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
load_csv = "load" + str(yearAnalysis) + ".csv" # Load CSV
genALL_input_csv = "gen_all_input_cc_ccgt_diesel.csv" # generator csv with var cost and max capacity for all generators
#genVRE_csv = "vre_gen.csv" # variable RE generator csv with dispatch capacity factors
genHYDRO_minGen_csv = "hydro_min_gen.csv"
genHYDRO_maxEnergy_csv = "hydro_max_energy.csv"
genMUSTRUN_csv = "mustrun_gen.csv" # must run generators like nuclear and run-of-river hydro that have a constant output through the timeseries (e.g. day)
#VoLL = 100000 # Value of lost load
genNEWCOAL_csv = "gen_new_coal_input.csv" # List of new coal plants
genNEWGASCT_csv = "gen_new_gas_ct_input.csv" # List of new CT gas plants
genNEWGASCCGT_csv = "gen_new_gas_ccgt_input.csv" # List of new CCGT gas plants
# EXOGENOUSLY SET COAL TO GAS RATIO (Then set the ratio_new_coal parameter)
all_new_coal = 'yes' # Set this flag to 'yes', if you want all coal capacity ELSE set it "to blank "no".
#set_coal_gas_ratio = 'yes'
#ratio_new_coal = 1 # Fraction of new conventional capacity from coal
#ratio_new_gas = 1-ratio_new_coal # Fraction of new conventional capacity from gas
# OUTAGE RATES
outage_rate_coal = 0.2 # These outage rates need to match the ones from the ED algorithm
outage_rate_gas_ct = 0.2 # These outage rates need to match the ones from the ED algorithm
outage_rate_gas_ccgt = 0.2 # These outage rates need to match the ones from the ED algorithm
outage_rate_diesel = 0.2 # These outage rates need to match the ones from the ED algorithm
outage_rate_other = 0.3 # These outage rates need to match the ones from the ED algorithm
reserve_margin_over_peak = 0.15 # This is the reserve margin for "available" generation capacity above peak demand
days_to_run = 365 # Number of days to run. Set to 365 for 1 year run

'''
#############################################
## INPUTS ##
#############################################
'''
scenarios = ["S0W200", "S50W150", "S100W100", "S150W50", "S200W0"] # List of VRE scenarios

## Load
load = pd.read_csv(inputPath + load_csv, sep=',')

## Read the capacity factor cross-over points 
cf_crossover = pd.read_csv(inputPath + "screening_curves\\" + "CF_crossover_points_sc2.csv", sep=',') # sc2 is USD10.7/MMbtu gas, IMF medium term 2009-2021 history and projections

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
    print "Processing " + scenarios[sc]
    ## Read the existing generator list
    # All generators
    genALL_input = pd.read_csv(inputPath + genALL_input_csv, sep=',')
    # Coal generators
    genCOAL_genCapacity = genALL_input[genALL_input['type'].isin(['coal'])][['generator', 'gen_capacity']]
    # Gas generators
    genGASCCGT_genCapacity = genALL_input[genALL_input['type'].isin(['gas_ccgt'])][['generator', 'gen_capacity']]
    genGASCT_genCapacity = genALL_input[genALL_input['type'].isin(['gas_ct'])][['generator', 'gen_capacity']]
    # Other generators
    genOTHER_genCapacity = genALL_input[genALL_input['type'].isin(['other'])][['generator', 'gen_capacity']]
    # Diesel generators
    genDIESEL_genCapacity = genALL_input[genALL_input['type'].isin(['diesel'])][['generator', 'gen_capacity']] 
    
    
    ## SCREENING CURVE CONVENTIONAL BUILDOUT
    ## Read the net load csv
    net_load_hydro_input = pd.read_csv(inputPath + "net_load\\" + "net_load_hydro_" + scenarios[sc] + ".csv", sep=',', index_col='Timepoint')
    
    # Get the net load
    net_load = pd.DataFrame(net_load_hydro_input['net_load_final'])
    # Peak net load
    net_load_peak = net_load['net_load_final'].max()    
    # Total energy 
    net_load_energy = net_load['net_load_final'].sum()
    # Sort net load
    net_load_sorted = net_load.sort_values(by='net_load_final', ascending=False)
    # Add a column for sorted index
    net_load_sorted['sorted_index'] = range(len(net_load_sorted))
    
    
    # Difference between rows
    net_load_sorted['net_load_diff'] = net_load_sorted['net_load_final'] - net_load_sorted['net_load_final'].shift(-1) 
    # Set the difference in last row (base row) as the net load itself. This is useful to calculate the last horizontal area under the net load
    net_load_sorted['net_load_diff'].iloc[-1] = net_load_sorted['net_load_final'].iloc[-1]
    # Horizontal Area Stripes under the net laod curve
    net_load_sorted['horizontal_net_load'] = net_load_sorted['net_load_diff'] * (net_load_sorted['sorted_index'] + 1)  
    # Cumulative sum of area
    net_load_sorted['cum_horizontal_net_load'] = net_load_sorted['horizontal_net_load'].cumsum()
    # Calculate the reverse net load for using to estimate capacity
    net_load_sorted['net_load_reversed'] = net_load_sorted['net_load_diff'].cumsum()
    # Percentage of net load energy as part of total net load energy
    
    # Get the crossover hours, then get the total conventional capacity required
    crossover_hours_ct = int(8760 * cf_crossover['gas_ct'])
    crossover_hours_ccgt = int(8760 * cf_crossover['gas_ccgt'])
    
    # Sequentially allocate capacity by technology
    capacity_allocated = 0 # Keep track of allocated capacity as it is assigned to every technology
    total_available_capacity_peaker = int(net_load_peak - net_load_sorted.loc[net_load_sorted['sorted_index']==crossover_hours_ct-1]['net_load_final'].iloc[0]) # For CT
    capacity_allocated = total_available_capacity_peaker
    total_available_capacity_mid = int(net_load_peak - net_load_sorted.loc[net_load_sorted['sorted_index']==crossover_hours_ccgt-1]['net_load_final'].iloc[0] - capacity_allocated) # For CCGT
    capacity_allocated = capacity_allocated + total_available_capacity_mid
    total_available_capacity_base = int(net_load_peak - capacity_allocated)
    
    # peak load
    load_peak = load['load'].max()
    
    # Add reserve margin to the total available capacity REQUIRED to meet net demand. Spreaad the reserve margin requirement across peaker, mid and base type plants
    total_available_capacity_peaker = total_available_capacity_peaker + reserve_margin_over_peak * load_peak * total_available_capacity_peaker/net_load_peak
    total_available_capacity_mid = total_available_capacity_mid + reserve_margin_over_peak * load_peak * total_available_capacity_mid/net_load_peak
    total_available_capacity_base = total_available_capacity_base + reserve_margin_over_peak * load_peak * total_available_capacity_base/net_load_peak
    
    # Required new capacity
    # New peak capacity
    new_capacity_ct = total_available_capacity_peaker - genGASCT_genCapacity['gen_capacity'].sum() * (1-outage_rate_gas_ct) - genDIESEL_genCapacity['gen_capacity'].sum() * (1-outage_rate_diesel)
    # New mid capacity
    new_capacity_ccgt = total_available_capacity_mid - genGASCCGT_genCapacity['gen_capacity'].sum() * (1-outage_rate_gas_ccgt)
    # New base capacity
    new_capacity_coal = total_available_capacity_base - genCOAL_genCapacity['gen_capacity'].sum() * (1-outage_rate_coal) - genOTHER_genCapacity['gen_capacity'].sum() * (1-outage_rate_other)
    
    # To restrict new conventional buildout to only coal    
    if all_new_coal == 'yes':
        new_capacity_coal = new_capacity_coal + new_capacity_ct + new_capacity_ccgt
        new_capacity_ct = 0
        new_capacity_ccgt = 0
    
     
    # new capacity required from coal and gas
    # [new capacity required = max annual net peak + reserve margin based on peak load - existing available fossil dispatchable capacity]
    #new_conv_dispatchable_capacity_required = max_annual_net_peak + reserve_margin_over_peak * load_peak - (genCOAL_genCapacity['gen_capacity'].sum() * (1-outage_rate_coal) + genGAS_genCapacity['gen_capacity'].sum() * (1-outage_rate_gas) + genOTHER_genCapacity['gen_capacity'].sum() * (1-outage_rate_other))
    
    # Read the new coal and gas lists
    genNEWCOAL_input = pd.read_csv(inputPath + genNEWCOAL_csv, sep=',')
    genNEWGASCCGT_input = pd.read_csv(inputPath + genNEWGASCCGT_csv, sep=',')
    genNEWGASCT_input = pd.read_csv(inputPath + genNEWGASCT_csv, sep=',')
    
    # Select new coal and gas capacity
    genNEWCOAL_sel = pd.DataFrame()
    genNEWGASCCGT_sel = pd.DataFrame()
    genNEWGASCT_sel = pd.DataFrame()
    
    available_capacity_sel = 0
    for g in range(len(genNEWCOAL_input)):
        if available_capacity_sel >= new_capacity_coal:
            print "Total new coal capacity selected: " + str(available_capacity_sel / (1-outage_rate_coal)) + " MW"
            break
        genNEWCOAL_sel = genNEWCOAL_sel.append(genNEWCOAL_input.iloc[g]) # Add new capacity to the selected list
        available_capacity_sel = available_capacity_sel + genNEWCOAL_input.iloc[g]['gen_capacity'] * (1-outage_rate_coal) # include outage rates
    
    available_capacity_sel = 0
    for g in range(len(genNEWGASCCGT_input)):
        if available_capacity_sel >= new_capacity_ccgt:
            print "Total new ccgt gas capacity selected: " + str(available_capacity_sel / (1-outage_rate_gas_ccgt)) + " MW"
            break
        genNEWGASCCGT_sel = genNEWGASCCGT_sel.append(genNEWGASCCGT_input.iloc[g]) # Add new capacity to the selected list
        available_capacity_sel = available_capacity_sel + genNEWGASCCGT_input.iloc[g]['gen_capacity'] * (1-outage_rate_gas_ccgt) # include outage rates
    
    available_capacity_sel = 0
    for g in range(len(genNEWGASCT_input)):
        if available_capacity_sel >= new_capacity_ct:
            print "Total new ct gas capacity selected: " + str(available_capacity_sel / (1-outage_rate_gas_ct)) + " MW"
            break
        genNEWGASCT_sel = genNEWGASCT_sel.append(genNEWGASCT_input.iloc[g]) # Add new capacity to the selected list
        available_capacity_sel = available_capacity_sel + genNEWGASCT_input.iloc[g]['gen_capacity'] * (1-outage_rate_gas_ct) # include outage rates
    
    genNEWGEN_all_sel = pd.concat([genNEWCOAL_sel, genNEWGASCCGT_sel, genNEWGASCT_sel])
    
    # Write out the new conventional generator buildout file
    if all_new_coal == "no":
        csvOutputGenSel = outputConvBuildoutPath + str(yearAnalysis) + "_conventional_capacity_" + scenarios[sc] + ".csv"
    else:
        csvOutputGenSel = outputConvBuildoutPath + str(yearAnalysis) + "_conventional_capacity_" + scenarios[sc] + "_allCoal" + ".csv"
        
    genNEWGEN_all_sel.to_csv(csvOutputGenSel, sep=',', index = False)

elapsed_time = (time.time() - start_time)/(60)
print str(elapsed_time) + " minutes"


