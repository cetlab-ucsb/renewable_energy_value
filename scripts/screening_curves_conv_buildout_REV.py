# -*- coding: utf-8 -*-
"""
Created on Mon Nov 07 10:32:29 2016

@author: Ranjit
"""
# v0 - Original script
# v1 - Added code to check whether new capacity requirement is less than existing requirement. Can handle mid or peaker requirement to be less than existing capacity

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
myPath = "G:\\Electricity_Models\\" 
#myPath = "C:\\Users\\akjohnson\\Desktop\\Ranjit\\"
inputPath = myPath + "renewable_energy_value\\india_REV_input\\"
# Ana note: for Mac, will probably work on Windows
# inputPath = os.path.join(os.getcwd(), "india_ED_input/")
# inputPathVRE = os.path.join(os.getcwd(), "india_ED_input/")

### SPECIFY SCENARIO
scenario_main = "high_cost_coal"
yearAnalysis = 2030
start_day = 1
end_day = 365

### INPUT SCENARIO CSV
inputScenario = pd.read_csv(inputPath + "REvalue_input_csv.csv", sep=',')
inputScenario = inputScenario[['parameter', scenario_main]]
inputScenario.fillna('', inplace = True)
inputScenario.set_index('parameter', inplace=True)

### SCENARIO SUFFIXES
scenario_suffix = inputScenario.loc['scenario_suffix'][scenario_main]
new_conventional_capacity_folder_suffix = inputScenario.loc['new_conventional_capacity_folder_suffix'][scenario_main]
net_load_folder_suffix = inputScenario.loc['net_load_folder_suffix'][scenario_main]
REvalue_folder_suffix = inputScenario.loc['REvalue_folder_suffix'][scenario_main]
battery_scenario = inputScenario.loc['battery_cap_gw_fkey'][scenario_main]
generator_cost_suffix = inputScenario.loc['generator_cost_suffix'][scenario_main]
scenario_suffix_operation = ""    ## User defined. e.g. "all_new_coal"

### PATHS
inputPathNetLoad = inputPath + "net_load\\" + net_load_folder_suffix + "\\" 

### CSVs
yearBase = inputScenario.loc['load_year'][scenario_main]
load_modified = inputScenario.loc['load_modified_suffix'][scenario_main]
load_csv = "load" + str(yearAnalysis) + load_modified + "_19EPS" + ".csv" # Load CSV
genALL_input_csv = "gen_all_input_cc_ccgt_diesel.csv" # generator csv with var cost and max capacity for all generators
genNEWCOAL_csv = "gen_new_coal_input.csv" # List of new coal plants
genNEWGASCT_csv = "gen_new_gas_ct_input.csv" # List of new CT gas plants
genNEWGASCCGT_csv = "gen_new_gas_ccgt_input.csv" # List of new CCGT gas plants

'''
#############################################
## INPUTS ##
#############################################
'''
scenarios = ["S0W0", "S0W200", "S50W150", "S100W100", "S150W50", "S200W0", "S0W300", "S75W225", "S150W150", "S225W75", "S300W0", "S0W400", "S100W300", "S200W200", "S300W100", "S400W0"]#, ["S0W0", "S0W200", "S50W150", "S100W100", "S150W50", "S200W0", "S0W300", "S75W225", "S150W150", "S225W75", "S300W0", "S0W400", "S100W300", "S200W200", "S300W100", "S400W0"] # List of VRE scenarios

## Load
load = pd.read_csv(inputPath + load_csv, sep=',')

## USER SPECIFIED PARAMETERS
VoLL = 100000 # Value of lost load
all_new_coal = 'no' # Set this flag to 'yes', if you want all coal capacity ELSE set it "to blank "no".
#coal_low_cap_cost = 'yes' # Set all_new_coal to 'no' Select the appropriate crossover csv
outage_rate_coal = 0.1 # These outage rates need to match the ones from the conventional buildout algorithm
outage_rate_gas_ct = 0.1 # These outage rates need to match the ones from the conventional buildout algorithm
outage_rate_gas_ccgt = 0.1 # These outage rates need to match the ones from the conventional buildout algorithm
outage_rate_diesel = 0.2 # These outage rates need to match the ones from the conventional buildout algorithm
outage_rate_other = 0.3 # These outage rates need to match the ones from the conventional buildout algorithm
derating_outages_conventional_gen = "yes"
reserve_margin_over_peak = 0.15 # This is the reserve margin for "available" generation capacity above peak demand

## Read the capacity factor cross-over points 
cf_crossover = pd.read_csv(inputPath + "screening_curves\\" + "CF_crossover_points.csv", sep=',') 
#if coal_low_cap_cost == 'yes':
#    crossover_suffix = "sc3"
#else:
#    crossover_suffix = "sc2"
#cf_crossover = pd.read_csv(inputPath + "screening_curves\\" + "CF_crossover_points_" + crossover_suffix + ".csv", sep=',') # sc2 is USD10.7/MMbtu gas, IMF medium term 2009-2021 history and projections


'''
#############################################
## OUTPUTS ##
#############################################
'''

# Path for output files for conventional generator builout
outputPathNEWCONV_capacity = myPath + "renewable_energy_value\\india_REV_input\\new_conventional_capacity\\"  + new_conventional_capacity_folder_suffix + "\\"

# Create output folder if path does not exist
if not os.path.exists(outputPathNEWCONV_capacity):
    os.makedirs(outputPathNEWCONV_capacity)
    
'''
#############################################
## SCENARIOS IN A LOOP ##
#############################################
'''
if all_new_coal == 'yes':
    scenario_suffix_operation = '_allCoal'
#elif coal_low_cap_cost == 'yes':
#    scenario_suffix = '_lowCapCostCoal'
#else:
#    scenario_suffix = ''


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
    net_load_hydro_input = pd.read_csv(inputPathNetLoad + "net_load_hydro_" + scenarios[sc] + "_" + net_load_folder_suffix + scenario_suffix_operation + ".csv", sep=',', index_col='Timepoint')
    
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
    #crossover_hours_ct = int(8760 * cf_crossover['gas_ct'])
    #crossover_hours_ccgt = int(8760 * cf_crossover['gas_ccgt'])
    crossover_hours_ct = int(8760 * cf_crossover[cf_crossover['parameter']=='gas_ct'][generator_cost_suffix])
    crossover_hours_ccgt = int(8760 * cf_crossover[cf_crossover['parameter']=='gas_ccgt'][generator_cost_suffix])
    
    # Sequentially allocate capacity by technology
    capacity_allocated = 0 # Keep track of allocated capacity as it is assigned to every technology
    total_available_capacity_peaker = int(net_load_peak - net_load_sorted.loc[net_load_sorted['sorted_index']==crossover_hours_ct-1]['net_load_final'].iloc[0]) # For CT
    capacity_allocated = total_available_capacity_peaker
    if crossover_hours_ccgt > crossover_hours_ct:
        total_available_capacity_mid = int(net_load_peak - net_load_sorted.loc[net_load_sorted['sorted_index']==crossover_hours_ccgt-1]['net_load_final'].iloc[0] - capacity_allocated) # For CCGT
        capacity_allocated = capacity_allocated + total_available_capacity_mid
    else:
        total_available_capacity_mid = 0
    total_available_capacity_base = int(net_load_peak - capacity_allocated)
    
    # peak load
    load_peak = load['load'].max()
    
    # Add reserve margin to the total available capacity REQUIRED to meet net demand. Spreaad the reserve margin requirement across peaker, mid and base type plants
    total_available_capacity_peaker = total_available_capacity_peaker + reserve_margin_over_peak * load_peak * total_available_capacity_peaker/net_load_peak
    total_available_capacity_mid = total_available_capacity_mid + reserve_margin_over_peak * load_peak * total_available_capacity_mid/net_load_peak
    total_available_capacity_base = total_available_capacity_base + reserve_margin_over_peak * load_peak * total_available_capacity_base/net_load_peak
    
    # Required new capacity
    excess_capacity = 0 # Keep track of existing capacity that is in excess of the required total capacity in each category
    # New peak capacity
    existing_available_capacity_peaker = genGASCT_genCapacity['gen_capacity'].sum() * (1-outage_rate_gas_ct) + genDIESEL_genCapacity['gen_capacity'].sum() * (1-outage_rate_diesel)
    if total_available_capacity_peaker > existing_available_capacity_peaker:
        new_capacity_ct = total_available_capacity_peaker - existing_available_capacity_peaker
    else:
        new_capacity_ct = 0
        excess_capacity = existing_available_capacity_peaker - total_available_capacity_peaker
    # New mid capacity
    existing_available_capacity_mid = genGASCCGT_genCapacity['gen_capacity'].sum() * (1-outage_rate_gas_ccgt)
    if total_available_capacity_mid > existing_available_capacity_mid:
        new_capacity_ccgt = total_available_capacity_mid - existing_available_capacity_mid 
    else:
        new_capacity_ccgt = 0
        excess_capacity = existing_available_capacity_mid - total_available_capacity_mid
    # New base capacity
    existing_available_capacity_base = genCOAL_genCapacity['gen_capacity'].sum() * (1-outage_rate_coal) + genOTHER_genCapacity['gen_capacity'].sum() * (1-outage_rate_other)
    if total_available_capacity_base > (existing_available_capacity_base + excess_capacity):
        new_capacity_coal = total_available_capacity_base - existing_available_capacity_base - excess_capacity
    else:
        new_capacity_coal = 0
    
    # To restrict new conventional buildout to only coal    (Need to change this code. What if outages are different for the generation technologies?)
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
    csvOutputGenSel = outputPathNEWCONV_capacity + str(yearAnalysis) + "_new_conventional_capacity_" + scenarios[sc] + "_" + new_conventional_capacity_folder_suffix + ".csv"
        
    genNEWGEN_all_sel.to_csv(csvOutputGenSel, sep=',', index = False)

elapsed_time = (time.time() - start_time)/(60)
print str(elapsed_time) + " minutes"


