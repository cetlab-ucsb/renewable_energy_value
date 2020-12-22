"""
Author: Ranjit Deshmukh
"""

## v4: This version was after print statement was removed. 
## v5: Adding outage rates to the Ed. Basically derating both min and max dispatch levels for coal, gas, and other
## v6: Adding diesel, ct, and ccgt
## v7: Adding suffix for scenario - in this case for "_allCoal"; Also added scenario_suffix_operations in case of changes to how the ED works e.g. 70% min gen.

#from pyomo.environ import *
from pyomo.environ import *
import numpy as np
import pandas as pd
import scipy.stats as stats
import math
import time
from time import gmtime, strftime
import datetime
import os
import csv
from pyomo.opt import SolverFactory
# import pyomo.environ
start_time = time.time()
print start_time
opt = SolverFactory("cplex")
# opt.options["mipgap"] = 4


'''
#############################################
## IMPORTING DATA - CSVS AND PATH ##
#############################################
'''
myPath = "E:\\Electricity_Models\\" 
inputPath = myPath + "renewable_energy_value\\india_REV_input\\"
# Ana note: for Mac, will probably work on Windows
# inputPath = os.path.join(os.getcwd(), "india_ED_input/")
# inputPathVRE = os.path.join(os.getcwd(), "india_ED_input/")

### SPECIFY SCENARIO
scenario_main = "hydro_high_cea_nuclear17"
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
scenario_suffix_operation = ""    ## User defined

### PATHS
inputPathVRE_gen_profiles = myPath + "renewable_energy_value\\india_REV_input\\REvalue_gen_profiles\\" + REvalue_folder_suffix + "\\"
inputPathVRE_capacity = myPath + "renewable_energy_value\\india_REV_input\\REvalue_capacity\\" + REvalue_folder_suffix + "\\"
inputPathNEWCONV_capacity = myPath + "renewable_energy_value\\india_REV_input\\new_conventional_capacity\\"  + new_conventional_capacity_folder_suffix + "\\"
inputPathLoadForecasts = inputPath + "load_forecasts\\"
outputPath = myPath + "renewable_energy_value\\india_REV_output\\"
outputPathDispatch = outputPath + "\\net_load\\" + net_load_folder_suffix + "\\"
#outputPathHydroDispatch = outputPath + scenario_suffix + "\\hydro_dispatch\\" 
outputPathNetLoad = inputPath + "net_load\\" + net_load_folder_suffix + "\\" 

### CSVs
results_all_scenarios_csv = "results_all_scenarios_net_load.csv"

yearBase = inputScenario.loc['load_year'][scenario_main]
load_modifier = inputScenario.loc['load_modified_suffix'][scenario_main][1:]
load_csv = "load" + str(yearAnalysis) + "_19EPS" + load_modifier + ".csv" # Load CSV
hydro_modifier = inputScenario.loc['hydro_energy_mod_suffix'][scenario_main]
nuclear_modifier = inputScenario.loc['nuclear_cap_suffix'][scenario_main]
gen_modifier = hydro_modifier + nuclear_modifier
genALL_input_csv = "gen_all_input_cc_ccgt_diesel" + gen_modifier + ".csv" # generator csv with var cost and max capacity for all generators
#genVRE_csv = "vre_gen.csv" # variable RE generator csv with dispatch capacity factors
genHYDRO_minGen_csv = "hydro_min_gen.csv"
genHYDRO_maxEnergy_csv = "hydro_max_energy" + hydro_modifier + ".csv"
genMUSTRUN_csv = "mustrun_gen" + nuclear_modifier + ".csv" # must run generators like nuclear and run-of-river hydro that have a constant output through the timeseries (e.g. day)
storBATTERY_csv = "battery_storage.csv"


'''
#############################################
## INPUTS ##
#############################################
'''
scenarios = ["S0W0", "S0W200", "S50W150", "S100W100", "S150W50", "S200W0", "S0W300", "S75W225", "S150W150", "S225W75", "S300W0", "S0W400", "S100W300", "S200W200", "S300W100", "S400W0", "S0W600", "S150W450","S300W300", "S450W150", "S600W0"]

## Load
load = pd.read_csv(inputPathLoadForecasts + load_csv, sep=',')
# Load dataframe to add to result at the end
load_all = load[['Timepoint', 'load']]
load_all.set_index('Timepoint', inplace=True)
    

## USER SPECIFIED PARAMETERS
VoLL = 100000 # Value of lost load
all_new_coal = 'no' # Set this flag to 'yes', if you want all coal capacity ELSE set it "to blank "no". Not used
coal_low_cap_cost = 'yes' # Not used
outage_rate_coal = 0.1 # These outage rates need to match the ones from the conventional buildout algorithm
outage_rate_gas_ct = 0.1 # These outage rates need to match the ones from the conventional buildout algorithm
outage_rate_gas_ccgt = 0.1 # These outage rates need to match the ones from the conventional buildout algorithm
outage_rate_diesel = 0.2 # These outage rates need to match the ones from the conventional buildout algorithm
outage_rate_other = 0.3 # These outage rates need to match the ones from the conventional buildout algorithm
derating_outages_conventional_gen = "yes"
#genCOAL_minGen_cf = 0.70 ## For now, the entire COAL fleet will have the same minimum CF, derated by the outage rate.
#genGASCCGT_minGen_cf = 0.50 ## For now, the entire CCGT GAS fleet will have the same minimum CF.
#genOTHER_minGen_cf = 0.70 ## For now, the entire OTHER fleet will have the same minimum CF.
#genGASCT_minGen_cf = 0.50 ## For now, the entire CT GAS fleet will have the same minimum CF. - not used
#genDIESEL_minGen_cf = 0.50 ## For now, the entire DIESEL fleet will have the same minimum CF. - not used
#storBATTERY_efficiency = 0.8 ## For now, entire battery storage has same charging efficiency. That's the roundtrip eff applied to only charging.
storBATTERY_initial_soc = 0.5 ## INitial state of charge of the battery storage
#storBATTERY_storCapacity_multiplier = 0 # Set zero for no battery storage; ;or adjust the relative ratio of capacity to energy. Currently set at 4h storage in csv
#storBATTERY_storEnergy_multiplier = 0 # Set zero for no battery storage; ;or adjust the relative ratio of capacity to energy. Currently set at 4h storage in csv
num_lookahead_days = int(inputScenario.loc['LA_days'][scenario_main]) # Number of days in addition to the realtime day to optimize battery and hydro storage. Set to zero if no lookahead/forecast is included.

gen_capacity_conventional_hypothetical = 400000 # capacity in MW
genCONVHYPOTHETICAL_cost_param_a = 0.01
genCONVHYPOTHETICAL_cost_param_b = 10**-7

'''
#############################################
## OUTPUTS ##
#############################################
'''
# Create output folder if path does not exist
if not os.path.exists(outputPath):
    os.makedirs(outputPath)
    
# Create output folder for dispatch for net load (non-linear) if path does not exist
if not os.path.exists(outputPathDispatch):
    os.makedirs(outputPathDispatch)

# Create output folder for net load if path does not exist
if not os.path.exists(outputPathNetLoad):
    os.makedirs(outputPathNetLoad)
    

# Read the ALL SCENARIO RESULTS file, or if it does not exist, create output dataframe 
if os.path.exists(outputPath + results_all_scenarios_csv):
    results_all_scenarios = pd.read_csv(outputPath + results_all_scenarios_csv, sep=',')
else:
    results_all_scenarios = pd.DataFrame(columns = ["scenario", "scenario_main", "ann_gen_total_MWh", 
                                        "ann_gen_vre_nocurt_MWh", "ann_gen_vre_MWh", "ann_curt_vre_MWh", "ann_gen_solarPV_MWh", "ann_gen_wind_MWh",
                                        "ann_gen_hydro_MWh", "ann_gen_nuclear_MWh",
                                        "ann_discharge_bat_storage_MWh", "ann_charge_bat_storage_MWh",
                                        "capacity_vre_MW", "capacity_solarPV_MW", "capacity_wind_MW", 
                                        "capacity_bat_storage_MW", "energy_bat_storage_MWh", "date_time"])


'''
#############################################
## SCENARIOS IN A LOOP ##
#############################################
'''
## Set the scenario suffix. Applied to new conventional capacity input and output scenario
#if all_new_coal == 'yes':
#    scenario_suffix = '_allCoal'
#elif coal_low_cap_cost == 'yes':
#    scenario_suffix = '_lowCapCostCoal'
#else:
#    scenario_suffix = ''

scenario_suffix_operation = ""    
    
for sc in range(len(scenarios)):
    
    # Read the results csv file again if there are more than one scenario. Write the output results after each loop, so run is saved even if computer crashes    
    if sc > 0:
        results_all_scenarios = pd.read_csv(outputPath + results_all_scenarios_csv, sep=',')
    
    ## VRE generation
    genVRE_all = pd.read_csv(inputPathVRE_gen_profiles + str(yearBase) + "_RE_gen_" + scenarios[sc] + ".csv", sep=',')
    # Add repeated data for lookahead periods
    for v in range(1, 1 + num_lookahead_days, 1):        
        genVRE_all_to_append = genVRE_all.loc[genVRE_all['Day'] == genVRE_all[-1:].iloc[0]['Day']]
        genVRE_all_to_append.loc[:,'Day'] += 1
        genVRE_all_to_append.loc[:,'Timepoint'] += 24 # Assuming the model has hourly timestep. Otherwise change this code
        genVRE_all = genVRE_all.append(genVRE_all_to_append, ignore_index = True)
    
    
    ## Add VRE generators and their capacities to the gen
    #genVRE_toAdd = genVRE_allScenarios.loc[genVRE_allScenarios['scenario'] == sc]
    genVRE_toAdd = pd.read_csv(inputPathVRE_capacity + str(yearBase) + "_RE_capacity_" + scenarios[sc] + ".csv", sep=',')
    genVRE_toAdd.drop(['scenario'], axis=1, inplace=True)
    genVRE_toAdd_melt = pd.melt(genVRE_toAdd, value_vars=list(genVRE_toAdd.columns.values))
    genVRE_toAdd_melt.columns = ['generator', 'gen_capacity']
    genVRE_toAdd_melt['var_cost'] = 0
    genVRE_toAdd_melt['type'] = 'vre'
    ## Create a dataframe for VRE potential generation
    vre_potential_generation_ts = pd.DataFrame(genVRE_all['solarPV'] * genVRE_toAdd['solarPV'][0])
    vre_potential_generation_ts['wind'] = genVRE_all['wind'] * genVRE_toAdd['wind'][0]
    vre_potential_generation_ts['Timepoint'] = genVRE_all['Timepoint']
    vre_potential_generation_ts.set_index('Timepoint', inplace=True)
    
    ## NEW CONVENTIONAL GENERATOR BUILDOUT
#    genCONV_toAdd = pd.read_csv(inputPathNEWCONV_capacity + str(yearAnalysis) + "_conventional_capacity_" + scenarios[sc] + scenario_suffix + ".csv", sep=',')
    
    ## Battery storage
    storBATTERY_input = pd.read_csv(inputPath + storBATTERY_csv, sep=',')  # storage in this case does not include hydro. Only rechargeable storage - batteries, PHS
    storBATTERY_input = storBATTERY_input.loc[storBATTERY_input['scenario'] == battery_scenario]
    
    ## All generators
    genALL_input = pd.read_csv(inputPath + genALL_input_csv, sep=',')
    # Add the VRE generators for the current scenario
    genALL_input = genALL_input.append(genVRE_toAdd_melt, ignore_index=True)
    # Add the new conventional generators for the current scenario
#    genALL_input = genALL_input.append(genCONV_toAdd, ignore_index=True)
    # Add the new battery storage 
    #genALL_input = genALL_input.append(genBATTERY_toAdd, ignore_index=True) # Not including storage as a generator
    
    # Drop all conventional generators other than must run, hydro, and vre
    genALL_input = genALL_input[genALL_input['type'].isin(['hydro', 'mustrun', 'vre'])]
    
    # Add the hypothetical generator record
    genCONV_HYPOTHETICAL_df = pd.DataFrame([[gen_capacity_conventional_hypothetical, 'GEN_CONV_HYPOTHETICAL', 'conv_hypo', 0]], columns = ['gen_capacity', 'generator', 'type', 'var_cost'])
    genALL_input = genALL_input.append(genCONV_HYPOTHETICAL_df, ignore_index = True)
    
#    ## Derating conventional generator capacity based on outage rates
#    if derating_outages_conventional_gen == "yes":
#        genALL_input.loc[genALL_input.type == 'coal', 'gen_capacity'] = genALL_input.loc[genALL_input.type == 'coal', 'gen_capacity'] * (1-outage_rate_coal)
#        genALL_input.loc[genALL_input.type == 'gas_ccgt', 'gen_capacity'] = genALL_input.loc[genALL_input.type == 'gas_ccgt', 'gen_capacity'] * (1-outage_rate_gas_ccgt)
#        genALL_input.loc[genALL_input.type == 'other', 'gen_capacity'] = genALL_input.loc[genALL_input.type == 'other', 'gen_capacity'] * (1-outage_rate_other)
#        genALL_input.loc[genALL_input.type == 'gas_ct', 'gen_capacity'] = genALL_input.loc[genALL_input.type == 'gas_ct', 'gen_capacity'] * (1-outage_rate_gas_ct)
#        genALL_input.loc[genALL_input.type == 'diesel', 'gen_capacity'] = genALL_input.loc[genALL_input.type == 'diesel', 'gen_capacity'] * (1-outage_rate_diesel)
    
    # GENERATOR LIST. This is used for the generator index.
    genALL  = genALL_input['generator'].tolist() 
    # Thermal and other dispatchable generators (other than hydro)
#    genCOAL = genALL_input[genALL_input['type'].isin(['coal'])]['generator'].tolist()
#    genGASCCGT = genALL_input[genALL_input['type'].isin(['gas_ccgt'])]['generator'].tolist()
#    genOTHER = genALL_input[genALL_input['type'].isin(['other'])]['generator'].tolist()
#    genGASCT = genALL_input[genALL_input['type'].isin(['gas_ct'])]['generator'].tolist()
#    genDIESEL = genALL_input[genALL_input['type'].isin(['diesel'])]['generator'].tolist()
    
    # Add a hypothetical conventional generator
    genCONVHYPOTHETICAL = genALL_input[genALL_input['type'].isin(['conv_hypo'])]['generator'].tolist()
    
    # VARIABLE COST
    genALL_varCost = genALL_input[['generator', 'var_cost']]
    genALL_varCost.set_index('generator', inplace=True)
    genALL_varCost = genALL_varCost.to_dict()['var_cost']

    
    # GENERATOR CAPACITY
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
#    # Capacity - # coal generators
#    genCOAL_genCapacity = genALL_input[genALL_input['type'].isin(['coal'])][['generator', 'gen_capacity']]
#    genCOAL_genCapacity.set_index('generator', inplace=True)
#    genCOAL_genCapacity = genCOAL_genCapacity.to_dict()['gen_capacity']
#    # Capacity - # ccgt gas generators
#    genGASCCGT_genCapacity = genALL_input[genALL_input['type'].isin(['gas_ccgt'])][['generator', 'gen_capacity']]
#    genGASCCGT_genCapacity.set_index('generator', inplace=True)
#    genGASCCGT_genCapacity = genGASCCGT_genCapacity.to_dict()['gen_capacity']
#    # Capacity - # ct gas generators
#    genGASCT_genCapacity = genALL_input[genALL_input['type'].isin(['gas_ct'])][['generator', 'gen_capacity']]
#    genGASCT_genCapacity.set_index('generator', inplace=True)
#    genGASCT_genCapacity = genGASCT_genCapacity.to_dict()['gen_capacity']
#    # Capacity - # diesel generators
#    genDIESEL_genCapacity = genALL_input[genALL_input['type'].isin(['diesel'])][['generator', 'gen_capacity']]
#    genDIESEL_genCapacity.set_index('generator', inplace=True)
#    genDIESEL_genCapacity = genDIESEL_genCapacity.to_dict()['gen_capacity']
#    # Capacity - # other generators - These include biomass cogeneration and diesel
#    genOTHER_genCapacity = genALL_input[genALL_input['type'].isin(['other'])][['generator', 'gen_capacity']]
#    genOTHER_genCapacity.set_index('generator', inplace=True)
#    genOTHER_genCapacity = genOTHER_genCapacity.to_dict()['gen_capacity']
    # Capacity - # must run generators
    genMUSTRUN_genCapacity = genALL_input[genALL_input['type'].isin(['mustrun'])][['generator', 'gen_capacity']]
    genMUSTRUN_genCapacity.set_index('generator', inplace=True)
    genMUSTRUN_genCapacity = genMUSTRUN_genCapacity.to_dict()['gen_capacity']
    
    # Capacity - # hypothetical conventional generator
    genCONVHYPOTHETICAL_genCapacity = genALL_input[genALL_input['type'].isin(['conv_hypo'])][['generator', 'gen_capacity']]
    genCONVHYPOTHETICAL_genCapacity.set_index('generator', inplace=True)
    genCONVHYPOTHETICAL_genCapacity = genCONVHYPOTHETICAL_genCapacity.to_dict()['gen_capacity']    
    
    ## OTHER GENERATOR DATA
    ## Hydro generation
    genHYDRO_minGen_all = pd.read_csv(inputPath + genHYDRO_minGen_csv, sep=',')
    genHYDRO_maxEnergy_all = pd.read_csv(inputPath + genHYDRO_maxEnergy_csv, sep=',')
    
    ## Must Run generation
    genMUSTRUN_all = pd.read_csv(inputPath + genMUSTRUN_csv, sep=',')
    
    ## BATTERY STORAGE
    # BATTERY STORAGE LIST. This is used for the storage index.
    storBATTERY = storBATTERY_input[storBATTERY_input['type'].isin(['bat_storage'])]['storage'].tolist()
    
    # BATTERY STORAGE VARIABLE COST
    storBATTERY_varCost = storBATTERY_input[['storage', 'var_cost']]
    storBATTERY_varCost.set_index('storage', inplace=True)
    storBATTERY_varCost = storBATTERY_varCost.to_dict()['var_cost']
    
    # BATTERY STORAGE POWER CAPACITY
    storBATTERY_storCapacity = storBATTERY_input[storBATTERY_input['type'].isin(['bat_storage'])][['storage', 'stor_capacity']] # type can be bat_storage or phs_storage if PHS has different constraints
    storBATTERY_storCapacity.set_index('storage', inplace=True)
    storBATTERY_storCapacity = storBATTERY_storCapacity.to_dict()['stor_capacity']
    
    # BATERY STORAGE ENERGY CAPACTY
    storBATTERY_storEnergy = storBATTERY_input[storBATTERY_input['type'].isin(['bat_storage'])][['storage', 'stor_energy']] # type can be bat_storage or phs_storage if PHS has different constraints
    storBATTERY_storEnergy.set_index('storage', inplace=True)
    storBATTERY_storEnergy = storBATTERY_storEnergy.to_dict()['stor_energy']
    
    # BATTERY STORAGE EFFICIENCY
    storBATTERY_efficiency = storBATTERY_input.loc[storBATTERY_input['type'].isin(['bat_storage'])].iloc[0]['efficiency']

    # If battery storage exists
    if storBATTERY_input['stor_energy'][storBATTERY_input['type']=='bat_storage'].sum() == 0:
        battery_storage_flag = 0
    else:
        battery_storage_flag = 1
    
    
    '''
    #############################################
    ## INPUT DATA IN A LOOP ##
    #############################################
    '''
    
    ## CREATE THE RESULTS DATAFRAMES ##
    dispatch_all_ts_annual = pd.DataFrame() # all generator dispatch
    dispatch_vre_ts_annual = pd.DataFrame() # Only vre dispatch
    dispatch_cost_ts_annual = pd.DataFrame() # annual dispatch cost for every day
    ## CREATE RESULTS VARIABLES
    dispatch_annual_gen_all = 0
    dispatch_annual_gen_solarPV = 0
    dispatch_annual_gen_wind = 0
    dispatch_annual_gen_vre = 0
#    dispatch_annual_gen_coal = 0
#    dispatch_annual_gen_gas_ccgt = 0
#    dispatch_annual_gen_other = 0
#    dispatch_annual_gen_gas_ct = 0
#    dispatch_annual_gen_diesel = 0
    dispatch_annual_gen_conv_hypo = 0
    dispatch_annual_gen_hydro = 0
    dispatch_annual_gen_nuclear = 0
    dispatch_cost_annual = 0
    
    discharge_annual_stor_battery = 0
    charge_annual_stor_battery = 0
    
    curtailment_annual_gen_vre = 0
    potential_gen_vre_2 = 0 # Bottom up potential just for the days that are solved. Previous DF used the whole year's data
    
    netnetload_ts_annual = pd.DataFrame() # Net net load used for screening curves
    
    # Set imbalance in allocated/available hydro storage energy and actual dispatched energy as zero
    genHYDRO_maxEnergy_available_dispatched_diff = 0
    
    #d = 1
    for d in range(start_day, end_day + 1):
        print "Solving step " + str(d)
        
        ## Days to be included for optimization step: Current day + lookahead days
        d_all = range(d, (d + num_lookahead_days + 1))
        ## Load
        load_ts = load.loc[load['Day'].isin(d_all)]
        # load_ts = load.loc[load['Day'] == d] # DELETE
        timepoints = load_ts['Timepoint'].tolist() # Initialize the timepoints from the load data frame. This is used for the Timepoint index for the whole optimization problem
        timepoints_realtime = load_ts.loc[load['Day'] == d]['Timepoint'].tolist() # Initialize the timepoints from the load data frame. This is used for the Timepoint index for only realtime dispatch e.g. current day
        timepoints_minus_first = timepoints[1:] # All but the first element of the timepoints
        timepoints_first = [timepoints[0]] # Just the first timepoint
        timepoints_last = [timepoints[-1]] # Just the last timepoint
        timepoints_plus_one = [timepoints[0] - 1] + timepoints
        timepoints_plus_one_first = [timepoints_plus_one[0]] # Just the first timepoint of timepoint plus one
        load_ts.drop(['Day', 'dateTime'], axis=1, inplace=True) # Keep only the timepoint and load columns
        load_ts.set_index('Timepoint', inplace=True)
        load_ts = load_ts.round(0)
        load_ts = load_ts.to_dict()['load']
        
        ## Hydro generation and energy limit
        # Min Gen
        genHYDRO_minGen_cf_ts = genHYDRO_minGen_all.loc[genHYDRO_minGen_all['Day'].isin(d_all)]
        genHYDRO_minGen_cf_ts.drop(['Day'], axis=1, inplace=True)
        genHYDRO = genHYDRO_minGen_cf_ts.columns.values.tolist()
        genHYDRO_minGen_cf_ts = pd.DataFrame(genHYDRO_minGen_cf_ts.mean(axis=0)) # mean of min gen across current and lookahead days
        genHYDRO_minGen_cf_ts = genHYDRO_minGen_cf_ts.to_dict().itervalues().next() #  convert to dictionary, and take the next level using itervalues because the index will keep changing in the loop
        # Max Energy
        genHYDRO_maxEnergy_ts = genHYDRO_maxEnergy_all.loc[genHYDRO_maxEnergy_all['Day'].isin(d_all)]
        genHYDRO_maxEnergy_ts.drop(['Day'], axis=1, inplace=True)
        genHYDRO_maxEnergy_ts = pd.DataFrame(genHYDRO_maxEnergy_ts.sum(axis=0)) + genHYDRO_maxEnergy_available_dispatched_diff # sum of max energy across current and lookahead days minus imbalance between allocated and dispatched for previous day
        genHYDRO_maxEnergy_ts = genHYDRO_maxEnergy_ts.to_dict().itervalues().next() # convert to dictionary, and take the next level using itervalues because the index will keep changing in the loop
        
        ## VRE generation
        genVRE_cf_ts = genVRE_all.loc[genVRE_all['Day'].isin(d_all)] # Select all the rows for a particular day in the loop
        genVRE_cf_ts.drop(['Day', 'dateTime'], axis=1, inplace=True) # Drop all columns except timepoints and VRE generators
        genVRE = genVRE_cf_ts.columns.values[1:].tolist() # List of VRE generators
        genVRE_cf_ts_melt = pd.melt(genVRE_cf_ts, id_vars=['Timepoint'], value_vars=genVRE) # Melt the table with Timepoints and VRE generators
        genVRE_cf_ts_melt.set_index(['Timepoint', 'variable'], inplace = True)
        genVRE_cf_ts= genVRE_cf_ts_melt.to_dict()["value"]
        
        
        ## Must run generation
        genMUSTRUN_cf_ts = genMUSTRUN_all.loc[genMUSTRUN_all['Day'].isin(d_all)]
        genMUSTRUN_cf_ts.drop(['Day'], axis=1, inplace=True)
        genMUSTRUN = genMUSTRUN_cf_ts.columns.values.tolist()
        genMUSTRUN_cf_ts = pd.DataFrame(genMUSTRUN_cf_ts.mean(axis=0)) # mean of min gen across current and lookahead days
        genMUSTRUN_cf_ts = genMUSTRUN_cf_ts.to_dict().itervalues().next() # convert to dictionary, and take the next level using itervalues because the index will keep changing in the loop
        # genMUSTRUN_cf_ts = genMUSTRUN_cf_ts.T.to_dict().itervalues().next() # Old Code before averaging: Take the transpose of the dataframe (in this case one row), then convert to dictionary, and take the next level using itervalues because the index will keep changing in the loop
        
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
        model.TIMEPOINTSMINUSFIRST = Set(initialize=timepoints_minus_first)
        model.TIMEPOINTSFIRST = Set(initialize=timepoints_first)
        model.TIMEPOINTSLAST = Set(initialize=timepoints_last)
        model.TIMEPOINTSPLUSONE = Set(initialize=timepoints_plus_one) # Use this for initializing energy storage state of charge
        model.TIMEPOINTSPLUSONEFIRST = Set(initialize=timepoints_plus_one_first) 
        model.GEN = Set(initialize=genALL) # Initialize with the list of all generators
        model.GENHYDRO = Set(within=model.GEN, initialize=genHYDRO) # Initialize with the HYDRO generator list ## Can use initialize=set(proj for proj in hydroEnergy)
        model.GENVRE = Set(within=model.GEN, initialize=genVRE) # Initialize with the VRE generator list
        model.GENMUSTRUN = Set(within=model.GEN, initialize=genMUSTRUN) # Initialize with the MUST RUN generator list
#        model.GENCOAL = Set(within=model.GEN, initialize=genCOAL) # Initialize with the COAL generator list
#        model.GENGASCCGT = Set(within=model.GEN, initialize=genGASCCGT) # Initialize with the GAS CCGT generator list
#        model.GENOTHER = Set(within=model.GEN, initialize=genOTHER) # Initialize with the OTHER generator list
#        model.GENGASCT = Set(within=model.GEN, initialize=genGASCT) # Initialize with the GAS CT generator list
#        model.GENDIESEL = Set(within=model.GEN, initialize=genDIESEL) # Initialize with the DIESEL generator list
        model.STORBATTERY = Set(initialize=storBATTERY) # Initialize with the BATTERY STORAGE generator list
        
        model.GENCONVHYPOTHETICAL = Set(within=model.GEN, initialize=genCONVHYPOTHETICAL)
        
        
        '''
        #############################################
        ## PARAMETERS ##
        #############################################
        '''
        
        model.load_mw = Param(model.TIMEPOINTS, default=load_ts)
        model.dispatch_cost = Param(model.GEN, default=genALL_varCost, doc="dispatch cost in $/MWh")
        model.gen_capacity = Param(model.GEN, default=genALL_genCapacity) # Max capacity of generators
        model.gen_min_cf_hydro = Param(model.GENHYDRO, default=genHYDRO_minGen_cf_ts) # Minimum generation hydro
        model.gen_energy_hydro = Param(model.GENHYDRO, default=genHYDRO_maxEnergy_ts) # Max energy hydro
        model.gen_cf_vre = Param(model.TIMEPOINTS, model.GENVRE, default=genVRE_cf_ts) # Capacity factors for VRE
        model.gen_cf_mustrun = Param(model.GENMUSTRUN, default=genMUSTRUN_cf_ts) # Capacity factors for MUST RUN
#        model.gen_min_cf_coal = Param(model.GENCOAL, default=genCOAL_minGen_cf) # Minimum generation coal
#        model.gen_min_cf_gas_ccgt = Param(model.GENGASCCGT, default=genGASCCGT_minGen_cf) # Minimum generation gas ccgt
#        model.gen_min_cf_other = Param(model.GENOTHER, default=genOTHER_minGen_cf) # Minimum generation other
#        model.gen_min_cf_gas_ct = Param(model.GENGASCT, default=genGASCT_minGen_cf) # Minimum generation gas ct - not used
#        model.gen_min_cf_gas_diesel = Param(model.GENDIESEL, default=genDIESEL_minGen_cf) # Minimum generation diesel - not used
        
        model.stor_capacity = Param(model.STORBATTERY, default=storBATTERY_storCapacity) # Max capacity of battery storage
        model.stor_energy = Param(model.STORBATTERY, default=storBATTERY_storEnergy) # Max energy of battery storage
        model.stor_initial_soc = Param(model.STORBATTERY, default=storBATTERY_initial_soc) # Initial state of charge of battery
        
        '''
        #############################################
        ## DECISION VARIABLES ##
        #############################################
        '''
        
        model.DispatchMW = Var(model.TIMEPOINTS, model.GEN, within=NonNegativeReals) # dispactH of generators
#        model.uc = Var(model.GEN, within=Binary) # unit commitment of generators
        model.load_unserved_mw = Var(model.TIMEPOINTS, within=NonNegativeReals) # unserved energy
        model.sd = Var(model.TIMEPOINTS, model.STORBATTERY, within = Binary) # flag for discharging or dispatch
        model.sc = Var(model.TIMEPOINTS, model.STORBATTERY, within = Binary) # flag for charging
        model.DischargeMW = Var(model.TIMEPOINTS, model.STORBATTERY, within=NonNegativeReals) # discharge from storage
        model.ChargeMW = Var(model.TIMEPOINTS, model.STORBATTERY, within=NonNegativeReals) # charge into storage
        model.EnergyStorageMWh = Var(model.TIMEPOINTSPLUSONE, model.STORBATTERY, within=NonNegativeReals) # energy level of storage
        
        '''
        #############################################
        ## OBJECTIVE FUNCTION ##
        #############################################
        '''
        
        def Dispatch_Cost_rule(mod):
            return (
                sum(
                    genCONVHYPOTHETICAL_cost_param_a * mod.DispatchMW[t, gen] + genCONVHYPOTHETICAL_cost_param_b * mod.DispatchMW[t, gen]**2
                    for gen in mod.GENCONVHYPOTHETICAL
                    for t in mod.TIMEPOINTS
                ) +
                sum(
                mod.load_unserved_mw[t] * VoLL
                for t in mod.TIMEPOINTS
                )
            )
        model.DispatchCost = Objective(rule=Dispatch_Cost_rule)
        
        
        '''
        #############################################
        ## CONSTRAINTS ##
        #############################################
        '''
        
        # Dispatch = load
        def Conservation_Of_Energy_rule(mod, t):
            return sum(mod.DispatchMW[t, gen] for gen in mod.GEN) + sum(mod.DischargeMW[t, stor] for stor in mod.STORBATTERY) == mod.load_mw[t] + sum(mod.ChargeMW[t, stor] for stor in mod.STORBATTERY) - mod.load_unserved_mw[t]
        model.Conservation_Of_Energy = Constraint(model.TIMEPOINTS, rule=Conservation_Of_Energy_rule)
        
        # Dispatch <= max capacity of each generator
        def Enforce_Max_Dispatch_Limit_rule(mod, t, gen):
            return mod.DispatchMW[t, gen] <= mod.gen_capacity[gen] #* mod.uc[gen] # I don't think I need the integer UC in this constraint
        model.Enforce_Max_Dispatch_Limit = Constraint(model.TIMEPOINTS, model.GEN, rule=Enforce_Max_Dispatch_Limit_rule)
        
#        # Dispatch >= min generation level for COAL (include unit commitment constraint)
#        def Enforce_Min_Dispatch_Limit_Coal_rule(mod, t, gen):
#            return mod.DispatchMW[t, gen] >= mod.gen_min_cf_coal[gen] * mod.gen_capacity[gen] * mod.uc[gen] # This constraint can be combined with the other two min gen constraints
#        model.Enforce_Min_Dispatch_Limit_Coal = Constraint(model.TIMEPOINTS, model.GENCOAL, rule=Enforce_Min_Dispatch_Limit_Coal_rule)
#        
#        # Dispatch >= min generation level for GAS CCGT (include unit commitment constraint)
#        def Enforce_Min_Dispatch_Limit_GasCCGT_rule(mod, t, gen):
#            return mod.DispatchMW[t, gen] >= mod.gen_min_cf_gas_ccgt[gen] * mod.gen_capacity[gen] * mod.uc[gen]
#        model.Enforce_Min_Dispatch_Limit_GasCCGT = Constraint(model.TIMEPOINTS, model.GENGASCCGT, rule=Enforce_Min_Dispatch_Limit_GasCCGT_rule)
#        
#        # Dispatch >= min generation level for OTHER (include unit commitment constraint)
#        def Enforce_Min_Dispatch_Limit_Other_rule(mod, t, gen):
#            return mod.DispatchMW[t, gen] >= mod.gen_min_cf_other[gen] * mod.gen_capacity[gen] * mod.uc[gen]
#        model.Enforce_Min_Dispatch_Limit_Other = Constraint(model.TIMEPOINTS, model.GENOTHER, rule=Enforce_Min_Dispatch_Limit_Other_rule)
        
#        # Dispatch >= min generation level for GAS CT (peaker unit - don't include unit commitment constraint)
#        def Enforce_Min_Dispatch_Limit_GasCT_rule(mod, t, gen):
#            return mod.DispatchMW[t, gen] >= mod.gen_min_cf_gas_ct[gen] * mod.gen_capacity[gen]
#        model.Enforce_Min_Dispatch_Limit_GasCT = Constraint(model.TIMEPOINTS, model.GENGASCT, rule=Enforce_Min_Dispatch_Limit_GasCT_rule)
#        
#        # Dispatch >= min generation level for DIESEL (peaker unit - don't include unit commitment constraint)
#        def Enforce_Min_Dispatch_Limit_Diesel_rule(mod, t, gen):
#            return mod.DispatchMW[t, gen] >= mod.gen_min_cf_diesel[gen] * mod.gen_capacity[gen]
#        model.Enforce_Min_Dispatch_Limit_Diesel = Constraint(model.TIMEPOINTS, model.GENDIESEL, rule=Enforce_Min_Dispatch_Limit_Diesel_rule)
    
        # Dispatch >= min generation level for Hydro
        def Enforce_Min_Dispatch_Limit_Hydro_rule(mod, t, gen):
            return mod.DispatchMW[t, gen] >= mod.gen_min_cf_hydro[gen] * mod.gen_capacity[gen]
        model.Enforce_Min_Dispatch_Limit_Hydro = Constraint(model.TIMEPOINTS, model.GENHYDRO, rule=Enforce_Min_Dispatch_Limit_Hydro_rule)
        
        # Total hydro energy is conserved across the time series (e.g. day)
        def Enforce_Hydro_Energy_Limit_rule(mod, gen):
            return sum(mod.DispatchMW[t, gen] for t in mod.TIMEPOINTS) == mod.gen_energy_hydro[gen]
        model.Enforce_Hydro_Energy_Limit = Constraint(model.GENHYDRO, rule=Enforce_Hydro_Energy_Limit_rule)
        
        # Dispatch <= VRE generation. Curtailment is ok. Could add another variable for curtailment and change this constraint to ==
        def Enforce_VRE_Dispatch_Limit_rule(mod, t, gen):
            return mod.DispatchMW[t, gen] <= mod.gen_cf_vre[t, gen] * mod.gen_capacity[gen]
        model.Enforce_VRE_Dispatch_Limit = Constraint(model.TIMEPOINTS, model.GENVRE, rule=Enforce_VRE_Dispatch_Limit_rule)
        
        # Dispatch == MUST RUN generation.
        def Enforce_MUSTRUN_Dispatch_rule(mod, t, gen):
            return mod.DispatchMW[t, gen] == mod.gen_cf_mustrun[gen] * mod.gen_capacity[gen]
        model.Enforce_MUSTRUN_Dispatch = Constraint(model.TIMEPOINTS, model.GENMUSTRUN, rule=Enforce_MUSTRUN_Dispatch_rule)
        
        # Discharge <= max capacity of each battery storage (includes batteries, pumped hydro)
        def Enforce_Max_Storage_Discharge_Limit_rule(mod, t, stor):
            return mod.DischargeMW[t, stor] <= mod.stor_capacity[stor] * mod.sd[t, stor]
        model.Enforce_Max_Storage_Discharge_Limit = Constraint(model.TIMEPOINTS, model.STORBATTERY, rule=Enforce_Max_Storage_Discharge_Limit_rule)    
        
        # Charge <= max capacity of each battery storage (includes batteries, pumped hydro)
        def Enforce_Max_Storage_Charge_Limit_rule(mod, t, stor):
            return mod.ChargeMW[t, stor] <= mod.stor_capacity[stor] * mod.sc[t, stor]
        model.Enforce_Max_Storage_Charge_Limit = Constraint(model.TIMEPOINTS, model.STORBATTERY, rule=Enforce_Max_Storage_Charge_Limit_rule)  
        
        # Battery energy <= max energy capacity of battery storage
        def Enforce_Storage_Energy_Limit_rule(mod, t, stor):
            return mod.EnergyStorageMWh[t, stor] <= mod.stor_energy[stor]
        model.Enforce_Storage_Energy_Limit = Constraint(model.TIMEPOINTS, model.STORBATTERY, rule=Enforce_Storage_Energy_Limit_rule)
        
        # Battery energy accounting
        def Enforce_Storage_Energy_Accounting_rule(mod, t, stor):
            return mod.EnergyStorageMWh[t, stor] == mod.EnergyStorageMWh[(t-1), stor] + storBATTERY_efficiency * mod.ChargeMW[t, stor] - mod.DischargeMW[t, stor]
        model.Enforce_Storage_Energy_Accounting = Constraint(model.TIMEPOINTS, model.STORBATTERY, rule=Enforce_Storage_Energy_Accounting_rule)
        
        # Battery energy initializing state of charge
        def Enforce_Storage_Energy_First_SOC_rule(mod, t, stor):
            return mod.EnergyStorageMWh[t, stor] == mod.stor_energy[stor] * mod.stor_initial_soc[stor]
        model.Enforce_Storage_Energy_First_SOC = Constraint(model.TIMEPOINTSPLUSONEFIRST, model.STORBATTERY, rule=Enforce_Storage_Energy_First_SOC_rule)
        
        # Battery energy last state of charge (same as first state of charge)
        def Enforce_Storage_Energy_Last_SOC_rule(mod, t, stor):
            return mod.EnergyStorageMWh[t, stor] == mod.stor_energy[stor] * mod.stor_initial_soc[stor]
        model.Enforce_Storage_Energy_Last_SOC = Constraint(model.TIMEPOINTSLAST, model.STORBATTERY, rule=Enforce_Storage_Energy_Last_SOC_rule)
        
        def Enforce_Storage_Charge_Discharge_Exclusivity_rule(mod, t, stor):
            return mod.sc[t, stor] + mod.sd[t, stor] <= 1
        model.Enforce_Storage_Charge_Discharge_Exclusivity = Constraint(model.TIMEPOINTS, model.STORBATTERY, rule=Enforce_Storage_Charge_Discharge_Exclusivity_rule)
        
                
        
        
        '''
        #############################################
        ## CREATE MODEL INSTANCE AND RUN THE MODEL ##
        #############################################
        '''
        
        print("Compiling...")
        model_instance = model.create_instance()
        print("Solving...")
        results = opt.solve(model_instance, tee=True)
        
        # print("Instance pprint...")
        # model_instance.pprint()
        
        print("Writing results...")
        results.write()
        
        dispatch_cost = model_instance.DispatchCost.expr() # Total dispatch cost
        print "Total dispatch cost: INR " + str(dispatch_cost) + " for day " + str(d) + " of scenario " + scenarios[sc] 
        
        # model_instance.solutions.load_from(results)
        
        '''
        #############################################
        ## RESULTS PROCESSING ##
        #############################################
        '''
        
        ## TIMEPOINTS FOR ONLY CURRENT DAY OR REALTIME DISPATCH WITHOUT LOOKAHEAD 
        timepoints = timepoints_realtime # for now, just change timepoints to timepoints_realtime. timepoints index will reset at the beginning of the loop.
        
        ## DISPATCH OF ALL GENERATORS
        #dispatch_all_ts = pd.DataFrame({'Day': d}, index = timepoints)
        dispatch_all_ts = pd.DataFrame(index = timepoints)
        for p in genALL:
            dispatch_ts_gen = [model_instance.DispatchMW[t,p].value for t in timepoints]
            dispatch_ts_gen_df = pd.DataFrame({p : dispatch_ts_gen}, index = timepoints)
            # dispatch_ts = dispatch_ts.concat(dispatch_ts_gen_df, axis = 1)
            dispatch_all_ts[p] = dispatch_ts_gen_df
        # dispatch_all_ts_annual = dispatch_all_ts_annual.append(dispatch_all_ts, ignore_index=True) # Add the day's dispatch to the annual result
        dispatch_annual_gen_all = np.around(dispatch_annual_gen_all + dispatch_all_ts.sum(axis=1).sum(axis=0), decimals = 2) # Total annual generation
        
        ## DISPATCH OF VRE GENERATORS
        dispatch_vre_ts = pd.DataFrame(index = timepoints)
        for p in genVRE:
            dispatch_ts_gen = [model_instance.DispatchMW[t,p].value for t in timepoints]
            dispatch_ts_gen_df = pd.DataFrame({p : dispatch_ts_gen}, index = timepoints)
            # dispatch_ts = dispatch_ts.concat(dispatch_ts_gen_df, axis = 1)
            dispatch_vre_ts[p] = dispatch_ts_gen_df
        #dispatch_vre_ts_annual = dispatch_vre_ts_annual.append(dispatch_vre_ts, ignore_index=True) # Add the day's dispatch to the annual result
        
        dispatch_annual_gen_solarPV = np.around(dispatch_annual_gen_solarPV + dispatch_vre_ts['solarPV'].sum(), decimals = 2) # total solar PV generation
        dispatch_annual_gen_wind = np.around(dispatch_annual_gen_wind + dispatch_vre_ts['wind'].sum(), decimals = 2) # total wind generation
        dispatch_annual_gen_vre = np.around(dispatch_annual_gen_vre + dispatch_vre_ts.sum(axis=1).sum(axis=0), decimals = 2) # total vre generation
        
#        ## DISPATCH OF COAL GENERATORS
#        dispatch_coal_ts = pd.DataFrame(index = timepoints)
#        for p in genCOAL:
#            dispatch_ts_gen = [model_instance.DispatchMW[t,p].value for t in timepoints]
#            dispatch_ts_gen_df = pd.DataFrame({p : dispatch_ts_gen}, index = timepoints)
#            # dispatch_ts = dispatch_ts.concat(dispatch_ts_gen_df, axis = 1)
#            dispatch_coal_ts[p] = dispatch_ts_gen_df
#        dispatch_annual_gen_coal = np.around(dispatch_annual_gen_coal + dispatch_coal_ts.sum(axis=1).sum(axis=0), decimals = 2) # Total annual generation
#        
#        ## DISPATCH OF GAS CCGT GENERATORS
#        dispatch_gas_ccgt_ts = pd.DataFrame(index = timepoints)
#        for p in genGASCCGT:
#            dispatch_ts_gen = [model_instance.DispatchMW[t,p].value for t in timepoints]
#            dispatch_ts_gen_df = pd.DataFrame({p : dispatch_ts_gen}, index = timepoints)
#            # dispatch_ts = dispatch_ts.concat(dispatch_ts_gen_df, axis = 1)
#            dispatch_gas_ccgt_ts[p] = dispatch_ts_gen_df
#        dispatch_annual_gen_gas_ccgt = np.around(dispatch_annual_gen_gas_ccgt + dispatch_gas_ccgt_ts.sum(axis=1).sum(axis=0), decimals = 2) # Total annual generation
#        
#        ## DISPATCH OF GAS CT GENERATORS
#        dispatch_gas_ct_ts = pd.DataFrame(index = timepoints)
#        for p in genGASCT:
#            dispatch_ts_gen = [model_instance.DispatchMW[t,p].value for t in timepoints]
#            dispatch_ts_gen_df = pd.DataFrame({p : dispatch_ts_gen}, index = timepoints)
#            # dispatch_ts = dispatch_ts.concat(dispatch_ts_gen_df, axis = 1)
#            dispatch_gas_ct_ts[p] = dispatch_ts_gen_df
#        dispatch_annual_gen_gas_ct = np.around(dispatch_annual_gen_gas_ct + dispatch_gas_ct_ts.sum(axis=1).sum(axis=0), decimals = 2) # Total annual generation
#        
#        ## DISPATCH OF GAS DIESEL GENERATORS
#        dispatch_diesel_ts = pd.DataFrame(index = timepoints)
#        for p in genDIESEL:
#            dispatch_ts_gen = [model_instance.DispatchMW[t,p].value for t in timepoints]
#            dispatch_ts_gen_df = pd.DataFrame({p : dispatch_ts_gen}, index = timepoints)
#            # dispatch_ts = dispatch_ts.concat(dispatch_ts_gen_df, axis = 1)
#            dispatch_diesel_ts[p] = dispatch_ts_gen_df
#        dispatch_annual_gen_diesel = np.around(dispatch_annual_gen_diesel + dispatch_diesel_ts.sum(axis=1).sum(axis=0), decimals = 2) # Total annual generation
#        
#        ## DISPATCH OF OTHER GENERATORS
#        dispatch_other_ts = pd.DataFrame(index = timepoints)
#        for p in genOTHER:
#            dispatch_ts_gen = [model_instance.DispatchMW[t,p].value for t in timepoints]
#            dispatch_ts_gen_df = pd.DataFrame({p : dispatch_ts_gen}, index = timepoints)
#            # dispatch_ts = dispatch_ts.concat(dispatch_ts_gen_df, axis = 1)
#            dispatch_other_ts[p] = dispatch_ts_gen_df
#        #dispatch_all_ts_annual = dispatch_all_ts_annual.append(dispatch_all_ts, ignore_index=True) # Add the day's dispatch to the annual result
#        dispatch_annual_gen_other = np.around(dispatch_annual_gen_other + dispatch_other_ts.sum(axis=1).sum(axis=0), decimals = 2) # Total annual generation
       
        ## DISCHARGE OF BATTERY STORAGE
        discharge_stor_battery_ts = pd.DataFrame(index = timepoints)
        for p in storBATTERY:
            discharge_ts_stor = [model_instance.DischargeMW[t,p].value for t in timepoints]
            discharge_ts_stor_df = pd.DataFrame({p : discharge_ts_stor}, index = timepoints)
            # dispatch_ts = dispatch_ts.concat(dispatch_ts_gen_df, axis = 1)
            discharge_stor_battery_ts[p] = discharge_ts_stor_df
        #dispatch_all_ts_annual = dispatch_all_ts_annual.append(dispatch_all_ts, ignore_index=True) # Add the day's dispatch to the annual result
        discharge_annual_stor_battery = np.around(discharge_annual_stor_battery + discharge_stor_battery_ts.sum(axis=1).sum(axis=0), decimals = 2) # Total annual generation    
        
        ## CHARGE OF BATTERY STORAGE
        charge_stor_battery_ts = pd.DataFrame(index = timepoints)
        for p in storBATTERY:
            charge_ts_stor = [model_instance.ChargeMW[t,p].value for t in timepoints]
            charge_ts_stor_df = pd.DataFrame({p : charge_ts_stor}, index = timepoints)
            # dispatch_ts = dispatch_ts.concat(dispatch_ts_gen_df, axis = 1)
            charge_stor_battery_ts[p] = charge_ts_stor_df * -1
        #dispatch_all_ts_annual = dispatch_all_ts_annual.append(dispatch_all_ts, ignore_index=True) # Add the day's dispatch to the annual result
        charge_annual_stor_battery = np.around(charge_annual_stor_battery + charge_stor_battery_ts.sum(axis=1).sum(axis=0), decimals = 2) # Total annual generation    

        ## ENERGY LEVEL OF BATTERY STORAGE
        energy_stor_battery_ts = pd.DataFrame(index = timepoints)
        for p in storBATTERY:
            energy_ts_stor = [model_instance.EnergyStorageMWh[t,p].value for t in timepoints]
            energy_ts_stor_df = pd.DataFrame({p : energy_ts_stor}, index = timepoints)
            # dispatch_ts = dispatch_ts.concat(dispatch_ts_gen_df, axis = 1)
            energy_stor_battery_ts[p] = energy_ts_stor_df
        #dispatch_all_ts_annual = dispatch_all_ts_annual.append(dispatch_all_ts, ignore_index=True) # Add the day's dispatch to the annual result
        #energy_annual_stor_battery = np.around(energy_annual_stor_battery + charge_stor_battery_ts.sum(axis=1).sum(axis=0), decimals = 2) # Total annual generation    

        ## DISPATCH OF HYPOTHETICAL CONVENTIONAL GENERATOR
        dispatch_conv_hypo_ts = pd.DataFrame(index = timepoints)
        for p in genCONVHYPOTHETICAL:
            dispatch_ts_gen = [model_instance.DispatchMW[t,p].value for t in timepoints]
            dispatch_ts_gen_df = pd.DataFrame({p : dispatch_ts_gen}, index = timepoints)
            # dispatch_ts = dispatch_ts.concat(dispatch_ts_gen_df, axis = 1)
            dispatch_conv_hypo_ts[p] = dispatch_ts_gen_df
        #dispatch_all_ts_annual = dispatch_all_ts_annual.append(dispatch_all_ts, ignore_index=True) # Add the day's dispatch to the annual result
        dispatch_annual_gen_conv_hypo = np.around(dispatch_annual_gen_conv_hypo + dispatch_conv_hypo_ts.sum(axis=1).sum(axis=0), decimals = 2) # Total annual generation

        ## SUMMARY DISPATCH OF ALL GENERATORS FOR THE DAY
        non_fossil_generators = ["HYDRO-STORAGE", "HYDRO-ROR", "NUCLEAR", "HYDRO-PONDAGE", "solarPV", "wind"]
        dispatch_non_fossil_ts = dispatch_all_ts[non_fossil_generators] # Choose all the non-fossil generator columns
#        dispatch_all_coal_ts= pd.DataFrame(dispatch_coal_ts.sum(axis=1), columns = ['Coal'])
#        dispatch_all_gas_ccgt_ts = pd.DataFrame(dispatch_gas_ccgt_ts.sum(axis=1), columns = ['Gas-CCGT'])
#        dispatch_all_gas_ct_ts = pd.DataFrame(dispatch_gas_ct_ts.sum(axis=1), columns = ['Gas-CT'])
#        dispatch_all_diesel_ts = pd.DataFrame(dispatch_diesel_ts.sum(axis=1), columns = ['Diesel'])
#        dispatch_all_other_ts = pd.DataFrame(dispatch_other_ts.sum(axis=1), columns = ['Other'])
        
        discharge_all_stor_battery_ts = pd.DataFrame(discharge_stor_battery_ts.sum(axis=1), columns = ['Bat-Storage-Discharge'])
        charge_all_stor_battery_ts = pd.DataFrame(charge_stor_battery_ts.sum(axis=1), columns = ['Bat-Storage-Charge'])
        energy_all_stor_battery_ts = pd.DataFrame(energy_stor_battery_ts.sum(axis=1), columns = ['Bat-Storage-Energy-MWh'])
        
        ## Renewable energy curtailment
        vre_curtailment = vre_potential_generation_ts.loc[timepoints].sum(axis=1) - dispatch_vre_ts.sum(axis=1)
        vre_curtailment = vre_curtailment.round(decimals=0)
        vre_curtailment = pd.DataFrame(vre_curtailment, columns = ['curtailment'])
        curtailment_annual_gen_vre = np.round(curtailment_annual_gen_vre + vre_curtailment.sum().sum())
        potential_gen_vre_2 = np.round(potential_gen_vre_2 + vre_potential_generation_ts.loc[timepoints].sum().sum(), decimals = 0)
        
        ## Load curve
        load_ts_df = load_all.loc[timepoints]
        
        ## Net Load curve. This Load - wind - solar
        netload_ts_df = pd.DataFrame(load_ts_df.sum(axis=1) - dispatch_vre_ts.sum(axis=1))
        netload_ts_df.columns = ['netload']
        
        ## Net Net load. This is load - wind - solar - hydroror - hydropndage - nuclear - hydrostorage - batterydischarge + batterycharge. Used for screening curves.
        # Note that charge ts is negative numbers. Hence added in this equation.
        netnetload_ts_df =  pd.DataFrame(load_ts_df.sum(axis=1) - dispatch_non_fossil_ts.sum(axis=1) - discharge_all_stor_battery_ts.sum(axis=1) - charge_all_stor_battery_ts.sum(axis=1))
        netnetload_ts_df.columns = ['netnetload']
        # netnetload annual
        netnetload_ts_annual = netnetload_ts_annual.append(netnetload_ts_df)
        
        # dispatch_all_gen_ts = pd.concat([dispatch_non_fossil_ts, dispatch_all_coal_ts, dispatch_all_gas_ccgt_ts, dispatch_all_gas_ct_ts, dispatch_all_diesel_ts, dispatch_all_other_ts, discharge_all_stor_battery_ts, charge_all_stor_battery_ts, vre_curtailment, load_ts_df, energy_all_stor_battery_ts], axis = 1)
        dispatch_all_gen_ts = pd.concat([dispatch_non_fossil_ts, discharge_all_stor_battery_ts, charge_all_stor_battery_ts, vre_curtailment, load_ts_df, netload_ts_df, netnetload_ts_df, energy_all_stor_battery_ts], axis = 1)
        
        dispatch_all_ts_annual = dispatch_all_ts_annual.append(dispatch_all_gen_ts) #, ignore_index=True) # Add the day's dispatch to the annual result
        
        ## Annual dispatch for Nuclear and Hydro
        dispatch_annual_gen_hydro = np.around(dispatch_annual_gen_hydro + dispatch_non_fossil_ts[['HYDRO-STORAGE', 'HYDRO-ROR', 'HYDRO-PONDAGE']].sum(axis=1).sum(axis=0), decimals = 2) 
        dispatch_annual_gen_nuclear = np.around(dispatch_annual_gen_nuclear + dispatch_non_fossil_ts['NUCLEAR'].sum(axis=0), decimals = 2) 

        ## TOTAL DISPATCH COST - BOTTOM UP METHOD (in case specific timepoints need to be specified e.g. first 24 hours)
#        genALL_varCost_df = pd.DataFrame.from_dict([genALL_varCost], orient='columns')
#        dispatch_cost_ts = dispatch_all_ts * genALL_varCost_df.loc[0] # Cost of each generator for each timepoint
#        dispatch_cost_bottomup = dispatch_cost_ts.sum().sum() # specify only first 24 hours
        
        # Flag if dispatch cost from optimization problem and bottom up summation is different
#        if (abs((dispatch_cost- dispatch_cost_bottomup)/dispatch_cost) > 0.001):
#            print "ERROR: dispatch cost from optimization problem and bottom up summation is different."
        
        ## TOTAL DISPATCH COST
#        dispatch_cost_df = pd.DataFrame([[d, dispatch_cost_bottomup]], columns = ['Day', 'Dispatch_Cost'])
#        dispatch_cost_ts_annual = dispatch_cost_ts_annual.append(dispatch_cost_df, ignore_index=True) # Daily dispatch cost time series
#        dispatch_cost_annual = dispatch_cost_annual + dispatch_cost_bottomup # total annual dispatch cost for summary USED TO BE DISPATCH_COST FROM OPTIMIZATION OUTPUT
        
        ## PARAMETERS THAT NEED TO BE PASSED TO THE NEXT LOOP
        # Battery initial state of charge for the next loop. Information from the lookahead period.
        if battery_storage_flag != 0:
            storBATTERY_initial_soc = energy_stor_battery_ts['BAT-STORAGE'][energy_stor_battery_ts.index[-1]] / storBATTERY_input['stor_energy'][storBATTERY_input['type']=='bat_storage'].sum()
        
        ## Difference between daily hydro storage available energy and hydro actual dispatched energy
        genHYDRO_maxEnergy_available_dispatched_diff = genHYDRO_maxEnergy_all.loc[genHYDRO_maxEnergy_all['Day'] == d][genHYDRO].iloc[0,0] - dispatch_non_fossil_ts['HYDRO-STORAGE'].sum(axis=0)

    
    '''
    ###############################################
    ## PROCESSING ANNUAL RESULTS FOR SCENARIO #####
    ###############################################
    '''
    
    ## NEW CONVENTIONAL CAPACITY
#    new_capacity_coal = genCONV_toAdd['gen_capacity'][genCONV_toAdd['type']=='coal'].sum() 
#    new_capacity_gas_ccgt = genCONV_toAdd['gen_capacity'][genCONV_toAdd['type']=='gas_ccgt'].sum()
#    new_capacity_gas_ct = genCONV_toAdd['gen_capacity'][genCONV_toAdd['type']=='gas_ct'].sum()
    
    ## VRE installed capacity
    capacity_solarPV = genVRE_toAdd['solarPV'][0]
    capacity_wind = genVRE_toAdd['wind'][0]
    capacity_vre = genVRE_toAdd.sum(axis=1)[0]
    
    ## INPUT OR UNCURTAILED VRE GENERATION - POTENTIAL GENERATION
    potential_gen_solarPV = capacity_solarPV * genVRE_all['solarPV'].sum()
    potential_gen_wind = capacity_wind * genVRE_all['wind'].sum()
    potential_gen_vre = potential_gen_solarPV + potential_gen_wind
    
    ## BATTERY STORAGE
    battery_storage_capacity = storBATTERY_input['stor_capacity'][storBATTERY_input['type']=='bat_storage'].sum()
    battery_storage_energy = storBATTERY_input['stor_energy'][storBATTERY_input['type']=='bat_storage'].sum()
    
    ## SUMMARY TABLE
    scenarioSummary = pd.DataFrame([[scenarios[sc] + "_" + net_load_folder_suffix + scenario_suffix_operation, scenario_main, dispatch_annual_gen_all,
                                     potential_gen_vre_2, dispatch_annual_gen_vre, curtailment_annual_gen_vre, dispatch_annual_gen_solarPV, dispatch_annual_gen_wind,
                                     dispatch_annual_gen_hydro, dispatch_annual_gen_nuclear,
                                     discharge_annual_stor_battery, charge_annual_stor_battery,
                                     capacity_vre, capacity_solarPV, capacity_wind, 
                                     battery_storage_capacity, battery_storage_energy, strftime("%Y-%m-%d %H:%M:%S")]], 
                                        columns = ["scenario", "scenario_main", "ann_gen_total_MWh", 
                                        "ann_gen_vre_nocurt_MWh", "ann_gen_vre_MWh", "ann_curt_vre_MWh", "ann_gen_solarPV_MWh", "ann_gen_wind_MWh",
                                        "ann_gen_hydro_MWh", "ann_gen_nuclear_MWh",
                                        "ann_discharge_bat_storage_MWh", "ann_charge_bat_storage_MWh",
                                        "capacity_vre_MW", "capacity_solarPV_MW", "capacity_wind_MW", 
                                        "capacity_bat_storage_MW", "energy_bat_storage_MWh", "date_time"])
    


    ## ADD SCENARIO RESULTS TO ALL SCENARIO RESULTS TABLE 
    # If the scenario exists, then overwrite the results of the old scenario, else append
    
    if any(results_all_scenarios.scenario == scenarios[sc] + "_" + net_load_folder_suffix + scenario_suffix_operation):
        print "Deleted scenario's old result"
        results_all_scenarios = results_all_scenarios[results_all_scenarios.scenario != scenarios[sc] + "_" + net_load_folder_suffix + scenario_suffix_operation]
        
    results_all_scenarios = results_all_scenarios.append(scenarioSummary)
    
    ## WRITING THE RESULTS OUT TO A CSV
    results_all_scenarios.to_csv(outputPath + results_all_scenarios_csv, sep=',', index = False) # write out all existing and new results
    
    ## WRITING THE DISPATCH TABLE TO CSV
    dispatch_all_ts_annual.to_csv(outputPathDispatch + "nonlinear_dispatch_all_gen_" + scenarios[sc] + "_" + net_load_folder_suffix + scenario_suffix_operation + ".csv", sep=',', index = True)
    
    ## WRITING THE NET NET LOAD TIME SERIES TO CSV - TO BE USED IN SCREENING CURVES CAPACITY EXPANSION SCRIPT
    netnetload_ts_annual.columns = ['net_load_final']
    netnetload_ts_annual = pd.concat([netnetload_ts_annual, dispatch_all_ts_annual[['HYDRO-STORAGE', 'Bat-Storage-Discharge', 'Bat-Storage-Charge']]], axis=1)
    netnetload_ts_annual.index.names = ['Timepoint']
    netnetload_ts_annual.to_csv(outputPathNetLoad + "net_load_hydro_" + scenarios[sc] + "_" + net_load_folder_suffix + scenario_suffix_operation + ".csv", sep=',', index = True)

elapsed_time = (time.time() - start_time)/(60)
print str(elapsed_time) + " minutes"

