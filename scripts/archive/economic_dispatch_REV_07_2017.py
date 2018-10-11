"""
Author: Ranjit Deshmukh
"""

## v4: This version was after print statement was removed. 
## v5: Adding outage rates to the Ed. Basically derating both min and max dispatch levels for coal, gas, and other
## v6: Adding diesel, ct, and ccgt
## v7: Adding suffix for scenario - in this case for "_allCoal"; Also added scenario_suffix_operations in case of changes to how the ED works e.g. 70% min gen.

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
### IMPORTING DATA

myPath = "C:\\Users\\akjohnson\\Desktop\\Ranjit\\"
inputPath = myPath + "india_ED\\india_ED_input\\"
inputPathVRE_gen_profiles = myPath + "india_ED\\india_ED_input\\REvalue_gen_profiles\\"
inputPathVRE_capacity = myPath + "india_ED\\india_ED_input\\REvalue_capacity\\"
inputPathNEWCONV_capacity = myPath + "india_ED\\india_ED_input\\new_conventional_capacity\\"
outputPath = myPath + "india_ED\\india_ED_output\\"
results_all_scenarios_csv = "results_all_scenarios.csv"

# Ana note: for Mac, will probably work on Windows
# inputPath = os.path.join(os.getcwd(), "india_ED_input/")
# inputPathVRE = os.path.join(os.getcwd(), "india_ED_input/")

yearAnalysis = 2030
yearBase = 2014
load_csv = "load" + str(yearAnalysis) + "_19EPS" + ".csv" # Load CSV
genALL_input_csv = "gen_all_input_cc_ccgt_diesel.csv" # generator csv with var cost and max capacity for all generators
#genVRE_csv = "vre_gen.csv" # variable RE generator csv with dispatch capacity factors
genHYDRO_minGen_csv = "hydro_min_gen.csv"
genHYDRO_maxEnergy_csv = "hydro_max_energy.csv"
genMUSTRUN_csv = "mustrun_gen.csv" # must run generators like nuclear and run-of-river hydro that have a constant output through the timeseries (e.g. day)
VoLL = 100000 # Value of lost load
all_new_coal = 'no' # Set this flag to 'yes', if you want all coal capacity ELSE set it "to blank "no".
coal_low_cap_cost = 'yes'
outage_rate_coal = 0.1 # These outage rates need to match the ones from the conventional buildout algorithm
outage_rate_gas_ct = 0.1 # These outage rates need to match the ones from the conventional buildout algorithm
outage_rate_gas_ccgt = 0.1 # These outage rates need to match the ones from the conventional buildout algorithm
outage_rate_diesel = 0.2 # These outage rates need to match the ones from the conventional buildout algorithm
outage_rate_other = 0.3 # These outage rates need to match the ones from the conventional buildout algorithm
derating_outages_conventional_gen = "yes"
days_to_run = 365

'''
#############################################
## INPUTS ##
#############################################
'''
scenarios = ["S0W0", "S0W200", "S50W150", "S100W100", "S150W50", "S200W0", "S0W300", "S75W225", "S150W150", "S225W75", "S300W0", "S0W400", "S100W300", "S200W200", "S300W100", "S400W0"]#, "S300W100"] # List of VRE scenarios ["S0W200", "S50W150", "S100W100", "S150W50", "S200W0"]

## Load
load = pd.read_csv(inputPath + load_csv, sep=',')

## Read the VRE capacities for all scenarios
# genVRE_allScenarios = pd.read_csv(inputPathVRE + str(yearBase) + "_RE_capacity_all_scenarios" + ".csv", sep=',')

'''
#############################################
## OUTPUTS ##
#############################################
'''
# Create output folder if path does not exist
if not os.path.exists(outputPath):
    os.makedirs(outputPath)

# Read the ALL SCENARIO RESULTS file, or if it does not exist, create output dataframe 
if os.path.exists(outputPath + results_all_scenarios_csv):
    results_all_scenarios = pd.read_csv(outputPath + results_all_scenarios_csv, sep=',')
else:
    results_all_scenarios = pd.DataFrame(columns = ["scenario", "dispatch_cost", "ann_gen_total_MWh", 
                                        "ann_gen_vre_MWh", "ann_gen_solarPV_MWh", "ann_gen_wind_MWh",
                                        "ann_gen_vre_nocurt_MWh", "ann_gen_coal_MWh", "ann_gen_gas_ccgt_MWh", "ann_gen_gas_ct_MWh", 
                                        "ann_gen_diesel_MWh", "ann_gen_other_MWh", "ann_gen_hydro_MWh", "ann_gen_nuclear_MWh",
                                        "new_capacity_coal_MW", "new_capacity_gas_ccgt_MW", "new_capacity_gas_ct_MW", 
                                        "capacity_vre_MW", "capacity_solarPV_MW", "capacity_wind_MW", "date_time"])


'''
#############################################
## SCENARIOS IN A LOOP ##
#############################################
'''
# Set the scenario suffix. Applied to new conventional capacity input and output scenario
if all_new_coal == 'yes':
    scenario_suffix = '_allCoal'
elif coal_low_cap_cost == 'yes':
    scenario_suffix = '_lowCapCostCoal'
else:
    scenario_suffix = ''

scenario_suffix_operation = "_70min"    
    
for sc in range(len(scenarios)):
    
    # Read the results csv file again if there are more than one scenario. Write the output results after each loop, so run is saved even if computer crashes    
    if sc > 0:
        results_all_scenarios = pd.read_csv(outputPath + results_all_scenarios_csv, sep=',')
    
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
    
    ## NEW CONVENTIONAL GENERATOR BUILDOUT
    genCONV_toAdd = pd.read_csv(inputPathNEWCONV_capacity + str(yearAnalysis) + "_conventional_capacity_" + scenarios[sc] + scenario_suffix + ".csv", sep=',')
    
    ## All generators
    genALL_input = pd.read_csv(inputPath + genALL_input_csv, sep=',')
    # Add the VRE generators for the current scenario
    genALL_input = genALL_input.append(genVRE_toAdd_melt, ignore_index=True)
    # Add the new conventional generators for the current scenario
    genALL_input = genALL_input.append(genCONV_toAdd, ignore_index=True)
    
    ## Derating conventional generator capacity based on outage rates
    if derating_outages_conventional_gen == "yes":
        genALL_input.ix[genALL_input.type == 'coal', 'gen_capacity'] = genALL_input.ix[genALL_input.type == 'coal', 'gen_capacity'] * (1-outage_rate_coal)
        genALL_input.ix[genALL_input.type == 'gas_ccgt', 'gen_capacity'] = genALL_input.ix[genALL_input.type == 'gas_ccgt', 'gen_capacity'] * (1-outage_rate_gas_ccgt)
        genALL_input.ix[genALL_input.type == 'other', 'gen_capacity'] = genALL_input.ix[genALL_input.type == 'other', 'gen_capacity'] * (1-outage_rate_other)
        genALL_input.ix[genALL_input.type == 'gas_ct', 'gen_capacity'] = genALL_input.ix[genALL_input.type == 'gas_ct', 'gen_capacity'] * (1-outage_rate_gas_ct)
        genALL_input.ix[genALL_input.type == 'diesel', 'gen_capacity'] = genALL_input.ix[genALL_input.type == 'diesel', 'gen_capacity'] * (1-outage_rate_diesel)
    
    #Initialize the generator list. This is used for the generator index.
    genALL  = genALL_input['generator'].tolist() 
    # Variable cost
    genALL_varCost = genALL_input[['generator', 'var_cost']]
    genALL_varCost.set_index('generator', inplace=True)
    genALL_varCost = genALL_varCost.to_dict()['var_cost']
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
    # Capacity - # coal generators
    genCOAL_genCapacity = genALL_input[genALL_input['type'].isin(['coal'])][['generator', 'gen_capacity']]
    genCOAL_genCapacity.set_index('generator', inplace=True)
    genCOAL_genCapacity = genCOAL_genCapacity.to_dict()['gen_capacity']
    # Capacity - # ccgt gas generators
    genGASCCGT_genCapacity = genALL_input[genALL_input['type'].isin(['gas_ccgt'])][['generator', 'gen_capacity']]
    genGASCCGT_genCapacity.set_index('generator', inplace=True)
    genGASCCGT_genCapacity = genGASCCGT_genCapacity.to_dict()['gen_capacity']
    # Capacity - # ct gas generators
    genGASCT_genCapacity = genALL_input[genALL_input['type'].isin(['gas_ct'])][['generator', 'gen_capacity']]
    genGASCT_genCapacity.set_index('generator', inplace=True)
    genGASCT_genCapacity = genGASCT_genCapacity.to_dict()['gen_capacity']
    # Capacity - # ccgt gas generators
    genDIESEL_genCapacity = genALL_input[genALL_input['type'].isin(['diesel'])][['generator', 'gen_capacity']]
    genDIESEL_genCapacity.set_index('generator', inplace=True)
    genDIESEL_genCapacity = genDIESEL_genCapacity.to_dict()['gen_capacity']
    # Capacity - # other generators - These include biomass cogeneration and diesel
    genOTHER_genCapacity = genALL_input[genALL_input['type'].isin(['other'])][['generator', 'gen_capacity']]
    genOTHER_genCapacity.set_index('generator', inplace=True)
    genOTHER_genCapacity = genOTHER_genCapacity.to_dict()['gen_capacity']
    # Capacity - # must run generators
    genMUSTRUN_genCapacity = genALL_input[genALL_input['type'].isin(['mustrun'])][['generator', 'gen_capacity']]
    genMUSTRUN_genCapacity.set_index('generator', inplace=True)
    genMUSTRUN_genCapacity = genMUSTRUN_genCapacity.to_dict()['gen_capacity']
    
    ## Hydro generation
    genHYDRO_minGen_all = pd.read_csv(inputPath + genHYDRO_minGen_csv, sep=',')
    genHYDRO_maxEnergy_all = pd.read_csv(inputPath + genHYDRO_maxEnergy_csv, sep=',')
    
    ## Must Run generation
    genMUSTRUN_all = pd.read_csv(inputPath + genMUSTRUN_csv, sep=',')
    
    ## Thermal and other dispatchable generators (other than hydro)
    genCOAL = genALL_input[genALL_input['type'].isin(['coal'])]['generator'].tolist()
    genGASCCGT = genALL_input[genALL_input['type'].isin(['gas_ccgt'])]['generator'].tolist()
    genOTHER = genALL_input[genALL_input['type'].isin(['other'])]['generator'].tolist()
    genGASCT = genALL_input[genALL_input['type'].isin(['gas_ct'])]['generator'].tolist()
    genDIESEL = genALL_input[genALL_input['type'].isin(['diesel'])]['generator'].tolist()
    
    
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
    dispatch_annual_gen_coal = 0
    dispatch_annual_gen_gas_ccgt = 0
    dispatch_annual_gen_other = 0
    dispatch_annual_gen_gas_ct = 0
    dispatch_annual_gen_diesel = 0
    dispatch_annual_gen_hydro = 0
    dispatch_annual_gen_nuclear = 0
    dispatch_cost_annual = 0
    
    #d = 1
    for d in range(1, days_to_run + 1):
        print "Solving step " + str(d)
        ## Load
        load_ts = load.loc[load['Day'] == d]
        timepoints = load_ts['Timepoint'].tolist() # Initialize the timepoints from the load data frame. This is used for the Timepoint index
        load_ts.drop(['Day', 'dateTime'], axis=1, inplace=True) # Keep only the timepoint and load columns
        load_ts.set_index('Timepoint', inplace=True)
        load_ts = load_ts.round(0)
        load_ts = load_ts.to_dict()['load']
        
        ## Hydro generation and energy limit
        # Min Gen
        genHYDRO_minGen_cf_ts = genHYDRO_minGen_all.loc[genHYDRO_minGen_all['Day'] == d]
        genHYDRO_minGen_cf_ts.drop(['Day'], axis=1, inplace=True)
        genHYDRO = genHYDRO_minGen_cf_ts.columns.values.tolist()
        genHYDRO_minGen_cf_ts = genHYDRO_minGen_cf_ts.T.to_dict().itervalues().next() # Take the transpose of the dataframe (in this case one row), then convert to dictionary, and take the next level using itervalues because the index will keep changing in the loop
        # Max Energy
        genHYDRO_maxEnergy_ts = genHYDRO_maxEnergy_all.loc[genHYDRO_maxEnergy_all['Day'] == d]
        genHYDRO_maxEnergy_ts.drop(['Day'], axis=1, inplace=True)
        genHYDRO_maxEnergy_ts = genHYDRO_maxEnergy_ts.T.to_dict().itervalues().next() # Take the transpose of the dataframe (in this case one row), then convert to dictionary, and take the next level using itervalues because the index will keep changing in the loop
        
        
        ## VRE generation
        genVRE_cf_ts = genVRE_all.loc[genVRE_all['Day'] == d] # Select all the rows for a particular day in the loop
        genVRE_cf_ts.drop(['Day', 'dateTime'], axis=1, inplace=True) # Drop all columns except timepoints and VRE generators
        genVRE = genVRE_cf_ts.columns.values[1:].tolist() # List of VRE generators
        genVRE_cf_ts_melt = pd.melt(genVRE_cf_ts, id_vars=['Timepoint'], value_vars=genVRE) # Melt the table with Timepoints and VRE generators
        genVRE_cf_ts_melt.set_index(['Timepoint', 'variable'], inplace = True)
        genVRE_cf_ts= genVRE_cf_ts_melt.to_dict()["value"]
        
        
        ## Must run generation
        genMUSTRUN_cf_ts = genMUSTRUN_all.loc[genMUSTRUN_all['Day'] == d]
        genMUSTRUN_cf_ts.drop(['Day'], axis=1, inplace=True)
        genMUSTRUN = genMUSTRUN_cf_ts.columns.values.tolist()
        genMUSTRUN_cf_ts = genMUSTRUN_cf_ts.T.to_dict().itervalues().next() # Take the transpose of the dataframe (in this case one row), then convert to dictionary, and take the next level using itervalues because the index will keep changing in the loop
        
        ## TEST CODE
        '''
        timePoints = list(range(1,5))
        generators = ["Coal1", "Coal2", "PV1", "Wind1", "Hydro1"]
        generatorsHydro = ["Hydro1"]
        load = {1: 60, 2:30, 3:40, 4:20}
        varCost = {"Coal1":2, "Coal2":3, "PV1":0, "Wind1":0, "Hydro1":0}
        genCapacity = {"Coal1":15, "Coal2":50, "PV1":20, "Wind1":10, "Hydro1":20}
        genMinLevel = {"Coal1":5, "Coal2":5, "PV1":0, "Wind1":0, "Hydro1":5} # don't need vre min gen because dispatch is within nonnegativereals. Only hydro and thermal min gen req.
        hydroEnergy = {"Hydro1":30}
        #genVRE = {(1,"PV1"):3, (2,"PV1"):4, (3,"PV1"): 8, (4,"PV1"): 10}
        '''
        
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
        model.GEN = Set(initialize=genALL) # Initialize with the list of all generators
        model.GENHYDRO = Set(within=model.GEN, initialize=genHYDRO) # Initialize with the HYDRO generator list ## Can use initialize=set(proj for proj in hydroEnergy)
        model.GENVRE = Set(within=model.GEN, initialize=genVRE) # Initialize with the VRE generator list
        model.GENMUSTRUN = Set(within=model.GEN, initialize=genMUSTRUN) # Initialize with the MUST RUN generator list
        model.GENCOAL = Set(within=model.GEN, initialize=genCOAL) # Initialize with the COAL generator list
        model.GENGASCCGT = Set(within=model.GEN, initialize=genGASCCGT) # Initialize with the GAS CCGT generator list
        model.GENOTHER = Set(within=model.GEN, initialize=genOTHER) # Initialize with the OTHER generator list
        model.GENGASCT = Set(within=model.GEN, initialize=genGASCT) # Initialize with the GAS CT generator list
        model.GENDIESEL = Set(within=model.GEN, initialize=genDIESEL) # Initialize with the DIESEL generator list
        
        
        '''
        #############################################
        ## PARAMETERS ##
        #############################################
        '''
        
        genCOAL_minGen_cf = 0.70 ## For now, the entire COAL fleet will have the same minimum CF, derated by the outage rate.
        genGASCCGT_minGen_cf = 0.50 ## For now, the entire CCGT GAS fleet will have the same minimum CF.
        genOTHER_minGen_cf = 0.70 ## For now, the entire OTHER fleet will have the same minimum CF.
        genGASCT_minGen_cf = 0.50 ## For now, the entire CT GAS fleet will have the same minimum CF. - not used
        genDIESEL_minGen_cf = 0.50 ## For now, the entire DIESEL fleet will have the same minimum CF. - not used
        
        model.load_mw = Param(model.TIMEPOINTS, default=load_ts)
        model.dispatch_cost = Param(model.GEN, default=genALL_varCost, doc="dispatch cost in $/MWh")
        model.gen_capacity = Param(model.GEN, default=genALL_genCapacity)
        model.gen_min_cf_hydro = Param(model.GENHYDRO, default=genHYDRO_minGen_cf_ts) # Minimum generation hydro
        model.gen_energy_hydro = Param(model.GENHYDRO, default=genHYDRO_maxEnergy_ts) # Max energy hydro
        model.gen_cf_vre = Param(model.TIMEPOINTS, model.GENVRE, default=genVRE_cf_ts) # Capacity factors for VRE
        model.gen_cf_mustrun = Param(model.GENMUSTRUN, default=genMUSTRUN_cf_ts) # Capacity factors for MUST RUN
        model.gen_min_cf_coal = Param(model.GENCOAL, default=genCOAL_minGen_cf) # Minimum generation coal
        model.gen_min_cf_gas_ccgt = Param(model.GENGASCCGT, default=genGASCCGT_minGen_cf) # Minimum generation gas ccgt
        model.gen_min_cf_other = Param(model.GENOTHER, default=genOTHER_minGen_cf) # Minimum generation other
        model.gen_min_cf_gas_ct = Param(model.GENGASCT, default=genGASCT_minGen_cf) # Minimum generation gas ct - not used
        model.gen_min_cf_gas_diesel = Param(model.GENDIESEL, default=genDIESEL_minGen_cf) # Minimum generation diesel - not used
        
        '''
        #############################################
        ## DECISION VARIABLES ##
        #############################################
        '''
        
        model.DispatchMW = Var(model.TIMEPOINTS, model.GEN, within=NonNegativeReals)
        model.uc = Var(model.GEN, within=Binary)
        model.load_unserved_mw = Var(model.TIMEPOINTS, within=NonNegativeReals)
        
        '''
        #############################################
        ## OBJECTIVE FUNCTION ##
        #############################################
        '''
        
        def Dispatch_Cost_rule(mod):
            return (
                sum(
                    mod.dispatch_cost[gen] * mod.DispatchMW[t, gen]
                    for gen in mod.GEN
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
            return sum(mod.DispatchMW[t, gen] for gen in mod.GEN) == mod.load_mw[t] - mod.load_unserved_mw[t]
        model.Conservation_Of_Energy = Constraint(model.TIMEPOINTS, rule=Conservation_Of_Energy_rule)
        
        # Dispatch <= max capacity of each generator
        def Enforce_Max_Dispatch_Limit_rule(mod, t, gen):
            return mod.DispatchMW[t, gen] <= mod.gen_capacity[gen] * mod.uc[gen]
        model.Enforce_Max_Dispatch_Limit = Constraint(model.TIMEPOINTS, model.GEN, rule=Enforce_Max_Dispatch_Limit_rule)
        
        # Dispatch >= min generation level for COAL (include unit commitment constraint)
        def Enforce_Min_Dispatch_Limit_Coal_rule(mod, t, gen):
            return mod.DispatchMW[t, gen] >= mod.gen_min_cf_coal[gen] * mod.gen_capacity[gen] * mod.uc[gen]
        model.Enforce_Min_Dispatch_Limit_Coal = Constraint(model.TIMEPOINTS, model.GENCOAL, rule=Enforce_Min_Dispatch_Limit_Coal_rule)
        
        # Dispatch >= min generation level for GAS CCGT (include unit commitment constraint)
        def Enforce_Min_Dispatch_Limit_GasCCGT_rule(mod, t, gen):
            return mod.DispatchMW[t, gen] >= mod.gen_min_cf_gas_ccgt[gen] * mod.gen_capacity[gen] * mod.uc[gen]
        model.Enforce_Min_Dispatch_Limit_GasCCGT = Constraint(model.TIMEPOINTS, model.GENGASCCGT, rule=Enforce_Min_Dispatch_Limit_GasCCGT_rule)
        
        # Dispatch >= min generation level for OTHER (include unit commitment constraint)
        def Enforce_Min_Dispatch_Limit_Other_rule(mod, t, gen):
            return mod.DispatchMW[t, gen] >= mod.gen_min_cf_other[gen] * mod.gen_capacity[gen] * mod.uc[gen]
        model.Enforce_Min_Dispatch_Limit_Other = Constraint(model.TIMEPOINTS, model.GENOTHER, rule=Enforce_Min_Dispatch_Limit_Other_rule)
        
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
            return mod.DispatchMW[t, gen] >= mod.gen_min_cf_hydro[gen] * mod.gen_capacity[gen] * mod.uc[gen]
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
        print "Total dispatch cost: INR " + str(dispatch_cost)
        
        # model_instance.solutions.load_from(results)
        
        '''
        #############################################
        ## RESULTS PROCESSING ##
        #############################################
        '''
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
        
        ## DISPATCH OF COAL GENERATORS
        dispatch_coal_ts = pd.DataFrame(index = timepoints)
        for p in genCOAL:
            dispatch_ts_gen = [model_instance.DispatchMW[t,p].value for t in timepoints]
            dispatch_ts_gen_df = pd.DataFrame({p : dispatch_ts_gen}, index = timepoints)
            # dispatch_ts = dispatch_ts.concat(dispatch_ts_gen_df, axis = 1)
            dispatch_coal_ts[p] = dispatch_ts_gen_df
        dispatch_annual_gen_coal = np.around(dispatch_annual_gen_coal + dispatch_coal_ts.sum(axis=1).sum(axis=0), decimals = 2) # Total annual generation
        
        ## DISPATCH OF GAS CCGT GENERATORS
        dispatch_gas_ccgt_ts = pd.DataFrame(index = timepoints)
        for p in genGASCCGT:
            dispatch_ts_gen = [model_instance.DispatchMW[t,p].value for t in timepoints]
            dispatch_ts_gen_df = pd.DataFrame({p : dispatch_ts_gen}, index = timepoints)
            # dispatch_ts = dispatch_ts.concat(dispatch_ts_gen_df, axis = 1)
            dispatch_gas_ccgt_ts[p] = dispatch_ts_gen_df
        dispatch_annual_gen_gas_ccgt = np.around(dispatch_annual_gen_gas_ccgt + dispatch_gas_ccgt_ts.sum(axis=1).sum(axis=0), decimals = 2) # Total annual generation
        
        ## DISPATCH OF GAS CT GENERATORS
        dispatch_gas_ct_ts = pd.DataFrame(index = timepoints)
        for p in genGASCT:
            dispatch_ts_gen = [model_instance.DispatchMW[t,p].value for t in timepoints]
            dispatch_ts_gen_df = pd.DataFrame({p : dispatch_ts_gen}, index = timepoints)
            # dispatch_ts = dispatch_ts.concat(dispatch_ts_gen_df, axis = 1)
            dispatch_gas_ct_ts[p] = dispatch_ts_gen_df
        dispatch_annual_gen_gas_ct = np.around(dispatch_annual_gen_gas_ct + dispatch_gas_ct_ts.sum(axis=1).sum(axis=0), decimals = 2) # Total annual generation
        
        ## DISPATCH OF GAS DIESEL GENERATORS
        dispatch_diesel_ts = pd.DataFrame(index = timepoints)
        for p in genDIESEL:
            dispatch_ts_gen = [model_instance.DispatchMW[t,p].value for t in timepoints]
            dispatch_ts_gen_df = pd.DataFrame({p : dispatch_ts_gen}, index = timepoints)
            # dispatch_ts = dispatch_ts.concat(dispatch_ts_gen_df, axis = 1)
            dispatch_diesel_ts[p] = dispatch_ts_gen_df
        dispatch_annual_gen_diesel = np.around(dispatch_annual_gen_diesel + dispatch_diesel_ts.sum(axis=1).sum(axis=0), decimals = 2) # Total annual generation
        
        ## DISPATCH OF OTHER GENERATORS
        dispatch_other_ts = pd.DataFrame(index = timepoints)
        for p in genOTHER:
            dispatch_ts_gen = [model_instance.DispatchMW[t,p].value for t in timepoints]
            dispatch_ts_gen_df = pd.DataFrame({p : dispatch_ts_gen}, index = timepoints)
            # dispatch_ts = dispatch_ts.concat(dispatch_ts_gen_df, axis = 1)
            dispatch_other_ts[p] = dispatch_ts_gen_df
        #dispatch_all_ts_annual = dispatch_all_ts_annual.append(dispatch_all_ts, ignore_index=True) # Add the day's dispatch to the annual result
        dispatch_annual_gen_other = np.around(dispatch_annual_gen_other + dispatch_other_ts.sum(axis=1).sum(axis=0), decimals = 2) # Total annual generation
        
        ## SUMMARY DISPATCH OF ALL GENERATORS FOR THE DAY
        non_fossil_generators = ["HYDRO-STORAGE", "HYDRO-ROR", "NUCLEAR", "HYDRO-PONDAGE", "solarPV", "wind"]
        dispatch_non_fossil_ts = dispatch_all_ts[non_fossil_generators] # Choose all the non-fossil generator columns
        dispatch_all_coal_ts= pd.DataFrame(dispatch_coal_ts.sum(axis=1), columns = ['Coal'])
        dispatch_all_gas_ccgt_ts = pd.DataFrame(dispatch_gas_ccgt_ts.sum(axis=1), columns = ['Gas-CCGT'])
        dispatch_all_gas_ct_ts = pd.DataFrame(dispatch_gas_ct_ts.sum(axis=1), columns = ['Gas-CT'])
        dispatch_all_diesel_ts = pd.DataFrame(dispatch_diesel_ts.sum(axis=1), columns = ['Diesel'])
        dispatch_all_other_ts = pd.DataFrame(dispatch_other_ts.sum(axis=1), columns = ['Other'])
        dispatch_all_gen_ts = pd.concat([dispatch_non_fossil_ts, dispatch_all_coal_ts, dispatch_all_gas_ccgt_ts, dispatch_all_gas_ct_ts, dispatch_all_diesel_ts, dispatch_all_other_ts], axis = 1)
        
        dispatch_all_ts_annual = dispatch_all_ts_annual.append(dispatch_all_gen_ts, ignore_index=True) # Add the day's dispatch to the annual result
        
        ## Annual dispatch for Nuclear and Hydro
        dispatch_annual_gen_hydro = np.around(dispatch_annual_gen_hydro + dispatch_non_fossil_ts[['HYDRO-STORAGE', 'HYDRO-ROR', 'HYDRO-PONDAGE']].sum(axis=1).sum(axis=0), decimals = 2) 
        dispatch_annual_gen_nuclear = np.around(dispatch_annual_gen_nuclear + dispatch_non_fossil_ts['NUCLEAR'].sum(axis=0), decimals = 2) 
        
        ## TOTAL DISPATCH COST
        dispatch_cost_df = pd.DataFrame([[d, dispatch_cost]], columns = ['Day', 'Dispatch_Cost'])
        dispatch_cost_ts_annual = dispatch_cost_ts_annual.append(dispatch_cost_df, ignore_index=True) # Daily dispatch cost time series
        dispatch_cost_annual = dispatch_cost_annual + dispatch_cost # total annual dispatch cost for summary
        
    
    '''
    ###############################################
    ## PROCESSING ANNUAL RESULTS FOR SCENARIO #####
    ###############################################
    '''
    
    ## NEW CONVENTIONAL CAPACITY
    new_capacity_coal = genCONV_toAdd['gen_capacity'][genCONV_toAdd['type']=='coal'].sum() 
    new_capacity_gas_ccgt = genCONV_toAdd['gen_capacity'][genCONV_toAdd['type']=='gas_ccgt'].sum()
    new_capacity_gas_ct = genCONV_toAdd['gen_capacity'][genCONV_toAdd['type']=='gas_ct'].sum()
    
    ## VRE installed capacity
    capacity_solarPV = genVRE_toAdd['solarPV'][0]
    capacity_wind = genVRE_toAdd['wind'][0]
    capacity_vre = genVRE_toAdd.sum(axis=1)[0]
    
    ## INPUT OR UNCURTAILED VRE GENERATION - POTENTIAL GENERATION
    potential_gen_solarPV = capacity_solarPV * genVRE_all['solarPV'].sum()
    potential_gen_wind = capacity_wind * genVRE_all['wind'].sum()
    potential_gen_vre = potential_gen_solarPV + potential_gen_wind
    
    ## SUMMARY TABLE
    scenarioSummary = pd.DataFrame([[scenarios[sc] + scenario_suffix + scenario_suffix_operation, dispatch_cost_annual, dispatch_annual_gen_all,
                                     potential_gen_vre, dispatch_annual_gen_vre, dispatch_annual_gen_solarPV, dispatch_annual_gen_wind,
                                      dispatch_annual_gen_coal, dispatch_annual_gen_gas_ccgt, dispatch_annual_gen_gas_ct,
                                     dispatch_annual_gen_diesel, dispatch_annual_gen_other, dispatch_annual_gen_hydro, dispatch_annual_gen_nuclear,
                                     new_capacity_coal, new_capacity_gas_ccgt, new_capacity_gas_ct, 
                                     capacity_vre, capacity_solarPV, capacity_wind, strftime("%Y-%m-%d %H:%M:%S")]], 
                                        columns = ["scenario", "dispatch_cost", "ann_gen_total_MWh", 
                                        "ann_gen_vre_nocurt_MWh", "ann_gen_vre_MWh", "ann_gen_solarPV_MWh", "ann_gen_wind_MWh",
                                        "ann_gen_coal_MWh", "ann_gen_gas_ccgt_MWh", "ann_gen_gas_ct_MWh", 
                                        "ann_gen_diesel_MWh", "ann_gen_other_MWh", "ann_gen_hydro_MWh", "ann_gen_nuclear_MWh",
                                        "new_capacity_coal_MW", "new_capacity_gas_ccgt_MW", "new_capacity_gas_ct_MW", 
                                        "capacity_vre_MW", "capacity_solarPV_MW", "capacity_wind_MW", "date_time"])
    


    ## ADD SCENARIO RESULTS TO ALL SCENARIO RESULTS TABLE 
    # If the scenario exists, then overwrite the results of the old scenario, else append
    
    if any(results_all_scenarios.scenario == scenarios[sc] + scenario_suffix + scenario_suffix_operation):
        print "Deleted scenario's old result"
        results_all_scenarios = results_all_scenarios[results_all_scenarios.scenario != scenarios[sc] + scenario_suffix + scenario_suffix_operation]
        
    results_all_scenarios = results_all_scenarios.append(scenarioSummary)
    
    ## WRITING THE RESULTS OUT TO A CSV
    results_all_scenarios.to_csv(outputPath + results_all_scenarios_csv, sep=',', index = False) # write out all existing and new results
    
    ## WRITING THE DISPATCH TABLE TO CSV
    dispatch_all_ts_annual.to_csv(outputPath + "dispatch_all_gen_" + scenarios[sc] + scenario_suffix + scenario_suffix_operation + ".csv", sep=',', index = True)

elapsed_time = (time.time() - start_time)/(60)
print str(elapsed_time) + " minutes"

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