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
opt = SolverFactory("glpk")


'''
#############################################
## IMPORTING DATA - CSVS AND PATH ##
#############################################
'''
### IMPORTING DATA
inputPath = "G:\\SWITCH\\india_ED\\test_data\\" 
load_csv = "load.csv" # Load CSV
genALL_input_csv = "gen_all_input.csv" # generator csv with var cost and max capacity for all generators
genVRE_csv = "vre_gen.csv" # variable RE generator csv with dispatch capacity factors
genHYDRO_minGen_csv = "hydro_min_gen.csv"
genHYDRO_maxEnergy_csv = "hydro_max_energy.csv"
genMUSTRUN_csv = "mustrun_gen.csv" # must run generators like nuclear and run-of-river hydro that have a constant output through the timeseries (e.g. day)


'''
#############################################
## INPUTS ##
#############################################
'''
## Load
load = pd.read_csv(inputPath + load_csv, sep=',')

## All generators
genALL_input = pd.read_csv(inputPath + genALL_input_csv, sep=',')
genALL  = genALL_input['generator'].tolist() #Initialize the generator list. This is used for the generator index. 
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
# Capacity - # thermal generators
genTHERM_genCapacity = genALL_input[genALL_input['type'].isin(['coal'])][['generator', 'gen_capacity']] 
genTHERM_genCapacity.set_index('generator', inplace=True)
genTHERM_genCapacity = genTHERM_genCapacity.to_dict()['gen_capacity']
# Capacity - # must run generators
genMUSTRUN_genCapacity = genALL_input[genALL_input['type'].isin(['mustrun'])][['generator', 'gen_capacity']] 
genMUSTRUN_genCapacity.set_index('generator', inplace=True)
genMUSTRUN_genCapacity = genMUSTRUN_genCapacity.to_dict()['gen_capacity']

## Hydro generation
genHYDRO_minGen_all = pd.read_csv(inputPath + genHYDRO_minGen_csv, sep=',')
genHYDRO_maxEnergy_all = pd.read_csv(inputPath + genHYDRO_maxEnergy_csv, sep=',')

## VRE generation
genVRE_all = pd.read_csv(inputPath + genVRE_csv, sep=',')

## Must Run generation
genMUSTRUN_all = pd.read_csv(inputPath + genMUSTRUN_csv, sep=',')

## Thermal and other dispatchable generators (other than hydro)
genTHERMAL = genALL_input[genALL_input['type'].isin(['coal'])]['generator'].tolist()

'''
#############################################
## INPUT DATA IN A LOOP ##
#############################################
'''

d = 1
## Load
load_ts = load.loc[load['Day'] == d]
timepoints = load_ts['Timepoint'].tolist() # Initialize the timepoints from the load data frame. This is used for the Timepoint index
load_ts.drop(['Day'], axis=1, inplace=True) # Keep only the timepoint and load columns
load_ts.set_index('Timepoint', inplace=True)
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
genVRE_cf_ts.drop(['Day'], axis=1, inplace=True) # Drop all columns except timepoints and VRE generators
genVRE = genVRE_cf_ts.columns.values[1:].tolist()
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
model.GENDISPATCH = model.TIMEPOINTS * model.GEN
model.GENHYDRO = Set(within=model.GEN, initialize=genHYDRO) # Initialize with the HYDRO generator list ## Can use initialize=set(proj for proj in hydroEnergy)
model.GENHYDRODISPATCH = model.TIMEPOINTS * model.GENHYDRO
model.GENVRE = Set(within=model.GEN, initialize=genVRE) # Initialize with the VRE generator list
model.GENVREDISPATCH = model.TIMEPOINTS * model.GENVRE
model.GENMUSTRUN = Set(within=model.GEN, initialize=genMUSTRUN) # Initialize with the MUST RUN generator list
model.GENMUSTRUNDISPATCH = model.TIMEPOINTS * model.GENMUSTRUN
model.GENTHERMAL = Set(within=model.GEN, initialize=genTHERMAL) # Initialize with the THERMAL generator list
model.GENTHERMALDISPATCH = model.TIMEPOINTS * model.GENTHERMAL


'''
#############################################
## PARAMETERS ##
#############################################
'''

genTHERMAL_minGen_cf = 0.55 ## For now, the entire fleet will have the same minimum CF. Later, split this in coal and gas

model.load_mw = Param(model.TIMEPOINTS, default=load_ts)
model.dispatch_cost = Param(model.GEN, default=genALL_varCost, doc="dispatch cost in $/MWh")
model.gen_capacity = Param(model.GEN, default=genALL_genCapacity)
model.gen_min_cf_thermal = Param(model.GENTHERMAL, default=genTHERMAL_minGen_cf) # Minimum generation thermal
model.gen_min_cf_hydro = Param(model.GENHYDRO, default=genHYDRO_minGen_cf_ts) # Minimum generation hydro
model.gen_energy_hydro = Param(model.GENHYDRO, default=genHYDRO_maxEnergy_ts) # Max energy hydro
model.gen_cf_vre = Param(model.GENVREDISPATCH, default=genVRE_cf_ts) # Capacity factors for VRE
model.gen_cf_mustrun = Param(model.GENMUSTRUN, default=genMUSTRUN_cf_ts) # Capacity factors for MUST RUN


'''
#############################################
## DECISION VARIABLES ##
#############################################
'''

model.DispatchMW = Var(model.GENDISPATCH, within=NonNegativeReals)
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
            for (t, gen) in mod.GENDISPATCH
        ) +
        sum(
        mod.load_unserved_mw[t] * 1000
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
    return sum(mod.DispatchMW[t, gen] for gen in mod.GEN) == mod.load_mw[t] + mod.load_unserved_mw[t]
model.Conservation_Of_Energy = Constraint(model.TIMEPOINTS, rule=Conservation_Of_Energy_rule)

# Dispatch <= max capacity of each generator
def Enforce_Max_Dispatch_Limit_rule(mod, t, gen):
    return mod.DispatchMW[t, gen] <= mod.gen_capacity[gen] * mod.uc[gen]
model.Enforce_Max_Dispatch_Limit = Constraint(model.GENDISPATCH, rule=Enforce_Max_Dispatch_Limit_rule)

# Dispatch >= min generation level for Thermal
def Enforce_Min_Dispatch_Limit_Thermal_rule(mod, t, gen):
    return mod.DispatchMW[t, gen] >= mod.gen_min_cf_thermal[gen] * mod.gen_capacity[gen] * mod.uc[gen]
model.Enforce_Min_Dispatch_Limit_Thermal = Constraint(model.GENTHERMALDISPATCH, rule=Enforce_Min_Dispatch_Limit_Thermal_rule)

# Dispatch >= min generation level for Hydro
def Enforce_Min_Dispatch_Limit_Hydro_rule(mod, t, gen):
    return mod.DispatchMW[t, gen] >= mod.gen_min_cf_hydro[gen] * mod.gen_capacity[gen] * mod.uc[gen]
model.Enforce_Min_Dispatch_Limit_Hydro = Constraint(model.GENHYDRODISPATCH, rule=Enforce_Min_Dispatch_Limit_Hydro_rule)

# Total hydro energy is conserved across the time series (e.g. day)
def Enforce_Hydro_Energy_Limit_rule(mod, gen):
    return sum(mod.DispatchMW[t, gen] for t in mod.TIMEPOINTS) == mod.gen_energy_hydro[gen]
model.Enforce_Hydro_Energy_Limit = Constraint(model.GENHYDRO, rule=Enforce_Hydro_Energy_Limit_rule)

# Dispatch <= VRE generation. Curtailment is ok. Could add another variable for curtailment and change this constraint to ==
def Enforce_VRE_Dispatch_Limit_rule(mod, t, gen):
    return mod.DispatchMW[t, gen] <= mod.gen_cf_vre[t, gen] * mod.gen_capacity[gen]
model.Enforce_VRE_Dispatch_Limit = Constraint(model.GENVREDISPATCH, rule=Enforce_VRE_Dispatch_Limit_rule)

# Dispatch == MUST RUN generation. 
def Enforce_MUSTRUN_Dispatch_rule(mod, t, gen):
    return mod.DispatchMW[t, gen] == mod.gen_cf_mustrun[gen] * mod.gen_capacity[gen]
model.Enforce_MUSTRUN_Dispatch = Constraint(model.GENMUSTRUNDISPATCH, rule=Enforce_MUSTRUN_Dispatch_rule)


'''
#############################################
## CREATE MODEL INSTANCE AND RUN THE MODEL ##
#############################################
'''
model_instance = model.create_instance()
results = opt.solve(model_instance)
model_instance.pprint()
results.write()    

print "Total dispatch cost: USD " + str(model_instance.DispatchCost.expr())

# model_instance.solutions.load_from(results)

'''
#############################################
## RESULTS PROCESSING ##
#############################################
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