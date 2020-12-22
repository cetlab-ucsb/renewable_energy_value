# -*- coding: utf-8 -*-
"""
Created on Wed Dec 13 15:53:31 2017

@author: Ranjit
"""

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


#'''
##############################################
### IMPORTING DATA - CSVS AND PATH ##
##############################################
#'''
#### IMPORTING DATA
#
##myPath = "C:\\Users\\akjohnson\\Desktop\\Ranjit\\"
#myPath = "G:\\Electricity_Models\\"
#inputPath = myPath + "renewable_energy_value\\india_REV_input\\"
#inputPathVRE_gen_profiles = myPath + "india_ED\\india_ED_input\\REvalue_gen_profiles\\"
#inputPathVRE_capacity = myPath + "india_ED\\india_ED_input\\REvalue_capacity\\"
#
## Ana note: for Mac, will probably work on Windows
## inputPath = os.path.join(os.getcwd(), "india_ED_input/")
## inputPathVRE = os.path.join(os.getcwd(), "india_ED_input/")
#
#yearAnalysis = 2030
#yearBase = 2014
#load_csv = "load" + str(yearAnalysis) + "_19EPS" + ".csv" # Load CSV
#genALL_input_csv = "gen_all_input_cc_ccgt_diesel.csv" # generator csv with var cost and max capacity for all generators
##genVRE_csv = "vre_gen.csv" # variable RE generator csv with dispatch capacity factors
#genHYDRO_minGen_csv = "hydro_min_gen.csv"
#genHYDRO_maxEnergy_csv = "hydro_max_energy.csv"
#genMUSTRUN_csv = "mustrun_gen.csv" # must run generators like nuclear and run-of-river hydro that have a constant output through the timeseries (e.g. day)
#
#days_to_run = 365
#
#'''
##############################################
### INPUTS ##
##############################################
#'''
#scenarios = ["S0W0", "S0W200", "S50W150", "S100W100", "S150W50", "S200W0", "S0W300", "S75W225", "S150W150", "S225W75", "S300W0", "S0W400", "S100W300", "S200W200", "S300W100", "S400W0"]#, "S300W100"] # List of VRE scenarios ["S0W200", "S50W150", "S100W100", "S150W50", "S200W0"]
#
### Load
#load = pd.read_csv(inputPath + load_csv, sep=',')
#
#
#'''
##############################################
### OUTPUTS ##
##############################################
#'''
#
## Path for output files for conventional generator builout

model = AbstractModel()

model.x = Var(initialize = 1.5)
model.y = Var(initialize = 1.5)

# Constraints
def first_rule(model):
    return model.x + model.y <= 2
model.first = Constraint(rule=first_rule)

def second_rule(model):
    return -model.x**2 + model.y >= 0
model.second = Constraint(rule=second_rule)

#Objective function
def rosenbrock(model):
    return (model.x - 2)**2 + (model.y - 1)**2
    #return (1.0-model.x)**2 + 100.0*(model.y - model.x**2)**2
model.obj = Objective(rule=rosenbrock, sense=minimize)

print("Compiling...")
model_instance = model.create_instance()
print("Solving...")
results = opt.solve(model_instance, tee=True)

# print("Instance pprint...")
# model_instance.pprint()

print("Writing results...")
results.write()