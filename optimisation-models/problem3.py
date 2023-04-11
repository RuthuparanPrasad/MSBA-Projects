#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Oct 27 21:13:57 2022

@author: Ruthuparan Prasad
Student ID: 2230308
Title: Assignment Problem 3

Sanders Fishing - LP Model: 
    
Let x1, x2, x3 be rnumber of egular products made in months 1, 2, 3 respectively
Let y1, y2, y3 be onumber of overtime products made in months 1, 2, 3 respectively
Let z1, z2, z3 be number of extra products made in months 1, 2, 3 respectively

Objective function:
    min z = 35x1 + 55x2 + 70x3 + 55y1 + 75y2 + 110y3 + 28z1 + 28z2 + 28z3
Subject to:
    x1 <= 80
    y1 <= 145
    x2 <= 105
    y2 <=145
    x3 <= 130
    y3 <= 45
    x1 + y1 - z1 = 160
    x2 + y2 + z1 - z2 = 190
    x3 + y3 + z2 - z3 = 100
    x1, x2, x3, y1, y2, y3, z1, z2, z3 >= 0
"""

#importing the necessary packages
import pulp as pl 
import pandas as pd

#creating some variables
monthly_demand = [160,190,100]

#reading the file
filename = "Data.xlsx"
df = pd.read_excel(filename, "Problem3", index_col = 0)

constraints = df.loc[df.index[:], df.columns[:]].to_dict()
products = list(df.index.values)

#inititating the model
model = pl.LpProblem("Sanders_Fishing", pl.LpMinimize)

#creating the decision variables
variables = pl.LpVariable.dicts("count_of", products, lowBound = 0)

#adding the decision variables
model += pl.lpSum(constraints["Cost"][p]*variables[p] for p in products)

#adding capacity constraints, using products[0:-3] as we dont have capacity constraints on excess products
for p in products[0:-3]:
    model += pl.lpSum(variables[p]) <= constraints["Capacity"][p]
    
#adding other constraints
model += pl.lpSum(variables["Regular_Month1"] + variables["Overtime_Month1"] - variables["Extra_Month1"]) == monthly_demand[0]

model += pl.lpSum(variables["Regular_Month2"] + variables["Overtime_Month2"] + variables["Extra_Month1"] - variables["Extra_Month2"] ) == monthly_demand[1]

model += pl.lpSum(variables["Regular_Month3"] + variables["Overtime_Month3"] + variables["Extra_Month2"] - variables["Extra_Month3"]) == monthly_demand[2]

#solving the model
model.solve()

#printing the results
print("\n------------SOLUTION------------")
print("Status:", pl.LpStatus[model.status])    
print("Minimum cost =", pl.value(model.objective))  
print("Minimum cost (rounded) =", round(pl.value(model.objective),2)) 

if (pl.LpStatus[model.status] == 'Optimal'):
    print("\nThe values of variables are: ")
    for v in model.variables():
        print(v.name, "=", v.varValue)





