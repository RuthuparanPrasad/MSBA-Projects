#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Oct 28 19:40:25 2022

@author: Ruthuparan Prasad
Student ID: 2230308
Title: Assignment Problem 5

Raw Material Shipping - LP Model:
    
Let x11 be the number of units shipped from Supplier 1 to Site 1
Let x12 be the number of units shipped from Supplier 1 to Site 2
Let x21 be the number of units shipped from Supplier 2 to Site 1
Let x22 be the number of units shipped from Supplier 2 to Site 2
Let x31 be the number of units shipped from Supplier 3 to Site 1
Let x32 be the number of units shipped from Supplier 3 to Site 2

Objective function:
    min z = 11*x11 + 11*x12 + 7*x21 + 6*x22 + 10*x31 + 7*x32
Subject to:
    0.7*x11 + 0.6*x21 + 0.6*x31 = 100
    0.9*x12 + 0.8*x22 + 06*x32 = 20
    x11 + x12 <= 90
    x21 + x22 <= 110
    x31 + x32 <= 50
    x11, x12, x21, x22, x31, x32 >= 0
    
"""

#importing the necessary packages
import pulp as pl 
import pandas as pd

#reading the file
filename = "Data.xlsx"
df = pd.read_excel(filename, "Problem5", index_col = 0)

constraints = df.loc[df.index[:], df.columns[:]].to_dict()
suppliers = list(df.index.values)[:-1]
sites = list(df.columns)[:2]

travels ={}
refining_percentage = {}

for sup in suppliers:
    for site in sites:
        travels[sup+"_"+site] = constraints[site][sup]
        refining_percentage[sup+"_"+site] = constraints["refine_"+site][sup]/100
        

#creating the model
model = pl.LpProblem("Raw_Material_Shipping", pl.LpMinimize)

#making the decision variables
variables = pl.LpVariable.dicts("cost_of", travels, lowBound = 0)

#creating the objective function
model += pl.lpSum(variables[t]*travels[t] for t in travels.keys())

#creating the constraints
for site in sites:
    model += pl.lpSum(variables[t]*refining_percentage[t] for t in travels.keys() if t.endswith(site)) == constraints[site]["Demand"]
    
for sup in suppliers:
    model += pl.lpSum(variables[t] for t in travels.keys() if t.startswith(sup)) <= constraints["Supply"][sup]
    
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
    






