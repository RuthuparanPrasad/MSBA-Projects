#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Oct 26 09:32:52 2022

@author: Ruthuparan Prasad
Student ID: 2230308
Title: Assignment Problem 2

Rug Manufacturer - LP Model:
    
Let x be the number of high grade rugs sold
Let y be the number of low grade rugs sold

Objective function: 
    min z = 475x + 325y
Subject to:
    35x + 15y <= 1800 (wool availability constraint)
    10x + 40y <= 1800 (nylon availability constraint)
    30x + 20y <= 700 (time constraint)
    x >= 0
    y >=0

"""

#importing the necessary packages
import pulp as pl 
import pandas as pd

#creating some variables
availability = {"Wool" : 1800, "Nylon" : 1800}
cost_per_sq_m = {"Wool" : 5, "Nylon" : 3} 
max_time = 700

#reading the file
filename = "Data.xlsx"
df = pd.read_excel(filename, "Problem2", index_col = 0)

#creating constraints and defining products
constraints = df.loc[df.index[:], df.columns[:]].to_dict()
products = list(df.index.values)[0:2]

#creating the income dictionary
income = {}
for p in products:
    inc_per_item = 0
    inc_per_item += constraints["Revenue"][p]-(constraints["Wool"][p]*cost_per_sq_m["Wool"] + 
 constraints["Nylon"][p]*cost_per_sq_m["Nylon"] +
 constraints["Labour"][p]*constraints["Hours"][p]
 )
    income[p] = inc_per_item

#creating the model
model = pl.LpProblem("Rug_Manufacturer", pl.LpMaximize)

#creating the variables
variables = pl.LpVariable.dicts("number_of", products, lowBound = 0)

#defining the objective function
model += pl.lpSum(variables[p] * income[p] for p in products)

#creating the availabiltiy constraints
model += pl.lpSum(constraints["Wool"][p]*variables[p] for p in products) <= availability["Wool"]

model += pl.lpSum(constraints["Nylon"][p]*variables[p] for p in products) <= availability["Nylon"]

#creating the time constraint
model += pl.lpSum(constraints["Hours"][p]*variables[p] for p in products) <= max_time

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
    


            