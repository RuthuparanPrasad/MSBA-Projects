#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Oct 28 10:51:10 2022

@author: Ruthuparan Prasad
Student ID: 2230308
Title: Assignment Problem 4

Oilco Oil Refining - LP Model:
    
Let xld be the no of units shipped between Los Angeles and Dallas
Let xlh be the no of units shipped between Los Angeles and Houston
Let xsd be the no of units shipped between San Diego and Dallas
Let xsh be the no of units shipped between San Diego and Houston
Let xdc be the no of units shipped between Dallas and Chicago
Let xdn be the no of units shipped between Dallas and New York
et xhc be the no of units shipped between Houston and Chicago
Let xhn be the no of units shipped between Houston and New York

NOTE: WE ARE CONSIDERING 1 UNIT= 1000 BARRELS, AND WE ARE ADDING THE COST OF REFINING IN DALLAS/HOUSTON AT THE END OF THE OBJECTIVE FUNCTION AND THEN SIMPLIFYING

Objective function: 
    min z = 30xld + 45xlh + 30xsd + 25xsh + 45xdc + 45xdn + 40xhc + 45xhn + 80xld + 80xsd + 60xlh + 60xsh
Simplified to:
    min z = 110xld + 105xlh + 110xsd + 85xsh + 45xdc + 45xdn + 40xhc + 45xhn
Subject to:
    xld + xlh <= 55.5
    xsd + xsh <= 35.5
    xdc + xhc = 35
    xdn + xhn = 39
    xld + xsd - xdc -xdn = 0
    xlh + xsh - xhc - xhn = 0
    xld, xlh, xsd, xsh, xdc, xdn, xhc, xhn >= 0
    
"""

#importing the necessary packages
import pulp as pl 
import pandas as pd

#creating some variables, considering 1 unit = 1000 barrels
supply = {"LA" : 55.5, "SD" : 35.5}
demand = {"NY" : 39, "CH" : 35}

#reading the file
filename = "Data.xlsx"
df = pd.read_excel(filename, "Problem4", index_col = 0)

constraints = df.loc[df.index[:], df.columns[:]].to_dict()
travels = list(df.index.values)

model = pl.LpProblem("Oilco_Oil_Refining", pl.LpMinimize)

#creating the variables
variables = pl.LpVariable.dicts("units_shipped", travels, lowBound = 0) 

#creating the objective function
model += pl.lpSum(variables[t]*constraints["shipping"][t] + constraints["refining"][t]*variables[t] for t in travels)

#creating the constraints
model += pl.lpSum(variables["LA_DA"] + variables["LA_HO"]) <= supply["LA"]

model += pl.lpSum(variables["SD_DA"] + variables["SD_HO"]) <= supply["SD"]
    
model += pl.lpSum(variables["DA_CH"] + variables["HO_CH"]) == demand["CH"]

model += pl.lpSum(variables["DA_NY"] + variables["HO_NY"]) == demand["NY"]

model += pl.lpSum(variables["LA_DA"] + variables["SD_DA"] - variables["DA_CH"] - variables["DA_NY"]) == 0

model += pl.lpSum(variables["LA_HO"] + variables["SD_HO"] - variables["HO_CH"] - variables["HO_NY"]) == 0

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



            




