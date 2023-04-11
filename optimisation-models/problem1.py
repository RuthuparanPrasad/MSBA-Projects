#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Oct 26 09:32:52 2022

@author: Ruthuparan Prasad

Advertising Campaign Problem - LP Model:
    
Let x be the amount spent on newspaper
Let y be the amount spent on radio

Objective function: 
    min z = x1 + x2

Subject to:
    40x + 20y >= 110000
    20x + 40y >= 144000
    x >=0
    y >=0

"""

#importing the necessary packages
import pulp as pl 
import pandas as pd

#reading the file
filename = Data.xlsx"
df = pd.read_excel(filename, "Problem1", index_col = 0)

constraints= df.loc[df.index[:], df.columns[:-1]].to_dict()
products = list(constraints.keys())
rhs_coeff = df.loc[df.index[:], df.columns[-1]].to_dict()


#creating the model
model = pl.LpProblem("Advertising_campaign", pl.LpMinimize)

#decision variables
variables = pl.LpVariable.dicts("amount_spent_on", products, lowBound = 0)

model += pl.lpSum(variables[i] for i in variables)

#creating the constraints
for r in rhs_coeff:
     model += pl.lpSum(constraints[u][r]*variables[u] for u in products) >= rhs_coeff[r]   
     
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
 



    


    









        



    


    




