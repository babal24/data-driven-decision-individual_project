#================================================
#Data Driven Decision Making
#Assignment 1
#Vasileios Gounaris Bampaletsos - 40314803

#================================================
#PROBLEM 1
#This script solves the LP model for the
#supermarket's shelf space.

#================================================
#problem 1B
#In this question we need to find
#the number of the jars of their own brand
#and of the other brands on the allocated 
#shelf space for MAXIMUM profit.
#BUT the supermarket makes a discount 
#in their own brand jars and the sales and profit change

#===============================================
#OMPR MODEL
#import the necesary packages for ompr
library(dplyr)
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)

#===============================================
#build the model with the objective equation
#and with the constraints
MIPModel() %>%
  
  #================================================
  #decision variables and set the type as integer
  #because we have jars(subjects)
  add_variable(x[i], i = 1:2, type = "integer") %>%
  
  #================================================
  #objective function to find the max profit
  #x1 = jars (own brand), x2 = jars (other brands)
  set_objective(0.20*x[1] + 0.25*x[2], "max") %>%
  
  #================================================
  #constrains
  #space per jar is 50cm2 and the max space is 5000cm2
  #own brand"s sales are 1.2 times more than the other brands
  add_constraint(50*x[1] + 50*x[2] <= 5000) %>% #shelf space
  add_constraint(x[1] >= 1.2*x[2]) %>% #sales
  set_bounds(x[i], lb=0, i=1:2) %>% #set bounds fro x1,x2
  
  #================================================
  #solving the problem and extract the results
  solve_model(with_ROI(solver="glpk", verbose=TRUE)) %>%
  get_solution(x[i]) %>%
  filter(value > 0)


#=======================================================
#IPSOLVE
#Problem-1b
#This script solves the problem for the supermarket
#shelf space with the ipsolve

#==============================
#import the ipsolve package
library(lpSolve)

#==============================
#create the objective function
#build a vector of coefficients
#from the objective function
objective <- c(
  0.20,  #profit per jar for own brand (x1)
  0.25)  #profit per jar for other brands (x2)

#==============================
#the constraints
#matrix with coefficients of constrains equations
const.mat <- matrix(c(
  50, 50, #shelf space equation
  1, -1.2, #the difference in sales
  1, 0, #nonnegativity constraint for x1
  0, 1), nrow=4, byrow = TRUE) #nonnegativity constraint for x2

#==============================
#the equality/inequality signs
const.dir <- c(
  "<=", #inequality sign of the 1st constraint(shelf space)
  ">=", #inequality sign of the 2nd constraint(sales' differnece)
  ">=", #inequaluty sign of the 3rd constraint(for x1)
  ">=") #inequaluty sign of the 4th constraint(for x2

#==============================
#the right hand side(rhs) parameters(constants)
const.rhs <- c(
  5000, #rhs value of the 1st constraint
  0, #rhs value of the 2nd constraint
  0, #rhs value of the 3rd constraint
  0) #rhs value of the 4th constraint

#================================
#mathematical programming setting
#find the opt objective(MAX profit)
lp(direction="max", objective, const.mat, const.dir, 
   const.rhs, all.int = TRUE)
#find the opt solution(Values of x1 and x2)
lp(direction="max", objective, const.mat, 
   const.dir, const.rhs,all.int = TRUE)$solution