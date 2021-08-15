#================================================
#Data Driven Decision Making
#Assignment 1
#Vasileios Gounaris Bampaletsos - 40314803

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
  #objective function
  #x1 = jars (own brand), x2 = jars (other brands)
  set_objective(3*(x[1]) + x[2], "min") %>%
  
  #================================================
  #constrains
  #space per jar is 50cm2 and the max space is 5000cm2
  #own brand"s sales are 1.2 times more than the other brands
  add_constraint(x[1] + 3*x[2] >= 7) %>% #shelf space
  add_constraint(x[1] >= 0) %>% 
  add_constraint(x[2] >= 0) %>% #sales
  set_bounds(x[i], lb=0, i=1:2) %>% #set bounds fro x1,x2
  
  #================================================
  #solving the problem and extract the results
  solve_model(with_ROI(solver="glpk", verbose=TRUE)) %>%
  get_solution(x[i]) %>%
  filter(value > 0)


#=======================================================
#IPSOLVE
#Problem-4
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
  3,  #profit per jar for own brand (x1)
  1)  #profit per jar for other brands (x2)

#==============================
#the constraints
#matrix with coefficients of constrains equations
const.mat <- matrix(c(
  1, 3, #shelf space equation
  1, 0, #nonnegativity constraint for x1
  0, 1), nrow=3, byrow = TRUE) #nonnegativity constraint for x2

#==============================
#the equality/inequality signs
const.div <- c(
  ">=", #inequality sign of the 2nd constraint(sales' differnece)
  ">=", #inequaluty sign of the 3rd constraint(for x1)
  ">=") #inequaluty sign of the 4th constraint(for x2

#==============================
#the right hand side(rhs) parameters(constants)
const.rhs <- c(
  7, #rhs value of the 1st constraint
  0, #rhs value of the 2nd constraint
  0) #rhs value of the 4th constraint

#================================
#mathematical programming setting
#find the opt objective(MAX profit)
lp(direction="min", objective, const.mat, const.div, 
   const.rhs, all.int = TRUE)
#find the opt solution(Values of x1 and x2)
lp(direction="min", objective, const.mat, 
   const.div, const.rhs, all.int = TRUE)$solution