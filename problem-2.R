#================================================
#Data Driven Decision Making
#Assignment 1
#Vasileios Gounaris Bampaletsos - 40314803

#================================================
#PROBLEM 2
#This script solves the LP model for the
#fleet planning
#In this model we need to find how many ships
#of each category should buy the company
#for maximum profit and find this number

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
  #because we have SHIPS(subjects)
  add_variable(x[i], i = 1:3, type = "integer") %>%
  
  #================================================
  #objective function to find the max profit
  #x1 = long distance ship 
  #x2 = medium distance ship 
  #x3 = short distance ship 
  set_objective(4.2*x[1] + 3.1*x[2] + 2.6*x[3], "max") %>%
  
  #================================================
  #constrains
  #set the price of each category and the budget of the company
  #(in millions of dollas)
  add_constraint(70*x[1] + 55*x[2] + 40*x[3] <= 2000) %>% 
  
  #set constrain for the maximum number of ships 
  #which they can manage
  add_constraint(x[1] + x[2] + x[3] <= 40) %>%
  
  #set the limit of the facilities capasity
  #company can manage up to 60 short ships
  #long=2 shorts
  #medium= 1.5 shorts
  add_constraint(x[1] <= 30) %>%
  add_constraint(x[2] <= 40) %>%
  add_constraint(x[3] <= 60) %>%
  set_bounds(x[i], lb=0, i=1:3) %>%
  
  #================================================
  #solving the problem and extract the results
  solve_model(with_ROI(solver="glpk", verbose=TRUE)) %>%
  get_solution(x[i]) %>%
  filter(value > 0)


#=======================================================
#IPSOLVE
#Problem-2
#This script solves the problem for fleet planning
#with the ipsolve in r

#==============================
#import the ipsolve package
library(lpSolve)

#==============================
#create the objective function
#build a vector of coefficients
#from the objective function
objective <- c(
  4.2, #net profit per long ship
  3.1, #net profit per medium ship
  2.6) #net profit per small ship

#==============================
#the constraints
#matrix with coefficients of constrains equations
const_mat <- matrix(c(
  70, 55, 40, #1st constraint: the prices of each ship category(in millions)
  1, 1, 1,   #2nd constraint: the limitations for personnel
  1, 0, 0,  #3rd constraint: the limitation for long ships
  0, 1, 0, #4th constraint: the limitation for medium ships
  0, 0, 1), #5th constraint: the limitation for short ships
  nrow=5,byrow=TRUE)

#==============================
#the equality/inequality signs
const_dir<-c(
  "<=", #inequality sign of the 1st constraint(prices)
  "<=", #inequality sign of the 1st constraint(company's personnel)
  "<=", #inequality sign of the 1st constraint(long ships)
  "<=", #inequality sign of the 1st constraint(medium ships)
  "<=") #inequality sign of the 1st constraint(short ships)

#==============================
#the right hand side(rhs) parameters(constants)
const_rhs <- c(
  2000, #rhs value of the 1st constraint (in millions)
  40, #rhs value of the 2nd constraint (in ships)
  30, #rhs value of the 3rd constraint (in ships)
  40, #rhs value of the 4th constraint (in ships)
  60) #rhs value of the 5th constraint (in ships)

#================================
#mathematical programming setting
#find the opt objective(MAX profit)
lp(direction="max", objective, const_mat, const_dir, 
   const_rhs, all.int = TRUE)
#find the opt solution(Values of x1 and x2 and x3)
lp(direction ="max", objective, const_mat, const_dir, 
   const_rhs, all.int = TRUE)$solution