#================================================
#Data Driven Decision Making
#Assignment 1
#Vasileios Gounaris Bampaletsos - 40314803

#================================================
#Problem 3 - ompr & Ipsolve

#===============================================
#import the necesary packages for ompr and ipsolve
library(dplyr)
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)
library(lpSolve)

#===============================================
#Matrix with City Dinstances(kilometers-km)(Cdist)
C_dist <- matrix(c(
  0, 976, 1808, 1984, 3029, 1532, 206, 2657,
  976, 0, 1198, 1043, 2105, 1383, 833, 1722,
  1808, 1198, 0, 794, 1418, 1325, 1597, 1020,
  1984, 1043, 794, 0, 1167, 2058, 1773, 829,
  3029, 2105, 1418, 1167, 0, 2750, 2818, 391,
  1532, 1383, 1325, 2058, 2750, 0, 1251, 2352,
  206, 833, 1597, 1773, 2818, 1251, 0, 2435,
  2657, 1722, 1020, 829, 391, 2352, 2435, 0), nrow=8, ncol=8, byrow = TRUE)

#================================================
#vector with order amount(kilograms-kg)(Vord_am)
Vord_am <- c(68, 41, 37, 54, 33, 39, 58, 51)

#matrix with order amount in kg per customer per plant (Mord_am)
Mord_am <- matrix(c(
  68, 68, 68, 68, 68, 68, 68, 68,
  41, 41, 41, 41, 41, 41, 41, 41,
  37, 37, 37, 37, 37, 37, 37, 37,
  54, 54, 54, 54, 54, 54, 54, 54,
  33, 33, 33, 33, 33, 33, 33, 33,
  39, 39, 39, 39, 39, 39, 39, 39,
  58, 58, 58, 58, 58, 58, 58, 58,
  51, 51, 51, 51, 51, 51, 51, 51),nrow=8,ncol=8, byrow = TRUE)
Mord_am

#================================================
#vector with order bid price (Vbidpr)
Vbid_pr <- c(75840, 44470, 46420, 87880, 43950, 21100, 74950, 84080)

#================================================
#Matrix with order bid price (Mbidpr)
Mbid_pr <- matrix(c(
  75840, 75840, 75840, 75840, 75840, 75840, 75840, 75840, 
  44470, 44470, 44470, 44470, 44470, 44470, 44470, 44470,
  46420, 46420, 46420, 46420, 46420, 46420, 46420, 46420,
  87880, 87880, 87880, 87880, 87880, 87880, 87880, 87880,
  43950, 43950, 43950, 43950, 43950, 43950, 43950, 43950,
  21100, 21100, 21100, 21100, 21100, 21100, 21100, 21100,
  74950, 74950, 74950, 74950, 74950, 74950, 74950, 74950,
  84080, 84080, 84080, 84080, 84080, 84080, 84080, 84080),nrow=8, ncol=8, byrow = TRUE)
Mbid_pr

#================================================
#create a matrix with fixed costs
#customer's bid price(Mbid_pr) - [customer's order amount(Mord_am)multipled by 10 euros per kilo]
# - [customer's order amount(Mord_am)multiple with cities distances] multiple with[0,01(cost per km)]
#with this matrix we have the costs of transformation and production per order: Fixed Costs(F_costs)
F_costs <- Mbid_pr - 10*Mord_am - (0.01*(Mord_am*C_dist))
F_costs

#================================================
#ipsolve model
#set the objective equation
#use the fixed costs and add the production cost per plant(-70000euros)
objective = c(as.vector(F_costs),rep(-70000,8))
objective

#build the constraint matrix with left hands of the equations
const_mat = matrix(c(68, 41, 37, 54, 33, 39, 58, 51,rep(0,56), -120,rep(0,7),rep(0,8), 
                     68, 41, 37, 54, 33, 39, 58, 51,rep(0,49), -120,rep(0,6),rep(0,16),
                     68, 41, 37, 54, 33, 39, 58, 51,rep(0,42), -120,rep(0,5),rep(0,24),
                     68, 41, 37, 54, 33, 39, 58, 51,rep(0,35), -120,rep(0,4),rep(0,32),
                     68, 41, 37, 54, 33, 39, 58, 51,rep(0,28), -120,rep(0,3),rep(0,40),
                     68, 41, 37, 54, 33, 39, 58, 51,rep(0,21), -120,rep(0,2),rep(0,48),
                     68, 41, 37, 54, 33, 39, 58, 51,rep(0,14), -120,0,rep(0,56),
                     68, 41, 37, 54, 33, 39, 58, 51,rep(0,7), -120,
                     rep(c(1,0,0,0,0,0,0,0),8),0,0,0,0,0,0,0,0,
                     rep(c(0,1,0,0,0,0,0,0),8),0,0,0,0,0,0,0,0,
                     rep(c(0,0,1,0,0,0,0,0),8),0,0,0,0,0,0,0,0,
                     rep(c(0,0,0,1,0,0,0,0),8),0,0,0,0,0,0,0,0,
                     rep(c(0,0,0,0,1,0,0,0),8),0,0,0,0,0,0,0,0,
                     rep(c(0,0,0,0,0,1,0,0),8),0,0,0,0,0,0,0,0,
                     rep(c(0,0,0,0,0,0,1,0),8),0,0,0,0,0,0,0,0,
                     rep(c(0,0,0,0,0,0,0,1),8),0,0,0,0,0,0,0,0),nrow=16, byrow = TRUE)
const_mat

#create the matrix for the equality/inequality signs
const_dir = c(rep("<=",8),rep("<=",8))
const_dir

#create a vector with right hands of the equations
const_rhs = c(rep(0,8),rep(1,8))
const_rhs

#solve the problem
lp("max",objective, const_mat, const_dir, const_rhs, all.bin = TRUE)
sol=lp("max",objective, const_mat, const_dir, const_rhs, all.bin = TRUE)$solution

#=======================================================
#ready code from the lectures
#it presents the solutions and the max profit
Narr = 2 				#Number of arrays of decision variables in the model 
v = c("x","y")	#Variable name (letter) of each array 
Rows = c(8,8)		#Number of rows in each array
Cols = c(8,1)	  #Number of columns in each array


rangeU = c(rep(0,Narr))		# value of k where an array ends 
for(n in 1:Narr){
  for(m in 1:n){		# counting number of vars up to that upper range value 
    rangeU[n] = rangeU[n] + Rows[m]*Cols[m]
  }
}

for(k in 1:length(sol)){
  a = Narr			# array number not yet known; initialized to Narr
  notFound = TRUE		# array not yet found  	
  n = Narr-1 			# initializing n before the loop 
  if(sol[k]!=0){
    while((notFound)&(n>=1)){
      if(k>rangeU[n]){
        a = n+1
        notFound = FALSE
      } else{
        a = n 
        n = n-1 
      }
    }
    if(a==1){		# p is the location of k within the array it lies, when counting row by row 
      p = k
    } else{
      p = k-rangeU[a-1]
    }
    i = floor(p/Cols[a])+1 
    j = p - Cols[a] * floor(p/Cols[a])
    if(j==0){
      i = i-1
      j <- Cols[a]
    }
    if(Rows[a]==1){
      print(paste("var indexed ",k,"  is  ",v[a],",",j," = ",sol[k]),quote=FALSE)
    } else if(Cols[a]==1){
      print(paste("var indexed ",k,"  is  ",v[a],",",i," = ",sol[k]),quote=FALSE)
    } else {
      print(paste("var indexed ",k,"  is  ",v[a],",",i,",",
                  j," = ",sol[k]),quote=FALSE)
    }
  } 
}
print("The solution values of the remaining variables are zero.")


#===============================================
#ompr MODEL
#build the model with the objective equation
#and with the constraints
MIPModel() %>%
  #================================================
  #decision variables and set the type as binary
  #i=orders, j=plants
  add_variable(x[i,j], i=1:8, j=1:8, type = "binary") %>%
  add_variable(y[j], j=1:8, type = "binary") %>%
  
  #================================================
  #objective function to find the max profit
  #double sum expression
  set_objective(sum_expr(sum_expr(F_costs[i,j]*x[i,j], i=1:8)-70000*y[j],j=1:8), "max") %>%
  
  #================================================
  #constraints 
  add_constraint(sum_expr(x[i,j],j=1:8)<=1, i=1:8) %>%
  #constraints for order amount(kg)
  add_constraint(sum_expr(x[i,j]*Vord_am[i],i=1:8)-120*y[j]<=0, j=1:8) %>%
  #solve the model and find the max profit and plants and orders
  solve_model(with_ROI(solver = "glpk", verbose=TRUE)) %>%
  get_solution(x[i,j]) %>%
  filter(value>0)