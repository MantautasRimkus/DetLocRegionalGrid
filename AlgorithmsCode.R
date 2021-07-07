#Code provides algorithms described in paper "Detection and Localization of 
#Faults in a Regional Power Grid"

#This repository provides the R code for the algorithms described in the paper "Detection and Localization of Faults in a Regional Power Grid". 

#Code Author - Mantautas Rimkus, Colorado State University, mantautas.rimkus@colostate.edu

#The code is organized following the chapters of the paper.

#For an illustration of the code, two training simulations is provided (training_simulation1.csv and training_simulation2.csv).

#Given .csv files follows the structure described in the paper, where each column represents different buses (pmus) and each row - observation starting at second 0 with 120 observations per second rate. 

#In training_simulation1.csv, line 7 is the true faulted line.
#In training_simulation2.csv, line 89 is the true faulted line.
#Faults were applied at second 600.

#Data range is from second 0 to second 660. 

#Information about grids connections is needed to run the localization algorithm.  
#For the regional grid described in the paper (WECC), information is stored in regional_grid.csv. 

#------------------------------------------------------------------------------------------
#Libraries
library("vroom")
library("tidyverse")
#install.packages("Rfast") one should have Rfast package installed. It is suggested not to load it
#as there can be errors if running tidyverse and Rfast simultaneously. 


#Data-----------------------------------------------------------

#Read files
regional_grid <- read.csv("regional_grid.csv")

#To run code for training_simulation2.csv, change the code line below. 
data_modulus <- (vroom::vroom(file = "training_simulation2.csv") %>%
   magrittr::set_colnames(paste("Bus", 1:122)) %>%
   as.matrix())[1:86400,1:122]

#Make time grid (in seconds). In general, most of the data sets would have time variable. Just assign
#it separately to the time_grid variable
max_time <- 900
min_time <- 0 
delta <- 1/120
time_grid <- seq(min_time,max_time-delta,by=delta)

#Detection algorithm-------------------------------------------

#Set tuning parameters
tau <- 15 #tuning parameter for detection (threshold)
S_0 <- 30 * delta
S_1 <- 0 

#Set inner parameters

Check <- 0 #0 if no fault, changes to 1 if the fault is detected
iteration_index <- (S_0+S_1)/delta +1
time <- time_grid[iteration_index] #Prepare where to start (as because of moving window, we need some prior data. )

#Algorithm 1 
#In the sake of interpretability, algorithm 1 is provided as a loop. 
#In general, there are several ways how to make it faster, like: 
#1. Reuse calculations for the previous iterations
#2. Do batches in `apply` or similar technique


time_fault <- NA
{
  while(Check==0 & time<max(time_grid))
  {
    #Push time by delta up
    iteration_index <- iteration_index+1
    time <- time_grid[iteration_index] #This could not be solved with time <- time+delta as it might introduce
    #very tiny differences because of R approximations. This way in general a bit slower, but safer
    
    #Prepare moving windows [t-S_0_S_1;t-S_1] and [t_S_1;t]
    first_window <- which(time-S_0-S_1<=time_grid & time-S_1 > time_grid)
    second_window <- which(time-S_1<=time_grid & time >= time_grid)
    
    #Find xbar and SD using indexes in the first window]
    xbar_t_vekt <- Rfast::colmeans(data_modulus[first_window,])
    sd_t_vekt <- Rfast::colVars(data_modulus[first_window,], std=TRUE)
    
    #Find m using using indexes in the second window (to avoid potential problems with R if there 
    #is only one row, we double the data. This approach proved to be safe from errors and in general
    #very fast.)
    
    m_t_vekt <- Rfast::colmeans(rbind(data_modulus[second_window,],data_modulus[second_window,]))
    
    #Find D_i(t,S_0,S_1) for each bus
    
    D_i <- abs(m_t_vekt-xbar_t_vekt)/sd_t_vekt
    
    #Find if the maximum exceeds threshold \tau
    
    if(max(D_i)>tau){
      time_fault <- time
      Check <- 1
      print(paste0("Fault detected at second ", time_fault, ". Algorithm 1 stopped"))
    }
  }
}


#if(is.na(time_fault)) {print(paste0("Fault was not detected over given time.")}

#Localization algorithm------------------------------------------------------------------

#Parameters

time_fault <- time_fault  #Naturally, the localization algorithm should be only used after the fault was detected.
index_fault <- which(time_grid==time_fault)

#Tuning parameters
tau_1 <- 0.1
S_0 <- 30*delta
S_1 <- 0

#Find which buses can be start buses

B_set <- unique(regional_grid$Start_Bus)

#Algorithm 2

  #Predict the start bus: Set time windows and calculate xbar and m statistics
first_window <- which(time_fault-S_0-S_1<=time_grid & time_fault-S_1 > time_grid)
second_window <- which(time_fault-S_1<=time_grid & time_fault >= time_grid)

xbar_t_vekt <- Rfast::colmeans(data_modulus[first_window,])
names(xbar_t_vekt) <- colnames(data_modulus)
m_t_vekt <- Rfast::colmeans(rbind(data_modulus[second_window,],data_modulus[second_window,]))
names(m_t_vekt) <- colnames(data_modulus)

  #Predict the start bus of the faulted line:
evaluated_for_buses <- abs(m_t_vekt-xbar_t_vekt)
names(evaluated_for_buses) <- colnames(data_modulus)
start_bus_prediction <- names(which.max(evaluated_for_buses[B_set]))

#Check how many potential end buses there possibly can be

B_prediction_start <- regional_grid$End_bus[regional_grid$Start_Bus==start_bus_prediction] %>% unique()

if(length(B_prediction_start)==1) { 
  
  #If there is only one possible end bus, we just choose it
  end_bus_prediction <- B_prediction_start 
  
} else {
  
  #If there is more than 1 possible prediction for end bus, we need to implement additional calculations
  
  #Calculate recovery statistics for the predicted start and find t_r. If t_r cannot be found,
  #we assume that the faulted line cannot be found.
  
  t_R <- NA #Set recovery time as unknown
  iteration_index <- which(time_grid == time_fault)
  
  #Algorithm to find the recovery time
  while(is.na(t_R) & time<max(time_grid))
  {
    iteration_index <- iteration_index+1
    time <- time_grid[iteration_index]
    
    #Calculating D* for the predicted start bus
    second_window_star <- which(time-S_1<=time_grid & time >= time_grid)
    m_t_vekt_previous <- Rfast::colmeans(rbind(data_modulus[second_window_star-1,],data_modulus[second_window_star-1,]))
    names(m_t_vekt_previous) <- colnames(data_modulus)
    m_t_vekt_current <- Rfast::colmeans(rbind(data_modulus[second_window_star,],data_modulus[second_window_star,]))
    names(m_t_vekt_current) <- colnames(data_modulus)
    
    D_star_start_bus_current <- (m_t_vekt_current[start_bus_prediction] - xbar_t_vekt[start_bus_prediction])
    D_star_start_bus_previous <- (m_t_vekt_previous[start_bus_prediction] - xbar_t_vekt[start_bus_prediction])
    
    #Checking if the the value of relative change exceeds the threshold
    
    recovery_start_bus <- abs((D_star_start_bus_current-D_star_start_bus_previous)/D_star_start_bus_previous)
    if(recovery_start_bus>tau_1){
      t_R <- time 
      print(paste("Recovery time found - ", t_R))
    }
  }
  
  #Pasitikrinimui
  savez <- c()
  savez <- c(savez, m_t_vekt_current[B_prediction_start])
  
  #Calculating D_star differences for possible end buses. 
  
  end_bus_prediction <-
    names(which.max(abs(m_t_vekt_current[B_prediction_start]-xbar_t_vekt[B_prediction_start])-
                      abs(m_t_vekt_previous[B_prediction_start]-xbar_t_vekt[B_prediction_start])))

}


print(paste("Predicted start bus - ", start_bus_prediction));
print(paste("Predicted end bus - ", end_bus_prediction));
print("Predicted faulted line/lines:")
regional_grid[(regional_grid$Start_Bus %in% start_bus_prediction & end_bus_prediction==regional_grid$End_bus),]








