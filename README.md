# DetLocRegionalGrid

This repository provides the R code for the algorithms described in the paper "Detection and Localization of Faults in a Regional Power Grid". 

Code Author - Mantautas Rimkus, Colorado State University, mantautas.rimkus@colostate.edu

The code is organized following the chapters of the paper.

For an illustration of the code, two training simulations is provided (training_simulation1.csv and training_simulation2.csv).

Given .csv files follows the structure described in the paper, where each column represents different buses (pmus) and each row - observation starting at second 0 with 120 observations per second rate. 

In training_simulation1.csv, line 7 is the true faulted line.
In training_simulation2.csv, line 100 is the true faulted line.
Faults were applied at second 600.

Information about grids connections is needed to run the localization algorithm.  For the regional grid described in the paper, information is stored in regional_grid.csv. 
