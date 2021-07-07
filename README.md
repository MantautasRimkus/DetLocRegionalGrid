# DetLocRegionalGrid

This repository provides the R code for the algorithms described in the paper “Detection and Localization of Faults in a Regional Power Grid”.

Code Author — Mantautas Rimkus, Colorado State University, mantautas.rimkus@colostate.edu

The code has been organized following the chapters of the paper.

For an illustration of the code, there are provided two training simulations (training_simulation1.csv and training_simulation2.csv).

Given .csv files follows the structure described in the paper, where each column represents different buses (pmus) and each row — observation starting at second 0 with 120 observations per second rate. There is a one difference in the data compare to the one described in the paper — training_simulation1.csv and training_simulation2.csv contains 12 minutes worth of data (instead of 15 minutes). This change was introduced due to the GitHub policy that allows to upload files of up to 100mb size.

In training_simulation1.csv, line 7 is the true faulted line.
In training_simulation2.csv, line 100 is the true faulted line.
Faults were applied at second 600.

One need to know information about a grid connection in order to run provided localization algorithm. For the regional grid described in the paper, information stored in regional_grid.csv.
