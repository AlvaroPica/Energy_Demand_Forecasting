# energydemandforecasting
UPC KTH Master Thesis on Energy Demand Forecasting 

This project represent the work of the master thesis work performed to complete the Innoenergy MSc in Energy for Smart Cities master (UPC - Master in Energy Engineering and KTH - MSc in Energy Innovation)

It has two main objectives:

- O1: Design a methodology to assess the performance of Energy Demand Forecasting models currently running in tertiary buildings.
- O2: Create and assess alternatives Machine Learning (aNN, kNN, Ranfom Forest) models to improve the performance of the current models.

The final report can be found at:

- https://goo.gl/Xk8rE4
- This GitHUB repository under the name: "Master_Thesis_Alvaro_Picatoste"

The structure of the R programm is detailed below:

-> Main.R

  This file is meant for fullfilling objective O1. For more details download PDF file. 
  
-> totalconsumption.R

  This file tackles Objective O2. It pre-processes the data before applying any algorithm.
  
  -> workingcalendar.R : Specific building calendar to differentiate between day categories.
  -> weathertotal : Local Temperatures measured by on-site meter.
  
 -> ann.R ; rf. R; knn.R
 
 Scripts for creating the ML models to predict the energy consumption. Normalization and Dummy variables are considered in each of these    scripts.

-> gatherresults.R; thesisfunctions.R

 Scripts to define the variables and the way to present the results.
 
For more information please read the PDF file with the extended report.
