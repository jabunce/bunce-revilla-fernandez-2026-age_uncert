# clear the workspace
rm (list = ls(all=TRUE))

# Load packages
# if you don't have one or more of these packages, you can install them like this: install.packages(c("lattice", "MASS"))
library(rstan)
library(rethinking)
library(lattice)
library(MASS) #for mvrnorm function
library(graphics)
library(grid)
library(boot) #for inverse logit function
library(plyr) #for Catalina's simulation code

library(cmdstanr)
library(posterior)
library(bayesplot)
library(dplyr) # pull function to extract data from model fit "tibbles"
library(stats) # uniroot function to solve equations
#library(HelpersMG) # for male and female symbols

# Set working directory: the path to the folder containing the subfolders Code, Plots, and Data
# on mac
#setwd("/Users/johnbunce/Dropbox/Matsigenka-Mestizo_project_2014/health_analysis/age_uncertainty_analysis/growth_22jul25/Matsigenka/analysis_h_fem_Github")
# on pc
setwd("C:/Users/John/Dropbox/Matsigenka-Mestizo_project_2014/health_analysis/age_uncertainty_analysis/growth_22jul25/Matsigenka/analysis_h_fem_Github")
# on server
#setwd("~/Matsigenka_age_uncert/analysis_hw_fem")

# Load helper functions
source("./Code/Functions.R")

# Prepare data (this can take a minute to process)
#source("./Code/PrepareData.R")




# 1. ############# simulate dataset #############

Nsims <- 30 # number of trajectories to simulate
Nages <- 25 # number of times to measure each individual (1 measurement per year) in full simulation

pNages <- 2 # number of times to measure each indiv in temporally sparse dataset
intAges <- 2 # number of years between measurements in sparse dataset

xsd <- 0.1  # multiply this by age since conception to get stdev for Normal distribution to model estimated year of conception (epsilon in the manuscript)

gsd <- 0.01 # stdev (on log scale) of group-level offset to the baseline trajectory parameter values in PriorPredictGroupStdev5.R

source("./Code/SimulateData_sparse.R") # can take a minute to run

SimData <- fullSimData # which simulated dataset to use: fullSimData (full dataset), pSimData (temporally sparse), xSimData (cross sectional)
source("./Code/PrepareData_sims.R")

# design priors
source("./Code/PriorPredictGroupStdev5.R")




# 2. ############# Plot simulated data set: Figure 1 in main text ####################

# In section 1 above, set Nsims <- 30, Nages <- 25, SimData <- fullSimData
source("./Code/Plot_sim_data.R")




# 3. ############## fit model that does not incorporate age uncertainty ####################

# Cross-sectional data, model with no age uncertainty

# 3a. In section 1 above, set Nsims <- 10, SimData <- xSimData, xsd <- 0.1, gsd <- 0.01
# 3b. Set the file names below to something meaningfull. Run section 1 (above) and then section 3 (this entire section).
# 3c. Repeat 3b four more times, each time changing the file names below. 
# 3d. In section 1 above, set Nsims <- 100, SimData <- xSimData, xsd <- 0.1, gsd <- 0.01
# 3e. Repeat 3b and 3c
# 3f. In section 1 above, set Nsims <- 200, SimData <- xSimData, xsd <- 0.1, gsd <- 0.01
# 3g. Repeat 3b and 3c
# The end result of these steps will be model results for 5 independently simulated cross-sectional datasets of 10 individuals,
# 5 independently simulated cross-sectional datasets of 100 individuals, and 5 independently simulated cross-sectional datasets of 200 individuals. 

# file names to save objects in project folder
modfit_name <- "m5_fit_sim10p10-1nu_10.RDS" 			# file name for the stan output for the fit model (a large file)
modpost_name <- "post5_sim10p10-1nu_10.RDS" 			# file name for the output draws for the fit model, needed for plotting (a large file) 
parlist_name <- "post5_sim10p10-1nu_paramList.RDS" 		# file name for object containing posterior samples for parameters of fit model, needed for plotting
comfem_name <- "post5_sim10p10-1nu_Comfem.RDS" 			# file name for object containing simulated data to which model is fit, needed for plotting 
trace_name <- "./Plots/traces_m5_sim10p10-1nu_10.pdf" 	# path/file name for the trace plots for the fit model (a large pdf) 

samps <- 4000	# number of mcmc samples
num_chains <- 4 # number of mcmc chains

# fit stan model without age uncertainty (m5)
source("./Code/FitMainModel5_sims_nouncert.R")



# 4. ############## fit model that does incorporate age uncertainty ####################

# Cross-sectional and sparse longitudinal data, model with age uncertainty

# 4a. In section 1 above, set Nsims <- 100, SimData <- xSimData, xsd <- 0.1, gsd <- 0.01
# 4b. Set the file names below to something meaningfull. Run section 1 (above) and then section 4 (this entire section).
# 4c. Repeat 4b four more times, each time changing the file names below. 
# 4d. In section 1 above, set Nsims <- 50, pNages <- 2, intAges <- 2, SimData <- pSimData, xsd <- 0.1, gsd <- 0.01
# 4e. Repeat 4b and 4c
# 4f. In section 1 above, set Nsims <- 10, SimData <- xSimData, xsd <- 0.1, gsd <- 0.01
# The end result of these steps will be model results for 5 independently simulated cross-sectional datasets of 100 individuals
# modeled with age uncertainty, 5 independently simulated sparse longitudinal datasets of 50 individuals each measured twice at a 2-year interval,
# and one simulated cross-sectional dataset of 10 individuals modeled with age uncertainty. 

# file names to save objects in project folder
modfit_name <- "m6_fit_sim50p101_1.RDS" 			# file name for the stan output for the fit model (a large file)
modpost_name <- "post6_sim50p101_1.RDS" 			# file name for the output draws for the fit model, needed for plotting (a large file)  
parlist_name <- "post6_sim50p101_1_paramList.RDS" 	# file name for object containing posterior samples for parameters of fit model, needed for plotting
comfem_name <- "post5_sim50p101_1_Comfem.RDS" 		# file name for object containing simulated data to which model is fit, needed for plotting 
trace_name <- "./Plots/traces_m6_sim50p101_1.pdf" 	# path/file name for the trace plots for the fit model (a large pdf)

samps <- 4000	# number of mcmc samples
num_chains <- 4 # number of mcmc chains

# fit stan model with age uncertainty (m6)
source("./Code/FitMainModel5_sims.R")



# 5. ############## fit model that does incorporate age uncertainty only to height data ####################

# Cross-sectional height (not weight) data, model with age uncertainty

# 5a. In section 1 above, set Nsims <- 100, SimData <- xSimData, xsd <- 0.1, gsd <- 0.01
# 5b. Set the file names below to something meaningfull. Run section 1 (above) and then section 5 (this entire section).
# 5c. Repeat 5b four more times, each time changing the file names below. 
# The end result of these steps will be model results for 5 independently simulated cross-sectional datasets of 100 individuals
# modeled with age uncertainty, but without weight data 

# file names to save objects in project folder
modfit_name <- "m7_fit_sim100nw101_1.RDS" 				# file name for the stan output for the fit model (a large file)
modpost_name <- "post7_sim100nw101_1.RDS" 				# file name for the output draws for the fit model, needed for plotting (a large file) 
parlist_name <- "post7_sim100nw101_1_paramList.RDS" 	# file name for object containing posterior samples for parameters of fit model, needed for plotting
comfem_name <- "post7_sim100nw101_1_Comfem.RDS" 		# file name for object containing simulated data to which model is fit, needed for plotting
trace_name <- "./Plots/traces_m7_sim100nw101_1.pdf" 	# path/file name for the trace plots for the fit model (a large pdf) 

samps <- 4000	# number of mcmc samples
num_chains <- 4 # number of mcmc chains

# fit stan model with age uncertainty, but only to height data (m7)
source("./Code/FitMainModel5_sims_miss.R")



# 6. ############## fit model that does not incorporate age uncertainty ####################

# Longitudinal data, model with no age uncertainty

# 6a. In section 1 above, set Nsims <- 10, pNages <- 10, intAges <- 1, SimData <- pSimData, xsd <- 0.1, gsd <- 0.01
# 6b. Set the file names below to something meaningfull. Run section 1 (above) and then section 6 (this entire section).
# 6c. Repeat 6b four more times, each time changing the file names below. 
# 6d. In section 1 above, set Nsims <- 20, pNages <- 5, intAges <- 2, SimData <- pSimData, xsd <- 0.1, gsd <- 0.01
# 6e. Repeat 6b and 6c
# 6f. In section 1 above, set Nsims <- 30, Nages <- 25, SimData <- fullSimData, xsd <- 0.1, gsd <- 0.01
# 6g. Repeat 6b and 6c
# The end result of these steps will be model results for 5 independently simulated longitudinal datasets of 10 individuals measured yearly for 10 years,
# 5 independently simulated longitudinal datasets of 20 individuals measured 5 times at 2-year intervals, and 5 independently simulated longitudinal
# datasets of 30 individuals measured yearly for 25 years. 

# file names to save objects in project folder
modfit_name <- "m5_fit_sim10p10-1nu_1.RDS" 			# file name for the stan output for the fit model (a large file)
modpost_name <- "post5_sim10p10-1nu_1.RDS" 			# file name for the output draws for the fit model, needed for plotting (a large file) 
parlist_name <- "post5_sim10p10-1nu_paramList.RDS" 		# file name for object containing posterior samples for parameters of fit model, needed for plotting
comfem_name <- "post5_sim10p10-1nu_Comfem.RDS" 			# file name for object containing simulated data to which model is fit, needed for plotting 
trace_name <- "./Plots/traces_m5_sim10p10-1nu_1.pdf" 	# path/file name for the trace plots for the fit model (a large pdf) 

samps <- 4000	# number of mcmc samples
num_chains <- 4 # number of mcmc chains

# fit stan model without age uncertainty (m5)
source("./Code/FitMainModel5_sims_nouncert.R")



# 7. ############## make plots ####################

# Plot simulated data with age uncertainty
# Figure 2 and A.3
# model output files from section 4 (above): cross-sectional dataset with 10 individuals modeled with age uncertainty
parlist_name1 <- "post6_sim10x101_1_paramList.RDS"
comfem_name1 <- "post6_sim10x101_1_Comfem.RDS"
modpost_name1 <- "post6_sim10x101_1.RDS"
# model output files for section 4 (above): cross-sectional dataset with 100 individuals modeled with age uncertainty
parlist_name2 <- "post6_sim100x101_1_paramList.RDS"
comfem_name2 <- "post6_sim100x101_1_Comfem.RDS"
modpost_name2 <- "post6_sim100x101_1.RDS"
source("./Code/Plot_age_uncert_nu.R") # real and observed data
source("./Code/Plot_age_uncert.R") # for appendix, real, observed, and estimated data



# model output files from section 3 (above): cross-sectional dataset with 10 individuals modeled without age uncertainty
modpost_name1 <- "post5_sim10x101_1.RDS"
modpost_name2 <- "post5_sim10x101_2.RDS"
modpost_name3 <- "post5_sim10x101_3.RDS"
modpost_name4 <- "post5_sim10x101_4.RDS"
modpost_name5 <- "post5_sim10x101_5.RDS"
comfem_name1 <- "post5_sim10x101_1_Comfem.RDS"
# model output files from section 3 (above): cross-sectional dataset with 100 individuals modeled without age uncertainty
modpost_name6 <- "post5_sim100x101_1.RDS"
modpost_name7 <- "post5_sim100x101_2.RDS"
modpost_name8 <- "post5_sim100x101_3.RDS"
modpost_name9 <- "post5_sim100x101_4.RDS"
modpost_name10 <- "post5_sim100x101_5.RDS"
comfem_name2 <- "post5_sim100x101_1_Comfem.RDS"
# model output files from section 3 (above): cross-sectional dataset with 200 individuals modeled without age uncertainty
modpost_name11 <- "post5_sim200x101_1.RDS"
modpost_name12 <- "post5_sim200x101_2.RDS"
modpost_name13 <- "post5_sim200x101_3.RDS"
modpost_name14 <- "post5_sim200x101_4.RDS"
modpost_name15 <- "post5_sim200x101_5.RDS"
comfem_name3 <- "post5_sim200x101_1_Comfem.RDS"
source("./Code/PlotCombinedModelOutput5_sims_nu.R") # x-sectional: 10, 100, 200 ### Figure 3
source("./Code/PlotAll_chars_sims_nu.R") 			# trajectory characteristics ### Figure 4
source("./Code/PlotAll_params_phases_sims_nu.R") 	# individual parameter posteriors ### Figure A.1
source("./Code/i_params_sims_nu.R") 				# i parameter posteriors ### Figure A.2
source("./Code/PlotAll_params_sims_nu.R") 			# overall parameter estimates ### Figure 5  


# model output files from section 4 (above): cross-sectional dataset with 100 individuals modeled with age uncertainty
modpost_name1 <- "post6_sim100x101_1.RDS"
modpost_name2 <- "post6_sim100x101_2.RDS"
modpost_name3 <- "post6_sim100x101_3.RDS"
modpost_name4 <- "post6_sim100x101_4.RDS"
modpost_name5 <- "post6_sim100x101_5.RDS"
comfem_name1 <- "post6_sim100x101_1_Comfem.RDS"
# model output files from section 5 (above): cross-sectional dataset with 100 individuals modeled with age uncertainty, height only
modpost_name6 <- "post7_sim100x101_1.RDS"
modpost_name7 <- "post7_sim100x101_2.RDS"
modpost_name8 <- "post7_sim100x101_3.RDS"
modpost_name9 <- "post7_sim100x101_4.RDS"
modpost_name10 <- "post7_sim100x101_5.RDS"
comfem_name2 <- "post7_sim100x101_1_Comfem.RDS"
# model output files from section 4 (above): longitudinal dataset of 50 individuals each measured twice at a 2-year interval, modeled with age uncertainty
modpost_name11 <- "post6_sim50p101_1.RDS"
modpost_name12 <- "post6_sim50p101_2.RDS"
modpost_name13 <- "post6_sim50p101_3.RDS"
modpost_name14 <- "post6_sim50p101_4.RDS"
modpost_name15 <- "post6_sim50p101_5.RDS"
comfem_name3 <- "post6_sim50p101_1_Comfem.RDS"
source("./Code/PlotCombinedModelOutput5_sims_100.R") # for appendix, 100: estimated age, no weight, sparse 50 ### Figure A.4
source("./Code/PlotAll_chars_sims_100.R") 			 # trajectory characteristics ### Figure A.5
source("./Code/PlotAll_params_phases_sims_100.R") 	 # individual parameter posteriors ### Figure A.6
source("./Code/i_params_sims_100.R") 				 # i parameter posteriors ### Figure A.7
source("./Code/PlotAll_params_sims_100.R") 			 # overall parameter estimates ### Figure A.8


# model output files from section 6 (above): longitudinal dataset with 10 individuals measured yearly for 10 years, modeled without age uncertainty
modpost_name1 <- "post5_sim10p10-1_1.RDS"
modpost_name2 <- "post5_sim10p10-1_2.RDS"
modpost_name3 <- "post5_sim10p10-1_3.RDS"
modpost_name4 <- "post5_sim10p10-1_4.RDS"
modpost_name5 <- "post5_sim10p10-1_5.RDS"
comfem_name1 <- "post5_sim10p10-1_1_Comfem.RDS"
# model output files from section 6 (above): longitudinal dataset with 20 individuals measured 5 times at 2-year intervals, modeled without age uncertainty
modpost_name6 <- "post5_sim20p5-2_1.RDS"
modpost_name7 <- "post5_sim20p5-2_2.RDS"
modpost_name8 <- "post5_sim20p5-2_3.RDS"
modpost_name9 <- "post5_sim20p5-2_4.RDS"
modpost_name10 <- "post5_sim20p5-2_5.RDS"
comfem_name2 <- "post5_sim20p5-2_Comfem.RDS"
# model output files from section 6 (above): longitudinal dataset with 30 individuals measured yearly for 25 years, modeled without age uncertainty
modpost_name11 <- "post5_sim30f101_1.RDS"
modpost_name12 <- "post5_sim30f101_2.RDS"
modpost_name13 <- "post5_sim30f101_3.RDS"
modpost_name14 <- "post5_sim30f101_4.RDS"
modpost_name15 <- "post5_sim30f101_5.RDS"
comfem_name3 <- "post5_sim30f101_1_Comfem.RDS"
source("./Code/PlotCombinedModelOutput5_sims_l.R") # for appendix, longitudinal: 30 full, 10 10measures every year, 20 5measures every 2 years ### Figure A.9
source("./Code/PlotAll_chars_sims_l.R") 		   # trajectory characteristics ### Figure A.10
source("./Code/PlotAll_params_phases_sims_l.R")    # individual parameter posteriors ### Figure A.11
source("./Code/i_params_sims_l.R") 				   # i parameter posteriors ### Figure A.12
source("./Code/PlotAll_params_sims_l.R") 		   # overall parameter estimates ### Figure A.13







