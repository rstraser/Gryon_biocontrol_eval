#********************
# required packages
#********************

# load libraries required for analyses
library(ggplot2)
library(viridis)
library(pipeR)
library(ggpubr)
library(survival)
library(survminer)
library(plotrix)
library(plyr)
library(doBy)
library(lme4)
library(multcomp)
library(Rmisc) 


#********************
# setwd
#********************

# set working directory
setwd("~/Documents/Documents - Rob’s MacBook Pro/Wilson Lab/Dissertation/Ch1_biocontrol/Gryon life parameters/life history/null parameters")



#********************
# useful functions
#********************

# function to calculate mean and standard deviation or standard error for each group
# varname: the name of a column containing the variable to be summarized
# groupnames: vector of column names to be used as grouping variables

# for generating standard deviation
# data_summary <- function(data, varname, groupnames){
#  require(plyr)
#  summary_func <- function(x, col){
#    c(mean = mean(x[[col]], na.rm=TRUE),
#      sd = sd(x[[col]], na.rm=TRUE))}
#  data_sum<-ddply(data, groupnames, .fun=summary_func,
#                  varname)
#  data_sum <- rename(data_sum, c("mean" = varname))
#  return(data_sum)}

# for generating standard error
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      se = sd(x[[col]]/sqrt(length(x[[col]])), na.rm=TRUE))}
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)}
