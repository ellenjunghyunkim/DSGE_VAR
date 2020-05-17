##################################
#Code to create dataset for euro area: output gap, interest rate, and inflation
#Aymeric Lachaux, May 2020
##################################

#preparation
rm(list=ls())
library(readxl)
library(tempdisagg)
library(tsbox)
library(zoo)
library(dplyr)

##################################
#1. Output gap
##################################

#extraction of annual potential gdp downloaded from ameco
gdp_potential <-read_xlsx("gdp_potential.xlsx")

#interpolation to quartely data using denton-cholette method
gdp_quarter <- td(gdp_potential ~ 1, to = "quarterly", method = "denton-cholette")
gdp_quarter <- predict(gdp_quarter)

#extraction of quartely real gdp downloaded from dbnomics
gdp_real <-read_xlsx("gdp_real.xlsx")

#conversion periods
gdp_quarter$time <- as.yearqtr(gdp_quarter$time, format = "%Y-%m-%d")
gdp_real$period <- as.yearqtr(gdp_real$period, format = "%Y-Q%q")

#change names
colnames(gdp_real)[1] <- "time"
colnames(gdp_real)[2] <- "gdp_r"
colnames(gdp_quarter)[2] <- "gdp_p"

#merge
gdp_data <- merge(gdp_quarter, gdp_real, by=c("time"))

#conversion units
gdp_data$gdp_p<- gdp_data$gdp_p*1000

#output gap 
gdp_data$gdp_gap <- gdp_data$gdp_r/gdp_data$gdp_p - 1
#output gap (log difference)
gdp_data$output_gap <-log(gdp_data$gdp_r)-log(gdp_data$gdp_p)

##################################
#2. Inflation
##################################

#extraction of quartely inflation downloaded from dbnomics
inflation <-read_xlsx("inflation.xlsx")

#format
inflation$period <- as.yearqtr(inflation$period, format = "%Y-Q%q")

#name
colnames(inflation)[1] <- "time"
colnames(inflation)[2] <- "inflation"

#merge
gdp_data <- merge(gdp_data, inflation, by=c("time"))

##################################
#3. Interest Rate
##################################

#extraction of quartely interest rate downloaded from dbnomics
interest_rate <-read_xlsx("interest_rate.xlsx")

#name
colnames(interest_rate)[1] <- "time"
colnames(interest_rate)[2] <- "interest_rate"

#format
interest_rate$time <- as.yearqtr(interest_rate$time, format = "%Y-Q%q")

#merge
gdp_data <- merge(gdp_data, interest_rate, by=c("time"))

##################################
#4. Export
##################################

#select columns
data_eurozone = subset(gdp_data, select = c("time", "output_gap", "interest_rate", "inflation"))

#percent
data_eurozone$output_gap <- data_eurozone$output_gap*100

#preparation for matlab
colnames(data_eurozone)[2] <- "y"
colnames(data_eurozone)[3] <- "r"
colnames(data_eurozone)[4] <- "pi"

#export
write.csv(data_eurozone, "data_eurozone.csv", row.names=FALSE)

#preparation for matlab
data_eurozone$time <- NULL
write.csv(data_eurozone, "eurozone.csv", row.names=FALSE)





