##################################
#Code VAR using  output gap, interest rate, and pi
#Jung Hyun Kim, 17 may 2020
#Aymeric Lachaux, 17 may 2020 for forecasting part

##################################

setwd("~/Documents/MiE2/Semester 2/appliedmacro/Project")

##Install packages
x <- c("vars", "reshape2", "dplyr", "ggplot2")
install.packages(x) 
lapply(x, library, character.only = TRUE)

install.packages("devtools")
library(devtools)
install_github("kthohr/BMR")
install.packages("Rcpp")
library(Rcpp)
library(BMR)
install.packages("vars")
library(vars)

data <- read.csv(file = 'data_eurozone.csv')

##################################
#1. Load Data
##################################

var_data <- data.matrix(data[,2:4])

###########################
#2. Lag selection
###########################

# set up data for estimation using `VAR()`
# select optimal lag

var <- VARselect(var_data, lag.max = 4, type = "const")
var$criteria

# We choose lag(1)


##################################
#3. Impuse Responsive Function using VAR
##################################

# Using lag(1), we conduct a VAR estimation and build VAR object.

var_est <- VAR(y = var_data, p = 1)
var_obj <- new(cvar)
var_obj$build(var_data,TRUE,1)
var_obj$boot(10000)

# This is to save impulse resposnvie funtion for 20 time period estimated 6 ahead.

library(ggplot2)
library(grid)

png(file = "VAR.png", width = 800, height = 600)
ellen(var_obj,20,var_names=colnames(data[,2:4]),save=FALSE)
dev.off()


##################################
#3. Forecasting using VAR(1)
##################################
#forecast 2@

df <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("y", "r", "pi")
colnames(df) <- x


for (i in 64:88) {
  var_data <- data.matrix(data[1:i,2:4])
  var_est <- VAR(y = var_data, p = 1)
  var_obj <- new(cvar)
  var_obj$build(var_data,TRUE,1)
  var_obj$estim()
  var_obj$boot(10000)
  row <- forecast.Rcpp_cvar(var_obj,periods= 2, shocks=TRUE,var_names=colnames(data[,2:4]) ,save=FALSE, plot = FALSE)$forecast_mean
  df<-rbind(df,row)}


colnames(df) <- x
y2q <- data.frame(df$y)
r2q <- data.frame(df$r)
p2q <- data.frame(df$pi)

Y2Q <- y2q[1:2,]
for (i in 2:25) {
  pair <- 2*i
  mer <- pair-1
  y2q1 <- y2q[mer:pair,]
  Y2Q <- cbind(Y2Q,y2q1)}


R2Q <- r2q[1:2,]
for (i in 2:25) {
  pair <- 2*i
  mer <- pair-1
  r2q1 <- r2q[mer:pair,]
  R2Q <- cbind(R2Q,r2q1)}


P2Q <- p2q[1:2,]
for (i in 2:25) {
  pair <- 2*i
  mer <- pair-1
  p2q1 <- p2q[mer:pair,]
  P2Q <- cbind(P2Q,p2q1)}

P2Q<-P2Q[ , colSums(is.na(P2Q)) == 0]
Y2Q<-Y2Q[ , colSums(is.na(Y2Q)) == 0]
R2Q<-R2Q[ , colSums(is.na(R2Q)) == 0]
write.csv(P2Q, "P2Q.csv",row.names=FALSE)
write.csv(Y2Q, "Y2Q.csv",row.names=FALSE)
write.csv(R2Q, "R2Q.csv",row.names=FALSE)





#forecast 4Q

df <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("y", "r", "pi")
colnames(df) <- x

for (i in 64:88) {
  var_data <- data.matrix(data[1:i,2:4])
  var_est <- VAR(y = var_data, p = 1)
  var_obj <- new(cvar)
  var_obj$build(var_data,TRUE,1)
  var_obj$estim()
  var_obj$boot(10000)
  row <- forecast.Rcpp_cvar(var_obj,periods= 4, shocks=TRUE,var_names=colnames(data[,2:4]) ,save=FALSE, plot = FALSE)$forecast_mean
  df<-rbind(df,row)}

colnames(df) <- x
y2q <- data.frame(df$y)
r2q <- data.frame(df$r)
p2q <- data.frame(df$pi)

Y2Q <- y2q[1:4,]
for (i in 2:100) {
  pair <- 4*i
  mer <- pair-3
  y2q1 <- y2q[mer:pair,]
  Y2Q <- cbind(Y2Q,y2q1)}


R2Q <- r2q[1:4,]
for (i in 2:100) {
  pair <- 4*i
  mer <- pair-3
  r2q1 <- r2q[mer:pair,]
  R2Q <- cbind(R2Q,r2q1)}


P2Q <- p2q[1:4,]
for (i in 2:100) {
  pair <- 4*i
  mer <- pair-3
  p2q1 <- p2q[mer:pair,]
  P2Q <- cbind(P2Q,p2q1)}

P2Q<-P2Q[ , colSums(is.na(P2Q)) == 0]
Y2Q<-Y2Q[ , colSums(is.na(Y2Q)) == 0]
R2Q<-R2Q[ , colSums(is.na(R2Q)) == 0]
write.csv(P2Q, "P4Q.csv",row.names=FALSE)
write.csv(Y2Q, "Y4Q.csv",row.names=FALSE)
write.csv(R2Q, "R4Q.csv",row.names=FALSE)


#forecast 6Q

df <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("y", "r", "pi")
colnames(df) <- x

for (i in 64:88) {
  var_data <- data.matrix(data[1:i,2:4])
  var_est <- VAR(y = var_data, p = 1)
  var_obj <- new(cvar)
  var_obj$build(var_data,TRUE,1)
  var_obj$estim()
  var_obj$boot(10000)
  row <- forecast.Rcpp_cvar(var_obj,periods= 6, shocks=TRUE,var_names=colnames(data[,2:4]) ,save=FALSE, plot = FALSE)$forecast_mean
  df<-rbind(df,row)}


colnames(df) <- x
y2q <- data.frame(df$y)
r2q <- data.frame(df$r)
p2q <- data.frame(df$pi)

Y2Q <- y2q[1:6,]
for (i in 2:100) {
  pair <- 6*i
  mer <- pair-5
  y2q1 <- y2q[mer:pair,]
  Y2Q <- cbind(Y2Q,y2q1)}


R2Q <- r2q[1:6,]
for (i in 2:100) {
  pair <- 6*i
  mer <- pair-5
  r2q1 <- r2q[mer:pair,]
  R2Q <- cbind(R2Q,r2q1)}


P2Q <- p2q[1:6,]
for (i in 2:100) {
  pair <- 6*i
  mer <- pair-5
  p2q1 <- p2q[mer:pair,]
  P2Q <- cbind(P2Q,p2q1)}

P2Q<-P2Q[ , colSums(is.na(P2Q)) == 0]
Y2Q<-Y2Q[ , colSums(is.na(Y2Q)) == 0]
R2Q<-R2Q[ , colSums(is.na(R2Q)) == 0]
write.csv(P2Q, "P6Q.csv",row.names=FALSE)
write.csv(Y2Q, "Y6Q.csv",row.names=FALSE)
write.csv(R2Q, "R6Q.csv",row.names=FALSE)


##################################
#This csv file values will be used to calculate RMSE#
##################################