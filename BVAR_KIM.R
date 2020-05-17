##################################
#Code BVAR using  output gap, interest rate, and inflation
#JUng Hyun Kim, 17 MAY 2020
##################################

setwd("~/Documents/MiE2/Semester 2/appliedmacro/Project")

##Install packages
install.packages("RcppArmadillo")
install.packages("ggplot2")
library(RcppArmadillo)
library(ggplot2)
install.packages("devtools")
library(devtools)
install_github("kthohr/BMR")
install.packages("Rcpp")
library(Rcpp)
library(BMR)
install.packages("vars")
library(vars)
install.packages("BVAR")
library(BVAR)


##################################
#1. Load Data
##################################
data <- read.csv(file = 'data_eurozone.csv')
bvar_data <- data.matrix(data[,2:4])



###########################
#2. Lag selection
###########################

# Compute BVAR to obtain marginal likelihood
# set up data for estimation using `bvar()`
# select optimal lag
bvar1 <- bvar(data = data[,2:4], lags = 1,
              n_draw = 1000, n_burn = 500, n_thin = 15, verbose = FALSE)
bvar2 <- bvar(data = data[,2:4], lags = 2,
              n_draw = 1000, n_burn = 500, n_thin = 15, verbose = FALSE)
bvar3 <- bvar(data = data[,2:4], lags = 3,
              n_draw = 1000, n_burn = 500, n_thin = 15, verbose = FALSE)
bvar4 <- bvar(data = data[,2:4], lags = 4,
              n_draw = 1000, n_burn = 500, n_thin = 15, verbose = FALSE)

mean(bvar1$ml)
mean(bvar2$ml)
mean(bvar3$ml)
mean(bvar4$ml)

##################################
#3. Impuse Responsive Function using BVAR
##################################

# Using BVAR(4), we conduct a BVAR estimation and build BVAR object.
# We set our prior according to the famous minnesota prior.

coef_prior <- c(0.9,0.9,0.9)
bvar_obj <- new(bvarm)
bvar_obj$build(bvar_data,TRUE,4)
bvar_obj$prior(coef_prior,1,1,0.5,0.5,100.0,1.0)
bvar_obj$gibbs(10000)

library(ggplot2)
library(grid)
png(file = "BVAR.png", width = 800, height = 600)
hi<- ellen(bvar_obj,20,var_names=colnames(data[,2:4]),save=FALSE)
dev.off()


##################################
#3. Forecasting using BVAR(4)
##################################
#forecast 2@

df <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("y", "r", "pi")
colnames(df) <- x


for (i in 64:88) {
  bvar_data <- data.matrix(data[1:i,2:4])
  bvar_obj$build(bvar_data,TRUE,4)
  bvar_obj$prior(coef_prior,1,1,0.5,0.5,100.0,1.0)
  bvar_obj$gibbs(10000)
  row <- forecast.Rcpp_bvarm(bvar_obj,periods= 2, shocks=TRUE,var_names=colnames(data[,2:4]) ,save=FALSE, plot = FALSE)$forecast_mean
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
coef_prior <- c(0.9,0.9,0.9)
bvar_obj <- new(bvarm)
df <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("y", "r", "pi")
colnames(df) <- x

for (i in 64:88) {
  bvar_data <- data.matrix(data[1:i,2:4])
  bvar_obj$build(bvar_data,TRUE,4)
  bvar_obj$prior(coef_prior,1,1,0.5,0.5,100.0,1.0)
  bvar_obj$gibbs(10000)
  row <- forecast.Rcpp_bvarm(bvar_obj,periods= 4, shocks=TRUE,var_names=colnames(data[,2:4]) ,save=FALSE, plot = FALSE)$forecast_mean
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
coef_prior <- c(0.9,0.9,0.9)
bvar_obj <- new(bvarm)
df <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("y", "r", "pi")
colnames(df) <- x

for (i in 64:88) {
  bvar_data <- data.matrix(data[1:i,2:4])
  bvar_obj$build(bvar_data,TRUE,4)
  bvar_obj$prior(coef_prior,1,1,0.5,0.5,100.0,1.0)
  bvar_obj$gibbs(10000)
  row <- forecast.Rcpp_bvarm(bvar_obj,periods= 6, shocks=TRUE,var_names=colnames(data[,2:4]) ,save=FALSE, plot = FALSE)$forecast_mean
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



IRF(bvar_obj,20,var_names=colnames(USMacroData),save=FALSE)
plot(bvar_obj,var_names=colnames(USMacroData),save=FALSE)
forecast(bvar_obj,shocks=TRUE,var_names=colnames(USMacroData),back_data=10,save=FALSE)

dd<- forecast.Rcpp_bvarm(bvar_obj,periods=24, shocks=FALSE,var_names=colnames(data[,2:4]),back_data=64,save=FALSE)
help("forecast.Rcpp_bvarm")

ddddd<- data.frame(dd$forecast_mean)
colnames(ddddd) <- colnames(data2)

write.csv(ddddd, file = "forecast_bvar.csv")



##################################
#This csv file values will be used to calculate RMSE#
##################################