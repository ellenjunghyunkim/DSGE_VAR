##################################
#Code VAR using  output gap, interest rate, and inflation
#JUng Hyun Kim, 26 april 2020
##################################

setwd("~/Documents/MiE2/Semester 2/appliedmacro/Project")

##Install packages
x <- c("vars", "reshape2", "dplyr", "ggplot2", "dynlm")
install.packages(x) 
lapply(x, library, character.only = TRUE)
data <- read.csv(file = 'data_eurozone.csv')

### If ever I want to use consumpton data
###headers = read.csv(file = 'data (1).csv', skip = 1, header = F, nrows = 1, as.is = T)
###consumption = read.csv(file = 'data (1).csv', skip = 5, header = F)
###colnames(consumption)= c("period", "consumption")


##################################
#1. Output gap
##################################

# load the U.S. macroeconomic data set
# format the date column
# define GDP as ts object

OutputGap <- ts(data$output_gap,
          start = c(1998, 1), 
          end = c(2019, 4), 
          frequency = 4)

# define OutputGap growth as a ts object
GDPGrowth <- ts(data$inflation,
                start = c(1998, 1), 
                end = c(2019, 4), 
                frequency = 4)


# 10-years Treasury bonds interest rate as a 'ts' object
Interest_rate <- ts(data$interest_rate, 
             start = c(1998, 1), 
             end = c(2019, 4), 
             frequency = 4)


###########################
## Dynamic Linear Models ##
###########################

# load the U.S. macroeconomic data set
# format the date column
# Estimate both equations using 'dynlm()'



VAR_EQ1 <- dynlm(GDPGrowth ~ L(GDPGrowth, 1:2) + L(Interest_rate, 1:2) +L(OutputGap, 1:2), 
                 start = c(1998, 1), 
                 end = c(2019, 4))

VAR_EQ2 <- dynlm(Interest_rate ~ L(GDPGrowth, 1:2) + L(Interest_rate, 1:2)+L(OutputGap, 1:2),
                 start = c(1998, 1),
                 end = c(2019, 4))
VAR_EQ3 <- dynlm(OutputGap ~ L(GDPGrowth, 1:2) + L(Interest_rate, 1:2)+L(OutputGap, 1:2),
                 start = c(1998, 1),
                 end = c(2019, 4))


VAR_EQ4 <- dynlm(GDPGrowth ~ L(GDPGrowth) + L(Interest_rate) +L(OutputGap), 
                 start = c(1998, 1), 
                 end = c(2019, 4))

# rename regressors for better readability
names(VAR_EQ1$coefficients) <- c("Intercept","Growth_t-1", 
                                 "Growth_t-2", "Interest_rate_t-1", "Interest_rate_t-2", "Output_gap_t-1", "Output_gap_t-2")
names(VAR_EQ2$coefficients) <- names(VAR_EQ1$coefficients)
names(VAR_EQ3$coefficients) <- names(VAR_EQ1$coefficients)

names(VAR_EQ4$coefficients) <- c("Intercept","Growth_t-1", "Interest_rate_t-1", "Output_gap_t-1")

# robust coefficient summaries
coeftest(VAR_EQ1, vcov. = sandwich)
coeftest(VAR_EQ2, vcov. = sandwich)
coeftest(VAR_EQ3, vcov. = sandwich)

coeftest(VAR_EQ4, vcov. = sandwich)



# set up data for estimation using `VAR()`
VAR_data <- window(ts.union(GDPGrowth, Interest_rate, OutputGap), start = c(1998, 1), end = c(2019, 4))

# estimate model coefficients using `VAR()`
VAR_est <- VAR(y = VAR_data, p = 1)
VAR_est

# obtain the adj. R^2 from the output of 'VAR()'
summary(VAR_est$varresult$GDPGrowth)$adj.r.squared
summary(VAR_est$varresult$Interest_rate)$adj.r.squared
summary(VAR_est$varresult$OutputGap)$adj.r.squared

forecasts <- predict(VAR_est)
forecasts

par(mar = rep(2, 4))
plot(forecasts)
##################################
#3. Var representaition
##################################

varmodel <- dplyr::select(data, output_gap, interest_rate, inflation) %>%
  VAR(p = 2, type = "const")

summary(varmodel)

AIC(varmodel)

###Impulse response function
varirfs <- irf(varmodel, n.ahead = 20)
plot(varirfs)
##################################
#2. I don't need this but for fun.
##################################
install.packages("tidyverse")
library(tidyverse)

library(lubridate)

myplot <- ggplot(data = data, aes(x=time, y=output_gap, group=1)) +
  geom_line()
myplot <- myplot + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  #scale_y_continuous(labels = comma)
  theme(axis.text.x=element_text(angle=-45, hjust=0.001))