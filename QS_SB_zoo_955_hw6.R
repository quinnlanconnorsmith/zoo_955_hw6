#Zoo 955
#QS and SB HW6 - simulation 
#Trying this out with some data from Sapelo ghost crab burrows 
#10 total replicates, 4 plots (treatments) per replicate 
#5 replicates in north(cut dunes, sharp transition), 5 in south (dune transition)

library(tidyverse)
head(gc_dia_r)
summary(gc_dia_r)
bd <- gc_dia_r$burrow_diameter
hist(bd)
#Long right tail

#Let's start with a subset dataset and go from there, the following data are from the first day (pre-manip) 
#We can work up from here if everything works 
#We'll start by making a linear model, grabbing the beta's and sigma squared, then building the sim 
#Let's look at the data 

summary(test_sim_data)
#This can also give us min and max so we know what values we're looking to be within 
#We'll be looking at a simpler question, essentially "within the 10 replicate sites,is there a relationship
#between number of burrows and burrow density?" 
#This will be very similar to Zurr chapter 5

#The below code is just goofing around 
Beta <- vector(length = 10)
for (i in 1:10){
  Mi <- summary(lm(burrow_count ~ mean_burrow_diameter,
                   subset = (replicate==i), data=test_sim_data))
  Beta[i] <- Mi$coefficients[2, 1]}
Beta

#Alright, so we have some beta values, some are positive, some are negative 
#This is used for the 2-stage analysis method, let's get a little spicy 
#Now let's incorporate the 'set' into it, whether it be north or south 
fset <- factor(c(0,0,0,0,0,1,1,1,1,1))

lm1 <- lm(Beta~fset)
summary(lm1)
#So it looks like north/south does not have an influence on burrow density/diamater in each plot 
#Let's move onto a Linear mixed effects model 

library(nlme)

mlme1 <- lme(burrow_count ~ mean_burrow_diameter, random = ~1 | replicate,
             data = test_sim_data)
summary(mlme1)

#Simulating the data - the actual homework 
summary(test_sim_data)
bur_count <- test_sim_data$burrow_count
var(bur_count)
bur_diameter <-test_sim_data$mean_burrow_diameter
var(bur_diameter)
#Sweet, so here's our two variances 