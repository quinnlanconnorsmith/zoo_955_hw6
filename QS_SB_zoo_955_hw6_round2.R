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



#burrow count Min = 4, Mean = 7.875, Max = 17
df <- data.frame(matrix(ncol = 2, nrow=1000))
colnames(df)<-c("bur_count", "bur_diameter")

#set regression coefficients

beta0 <- 1
beta1 <- 2


#make a function to simulate data, run models, and calculate probability of rejecting null
e1 = rnorm(1000, 7.875, 9.9)
e2 = rnorm(1000, 22.23, 33.17)
error = rnorm(1000, 0, 0.5)

y1 = beta0+(beta1*e1)+(beta2*e2)+error

m1 = lm(y1~e1+e2)
summary(m1)
plot(m1)


df$bur_count <- m1[[5]]

for(i in 1:nrow(df)){
 
  
}


#Commented out because this is all in the sim function below, left because I think its easier to follow here
# nsite = 10
# nplot = 4 
# mean of burrow count
# mu.bur = mean(bur_count)
# standard deviation of burrow count
# sd.bur = sd(bur_count)
# variance of burrow count
# var.bur = var(bur_count)
# 
# site = rep(LETTERS[1:nsite], each = nplot) 
# plot = letters[1:(nstand*nplot)]
# 
# site.eff = rnorm(nsite, mu.plot, sd.plot)
# site.eff = rep(site.eff, each = nplot) 
# 
# plot.eff = rnorm(nsite*nplot, mu.plot, sd.plot)
# 
# dat = data.frame(site, site.eff, plot, plot.eff)


sim = function(nsite = 10, nplot = 4, mu = mean(bur_count), sigma_s = sd(bur_count), sigma = var(bur_count)){
  #calculate site effects by running r norm for a given site with mu and sigma define above
  #Rep function repeats the rnorm calculation for ethe number of plots 
  site.eff = rep( rnorm(nsite, mu, sigma_s), each = nplot)
  #Applies captial letter identifiers to each of the 10 sites
  site = rep(LETTERS[1:nsite], each = nplot)
  #Runs rnorm again but this time for each plot individually 
  plot.eff = rnorm(nsite*nplot, mu, sigma)
  #Creates a response category based off of the equation yt=μ+(bs)t+ϵt 
  resp = mu + site.eff + plot.eff
  dat = data.frame(site, resp)
  #runs a lme off of the resp variable using Site as the random 
  lme(resp ~ 1, random = ~1|site, data = dat)
}

#Creates a data frame to house infromation from each simulation, nrow determines number of times run
df <- data.frame(matrix(ncol = 3, nrow=100))
colnames(df)<-c("fixed.eff", "random.eff", "p.value")
#Used for tidy funciton in for loop, useful for extracting value from list
library(broom.mixed)
for (i in 1:nrow(df)){
  #Defines 1 simulation run for each loop, otherwise simu would re-run multiple times in the same loop
  s <- sim()
  #extracts fixed effects from simulation
  fixed <- tidy(s, effects = "fixed")
  #extracts random effects from simulation
  random <- tidy(s, effects = "ran_pars", scales = "vcov")
  #pulls values for each column
  df$fixed.eff[i] <- fixed$estimate[1]
  df$random.eff[i] <- random$estimate[1]
  df$p.value[i] <- fixed$p.value[1]
}










