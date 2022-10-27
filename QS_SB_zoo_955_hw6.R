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
#To make things easier let's just focus on burrow count 
summary(test_sim_data2)
bur_count1 <- filter(test_sim_data2, group =="1")
bur_count2 <- filter(test_sim_data2, group =="2")
bur_count3 <- filter(test_sim_data2, group =="3")
bur_count4 <- filter(test_sim_data2, group =="4")
bur_count5 <- filter(test_sim_data2, group =="5")


bur_count1v <- bur_count1$burrow_count
var(bur_count1v)
summary(bur_count1)

bur_count2v <- bur_count2$burrow_count
var(bur_count2v)
summary(bur_count2)

bur_count3v <- bur_count3$burrow_count
var(bur_count3v)
summary(bur_count3)

bur_count4v <- bur_count4$burrow_count
var(bur_count4v)
summary(bur_count4)

bur_count5v <- bur_count5$burrow_count
var(bur_count5v)
summary(bur_count5)

#set regression coefficients
beta0 <- 1
beta1 <- 2


#Some baseline simulation of data 
x1=rnorm(1000,7.875,9.90)
x2=rnorm(1000,22.23, 33.17)
error=rnorm(1000,0,0.5)
#Generate the dependent variable (b0=1, b1=2, b2=3)
y1=beta0+(beta1*x1)+(beta2*x2)+error
#create the model
m1=lm(y1~x1+x2)
summary(m1)
plot(m1)


df<- data.frame(matrix(ncol=2, nrow=1000))
colnames(df)<- c("bur_count","bur_diameter")

for(i in 1:nrow(df)){
  #run function over 5 n levels and store probs for two model types as a list
  list<- (df$n[i])
  #extract first probability in list
  df$bur_count[i]<- as.numeric(unlist(list[1]))
  #extract second probability in list
 df$bur_diameter[i]<- as.numeric(unlist(list[2]))
}


