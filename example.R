#Example from IH and RB

require(tidyverse)

#create a data frame to store simulation data
p<- data.frame(matrix(ncol=2, nrow=1000))
colnames(p)<- c("p.plm","p.llm")

#set regression coefficients
beta0 <- 1
beta1 <- 0.2

#make a function to simulate data, run models, and calculate probability of rejecting null
prob.rej.null<- function(n){
  for(i in 1:nrow(p)){
    
    #generate covariate values
    x <- runif(n=n, min=10, max=11.5)
    #compute mu's
    mu<- exp(beta0 + beta1 * x)
    #generate Y-values
    y <- rpois(n=n, lambda=mu)
    #organize generated data in data frame
    data <- data.frame(y=y, x=x, logy=log(y))
    #generalized linear model on un-transformed count data
    plm <- glm(y ~ x, family = poisson, data = data)
    #linear model on log-transformed count data
    llm <- glm(logy ~ x, family = gaussian, data = data)
    #extract p-values from both model types
    p$p.plm[i]<- summary(plm)$coefficients[2,4]
    p$p.llm[i]<- summary(llm)$coefficients[2,4]
    
  }
  #calculate the number of times out of 1000 where the null was rejected
  #at alpha 0.05
  prob.plm<- sum(p$p.plm <0.05)/1000
  prob.llm<- sum(p$p.llm <0.05)/1000
  #probability of rejecting the null is the function output
  return(list(prob.plm,prob.llm))
}

#make a data frame to hold outcomes of simulations with different sample sizes
comp<- data.frame(n=c(25,50,100,150,500), prob.plm=NA, prob.llm=NA)

for(i in 1:nrow(comp)){
  #run function over 5 n levels and store probs for two model types as a list
  list<- prob.rej.null(comp$n[i])
  #extract first probability in list
  comp$prob.plm[i]<- as.numeric(unlist(list[1]))
  #extract second probability in list
  comp$prob.llm[i]<- as.numeric(unlist(list[2]))
}

#plot
comp%>%
  pivot_longer(cols=c("prob.plm","prob.llm"),names_to="mod.type",values_to="prob")%>%
  ggplot(aes(x=n, y=prob,color=mod.type))+
  geom_jitter(size=3,width=3)+
  scale_y_continuous(limits=c(0,1))+
  xlab("Sample Size")+
  ylab("Probability of Rejecting Null")+
  scale_color_manual(name="Model Type",
                     labels=c("Linear","Poisson"),
                     values=c("purple","green"))+
  theme_bw()
