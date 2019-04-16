
library(readxl)

################Prospect evaluation#######################################
######################################################
#number of simulation for monte carlo simulation
Nsim <- 10000
######################################################
# Gross rock volume that is based on normal distrubtion in cu meters with the mean
# of 40 million cu meter and sd of 1 million cu meter
GRV <- rnorm(Nsim,mean = 4*10^7, sd=10^6)
# convresion to the Million
GRV_MM <- GRV/10^6
hist(GRV_MM,col = 'green')
#################################################
# net to gross with the uniform distribution 
NTG <- runif(Nsim,min = 0.2,max = 0.6)
hist(NTG,col = 'green')
##################################################
# Porosity with normal distribution with mean of 0.25 and sd= 0.1
Por <- rnorm(Nsim,mean = 0.25,sd=0.05)
hist(Por,col = 'green',xlim = c(0,0.5))
##############################################

# the function which create the triangle ditribution, 
#the only thing meeded is just to click it
genetri <- function(Nsim,a,b,c) {
  U <- runif(Nsim)                             # uniform u values
  X=rep(0,Nsim)           
  for (i in 1:Nsim)
    if (U [i] < ((c-a)/(b-a))){
      X[i] <-  a+sqrt(U[i]*(b-a)*(c-a))        # check is done based on U value accordig to the theory
    }else {
      X[i] <-  b-sqrt((1-U[i])*(b-a)*(b-c))               
    }
  return(X)
}
# Oil saturation is in triangle dis with min of 0.15,mode is 0.5 and max of 0.7 
So <- genetri(Nsim,a=0.15,b=0.7,c=0.5)
hist(So,col = 'green')
##################################################
# Formation volume factor for dry gas
#FVF <- 1/300
FVF <- runif(Nsim,min = 0.003,max = 0.01)
hist(FVF,col = 'green',xlim = c(0.002,0.011),breaks = 25)
###############################################
# calculation of Hydrocarbon in Place in Cu meters
HCIP <- GRV*NTG*Por*So/FVF

# Conversion from cu meters to billion cu ft

HCIP_B_cuft <- HCIP*35.31/10^9

####################################################
# histogram based on result of monte carlo simulation
hist(HCIP_B_cuft,col = 'red',probability = T,main = 'Prospect Evaluation'
     ,xlab = 'Original Gas in Place in (B cu ft3)',xlim = c(0,60),breaks = 50)

###########################################################

quantile(HCIP_B_cuft,c(0.1,0.5,0.9))
ss <- ecdf(HCIP_B_cuft)
plot(ss,main='Cumulative Distribution for the Prospect',col = "red",lwd = 5,xlab='OGIP in B cu ft3')
####################################################
