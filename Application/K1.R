library(mvtnorm)
library(bvartools)
library(LaplacesDemon)

load("Data_applicazione.RData")

#Number of states
#k = 1
#U = 1:k

#Function
source("Omega_sampler_general_model_speed.R")

#Initial settings
k = 1
U = 1:k
d = 1

#Hyperparameters
mm = rep(0,d)
Vm = diag(100, d)

alpha = 2
S = matrix(1,d,d)
diag(S) = 1

#Iterations
R = 50000

#Initial settings
set.seed(123)

mu_iniz = matrix(rnorm(d*k, 0, sqrt(Vm)), d, k)
sigma_iniz = array(rinvgamma(1,alpha,S), dim = c(d, d, k))

#Initial u
U_ex = array(1, dim = c(n,Ti,R))
u = U_ex[,,1]

#Cont
mu = array(0, dim = c(d, k, R))
sigma = array(0, dim = c(d, d, k, R))

#Init
mu[,,1] = mu_iniz
sigma[,,,1] = sigma_iniz

#Full par
solvesigmap1 = (Vm)^(-1)
solvesigmap1mup1 = mm*(Vm)^(-1)

set.seed(123)
for(r in 2:R){
  for(v in 1:k){
    ns = ifelse(as.vector(u)==v,1,0)
    yn = as.vector(y)*ns
    mu[,v,r] = rnorm(1, (solvesigmap1mup1 + sum(yn)/(sigma[,,v,r-1]))*(sum(ns)/(sigma[,,v,r-1]) + solvesigmap1)^(-1),sqrt((sum(ns)/(sigma[,,v,r-1]) + solvesigmap1)^(-1)))
    sum = yn
    pos = ifelse(sum==0,1,0)
    #posiz = which(pos==1,arr.ind = TRUE)
    #ynn = yn[-c(posiz)]
    Sn = S + 0.5*sum((yn-mu[,v,r])^2)
    an = alpha + sum(ns)*0.5
    sigma[,,v,r] = rinvgamma(1,an,Sn)
  }
}

save.image(paste0("K1.Rdata"))


