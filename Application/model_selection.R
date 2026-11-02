library(LaplacesDemon)
#Set WD
load("K1.Rdata")
#K1
seque = seq(2,50000,100)
j=0
loglike1 = matrix(NA, n*Ti, length(seque))
for(r in seque){
  j = j+1
  for(i in 1:n){
    for(t in 1:Ti){
      loglike1[t+(i-1)*Ti,j] = dnorm(y[i,t], mu[1,1,r], sqrt(sigma[1,1,1,r]), log = TRUE)
    }
  }
}

WAIC1 = WAIC(loglike1)

#K2
load("K2.Rdata")
seque = seq(2,50000,100)
j=0
loglike2 = matrix(NA, n*Ti, length(seque))
for(r in seque){
  j=j+1
  for(i in 1:n){
    for(t in 1:Ti){
      loglike2[t+(i-1)*Ti,j] = dnorm(y[i,t], mu[1,U_ex[i,t,r],r], sqrt(sigma[1,1,U_ex[i,t,r],r]), log = TRUE)
    }
  }
}

WAIC2 = WAIC(loglike2)

#K3
load("K3.Rdata")
seque = seq(2,50000,100)
j=0
loglike3 = matrix(NA, n*Ti, length(seque))
for(r in seque){
  j=j+1
  for(i in 1:n){
    for(t in 1:Ti){
      loglike3[t+(i-1)*Ti,j] = dnorm(y[i,t], mu[1,U_ex[i,t,r],r], sqrt(sigma[1,1,U_ex[i,t,r],r]), log = TRUE)
    }
  }
}

WAIC3 = WAIC(loglike3)

#K4
load("K4.Rdata")
seque = seq(2,50000,100)
j=0
loglike4 = matrix(NA, n*Ti, length(seque))
for(r in seque){
  j=j+1
  for(i in 1:n){
    for(t in 1:Ti){
      loglike4[t+(i-1)*Ti,j] = dnorm(y[i,t], mu[1,U_ex[i,t,r],r], sqrt(sigma[1,1,U_ex[i,t,r],r]), log = TRUE)
    }
  }
}

WAIC4 = WAIC(loglike4)

#K5
load("K5.Rdata")
seque = seq(2,50000,100)
j=0
loglike5 = matrix(NA, n*Ti, length(seque))
for(r in seque){
  j=j+1
  for(i in 1:n){
    for(t in 1:Ti){
      loglike5[t+(i-1)*Ti,j] = dnorm(y[i,t], mu[1,U_ex[i,t,r],r], sqrt(sigma[1,1,U_ex[i,t,r],r]), log = TRUE)
    }
  }
}

WAIC5 = WAIC(loglike5)

#est
load("K4.Rdata")

#check diagnostic geweke.diag
library(coda)
Z_ = 2.576

#Beta
abs(geweke.diag(as.mcmc(Beta_es[1, seq(25000, 50000, 1)]))$z) <= Z_
abs(geweke.diag(as.mcmc(Beta_es[2, seq(25000, 50000, 1)]))$z) <= Z_
abs(geweke.diag(as.mcmc(Beta_es[3, seq(25000, 50000, 1)]))$z) <= Z_

#Betas
abs(geweke.diag(as.mcmc(Betas_es[1, seq(25000, 50000, 1)]))$z) <= Z_
abs(geweke.diag(as.mcmc(Betas_es[2, seq(25000, 50000, 1)]))$z) <= Z_
abs(geweke.diag(as.mcmc(Betas_es[3, seq(25000, 50000, 1)]))$z) <= Z_

#Gamma
abs(geweke.diag(as.mcmc(Gamma_es[1, 2, seq(25000, 50000, 1)]))$z) <= Z_
abs(geweke.diag(as.mcmc(Gamma_es[1, 3, seq(25000, 50000, 1)]))$z) <= Z_
abs(geweke.diag(as.mcmc(Gamma_es[1, 4, seq(25000, 50000, 1)]))$z) <= Z_
abs(geweke.diag(as.mcmc(Gamma_es[2, 3, seq(25000, 50000, 1)]))$z) <= Z_
abs(geweke.diag(as.mcmc(Gamma_es[2, 4, seq(25000, 50000, 1)]))$z) <= Z_
abs(geweke.diag(as.mcmc(Gamma_es[3, 4, seq(25000, 50000, 1)]))$z) <= Z_

#Gammas
abs(geweke.diag(as.mcmc(Gammas_es[1, 2, seq(25000, 50000, 1)]))$z) <= Z_
abs(geweke.diag(as.mcmc(Gammas_es[1, 3, seq(25000, 50000, 1)]))$z) <= Z_
abs(geweke.diag(as.mcmc(Gammas_es[1, 4, seq(25000, 50000, 1)]))$z) <= Z_
abs(geweke.diag(as.mcmc(Gammas_es[2, 3, seq(25000, 50000, 1)]))$z) <= Z_
abs(geweke.diag(as.mcmc(Gammas_es[2, 4, seq(25000, 50000, 1)]))$z) <= Z_
abs(geweke.diag(as.mcmc(Gammas_es[3, 4, seq(25000, 50000, 1)]))$z) <= Z_

#Delta
abs(geweke.diag(as.mcmc(Delta_es[1, 2, seq(25000, 50000, 1)]))$z) <= Z_
abs(geweke.diag(as.mcmc(Delta_es[1, 3, seq(25000, 50000, 1)]))$z) <= Z_
abs(geweke.diag(as.mcmc(Delta_es[1, 4, seq(25000, 50000, 1)]))$z) <= Z_
abs(geweke.diag(as.mcmc(Delta_es[2, 1, seq(25000, 50000, 1)]))$z) <= Z_
abs(geweke.diag(as.mcmc(Delta_es[2, 3, seq(25000, 50000, 1)]))$z) <= Z_
abs(geweke.diag(as.mcmc(Delta_es[2, 4, seq(25000, 50000, 1)]))$z) <= Z_
abs(geweke.diag(as.mcmc(Delta_es[3, 1, seq(25000, 50000, 1)]))$z) <= Z_
abs(geweke.diag(as.mcmc(Delta_es[3, 2, seq(25000, 50000, 1)]))$z) <= Z_
abs(geweke.diag(as.mcmc(Delta_es[3, 4, seq(25000, 50000, 1)]))$z) <= Z_
abs(geweke.diag(as.mcmc(Delta_es[4, 1, seq(25000, 50000, 1)]))$z) <= Z_
abs(geweke.diag(as.mcmc(Delta_es[4, 2, seq(25000, 50000, 1)]))$z) <= Z_
abs(geweke.diag(as.mcmc(Delta_es[4, 3, seq(25000, 50000, 1)]))$z) <= Z_

#minimum acceptance rate
mean((c(accb,accbs,accd[row(accd)!=col(accd)],accg[upper.tri(accg)],accgs[upper.tri(accgs)]))/50000)

#Estimate
mean(mu[1,1,seq(25000,50000,100)])
mean(mu[1,2,seq(25000,50000,100)])
mean(mu[1,3,seq(25000,50000,100)])
mean(mu[1,4,seq(25000,50000,100)])
mean(sqrt(sigma[1,1,1,seq(25000,50000,100)]))
mean(sqrt(sigma[1,1,2,seq(25000,50000,100)]))
mean(sqrt(sigma[1,1,3,seq(25000,50000,100)]))
mean(sqrt(sigma[1,1,4,seq(25000,50000,100)]))
mean(Beta_es[1,seq(25000,50000,100)])
mean(Beta_es[2,seq(25000,50000,100)])
mean(Beta_es[3,seq(25000,50000,100)])
mean(Betas_es[1,seq(25000,50000,100)])
mean(Betas_es[2,seq(25000,50000,100)])
mean(Betas_es[3,seq(25000,50000,100)])
mean(Gamma_es[1,2,seq(25000,50000,100)])
mean(Gamma_es[1,3,seq(25000,50000,100)])
mean(Gamma_es[1,4,seq(25000,50000,100)])
mean(Gamma_es[2,3,seq(25000,50000,100)])
mean(Gamma_es[2,4,seq(25000,50000,100)])
mean(Gamma_es[3,4,seq(25000,50000,100)])
mean(Gammas_es[1,2,seq(25000,50000,100)])
mean(Gammas_es[1,3,seq(25000,50000,100)])
mean(Gammas_es[1,4,seq(25000,50000,100)])
mean(Gammas_es[2,3,seq(25000,50000,100)])
mean(Gammas_es[2,4,seq(25000,50000,100)])
mean(Gammas_es[3,4,seq(25000,50000,100)])
mean(Delta_es[1,2,seq(25000,50000,100)])
mean(Delta_es[1,3,seq(25000,50000,100)])
mean(Delta_es[1,4,seq(25000,50000,100)])
mean(Delta_es[2,1,seq(25000,50000,100)])
mean(Delta_es[2,3,seq(25000,50000,100)])
mean(Delta_es[2,4,seq(25000,50000,100)])
mean(Delta_es[3,1,seq(25000,50000,100)])
mean(Delta_es[3,2,seq(25000,50000,100)])
mean(Delta_es[3,4,seq(25000,50000,100)])
mean(Delta_es[4,1,seq(25000,50000,100)])
mean(Delta_es[4,2,seq(25000,50000,100)])
mean(Delta_es[4,3,seq(25000,50000,100)])

#-------------------------------------------
