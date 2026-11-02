#Packages
library(igraph)
library(LaplacesDemon)
library(mvtnorm)
library(bvartools)
#Please set the WD 
Simu = 50

for(ss in 1:Simu){
  #Dataset generated for exchange
  #setwd("")
  load(paste0("Sim_Gnm_k2_n40_t5_sigmasqrt1_rv_r5_number_",ss,".Rdata"))  
  rm(list=ls()[!ls() %in% c("u_real","y","yf","U_ex","n","ss","Simu")])
  
  #Function MCMC ratio
  logps = function(u,n,Ti,Com,Beta,Betas,Gamma,Gammas,Delta){
    fin1 = fin2 = rep(NA,n)
    fin3 = matrix(NA,n,Ti-2)
    fin = matrix(NA,n,k)
    res = matrix(NA, n, k)
    res2 = matrix(NA, n, k)
    res3 = matrix(NA, n, k)
    res4 = matrix(NA, n, k)
    eq_a = eq_b = eq_c = matrix(NA,n,k)
    matr = matrix(rep(c(1:k), rep(n,1)),k,n)
    for(t in 1:Ti){
      if(t==1){
        for(l in 1:n){
          res2[l,] = Delta[1:k,u[l,2]]
          res3[l,] = Beta[1:k]
          neigh = which(Com[l,]==1)
          counts = table(factor(u[neigh,t], levels=1:k))
          res[l,] = Gamma %*% as.numeric(counts)
          fin[l,] = res[l,]+res2[l,]+res3[l,]
          eq_a[l,] = exp(fin[l,])/sum(exp(fin[l,]))
          fin1[l] = eq_a[l,u[l,t]]
        }
      }else if(t == Ti){
        for(l in 1:n){
          res4[l,] = Delta[u[l,Ti-1],1:k]
          res3[l,] = Betas[1:k]
          neigh = which(Com[l,]==1)
          counts = table(factor(u[neigh,t], levels=1:k))
          res[l,] = Gammas %*% as.numeric(counts)
          fin[l,] = res[l,]+res3[l,]+res4[l,]
          eq_b[l,] = exp(fin[l,])/sum(exp(fin[l,]))
          fin2[l] = eq_b[l,u[l,t]]
        }
      }else{
        for(l in 1:n){
          res2[l,] = Delta[1:k,u[l,t+1]]
          res4[l,] = Delta[u[l,t-1],1:k]
          res3[l,] = Betas[1:k]
          neigh = which(Com[l,]==1)
          counts = table(factor(u[neigh,t], levels=1:k))
          res[l,] = Gammas %*% as.numeric(counts)
          fin[l,] = res[l,]+res2[l,]+res3[l,]+res4[l,]
          eq_c[l,] = exp(fin[l,])/sum(exp(fin[l,]))
          fin3[l,t-1] = eq_c[l,u[l,t]]
        }
      }
    }
    sum(log(fin1)) + sum(log(fin2)) + sum(log(c(fin3)))
  }
  
  #Initial settings
  n = 40
  Ti = 5
  k = 1
  U = 1:k
  d=2
  
  #Parameters
  Gamma_ = matrix(c(0,-1,-1,0), 2,2)
  Delta_ = matrix(c(0,-1,-1,0), 2,2)
  Beta_ = c(-1,0)
  Gammas_ = matrix(c(0,-1/2,-1/2,0), 2,2)
  Betas_ = c(1/2,0)
  
  #Matrix neigh
  C = u_real$C
  Com = u_real$Com
  Cl = t(C)
  
  #Prior
  log_prior_theta = function(x, mb, sb){
    dnorm(x, mb, sb, log = TRUE)
  }
  
  #Hyperparameter
  mb = 0
  sb = 1
  sb_b = 1
  
  mm = rep(0,d)
  Vm = diag(100, d)
  
  half_alpha = ceiling((d+1)/2) + 1
  alpha = half_alpha*2
  S = matrix(half_alpha,d,d)
  diag(S) = alpha
  
  #Iterations
  R = 10000
  
  
  #Initial settings
  set.seed(123 + (ss-1))
  
  mu_iniz = matrix(rnorm(d*k, 0, 100), d, k)
  sigma_iniz = array(rinvwishart(alpha,S), dim = c(d, d, k))
  
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
  solvesigmap1 = solve(Vm)
  solvesigmap1mup1 = solve(Vm)%*%mm
  
  set.seed(123 + (ss-1))
  for(r in 2:R){
    for(v in 1:k){
      ns = ifelse(as.vector(u)==v,1,0)
      yn = y*matrix(rep(ns,d), nrow = d, byrow = TRUE)
      mu[,v,r] = rmvnorm(1,solve(solvesigmap1+sum(ns)*solve(sigma[,,v,r-1]))%*%(solvesigmap1mup1 + solve(sigma[,,v,r-1])%*%rowSums(yn)),solve((solvesigmap1+sum(ns)*solve(sigma[,,v,r-1]))))
      sum = colSums(yn)
      pos = ifelse(sum==0,1,0)
      posiz = which(pos==1,arr.ind = TRUE)
      ynn = as.matrix(yn[,-c(posiz)])
      if(dim(ynn)[2]==0){
        Sn = S
      }else{
        Sa = array((ynn-mu[,v,r]), dim = c(d,1,ncol(ynn)))
        St = matrix(rowSums(apply(Sa, 3, function(x) x%*%t(x))), nrow = d)
        Sn = S + St
      }
      an = alpha + sum(ns)
      sigma[,,v,r] = rinvwishart(an,Sn)
    }
  }
  #setwd("")
  save.image(paste0("Sim_Gnm_k1_n40_t5_sigmasqrt1_rv_pseudo_number_waic_",ss,".Rdata"))
  rm(list = ls())
}
