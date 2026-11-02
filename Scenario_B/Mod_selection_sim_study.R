library("mvtnorm")
library("LaplacesDemon")
Simu = 50
WAIC1 = WAIC2 = WAIC3 = rep(NA,50)
for(ss in 1:Simu){
  #Dataset generated for exchange
  #setwd("")
  load(paste0("Sim_Gnm_k1_n40_t5_sigmasqrt1_rv_pseudo_number_waic_",ss,".Rdata"))  
  
  #K1
  bb = 5000
  loglike1 = matrix(NA, n*Ti, 10000-bb)
  for(r in 1:(10000-bb)){
    for(i in 1:n){
      for(t in 1:Ti){
        loglike1[t+(i-1)*Ti,r] = dmvnorm(matrix(y[,t+(i-1)*Ti],1,2), matrix(mu[,1,bb+r],1,2), sigma[,,1,bb+r], log = TRUE)
      }
    }
  }
  
  WAIC1[ss] = WAIC(loglike1)$WAIC
  
  #K2
  load(paste0("Sim_Gnm_k2_n40_t5_sigmasqrt1_rv_pseudo_number_adj_",ss,".Rdata"))  
  bb = 5000
  loglike2 = matrix(NA, n*Ti, 10000-bb)
  for(r in 1:(10000-bb)){
    for(i in 1:n){
      for(t in 1:Ti){
        loglike2[t+(i-1)*Ti,r] = dmvnorm(matrix(y[,t+(i-1)*Ti],1,2), matrix(mu[,U_ex[i,t,bb+r],bb+r],1,2), sigma[,,U_ex[i,t,bb+r],bb+r], log = TRUE)
      }
    }
  }
  
  WAIC2[ss] = WAIC(loglike2)$WAIC
  
  #K3
  load(paste0("Sim_Gnm_k3_n40_t5_sigmasqrt1_rv_pseudo_number_waic_",ss,".Rdata"))  
  bb = 5000
  loglike3 = matrix(NA, n*Ti, 10000-bb)
  for(r in 1:(10000-bb)){
    for(i in 1:n){
      for(t in 1:Ti){
        loglike3[t+(i-1)*Ti,r] = dmvnorm(matrix(y[,t+(i-1)*Ti],1,2), matrix(mu[,U_ex[i,t,bb+r],bb+r],1,2), sigma[,,U_ex[i,t,bb+r],bb+r], log = TRUE)
      }
    }
  }
  
  WAIC3[ss] = WAIC(loglike3)$WAIC
  
}  

save.image(paste0("Mod_sel.Rdata"))



