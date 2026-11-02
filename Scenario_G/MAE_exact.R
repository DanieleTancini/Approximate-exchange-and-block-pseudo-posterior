#MAE and MSE
S=50
Beta_estimate = matrix(NA,2,S)
Betas_estimate = matrix(NA,2,S)
Gamma_estimate = matrix(NA,4,S)
Gammas_estimate = matrix(NA,4,S)
Delta_estimate = matrix(NA,4,S)

Beta_estimatep = matrix(NA,2,S)
Betas_estimatep = matrix(NA,2,S)
Gamma_estimatep = matrix(NA,4,S)
Gammas_estimatep = matrix(NA,4,S)
Delta_estimatep = matrix(NA,4,S)

Beta_e = matrix(NA,2,S)
Betas_e = matrix(NA,2,S)
Gamma_e = matrix(NA,4,S)
Gammas_e = matrix(NA,4,S)
Delta_e = matrix(NA,4,S)

in_it = 5001
fin_it = 10000
ite = 5000

#Load considering WD

for(s in 1:S){
  
  load(paste0("Sim_lattice_k2_n4_t2_sigmasqrt1_rv_r5_",s,".Rdata"))
  Beta_estimate[1:2,s] = rowMeans(Beta_es[,in_it:fin_it])
  Betas_estimate[1:2,s] = rowMeans(Betas_es[,in_it:fin_it])
  Gamma_estimate[1:4,s] = rowMeans(matrix(Gamma_es[,,in_it:fin_it],4,ite))
  Gammas_estimate[1:4,s] = rowMeans(matrix(Gammas_es[,,in_it:fin_it],4,ite))
  Delta_estimate[1:4,s] = rowMeans(matrix(Delta_es[,,in_it:fin_it],4,ite))
  
  load(paste0("Sim_lattice_k2_n4_t2_sigmasqrt1_pseudo_adj_",s,".Rdata"))
  Beta_estimatep[1:2,s] = rowMeans(Beta_es[,in_it:fin_it])
  Betas_estimatep[1:2,s] = rowMeans(Betas_es[,in_it:fin_it])
  Gamma_estimatep[1:4,s] = rowMeans(matrix(Gamma_es[,,in_it:fin_it],4,ite))
  Gammas_estimatep[1:4,s] = rowMeans(matrix(Gammas_es[,,in_it:fin_it],4,ite))
  Delta_estimatep[1:4,s] = rowMeans(matrix(Delta_es[,,in_it:fin_it],4,ite))
  
  load(paste0("Sim_lattice_k2_n4_t2_sigmasqrt1_exact_",s,".Rdata"))
  Beta_e[1:2,s] = rowMeans(Beta_es[,in_it:fin_it])
  Betas_e[1:2,s] = rowMeans(Betas_es[,in_it:fin_it])
  Gamma_e[1:4,s] = rowMeans(matrix(Gamma_es[,,in_it:fin_it],4,ite))
  Gammas_e[1:4,s] = rowMeans(matrix(Gammas_es[,,in_it:fin_it],4,ite))
  Delta_e[1:4,s] = rowMeans(matrix(Delta_es[,,in_it:fin_it],4,ite))
  
}

S=50

names = rep(c("beta", "betas", "gamma21", "gamma12", "gammas21", "gammas12", "delta21", "delta12"), c(S*2,S*2,S*2,S*2,S*2,S*2,S*2,S*2))
mcmc = rep(rep(c("exchange", "pseudo"), c(S,S)),8)
res = c(abs(Beta_estimate[1,]-Beta_e[1,]),abs(Beta_estimatep[1,]-Beta_e[1,]),abs(Betas_estimate[1,]-Betas_e[1,]),abs(Betas_estimatep[1,]-Betas_e[1,]),abs(Gamma_estimate[2,]-Gamma_e[2,]),abs(Gamma_estimatep[2,]-Gamma_e[2,]),abs(Gamma_estimate[3,]-Gamma_e[3,]),abs(Gamma_estimatep[3,]-Gamma_e[3,]),abs(Gammas_estimate[2,]-Gammas_e[2,]),abs(Gammas_estimatep[2,]-Gammas_e[2,]),abs(Gammas_estimate[3,]-Gammas_e[3,]),abs(Gammas_estimatep[3,]-Gammas_e[3,]),abs(Delta_estimate[2,]-Delta_e[2,]),abs(Delta_estimatep[2,]-Delta_e[2,]),abs(Delta_estimate[3,]-Delta_e[3,]),abs(Delta_estimatep[3,]-Delta_e[3,]))

mae = matrix(NA, 8, 2)
s1 = seq(1, 66, 2)
s2 = seq(2, 66, 2)
for(i in 1:2){
  for(j in 1:8){
    if(i == 1){
      mae[j,i] = mean(res[((1)+(50*(s1[j]-1))):(50*(s1[j]))])
    }else if(i == 2){
      mae[j,i] = mean(res[((1)+(50*(s2[j]-1))):(50*(s2[j]))])
    }
  }
}

round(mae,3)

res = res^2
mse = matrix(NA, 8, 2)
s1 = seq(1, 66, 2)
s2 = seq(2, 66, 2)
for(i in 1:2){
  for(j in 1:8){
    if(i == 1){
      mse[j,i] = mean(res[((1)+(50*(s1[j]-1))):(50*(s1[j]))])
    }else if(i == 2){
      mse[j,i] = mean(res[((1)+(50*(s2[j]-1))):(50*(s2[j]))])
    }
  }
}

round(sqrt(mse),3)


