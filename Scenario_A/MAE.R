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

Beta_estimatepb = matrix(NA,2,S)
Betas_estimatepb = matrix(NA,2,S)
Gamma_estimatepb = matrix(NA,4,S)
Gammas_estimatepb = matrix(NA,4,S)
Delta_estimatepb = matrix(NA,4,S)

in_it = 5001
fin_it = 10000
ite = 5000

#Load considering WD

for(s in 1:S){
  
  load(paste0("Sim_lattice_k2_n9_t5_sigmasqrt1_rv_r5_",s,".Rdata"))
  Beta_estimate[1:2,s] = rowMeans(Beta_es[,in_it:fin_it])
  Betas_estimate[1:2,s] = rowMeans(Betas_es[,in_it:fin_it])
  Gamma_estimate[1:4,s] = rowMeans(matrix(Gamma_es[,,in_it:fin_it],4,ite))
  Gammas_estimate[1:4,s] = rowMeans(matrix(Gammas_es[,,in_it:fin_it],4,ite))
  Delta_estimate[1:4,s] = rowMeans(matrix(Delta_es[,,in_it:fin_it],4,ite))
  
  load(paste0("Sim_lattice_k2_n9_t5_sigmasqrt1_pseudo_adj_",s,".Rdata"))
  Beta_estimatep[1:2,s] = rowMeans(Beta_es[,in_it:fin_it])
  Betas_estimatep[1:2,s] = rowMeans(Betas_es[,in_it:fin_it])
  Gamma_estimatep[1:4,s] = rowMeans(matrix(Gamma_es[,,in_it:fin_it],4,ite))
  Gammas_estimatep[1:4,s] = rowMeans(matrix(Gammas_es[,,in_it:fin_it],4,ite))
  Delta_estimatep[1:4,s] = rowMeans(matrix(Delta_es[,,in_it:fin_it],4,ite))
  
  load(paste0("Sim_lattice_k2_n9_t5_sigmasqrt1_pseudo_block",s,".Rdata"))
  Beta_estimatepb[1:2,s] = rowMeans(Beta_es[,in_it:fin_it])
  Betas_estimatepb[1:2,s] = rowMeans(Betas_es[,in_it:fin_it])
  Gamma_estimatepb[1:4,s] = rowMeans(matrix(Gamma_es[,,in_it:fin_it],4,ite))
  Gammas_estimatepb[1:4,s] = rowMeans(matrix(Gammas_es[,,in_it:fin_it],4,ite))
  Delta_estimatepb[1:4,s] = rowMeans(matrix(Delta_es[,,in_it:fin_it],4,ite))
  
}

S=50

names = rep(c("beta", "betas", "gamma21", "gamma12", "gammas21", "gammas12", "delta21", "delta12"), c(S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3))
mcmc = rep(rep(c("exchange", "block pseudo", "pseudo"), c(S,S,S)),8)
res = c(abs(Beta_estimate[1,]-Beta_[1]),abs(Beta_estimatepb[1,]-Beta_[1]),abs(Beta_estimatep[1,]-Beta_[1]),abs(Betas_estimate[1,]-Betas_[1]),abs(Betas_estimatepb[1,]-Betas_[1]),abs(Betas_estimatep[1,]-Betas_[1]),abs(Gamma_estimate[2,]-Gamma_[2,1]),abs(Gamma_estimatepb[2,]-Gamma_[2,1]),abs(Gamma_estimatep[2,]-Gamma_[2,1]),abs(Gamma_estimate[3,]-Gamma_[1,2]),abs(Gamma_estimatepb[3,]-Gamma_[1,2]),abs(Gamma_estimatep[3,]-Gamma_[1,2]),abs(Gammas_estimate[2,]-Gammas_[2,1]),abs(Gammas_estimatepb[2,]-Gammas_[2,1]),abs(Gammas_estimatep[2,]-Gammas_[2,1]),abs(Gammas_estimate[3,]-Gammas_[1,2]),abs(Gammas_estimatepb[3,]-Gammas_[1,2]),abs(Gammas_estimatep[3,]-Gammas_[1,2]),abs(Delta_estimate[2,]-Delta_[2,1]),abs(Delta_estimatepb[2,]-Delta_[2,1]),abs(Delta_estimatep[2,]-Delta_[2,1]),abs(Delta_estimate[3,]-Delta_[1,2]),abs(Delta_estimatepb[3,]-Delta_[1,2]),abs(Delta_estimatep[3,]-Delta_[1,2]))

mae = matrix(NA, 8, 3)
s1 = seq(1, 66, 3)
s2 = seq(2, 66, 3)
s3 = seq(3, 66, 3)
for(i in 1:3){
  for(j in 1:8){
    if(i == 1){
      mae[j,i] = mean(res[((1)+(50*(s1[j]-1))):(50*(s1[j]))])
    }else if(i == 2){
      mae[j,i] = mean(res[((1)+(50*(s2[j]-1))):(50*(s2[j]))])
    }else{
      mae[j,i] = mean(res[((1)+(50*(s3[j]-1))):(50*(s3[j]))])
    }
  }
}

round(mae,3)

res = c((Beta_estimate[1,] - Beta_[1])^2, (Beta_estimatepb[1,] - Beta_[1])^2, (Beta_estimatep[1,] - Beta_[1])^2, (Betas_estimate[1,] - Betas_[1])^2, (Betas_estimatepb[1,] - Betas_[1])^2, (Betas_estimatep[1,] - Betas_[1])^2, (Gamma_estimate[2,] - Gamma_[2,1])^2, (Gamma_estimatepb[2,] - Gamma_[2,1])^2, (Gamma_estimatep[2,] - Gamma_[2,1])^2, (Gamma_estimate[3,] - Gamma_[1,2])^2, (Gamma_estimatepb[3,] - Gamma_[1,2])^2, (Gamma_estimatep[3,] - Gamma_[1,2])^2, (Gammas_estimate[2,] - Gammas_[2,1])^2, (Gammas_estimatepb[2,] - Gammas_[2,1])^2, (Gammas_estimatep[2,] - Gammas_[2,1])^2, (Gammas_estimate[3,] - Gammas_[1,2])^2, (Gammas_estimatepb[3,] - Gammas_[1,2])^2, (Gammas_estimatep[3,] - Gammas_[1,2])^2, (Delta_estimate[2,] - Delta_[2,1])^2, (Delta_estimatepb[2,] - Delta_[2,1])^2, (Delta_estimatep[2,] - Delta_[2,1])^2, (Delta_estimate[3,] - Delta_[1,2])^2, (Delta_estimatepb[3,] - Delta_[1,2])^2, (Delta_estimatep[3,] - Delta_[1,2])^2)

mse = matrix(NA, 8, 3)
s1 = seq(1, 66, 3)
s2 = seq(2, 66, 3)
s3 = seq(3, 66, 3)
for(i in 1:3){
  for(j in 1:8){
    if(i == 1){
      mse[j,i] = mean(res[((1)+(50*(s1[j]-1))):(50*(s1[j]))])
    }else if(i == 2){
      mse[j,i] = mean(res[((1)+(50*(s2[j]-1))):(50*(s2[j]))])
    }else{
      mse[j,i] = mean(res[((1)+(50*(s3[j]-1))):(50*(s3[j]))])
    }
  }
}

round(sqrt(mse),3)
