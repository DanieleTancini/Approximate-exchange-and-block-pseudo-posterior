#MAE and MSE
S=50
Beta_estimate50 = matrix(NA,2,S)
Betas_estimate50 = matrix(NA,2,S)
Gamma_estimate50 = matrix(NA,4,S)
Gammas_estimate50 = matrix(NA,4,S)
Delta_estimate50 = matrix(NA,4,S)

Beta_estimate = matrix(NA,2,S)
Betas_estimate = matrix(NA,2,S)
Gamma_estimate = matrix(NA,4,S)
Gammas_estimate = matrix(NA,4,S)
Delta_estimate = matrix(NA,4,S)

in_it = 5001
fin_it = 10000
ite = 5000

for(s in 1:S){
  
  load(paste0("~/CSDA_n40_t5_k2_r50/Sim_Gnm_k2_n40_t5_sigmasqrt1_rv_r5_number_",s,".Rdata"))
  Beta_estimate50[1:2,s] = rowMeans(Beta_es[,in_it:fin_it])
  Betas_estimate50[1:2,s] = rowMeans(Betas_es[,in_it:fin_it])
  Gamma_estimate50[1:4,s] = rowMeans(matrix(Gamma_es[,,in_it:fin_it],4,ite))
  Gammas_estimate50[1:4,s] = rowMeans(matrix(Gammas_es[,,in_it:fin_it],4,ite))
  Delta_estimate50[1:4,s] = rowMeans(matrix(Delta_es[,,in_it:fin_it],4,ite))
  
  load(paste0("~/CSDA_n40_t5_k2/Sim_Gnm_k2_n40_t5_sigmasqrt1_rv_r5_number_",s,".Rdata"))
  Beta_estimate[1:2,s] = rowMeans(Beta_es[,in_it:fin_it])
  Betas_estimate[1:2,s] = rowMeans(Betas_es[,in_it:fin_it])
  Gamma_estimate[1:4,s] = rowMeans(matrix(Gamma_es[,,in_it:fin_it],4,ite))
  Gammas_estimate[1:4,s] = rowMeans(matrix(Gammas_es[,,in_it:fin_it],4,ite))
  Delta_estimate[1:4,s] = rowMeans(matrix(Delta_es[,,in_it:fin_it],4,ite))
}

S=50

names = rep(c("beta", "betas", "gamma21", "gamma12", "gammas21", "gammas12", "delta21", "delta12"), c(S*2,S*2,S*2,S*2,S*2,S*2,S*2,S*2))
mcmc = rep(rep(c("exchange50", "exchange"), c(S,S)),8)
res = c(abs(Beta_estimate50[1,]-Beta_[1]),abs(Beta_estimate[1,]-Beta_[1]),abs(Betas_estimate50[1,]-Betas_[1]),abs(Betas_estimate[1,]-Betas_[1]),abs(Gamma_estimate50[2,]-Gamma_[2,1]),abs(Gamma_estimate[2,]-Gamma_[2,1]),abs(Gamma_estimate50[3,]-Gamma_[1,2]),abs(Gamma_estimate[3,]-Gamma_[1,2]),abs(Gammas_estimate50[2,]-Gammas_[2,1]),abs(Gammas_estimate[2,]-Gammas_[2,1]),abs(Gammas_estimate50[3,]-Gammas_[1,2]),abs(Gammas_estimate[3,]-Gammas_[1,2]),abs(Delta_estimate50[2,]-Delta_[2,1]),abs(Delta_estimate[2,]-Delta_[2,1]),abs(Delta_estimate50[3,]-Delta_[1,2]),abs(Delta_estimate[3,]-Delta_[1,2]))

mae = matrix(NA, 8, 2)
s1 = seq(1, 66, 2)
s2 = seq(2, 66, 2)
for(i in 1:2){
  for(j in 1:8){
    if(i == 1){
      mae[j,i] = mean(res[((1)+(50*(s1[j]-1))):(50*(s1[j]))])
    }else{
      mae[j,i] = mean(res[((1)+(50*(s2[j]-1))):(50*(s2[j]))])
    }
  }
}

round(mae,3)

res = c((Beta_estimate50[1,]-Beta_[1])^2,(Beta_estimate[1,]-Beta_[1])^2,(Betas_estimate50[1,]-Betas_[1])^2,(Betas_estimate[1,]-Betas_[1])^2,(Gamma_estimate50[2,]-Gamma_[2,1])^2,(Gamma_estimate[2,]-Gamma_[2,1])^2,(Gamma_estimate50[3,]-Gamma_[1,2])^2,(Gamma_estimate[3,]-Gamma_[1,2])^2,(Gammas_estimate50[2,]-Gammas_[2,1])^2,(Gammas_estimate[2,]-Gammas_[2,1])^2,(Gammas_estimate50[3,]-Gammas_[1,2])^2,(Gammas_estimate[3,]-Gammas_[1,2])^2,(Delta_estimate50[2,]-Delta_[2,1])^2,(Delta_estimate[2,]-Delta_[2,1])^2,(Delta_estimate50[3,]-Delta_[1,2])^2,(Delta_estimate[3,]-Delta_[1,2])^2)

mse = matrix(NA, 8, 2)
s1 = seq(1, 66, 2)
s2 = seq(2, 66, 2)
for(i in 1:2){
  for(j in 1:8){
    if(i == 1){
      mse[j,i] = mean(res[((1)+(50*(s1[j]-1))):(50*(s1[j]))])
    }else{
      mse[j,i] = mean(res[((1)+(50*(s2[j]-1))):(50*(s2[j]))])
    }
  }
}

round(sqrt(mse),3)
