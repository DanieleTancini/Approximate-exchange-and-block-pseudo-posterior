#MAE and MSE 
S=50
Beta_estimate = matrix(NA,5,S)
Betas_estimate = matrix(NA,5,S)
Gamma_estimate = matrix(NA,25,S)
Gammas_estimate = matrix(NA,25,S)
Delta_estimate = matrix(NA,25,S)

Beta_estimatep = matrix(NA,5,S)
Betas_estimatep = matrix(NA,5,S)
Gamma_estimatep = matrix(NA,25,S)
Gammas_estimatep = matrix(NA,25,S)
Delta_estimatep = matrix(NA,25,S)

Beta_estimatepb = matrix(NA,5,S)
Betas_estimatepb = matrix(NA,5,S)
Gamma_estimatepb = matrix(NA,25,S)
Gammas_estimatepb = matrix(NA,25,S)
Delta_estimatepb = matrix(NA,25,S)

in_it = 5001
fin_it = 10000
ite = 5000

#Load considering WD

for(s in 1:S){
  
  load(paste0("Sim_Gnm_k5_n40_t5_sigmasqrt1_rv_r5_number_",s,".Rdata"))
  Beta_estimate[1:5,s] = rowMeans(Beta_es[,in_it:fin_it])
  Betas_estimate[1:5,s] = rowMeans(Betas_es[,in_it:fin_it])
  Gamma_estimate[1:25,s] = rowMeans(matrix(Gamma_es[,,in_it:fin_it],25,ite))
  Gammas_estimate[1:25,s] = rowMeans(matrix(Gammas_es[,,in_it:fin_it],25,ite))
  Delta_estimate[1:25,s] = rowMeans(matrix(Delta_es[,,in_it:fin_it],25,ite))
  
  load(paste0("Sim_Gnm_k5_n40_t5_sigmasqrt1_rv_pseudo_number_adj_",s,".Rdata"))
  Beta_estimatep[1:5,s] = rowMeans(Beta_es[,in_it:fin_it])
  Betas_estimatep[1:5,s] = rowMeans(Betas_es[,in_it:fin_it])
  Gamma_estimatep[1:25,s] = rowMeans(matrix(Gamma_es[,,in_it:fin_it],25,ite))
  Gammas_estimatep[1:25,s] = rowMeans(matrix(Gammas_es[,,in_it:fin_it],25,ite))
  Delta_estimatep[1:25,s] = rowMeans(matrix(Delta_es[,,in_it:fin_it],25,ite))
  
  load(paste0("Sim_Gnm_k5_n40_t5_sigmasqrt1_rv_pseudo_20_block_adj_",s,".Rdata"))
  Beta_estimatepb[1:5,s] = rowMeans(Beta_es[,in_it:fin_it])
  Betas_estimatepb[1:5,s] = rowMeans(Betas_es[,in_it:fin_it])
  Gamma_estimatepb[1:25,s] = rowMeans(matrix(Gamma_es[,,in_it:fin_it],25,ite))
  Gammas_estimatepb[1:25,s] = rowMeans(matrix(Gammas_es[,,in_it:fin_it],25,ite))
  Delta_estimatepb[1:25,s] = rowMeans(matrix(Delta_es[,,in_it:fin_it],25,ite))
  
}

S=50

names = rep(c("beta1", "beta2", "beta3", "beta4", "betas1", "betas2", "betas3", "betas4", "gamma21", "gamma31", "gamma41", "gamma51", "gamma12", "gamma32", "gamma42", "gamma52", "gamma13", "gamma23", "gamma43", "gamma53", "gamma14", "gamma24", "gamma34", "gamma54",  "gamma15", "gamma25", "gamma35", "gamma45", "gammas21", "gammas31", "gammas41", "gammas51", "gammas12", "gammas32", "gammas42", "gammas52", "gammas13", "gammas23", "gammas43", "gammas53", "gammas14", "gammas24", "gammas34", "gammas54",  "gammas15", "gammas25", "gammas35", "gammas45", "delta21", "delta31", "delta41", "delta51", "delta12", "delta32","delta42", "delta52", "delta13", "delta23", "delta43", "delta53", "delta14", "delta24", "delta34", "delta54", "delta15", "delta25", "delta35", "delta45"), c(S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3))
mcmc = rep(rep(c("exchange", "block pseudo", "pseudo"), c(S,S,S)),68)
res = c(abs(Beta_estimate[1,]-Beta_[1]),abs(Beta_estimatepb[1,]-Beta_[1]),abs(Beta_estimatep[1,]-Beta_[1]), abs(Beta_estimate[2,]-Beta_[2]),abs(Beta_estimatepb[2,]-Beta_[2]),abs(Beta_estimatep[2,]-Beta_[2]), abs(Beta_estimate[3,]-Beta_[3]),abs(Beta_estimatepb[3,]-Beta_[3]),abs(Beta_estimatep[3,]-Beta_[3]), abs(Beta_estimate[4,]-Beta_[4]),abs(Beta_estimatepb[4,]-Beta_[4]),abs(Beta_estimatep[4,]-Beta_[4]), abs(Betas_estimate[1,]-Betas_[1]),abs(Betas_estimatepb[1,]-Betas_[1]),abs(Betas_estimatep[1,]-Betas_[1]), abs(Betas_estimate[2,]-Betas_[2]),abs(Betas_estimatepb[2,]-Betas_[2]),abs(Betas_estimatep[2,]-Betas_[2]), abs(Betas_estimate[3,]-Betas_[3]),abs(Betas_estimatepb[3,]-Betas_[3]),abs(Betas_estimatep[3,]-Betas_[3]), abs(Betas_estimate[4,]-Betas_[4]),abs(Betas_estimatepb[4,]-Betas_[4]),abs(Betas_estimatep[4,]-Betas_[4]), abs(Gamma_estimate[2,]-Gamma_[2,1]),abs(Gamma_estimatepb[2,]-Gamma_[2,1]),abs(Gamma_estimatep[2,]-Gamma_[2,1]), abs(Gamma_estimate[3,]-Gamma_[3,1]),abs(Gamma_estimatepb[3,]-Gamma_[3,1]),abs(Gamma_estimatep[3,]-Gamma_[3,1]), abs(Gamma_estimate[4,]-Gamma_[4,1]),abs(Gamma_estimatepb[4,]-Gamma_[4,1]),abs(Gamma_estimatep[4,]-Gamma_[4,1]), abs(Gamma_estimate[5,]-Gamma_[5,1]),abs(Gamma_estimatepb[5,]-Gamma_[5,1]),abs(Gamma_estimatep[5,]-Gamma_[5,1]), abs(Gamma_estimate[6,]-Gamma_[1,2]),abs(Gamma_estimatepb[6,]-Gamma_[1,2]),abs(Gamma_estimatep[6,]-Gamma_[1,2]),
        abs(Gamma_estimate[8,]-Gamma_[3,2]),abs(Gamma_estimatepb[8,]-Gamma_[3,2]),abs(Gamma_estimatep[8,]-Gamma_[3,2]),
        abs(Gamma_estimate[9,]-Gamma_[4,2]),abs(Gamma_estimatepb[9,]-Gamma_[4,2]),abs(Gamma_estimatep[9,]-Gamma_[4,2]),
        abs(Gamma_estimate[10,]-Gamma_[5,2]),abs(Gamma_estimatepb[10,]-Gamma_[5,2]),abs(Gamma_estimatep[10,]-Gamma_[5,2]),
        abs(Gamma_estimate[11,]-Gamma_[1,3]),abs(Gamma_estimatepb[11,]-Gamma_[1,3]),abs(Gamma_estimatep[11,]-Gamma_[1,3]),
        abs(Gamma_estimate[12,]-Gamma_[2,3]),abs(Gamma_estimatepb[12,]-Gamma_[2,3]),abs(Gamma_estimatep[12,]-Gamma_[2,3]),
        abs(Gamma_estimate[14,]-Gamma_[4,3]),abs(Gamma_estimatepb[14,]-Gamma_[4,3]),abs(Gamma_estimatep[14,]-Gamma_[4,3]),
        abs(Gamma_estimate[15,]-Gamma_[5,3]),abs(Gamma_estimatepb[15,]-Gamma_[5,3]),abs(Gamma_estimatep[15,]-Gamma_[5,3]),
        abs(Gamma_estimate[16,]-Gamma_[1,4]),abs(Gamma_estimatepb[16,]-Gamma_[1,4]),abs(Gamma_estimatep[16,]-Gamma_[1,4]),
        abs(Gamma_estimate[17,]-Gamma_[2,4]),abs(Gamma_estimatepb[17,]-Gamma_[2,4]),abs(Gamma_estimatep[17,]-Gamma_[2,4]),
        abs(Gamma_estimate[18,]-Gamma_[3,4]),abs(Gamma_estimatepb[18,]-Gamma_[3,4]),abs(Gamma_estimatep[18,]-Gamma_[3,4]),
        abs(Gamma_estimate[20,]-Gamma_[5,4]),abs(Gamma_estimatepb[20,]-Gamma_[5,4]),abs(Gamma_estimatep[20,]-Gamma_[5,4]),
        abs(Gamma_estimate[21,]-Gamma_[1,5]),abs(Gamma_estimatepb[21,]-Gamma_[1,5]),abs(Gamma_estimatep[21,]-Gamma_[1,5]),
        abs(Gamma_estimate[22,]-Gamma_[2,5]),abs(Gamma_estimatepb[22,]-Gamma_[2,5]),abs(Gamma_estimatep[22,]-Gamma_[2,5]),
        abs(Gamma_estimate[23,]-Gamma_[3,5]),abs(Gamma_estimatepb[23,]-Gamma_[3,5]),abs(Gamma_estimatep[23,]-Gamma_[3,5]),
        abs(Gamma_estimate[24,]-Gamma_[4,5]),abs(Gamma_estimatepb[24,]-Gamma_[4,5]),abs(Gamma_estimatep[24,]-Gamma_[4,5]),  
        abs(Gammas_estimate[2,]-Gammas_[2,1]),abs(Gammas_estimatepb[2,]-Gammas_[2,1]),abs(Gammas_estimatep[2,]-Gammas_[2,1]),
        abs(Gammas_estimate[3,]-Gammas_[3,1]),abs(Gammas_estimatepb[3,]-Gammas_[3,1]),abs(Gammas_estimatep[3,]-Gammas_[3,1]),
        abs(Gammas_estimate[4,]-Gammas_[4,1]),abs(Gammas_estimatepb[4,]-Gammas_[4,1]),abs(Gammas_estimatep[4,]-Gammas_[4,1]),
        abs(Gammas_estimate[5,]-Gammas_[5,1]),abs(Gammas_estimatepb[5,]-Gammas_[5,1]),abs(Gammas_estimatep[5,]-Gammas_[5,1]),
        abs(Gammas_estimate[6,]-Gammas_[1,2]),abs(Gammas_estimatepb[6,]-Gammas_[1,2]),abs(Gammas_estimatep[6,]-Gammas_[1,2]),
        abs(Gammas_estimate[8,]-Gammas_[3,2]),abs(Gammas_estimatepb[8,]-Gammas_[3,2]),abs(Gammas_estimatep[8,]-Gammas_[3,2]),
        abs(Gammas_estimate[9,]-Gammas_[4,2]),abs(Gammas_estimatepb[9,]-Gammas_[4,2]),abs(Gammas_estimatep[9,]-Gammas_[4,2]),
        abs(Gammas_estimate[10,]-Gammas_[5,2]),abs(Gammas_estimatepb[10,]-Gammas_[5,2]),abs(Gammas_estimatep[10,]-Gammas_[5,2]),
        abs(Gammas_estimate[11,]-Gammas_[1,3]),abs(Gammas_estimatepb[11,]-Gammas_[1,3]),abs(Gammas_estimatep[11,]-Gammas_[1,3]),
        abs(Gammas_estimate[12,]-Gammas_[2,3]),abs(Gammas_estimatepb[12,]-Gammas_[2,3]),abs(Gammas_estimatep[12,]-Gammas_[2,3]),
        abs(Gammas_estimate[14,]-Gammas_[4,3]),abs(Gammas_estimatepb[14,]-Gammas_[4,3]),abs(Gammas_estimatep[14,]-Gammas_[4,3]),
        abs(Gammas_estimate[15,]-Gammas_[5,3]),abs(Gammas_estimatepb[15,]-Gammas_[5,3]),abs(Gammas_estimatep[15,]-Gammas_[5,3]),
        abs(Gammas_estimate[16,]-Gammas_[1,4]),abs(Gammas_estimatepb[16,]-Gammas_[1,4]),abs(Gammas_estimatep[16,]-Gammas_[1,4]),
        abs(Gammas_estimate[17,]-Gammas_[2,4]),abs(Gammas_estimatepb[17,]-Gammas_[2,4]),abs(Gammas_estimatep[17,]-Gammas_[2,4]),
        abs(Gammas_estimate[18,]-Gammas_[3,4]),abs(Gammas_estimatepb[18,]-Gammas_[3,4]),abs(Gammas_estimatep[18,]-Gammas_[3,4]),
        abs(Gammas_estimate[20,]-Gammas_[5,4]),abs(Gammas_estimatepb[20,]-Gammas_[5,4]),abs(Gammas_estimatep[20,]-Gammas_[5,4]),
        abs(Gammas_estimate[21,]-Gammas_[1,5]),abs(Gammas_estimatepb[21,]-Gammas_[1,5]),abs(Gammas_estimatep[21,]-Gammas_[1,5]),
        abs(Gammas_estimate[22,]-Gammas_[2,5]),abs(Gammas_estimatepb[22,]-Gammas_[2,5]),abs(Gammas_estimatep[22,]-Gammas_[2,5]),
        abs(Gammas_estimate[23,]-Gammas_[3,5]),abs(Gammas_estimatepb[23,]-Gammas_[3,5]),abs(Gammas_estimatep[23,]-Gammas_[3,5]),
        abs(Gammas_estimate[24,]-Gammas_[4,5]),abs(Gammas_estimatepb[24,]-Gammas_[4,5]),abs(Gammas_estimatep[24,]-Gammas_[4,5]), 
        abs(Delta_estimate[2,]-Delta_[2,1]),abs(Delta_estimatepb[2,]-Delta_[2,1]),abs(Delta_estimatep[2,]-Delta_[2,1]),
        abs(Delta_estimate[3,]-Delta_[3,1]),abs(Delta_estimatepb[3,]-Delta_[3,1]),abs(Delta_estimatep[3,]-Delta_[3,1]),
        abs(Delta_estimate[4,]-Delta_[4,1]),abs(Delta_estimatepb[4,]-Delta_[4,1]),abs(Delta_estimatep[4,]-Delta_[4,1]),
        abs(Delta_estimate[5,]-Delta_[5,1]),abs(Delta_estimatepb[5,]-Delta_[5,1]),abs(Delta_estimatep[5,]-Delta_[5,1]),
        abs(Delta_estimate[6,]-Delta_[1,2]),abs(Delta_estimatepb[6,]-Delta_[1,2]),abs(Delta_estimatep[6,]-Delta_[1,2]),
        abs(Delta_estimate[8,]-Delta_[3,2]),abs(Delta_estimatepb[8,]-Delta_[3,2]),abs(Delta_estimatep[8,]-Delta_[3,2]),
        abs(Delta_estimate[9,]-Delta_[4,2]),abs(Delta_estimatepb[9,]-Delta_[4,2]),abs(Delta_estimatep[9,]-Delta_[4,2]),
        abs(Delta_estimate[10,]-Delta_[5,2]),abs(Delta_estimatepb[10,]-Delta_[5,2]),abs(Delta_estimatep[10,]-Delta_[5,2]),
        abs(Delta_estimate[11,]-Delta_[1,3]),abs(Delta_estimatepb[11,]-Delta_[1,3]),abs(Delta_estimatep[11,]-Delta_[1,3]),
        abs(Delta_estimate[12,]-Delta_[2,3]),abs(Delta_estimatepb[12,]-Delta_[2,3]),abs(Delta_estimatep[12,]-Delta_[2,3]),
        abs(Delta_estimate[14,]-Delta_[4,3]),abs(Delta_estimatepb[14,]-Delta_[4,3]),abs(Delta_estimatep[14,]-Delta_[4,3]),
        abs(Delta_estimate[15,]-Delta_[5,3]),abs(Delta_estimatepb[15,]-Delta_[5,3]),abs(Delta_estimatep[15,]-Delta_[5,3]),
        abs(Delta_estimate[16,]-Delta_[1,4]),abs(Delta_estimatepb[16,]-Delta_[1,4]),abs(Delta_estimatep[16,]-Delta_[1,4]),
        abs(Delta_estimate[17,]-Delta_[2,4]),abs(Delta_estimatepb[17,]-Delta_[2,4]),abs(Delta_estimatep[17,]-Delta_[2,4]),
        abs(Delta_estimate[18,]-Delta_[3,4]),abs(Delta_estimatepb[18,]-Delta_[3,4]),abs(Delta_estimatep[18,]-Delta_[3,4]),
        abs(Delta_estimate[20,]-Delta_[5,4]),abs(Delta_estimatepb[20,]-Delta_[5,4]),abs(Delta_estimatep[20,]-Delta_[5,4]),
        abs(Delta_estimate[21,]-Delta_[1,5]),abs(Delta_estimatepb[21,]-Delta_[1,5]),abs(Delta_estimatep[21,]-Delta_[1,5]),
        abs(Delta_estimate[22,]-Delta_[2,5]),abs(Delta_estimatepb[22,]-Delta_[2,5]),abs(Delta_estimatep[22,]-Delta_[2,5]),
        abs(Delta_estimate[23,]-Delta_[3,5]),abs(Delta_estimatepb[23,]-Delta_[3,5]),abs(Delta_estimatep[23,]-Delta_[3,5]),
        abs(Delta_estimate[24,]-Delta_[4,5]),abs(Delta_estimatepb[24,]-Delta_[4,5]),abs(Delta_estimatep[24,]-Delta_[4,5]))

mae = matrix(NA, 68, 3)
s1 = seq(1, 204, 3)
s2 = seq(2, 204, 3)
s3 = seq(3, 204, 3)
for(i in 1:3){
  for(j in 1:68){
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

res = res^2
mse = matrix(NA, 68, 3)
s1 = seq(1, 204, 3)
s2 = seq(2, 204, 3)
s3 = seq(3, 204, 3)
for(i in 1:3){
  for(j in 1:68){
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


