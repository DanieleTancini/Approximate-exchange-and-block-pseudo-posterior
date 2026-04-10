#Run this script after executing the code in the Simulation Study folder. It compiles and presents the results generated during the simulation study.
#Please note that the order in round(mae,3), for \gamma \gamma^* and \delta is by row, instead of by column as in the paper

#--------------------
#Scenario A
#--------------------

#Set WD
setwd("")
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

#------------------
#Scenario B
#------------------

setwd("")
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

for(s in 1:S){
  
  load(paste0("Sim_Gnm_k2_n40_t5_sigmasqrt1_rv_r5_number_",s,".Rdata"))
  Beta_estimate[1:2,s] = rowMeans(Beta_es[,in_it:fin_it])
  Betas_estimate[1:2,s] = rowMeans(Betas_es[,in_it:fin_it])
  Gamma_estimate[1:4,s] = rowMeans(matrix(Gamma_es[,,in_it:fin_it],4,ite))
  Gammas_estimate[1:4,s] = rowMeans(matrix(Gammas_es[,,in_it:fin_it],4,ite))
  Delta_estimate[1:4,s] = rowMeans(matrix(Delta_es[,,in_it:fin_it],4,ite))
  
  load(paste0("Sim_Gnm_k2_n40_t5_sigmasqrt1_rv_pseudo_number_adj_",s,".Rdata"))
  Beta_estimatep[1:2,s] = rowMeans(Beta_es[,in_it:fin_it])
  Betas_estimatep[1:2,s] = rowMeans(Betas_es[,in_it:fin_it])
  Gamma_estimatep[1:4,s] = rowMeans(matrix(Gamma_es[,,in_it:fin_it],4,ite))
  Gammas_estimatep[1:4,s] = rowMeans(matrix(Gammas_es[,,in_it:fin_it],4,ite))
  Delta_estimatep[1:4,s] = rowMeans(matrix(Delta_es[,,in_it:fin_it],4,ite))
  
  load(paste0("Sim_Gnm_k2_n40_t5_sigmasqrt1_rv_pseudo_block_adj_",s,".Rdata"))
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

#------------------
#Scenario C
#------------------

setwd("")
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

for(s in 1:S){
  
  load(paste0("Sim_Gnm_k2_n40_t10_sigmasqrt1_rv_r5_number_",s,".Rdata"))
  Beta_estimate[1:2,s] = rowMeans(Beta_es[,in_it:fin_it])
  Betas_estimate[1:2,s] = rowMeans(Betas_es[,in_it:fin_it])
  Gamma_estimate[1:4,s] = rowMeans(matrix(Gamma_es[,,in_it:fin_it],4,ite))
  Gammas_estimate[1:4,s] = rowMeans(matrix(Gammas_es[,,in_it:fin_it],4,ite))
  Delta_estimate[1:4,s] = rowMeans(matrix(Delta_es[,,in_it:fin_it],4,ite))
  
  load(paste0("Sim_Gnm_k2_n40_t10_sigmasqrt1_rv_pseudo_number_adj",s,".Rdata"))
  Beta_estimatep[1:2,s] = rowMeans(Beta_es[,in_it:fin_it])
  Betas_estimatep[1:2,s] = rowMeans(Betas_es[,in_it:fin_it])
  Gamma_estimatep[1:4,s] = rowMeans(matrix(Gamma_es[,,in_it:fin_it],4,ite))
  Gammas_estimatep[1:4,s] = rowMeans(matrix(Gammas_es[,,in_it:fin_it],4,ite))
  Delta_estimatep[1:4,s] = rowMeans(matrix(Delta_es[,,in_it:fin_it],4,ite))
  
  load(paste0("Sim_Gnm_k2_n40_t10_sigmasqrt1_rv_pseudo_block_adj_",s,".Rdata"))
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


#------------------
#Scenario D
#------------------

setwd("")
S=50
Beta_estimate = matrix(NA,3,S)
Betas_estimate = matrix(NA,3,S)
Gamma_estimate = matrix(NA,9,S)
Gammas_estimate = matrix(NA,9,S)
Delta_estimate = matrix(NA,9,S)

Beta_estimatep = matrix(NA,3,S)
Betas_estimatep = matrix(NA,3,S)
Gamma_estimatep = matrix(NA,9,S)
Gammas_estimatep = matrix(NA,9,S)
Delta_estimatep = matrix(NA,9,S)

Beta_estimatepb = matrix(NA,3,S)
Betas_estimatepb = matrix(NA,3,S)
Gamma_estimatepb = matrix(NA,9,S)
Gammas_estimatepb = matrix(NA,9,S)
Delta_estimatepb = matrix(NA,9,S)

in_it = 5001
fin_it = 10000
ite = 5000

for(s in 1:S){
  
  load(paste0("Sim_Gnm_k3_n40_t5_sigmasqrt1_rv_pseudo_number_adj",s,".Rdata"))
  Beta_estimatep[1:3,s] = rowMeans(Beta_es[,in_it:fin_it])
  Betas_estimatep[1:3,s] = rowMeans(Betas_es[,in_it:fin_it])
  Gamma_estimatep[1:9,s] = rowMeans(matrix(Gamma_es[,,in_it:fin_it],9,ite))
  Gammas_estimatep[1:9,s] = rowMeans(matrix(Gammas_es[,,in_it:fin_it],9,ite))
  Delta_estimatep[1:9,s] = rowMeans(matrix(Delta_es[,,in_it:fin_it],9,ite))
  
  load(paste0("Sim_Gnm_k3_n40_t5_sigmasqrt1_rv_pseudo_block_adj_",s,".Rdata"))
  Beta_estimatepb[1:3,s] = rowMeans(Beta_es[,in_it:fin_it])
  Betas_estimatepb[1:3,s] = rowMeans(Betas_es[,in_it:fin_it])
  Gamma_estimatepb[1:9,s] = rowMeans(matrix(Gamma_es[,,in_it:fin_it],9,ite))
  Gammas_estimatepb[1:9,s] = rowMeans(matrix(Gammas_es[,,in_it:fin_it],9,ite))
  Delta_estimatepb[1:9,s] = rowMeans(matrix(Delta_es[,,in_it:fin_it],9,ite))
 
  load(paste0("Sim_Gnm_k3_n40_t5_sigmasqrt1_rv_r5_number_prova_",s,".Rdata"))
  Beta_estimate[1:3,s] = rowMeans(Beta_es[,in_it:fin_it])
  Betas_estimate[1:3,s] = rowMeans(Betas_es[,in_it:fin_it])
  Gamma_estimate[1:9,s] = rowMeans(matrix(Gamma_es[,,in_it:fin_it],9,ite))
  Gammas_estimate[1:9,s] = rowMeans(matrix(Gammas_es[,,in_it:fin_it],9,ite))
  Delta_estimate[1:9,s] = rowMeans(matrix(Delta_es[,,in_it:fin_it],9,ite)) 
}

S=50

names = rep(c("beta1", "beta2", "betas1", "betas2", "gamma21", "gamma31", "gamma12", "gamma32", "gamma13", "gamma23", "gammas21", "gammas31", "gammas12", "gammas32", "gammas13", "gammas23", "delta21", "delta31", "delta12", "delta32", "delta13", "delta23"), c(S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3,S*3))
mcmc = rep(rep(c("exchange", "block", "pseudo"), c(S,S,S)),22)
res = c(abs(Beta_estimate[1,]-Beta_[1]),abs(Beta_estimatepb[1,]-Beta_[1]),abs(Beta_estimatep[1,]-Beta_[1]),abs(Beta_estimate[2,]-Beta_[2]),abs(Beta_estimatepb[2,]-Beta_[2]),abs(Beta_estimatep[2,]-Beta_[2]),abs(Betas_estimate[1,]-Betas_[1]),abs(Betas_estimatepb[1,]-Betas_[1]),abs(Betas_estimatep[1,]-Betas_[1]),abs(Betas_estimate[2,]-Betas_[2]),abs(Betas_estimatepb[2,]-Betas_[2]),abs(Betas_estimatep[2,]-Betas_[2]),abs(Gamma_estimate[2,]-Gamma_[2,1]),abs(Gamma_estimatepb[2,]-Gamma_[2,1]),abs(Gamma_estimatep[2,]-Gamma_[2,1]),abs(Gamma_estimate[3,]-Gamma_[3,1]),abs(Gamma_estimatepb[3,]-Gamma_[3,1]),abs(Gamma_estimatep[3,]-Gamma_[3,1]),abs(Gamma_estimate[4,]-Gamma_[1,2]),abs(Gamma_estimatepb[4,]-Gamma_[1,2]),abs(Gamma_estimatep[4,]-Gamma_[1,2]),abs(Gamma_estimate[6,]-Gamma_[3,2]),abs(Gamma_estimatepb[6,]-Gamma_[3,2]),abs(Gamma_estimatep[6,]-Gamma_[3,2]),abs(Gamma_estimate[7,]-Gamma_[1,3]),abs(Gamma_estimatepb[7,]-Gamma_[1,3]),abs(Gamma_estimatep[7,]-Gamma_[1,3]),abs(Gamma_estimate[8,]-Gamma_[2,3]),abs(Gamma_estimatepb[8,]-Gamma_[2,3]),abs(Gamma_estimatep[8,]-Gamma_[2,3]),abs(Gammas_estimate[2,]-Gammas_[2,1]),abs(Gammas_estimatepb[2,]-Gammas_[2,1]),abs(Gammas_estimatep[2,]-Gammas_[2,1]),abs(Gammas_estimate[3,]-Gammas_[3,1]),abs(Gammas_estimatepb[3,]-Gammas_[3,1]),abs(Gammas_estimatep[3,]-Gammas_[3,1]),abs(Gammas_estimate[4,]-Gammas_[1,2]),abs(Gammas_estimatepb[4,]-Gammas_[1,2]),abs(Gammas_estimatep[4,]-Gammas_[1,2]),abs(Gammas_estimate[6,]-Gammas_[3,2]),abs(Gammas_estimatepb[6,]-Gammas_[3,2]),abs(Gammas_estimatep[6,]-Gammas_[3,2]),abs(Gammas_estimate[7,]-Gammas_[1,3]),abs(Gammas_estimatepb[7,]-Gammas_[1,3]),abs(Gammas_estimatep[7,]-Gammas_[1,3]),abs(Gammas_estimate[8,]-Gammas_[2,3]),abs(Gammas_estimatepb[8,]-Gammas_[2,3]),abs(Gammas_estimatep[8,]-Gammas_[2,3]),abs(Delta_estimate[2,]-Delta_[2,1]),abs(Delta_estimatepb[2,]-Delta_[2,1]),abs(Delta_estimatep[2,]-Delta_[2,1]),abs(Delta_estimate[3,]-Delta_[3,1]),abs(Delta_estimatepb[3,]-Delta_[3,1]),abs(Delta_estimatep[3,]-Delta_[3,1]),abs(Delta_estimate[4,]-Delta_[1,2]),abs(Delta_estimatepb[4,]-Delta_[1,2]),abs(Delta_estimatep[4,]-Delta_[1,2]),abs(Delta_estimate[6,]-Delta_[3,2]),abs(Delta_estimatepb[6,]-Delta_[3,2]),abs(Delta_estimatep[6,]-Delta_[3,2]),abs(Delta_estimate[7,]-Delta_[1,3]),abs(Delta_estimatepb[7,]-Delta_[1,3]),abs(Delta_estimatep[7,]-Delta_[1,3]),abs(Delta_estimate[8,]-Delta_[2,3]),abs(Delta_estimatepb[8,]-Delta_[2,3]),abs(Delta_estimatep[8,]-Delta_[2,3]))

mae = matrix(NA, 22, 3)
s1 = seq(1, 66, 3)
s2 = seq(2, 66, 3)
s3 = seq(3, 66, 3)
for(i in 1:3){
  for(j in 1:22){
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





