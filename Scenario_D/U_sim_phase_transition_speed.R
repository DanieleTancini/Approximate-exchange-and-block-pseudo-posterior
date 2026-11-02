#U simulator 

U_sim = function(n,Ti,k,iniz_C,Beta,Betas,Delta,Gamma,Gammas,iter,Com,post_exp){
  #Iterations
  R = iter
  
  #Initial Hidden states
  U = 1:k
  mU = matrix(U,k,n)
  u = sample(U, n*Ti, replace = TRUE)
  u = matrix(u, n, Ti)
  
  #Neigh matrix
  if(iniz_C == "random"){
    C = matrix(0,n,n)
    C[upper.tri(C)]=rbinom(n*(n-1)/2,1,1/(n-1))
    Com = C + t(C)
  }else{
    Com = Com
    C = matrix(0,n,n)
    C[upper.tri(C, diag = FALSE)] = Com[upper.tri(Com, diag = FALSE)]
  }
  Cl = t(C)
  
  #U_generator
  U_est = array(NA, dim = c(n,Ti,R))
  U_est[,,1] = u
  
  #Comp
  fin = matrix(NA, n, k)
  eq_a = eq_b = eq_c = matrix(NA, n, k)
  
  #Comp
  res = matrix(NA, n, k)
  res2 = matrix(NA, n, k)
  res3 = matrix(NA, n, k)
  res4 = matrix(NA, n, k)
  matr = matrix(rep(c(1:k), rep(n,1)),k,n)
  
  for(r in 2:R){
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
          u[l,t] = sample(U, 1, prob = eq_a[l,], replace = TRUE)
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
          u[l,t] = sample(U, 1, prob = eq_b[l,], replace = TRUE)
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
          u[l,t] = sample(U, 1, prob = eq_c[l,], replace = TRUE)
        }
      }
    }
    U_est[,,r] = u
  }
  #Posterior
  if(post_exp == "TRUE"){
    mean_U = matrix(NA, n, Ti)
    for(i in 1:n){
      for(t in 1:Ti){
        mean_U[i,t] = as.integer(names(which.max(table(U_est[i,t,]))))
      }
    }
    res = list(mean_u = mean_U, C = C, Com = Com, U_est = U_est)
  }else{
    res = list(C = C, Com = Com, U_est = U_est[,,iter])
  }
}  