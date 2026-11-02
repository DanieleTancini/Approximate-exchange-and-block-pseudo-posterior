#Set WD Scenario B
load("Sim_Gnm_k2_n40_t5_sigmasqrt1_rv_r5_number_1.Rdata")

par(mfrow = c(3, 2), mar = c(6, 6, 5, 3),
    mgp = c(3.5, 1.5, 0), cex.main = 3,  cex.lab  = 3,  cex.axis = 3)

plot(Beta_es[1,], type = "l",
     main = expression(beta[1]), ylim = c(-3,2),
     xlab = "Iteration", ylab = "Value", lwd = 1.5)

plot(Betas_es[1,], type = "l",
     main = expression(beta[1]^"*"), ylim = c(-0.5,2),
     xlab = "Iteration", ylab = "", lwd = 1.5)

plot(Gamma_es[1,2,], type = "l",
     main = expression(gamma[12]), ylim = c(-2.5,2),
     xlab = "Iteration", ylab = "Value", lwd = 1.5)

plot(Gammas_es[1,2,], type = "l",
     main = expression(gamma[12]^"*"), ylim = c(-1.5,0.5),
     xlab = "Iteration", ylab = "", lwd = 1.5)

plot(Delta_es[1,2,], type = "l",
     main = expression(delta[12]), ylim = c(-2,1),
     xlab = "Iteration", ylab = "Value", lwd = 1.5)

plot(Delta_es[2,1,], type = "l",
     main = expression(delta[21]), ylim = c(-2,1),
     xlab = "Iteration", ylab = "", lwd = 1.5)

load("Sim_Gnm_k2_n40_t5_sigmasqrt1_rv_pseudo_number_adj_1.Rdata")

par(mfrow = c(3, 2), mar = c(6, 6, 5, 3),
    mgp = c(3.5, 1.5, 0), cex.main = 3,  cex.lab  = 3,  cex.axis = 3)

plot(Beta_es[1,], type = "l",
     main = expression(beta[1]), ylim = c(-3,2),
     xlab = "Iteration", ylab = "Value", lwd = 1.5)

plot(Betas_es[1,], type = "l",
     main = expression(beta[1]^"*"), ylim = c(-0.5,2),
     xlab = "Iteration", ylab = "", lwd = 1.5)

plot(Gamma_es[1,2,], type = "l",
     main = expression(gamma[12]), ylim = c(-2.5,2),
     xlab = "Iteration", ylab = "Value", lwd = 1.5)

plot(Gammas_es[1,2,], type = "l",
     main = expression(gamma[12]^"*"), ylim = c(-1.5,0.5),
     xlab = "Iteration", ylab = "", lwd = 1.5)

plot(Delta_es[1,2,], type = "l",
     main = expression(delta[12]), ylim = c(-2,1),
     xlab = "Iteration", ylab = "Value", lwd = 1.5)

plot(Delta_es[2,1,], type = "l",
     main = expression(delta[21]), ylim = c(-2,1),
     xlab = "Iteration", ylab = "", lwd = 1.5)

load("Sim_Gnm_k2_n40_t5_sigmasqrt1_rv_pseudo_block_adj_1.Rdata")

par(mfrow = c(3, 2), mar = c(6, 6, 5, 3),
    mgp = c(3.5, 1.5, 0), cex.main = 3,  cex.lab  = 3,  cex.axis = 3)

plot(Beta_es[1,], type = "l",
     main = expression(beta[1]), ylim = c(-3,2),
     xlab = "Iteration", ylab = "Value", lwd = 1.5)

plot(Betas_es[1,], type = "l",
     main = expression(beta[1]^"*"), ylim = c(-0.5,2),
     xlab = "Iteration", ylab = "", lwd = 1.5)

plot(Gamma_es[1,2,], type = "l",
     main = expression(gamma[12]), ylim = c(-2.5,2),
     xlab = "Iteration", ylab = "Value", lwd = 1.5)

plot(Gammas_es[1,2,], type = "l",
     main = expression(gamma[12]^"*"), ylim = c(-1.5,0.5),
     xlab = "Iteration", ylab = "", lwd = 1.5)

plot(Delta_es[1,2,], type = "l",
     main = expression(delta[12]), ylim = c(-2,1),
     xlab = "Iteration", ylab = "Value", lwd = 1.5)

plot(Delta_es[2,1,], type = "l",
     main = expression(delta[21]), ylim = c(-2,1),
     xlab = "Iteration", ylab = "", lwd = 1.5)

par(mfrow = c(1, 1))
