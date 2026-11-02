#Set WD for Scenario D
#setwd("")

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
  
  load(paste0("Sim_Gnm_k2_n40_t5_sigmasqrt1_rv_r5_number_strong_",s,".Rdata"))
  Beta_estimate[1:2,s] = rowMeans(Beta_es[,in_it:fin_it])
  Betas_estimate[1:2,s] = rowMeans(Betas_es[,in_it:fin_it])
  Gamma_estimate[1:4,s] = rowMeans(matrix(Gamma_es[,,in_it:fin_it],4,ite))
  Gammas_estimate[1:4,s] = rowMeans(matrix(Gammas_es[,,in_it:fin_it],4,ite))
  Delta_estimate[1:4,s] = rowMeans(matrix(Delta_es[,,in_it:fin_it],4,ite))
  
  load(paste0("Sim_Gnm_k2_n40_t5_sigmasqrt1_rv_pseudo_number_adj_strong_",s,".Rdata"))
  Beta_estimatep[1:2,s] = rowMeans(Beta_es[,in_it:fin_it])
  Betas_estimatep[1:2,s] = rowMeans(Betas_es[,in_it:fin_it])
  Gamma_estimatep[1:4,s] = rowMeans(matrix(Gamma_es[,,in_it:fin_it],4,ite))
  Gammas_estimatep[1:4,s] = rowMeans(matrix(Gammas_es[,,in_it:fin_it],4,ite))
  Delta_estimatep[1:4,s] = rowMeans(matrix(Delta_es[,,in_it:fin_it],4,ite))
  
  load(paste0("Sim_Gnm_k2_n40_t5_sigmasqrt1_rv_pseudo_block_adj_strong_",s,".Rdata"))
  Beta_estimatepb[1:2,s] = rowMeans(Beta_es[,in_it:fin_it])
  Betas_estimatepb[1:2,s] = rowMeans(Betas_es[,in_it:fin_it])
  Gamma_estimatepb[1:4,s] = rowMeans(matrix(Gamma_es[,,in_it:fin_it],4,ite))
  Gammas_estimatepb[1:4,s] = rowMeans(matrix(Gammas_es[,,in_it:fin_it],4,ite))
  Delta_estimatepb[1:4,s] = rowMeans(matrix(Delta_es[,,in_it:fin_it],4,ite))
  
}


#------------------------------
library(ggplot2)
library(tidyr)

df = data.frame(
  Method = rep(c("Approx. EA", "BPP", "PP"), each = ncol(Beta_estimate)),
  
  Beta = c(
    Beta_estimate[1, ] - Beta_[1],
    Beta_estimatepb[1, ] - Beta_[1],
    Beta_estimatep[1, ] - Beta_[1]
  ),
  
  Betas = c(
    Betas_estimate[1, ] - Betas_[1],
    Betas_estimatepb[1, ] - Betas_[1],
    Betas_estimatep[1, ] - Betas_[1]
  ),
  
  Gamma = c(
    Gamma_estimate[2, ] - Gamma_[1, 2],
    Gamma_estimatepb[2, ] - Gamma_[1, 2],
    Gamma_estimatep[2, ] - Gamma_[1, 2]
  ),
  
  Gammas = c(
    Gammas_estimate[2, ] - Gammas_[1, 2],
    Gammas_estimatepb[2, ] - Gammas_[1, 2],
    Gammas_estimatep[2, ] - Gammas_[1, 2]
  ),
  
  Delta12 = c(
    Delta_estimate[2, ] - Delta_[1, 2],
    Delta_estimatepb[2, ] - Delta_[1, 2],
    Delta_estimatep[2, ] - Delta_[1, 2]
  ),
  
  Delta21 = c(
    Delta_estimate[3, ] - Delta_[2, 1],
    Delta_estimatepb[3, ] - Delta_[2, 1],
    Delta_estimatep[3, ] - Delta_[2, 1]
  )
)

# Convert to long format
df_long = pivot_longer(
  df,
  cols = -Method,
  names_to = "Parameter",
  values_to = "Error"
)

df_long$Method = factor(
  df_long$Method,
  levels = c("Approx. EA", "BPP", "PP")
)

# Order and label the parameters
df_long$Parameter = factor(
  df_long$Parameter,
  levels = c("Beta", "Betas", "Gamma", "Gammas", "Delta12", "Delta21"),
  labels = c(
    "beta[1]",
    "beta[1]^'*'",
    "gamma[12]",
    "gamma[12]^'*'",
    "delta[12]",
    "delta[21]"
  )
)

# Plot
ggplot(df_long, aes(x = Method, y = Error)) +
  geom_boxplot(
    width = 0.65,
    outlier.size = 0.8,
    fill = "grey"
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "red",
    linewidth = 0.4
  ) +
  facet_wrap(
    ~ Parameter,
    ncol = 2,
    nrow = 3,
    scales = "free_y",
    labeller = label_parsed
  ) +
  labs(
    x = NULL,
    y = "Estimation error"
  ) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 20, face = "bold"),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )
