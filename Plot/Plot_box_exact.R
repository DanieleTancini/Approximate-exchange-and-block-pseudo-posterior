#Set WD for Scenario G
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

#------------------------------
library(ggplot2)
library(tidyr)

df = data.frame(
  Method = rep(c("Approx. EA", "PP"), each = ncol(Beta_estimate)),
  
  Beta = c(
    Beta_estimate[1, ] - Beta_e[1,],
    Beta_estimatep[1, ] - Beta_e[1,]
  ),
  
  Betas = c(
    Betas_estimate[1, ] - Betas_e[1,],
    Betas_estimatep[1, ] - Betas_e[1,]
  ),
  
  Gamma = c(
    Gamma_estimate[2, ] - Gamma_e[2, ],
    Gamma_estimatep[2, ] - Gamma_e[2, ]
  ),
  
  Gammas = c(
    Gammas_estimate[2, ] - Gammas_e[2, ],
    Gammas_estimatep[2, ] - Gammas_e[2, ]
  ),
  
  Delta12 = c(
    Delta_estimate[2, ] - Delta_e[2, ],
    Delta_estimatep[2, ] - Delta_e[2, ]
  ),
  
  Delta21 = c(
    Delta_estimate[3, ] - Delta_e[3, ],
    Delta_estimatep[3, ] - Delta_e[3, ]
  )
)

#Convert to long format
df_long = pivot_longer(
  df,
  cols = -Method,
  names_to = "Parameter",
  values_to = "Error"
)

df_long$Method = factor(
  df_long$Method,
  levels = c("Approx. EA", "PP")
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
