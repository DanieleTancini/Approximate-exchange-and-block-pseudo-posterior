Repository contents

This repository contains the R code used for the paper “Approximate exchange and block pseudo-posterior algorithms for spatio-temporal hidden Markov models” by D. Tancini (University of Perugia, Italy), R. Rastelli (University College Dublin, Ireland), and F. Bartolucci (University of Perugia, Italy).

The repository is organized into 11 folders, corresponding to the 9 simulation scenarios (A–I), the real-data application, and the generation of plots.
Each simulation scenario folder contains the following R scripts:

1) Latent process simulator (U_sim_phase_transition_speed.R): simulates the latent process.

2) Auxiliary process simulator (Omega_sampler_general_model_speed.R): generates auxiliary samples required by the algorithms.

3) Approximate exchange algorithm: implements the approximate exchange algorithm.

4) Pseudo-posterior algorithm (pseudo): implements the pseudo-posterior algorithm.

5) Block pseudo-posterior algorithm (pseudo_block): implements the block pseudo-posterior algorithm.

6) Performance measures (MAE.R): computes the mean absolute error and root mean squared error  for the corresponding scenario.
  
In addition, the folder for Scenario G includes the script Sim_exact_k2_n4_T2.R, which implements the exact algorithm for posterior inference and provides a benchmark for comparison with the approximate methods.
The remaining folders contain the code for the real-data application and for generating the plots presented in the paper.
