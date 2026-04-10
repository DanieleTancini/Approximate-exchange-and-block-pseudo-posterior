This repository contains the R functions and the replicability file used for the paper "Approximate exchange and block pseudo-posterior algorithms for spatio-temporal hidden
Markov models"  

# ----------------------------
Replicability folder: Replicability for simulation study
# ----------------------------

This folder contains:
- Simulation_study_lattice_n9_t5.R, the R file for the replicability of the 50 simulated samples related to Scenario A using the approximate exchange algorithm;
- Simulation_study_lattice_n9_t5_pseudo.R, the R file for the replicability of the 50 simulated samples related to Scenario A using the pseudo-posterior approach;
- Simulation_study_lattice_n9_t5_pseudo_block.R, the R file for the replicability of the 50 simulated samples related to Scenario A using the block pseudo-posterior approach;
- Simulation_gnm_k2_n40_t5.R, the R file for the replicability of the 50 simulated samples related to Scenario B using the approximate exchange algorithm;
- Simulation_gnm_k2_n40_t5_pseudo.R, the R file for the replicability of the 50 simulated samples related to Scenario B using the pseudo-posterior approach;
- Simulation_gnm_k2_n40_t5_pseudo_block.R, the R file for the replicability of the 50 simulated samples related to Scenario B using the block pseudo-posterior approach;
- Simulation_gnm_k2_n40_t10.R, the R file for the replicability of the 50 simulated samples related to Scenario C using the approximate exchange algorithm;
- Simulation_gnm_k2_n40_t10_pseudo.R, the R file for the replicability of the 50 simulated samples related to Scenario C using the pseudo-posterior approach;
- Simulation_gnm_k2_n40_t10_pseudo_block.R, the R file for the replicability of the 50 simulated samples related to Scenario C using the block pseudo-posterior approach;
- Simulation_gnm_k3_n40_t5.R, the R file for the replicability of the 50 simulated samples related to Scenario D using the approximate exchange algorithm;
- Simulation_gnm_k3_n40_t5_pseudo.R, the R file for the replicability of the 50 simulated samples related to Scenario D using the pseudo-posterior approach;
- Simulation_gnm_k3_n40_t5_pseudo_block.R, the R file for the replicability of the 50 simulated samples related to Scenario D using the block pseudo-posterior approach;
- Omega_sampler_general_model_speed.R, the R function for the auxiliary process;
- U_sim_phase_transition_speed.R, the R function for the latent process;
- MAE_simulation, the R file for the MAEs of the simulation study.

Please start running the approximate exchange algorithms, as they generate the samples used by the other methods

# ----------------------------
Replicability folder: Replicability for synthetic data analysis
# ----------------------------

This folder contains:
- Omega_sampler_general_model_speed.R, the R function for the auxiliary process;
- U_sim_phase_transition_speed.R, the R function for the latent process;
- Synthetic_data_lattice_n9_t5_ap_exch.R, the R file for the replicability of the first synthetic dataset estimated using the approximate exchange algorithm;
- Synthetic_data_lattice_n9_t5_block_pseudo.R, the R file for the replicability of the first synthetic dataset estimated using the block pseudo-posterior algorithm;
- Synthetic_data_lattice_n9_t5_pseudo.R, the R file for the replicability of first the synthetic dataset estimated using the pseudo-posterior algorithm;
- Synthetic_data_n40_t10_ap_exch.R, the R file for the replicability of the second synthetic dataset estimated using the approximate exchange algorithm;
- Synthetic_data_n40_t10_block_pseudo.R, the R file for the replicability of the second syntheticdataset estimated using the block pseudo-posterior algorithm;
- Synthetic_data_n40_t10_pseudo.R, the R file for the replicability of the second synthetic dataset estimated using the pseudo-posterior algorithm;
- Synthetic data analysis, the R file for the analysis of the results and plots.

Please start running the approximate exchange algorithms, as they generate the samples used by the other methods

# ----------------------------
Application folder: Replicability for application
# ----------------------------

This folder contains:
- Data_applicazione.RData, the RData file contains the preprocessed data. The original dataset is available https://www.istat.it/comunicato-stampa/andamento-meteo-climatico-in-italia-anni-2000-2009/;
- K3.R, the R file for the replicability of the model estimated;
- Omega_sampler_general_model_speed.R, the R function for the auxiliary process.
