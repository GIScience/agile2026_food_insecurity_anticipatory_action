

⚠️ *Still under construction*
This repository is currently under construction and is being prepared for reproducibility review.  


# FEWS NET vs IPC – Somalia (2017–2025) comparison

This repository contains the preprocessing and analysis code used in the paper:
**Food Insecurity Projections for Anticipatory Action: A Comparative Spatiotemporal Analysis of FEWS NET and the IPC in Somalia**.

The workflow harmonizes FEWS NET and IPC acute food insecurity classifications, builds comparable time series, aggregates to admin-2 districts, and performs analysis on similarity of historical data and a prediction skill assessment



## Repository structure

- `data/`  
  Input datasets

- `FEWSNET_1_geom_join.R`  
- `FEWSNET_2_ts_building.R`  
- `FEWSNET_3_adm2_join.R`  
  FEWS NET preprocessing pipeline

- `IPC_1_data_merging.R`  
- `IPC_2_ts_building.R`  
- `IPC_3_adm2_join.R`  
  IPC preprocessing pipeline