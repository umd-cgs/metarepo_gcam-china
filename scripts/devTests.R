# testing generate_figures

# R studio top right window
# Go to more>document
# Install and Restart

library(magrittr)
library(jgcricolors)
library(dplyr)
library(rmap)

images = r"{C:\Users\thom927\Documents\metarepos\khan-etal_2022_tethysSSPRCP\webpage\images\}"
folder = "C:/Users/thom927/Documents/Data/tethysDemeterOutputs"
GCAM_withdrawals_csv = "C:/Users/thom927/Documents/Data/GrahamGCAM/water_withdrawals_by_mapping_source.csv"
GCAM_consumption_csv = "C:/Users/thom927/Documents/Data/GrahamGCAM/water_consumption_by_mapping_source.csv"


out <- generate_figures(annual_rds = "annual_data.rds",
                        monthly_rds = "monthly_data.rds",
                        folder = "C:/Users/thom927/Documents/Data/tethysDemeterOutputs",
                        temporal_scale = "all")



