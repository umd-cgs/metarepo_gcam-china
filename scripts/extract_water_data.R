# Extracting raw GCAM data
# Graham, N.T.; Hejazi, M.I.; Chen, M.; Davies, E.; Edmonds, J.A; Kim, S.H.; Turner, S., 2023,
# "GCAM v4.3 SSP-RCP-GCM Output Databases", https://doi.org/10.7910/DVN/DYV29J, Harvard Dataverse, V2

# Install R
# This is an R script and can be run using R
# https://www.r-project.org/

# Install gcamextractor
# Use gcamextractor to extract data (User Guide: https://jgcri.github.io/gcamextractor/)
# Un-comment following lines if gcamextractor needs to be installed.
# install.packages(devtools)
# devtools::install_github("JGCRI/gcamextractor)

# Download data.
# For example: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/DYV29J
# This example uses downloaded ssp1_rcp26_gfdl.zip
# Unzip the folder.

# Extract water data
library(gcamextractor)
dataGCAM <- gcamextractor::readgcam(gcamdatabase = "ssp1_rcp26_gfdl",
                                    paramsSelect = c("watConsumBySec","watWithdrawBySec"),
                                    folder = "gcam_water_outputs")

# View your data
df <- dataGCAM$data; df
dfParam <- dataGCAM$dataAggParam; dfParam
dfClass1 <- dataGCAM$dataAggClass1; dfClass1
dfClass2 <- dataGCAM$dataAggClass1; dfClass2
