# run this code at the beginning to setup working directory, load required packages, sourcing required functions when
# starting working in R for the first time.

# change this to your path
# setwd('/home/sahas3/Desktop/DataAnalytics/ge_lightning/');
 setwd('/home/sahas3/MyFiles/Study_Current_Sem/Fall_2016/DataAnalytics/');
# setwd('/home/juliuslab/Desktop/SayanWork/ge_lightning/');

#  packages being used : add any package here
packageNames <- c('ggplot2', 'xlsx', 'rgdal', 'ggmap', 'fields', # 'cluster', 'mixtools', 'dplyr', 'lattice',
                      'MASS', # 'rpart', 'doParallel', 'doMC', 'grImport',
                      'car', 'reshape2', 'caret', 'e1071', 'clusterGeneration', 'optimbase', 'fitdistrplus',
                      'pROC', 'png', 'caTools', #'caretEnsemble',
                       # 'gsubfn',
                       'parallel', 'doParallel', 'grid')

# if (Sys.info()['sysname'] == 'Linux')
# {
#     isLinux = T
#     packageNames <- c(packageNames, 'doMC') # doMC is not available for WINDOWS
# } else {
#     isLinux = F
#     packageNames <- c(packageNames, 'doParallel')
# }

# install any package not already installed
new.packages <- packageNames[!(packageNames %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load all packages
tmpVal = lapply(packageNames, library, character.only = TRUE)

# load initial workspace image
load("./WorkSpaceData/initWrkSpc.RData")

# # if loaded image no need to source the functions unless you changed them