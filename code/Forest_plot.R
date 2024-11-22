#---------------------------------------------Loading the libraries----------------------------------------------------------#

libs <- c("haven", "mice", "dplyr", "tidyverse", "finalfit", "writexl", "lme4", "foreign", "readxl", "broom", 
          "gtsummary", "gt", "sjlabelled", "data.table", "DiagrammeRsvg", "DiagrammeR", "gridExtra", "grid", 
          "tidymodels", "msm", "flexsurv", "minqa", "survival", "mstate", "lattice", "latticeExtra", 
          "RColorBrewer", "diagram", "openxlsx", "janitor", "Hmisc", "magrittr", "rstan", "lmtest", "caret")

for(ilib in libs){
  if(!(ilib %in% installed.packages())){
    install.packages(ilib)
  }
  library(ilib, character.only = T)
}


#--------------------------------------load both male and female datasets ---------------------------------------------------#


load("D:\\APHRC\\APHRC-projects\\MSM\\MSM-Residence-2024\\Data\\male_all_hazard_data.RData")

load("D:\\APHRC\\APHRC-projects\\MSM\\MSM-Residence-2024\\Data\\female_all_hazard_data.RData")
