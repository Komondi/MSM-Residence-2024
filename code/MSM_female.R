#---------------------------------------------Loading the libraries----------------------------------------------------#

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


#--------------------------------------Residency Data, 2002 to 2015----------------------------------------------------#
#replace with simulated data for learning (female simulated data)
NUHDSS_Resi_Data_2002_2015 <- readRDS("D:\\APHRC\\APHRC-projects\\MSM\\MSM-Residence-2024\\Data\\NUHDSS_Final_MSM.rds")


#--------------------------------------------transition matrix---------------------------------------------------------#

trans_mat <- transMat(x = list( c(3, 5, 7), c(3, 5, 7), c(4), c(3,5,7),c(6),c(3,5,7), c()),
                      names = c("Enumeration","Birth","Exit","Entry","Outmigration","Inmigration","Death"))



#----------------------------------------Diagram for the transition matrix---------------------------------------------#

diagram::plotmat(t(trans_mat), name = c("Enumeration(1)", "Birth(2)", "Exit(3)","Entry(4)","Outmigration(5)",
                                        "Inmigration(6)","Death(7)"), lwd = 1.5, box.lwd = 2, cex.txt = 0.8, box.size = 0.11,  
                 box.type = "square", box.prop = 0.15, box.col = "forestgreen",  arr.length=.35, arr.width=.5, 
                 self.cex = .6, self.shifty = -.01, self.shiftx = .14, main = " ") #light blue (use green)


#--------------------------------------------Female data---------------------------------------------------------------#

#Do not use line 39. Use the loaded simulated data which is female data already
Female_data <- NUHDSS_Resi_Data_2002_2015 %>% filter(gender_of_NUHDSS_individual %in% c('Female'))

#-------------------------------------------chain of movement for model------------------------------------------------#

(smatrix<-statetable.msm(Event, ID, data = Female_data)) 


round(prop.table(smatrix, margin=1)*100, 0)

#--------------------------------------------Get initial estimates-----------------------------------------------------#

(Q<-matrix(c(0.09, 0, 0.08, 0,0.09,0,0.6,
             0, 0.4, 0.5, 0, 0.3, 0, 0.4,
             0, 0, 0.91, 0.2, 0,0, 0.2,
             0,0,0.25,0.81,0.22,0,0.2,
             0,0,0,0,0.1,0.23,0,
             0, 0, 0.61,0, 0.42,0.3,0.6,
             0, 0, 0, 0, 0, 0, 0),7,7, byrow = T))

(Q.crude <- crudeinits.msm(Event_1 ~ days, ID, data = Female_data, qmatrix = Q))

#----------------------------------Model without the covariates (simple bidirectional model)---------------------------#

msm_model_female_simple <- msm(Event_1 ~ days, ID, data = Female_data , exacttimes = TRUE, gen.inits = T, qmatrix = Q.crude, 
                           control = list(fnscale = 6e+05, trace = 0, REPORT = 1, maxit = 100000), opt.method = "optim")




print("----------------------------------Final female model running---------------------------------------------------")


msm_model_female <- msm(Event_1 ~ days, ID, data = Female_data , exacttimes = TRUE, gen.inits = T,
                        covariates = ~ slum_area_in_NUHDSS  + ethnicity_of_NUHDSS_individual + 
                          age_in_completed_years + type_of_area_in_Kenya_in_which_individual_was_born, qmatrix = Q.crude, 
                        control = list(fnscale = 6e+05, trace = 0, REPORT = 1, maxit = 100000), opt.method = "optim")


msm_model_female

save(msm_model_female, file="D:\\APHRC\\APHRC-projects\\MSM\\MSM-Residence-2024\\Data\\msm_model_female.RData")

#---------------------------------------------goodness of fit----------------------------------------------------------#

logLik(msm_model_female)

lrtest(msm_model_female_simple, msm_model_female)


#---------------------------------------simulate date------------------------------------------------------------------#

female_simulated_data = simfitted.msm(msm_model_female)



#--------------------------------------------------other rates---------------------------------------------------------#

qmatrix.msm(msm_model_female)
pmatrix.msm(msm_model_female, t=2)
sojourn.msm(msm_model_female)
msm_model_female$paramdata$npars ## get number of parameters
AIC(msm_model_female) ## AIC


prev = prevalence.msm( x = msm_model_female, covariates = 'mean', ci = 'normal' )

prev_plot = plot.prevalence.msm(x = msm_model_female, prev.obj = prev, exacttimes = TRUE, M = FALSE, ci = TRUE )

summary(msm_model_female)

#----------------------------------------Plotting transition probabilities---------------------------------------------#

# 4901

plotdat <-NULL


for (i in 1:7){
  for( j in 1:7){
    if(i !=j){
      to_plot<-data.frame(Transitions=c(1:200), Time=c(1:200), Value=c(1:200))
      l=0
      for(k in seq(0,5000,length=200)){
        # browser()
        l=l+1
        to_plot$Transitions[l]<-paste("State",i,"-",j)
        to_plot$Time[l]<-k
        to_plot$Value[l] <- pmatrix.msm(msm_model_female, t=k)[i,j]
        
      }
      plotdat<-rbind(to_plot, plotdat)
    }
  }
}



plotdat<-plotdat %>%  filter(!Transitions %in% c("State 1 - 2","State 1 - 4","State 2 - 1","State 2 - 4", "State 3 - 1",
                                                 "State 3 - 2","State 3 - 5","State 4 - 1", "State 4 - 2", "State 5 - 1", 
                                                 "State 5 - 2","State 5 - 3", "State 5 - 4","State 5 - 5", "State 7 - 5", 
                                                 "State 7 - 6", "State 7 - 4", "State 7 - 3", "State 7 - 2", "State 7 - 1",
                                                 "State 6 - 4", "State 6 - 2", "State 6 - 1", "State 5 - 7", "State 4 - 6",
                                                 "State 3 - 7", "State 3 - 6", "State 2 - 6", "State 1 - 6"))



ggplot(plotdat, aes(x = Time, y = Value, color = Transitions, group = Transitions)) +  geom_line(linewidth = 1) +  
  geom_point(size = 0.5) +  labs(x = "Time (Days)", y ="Transition probability", color = "Transitions", 
      title = NULL) +   theme_bw() +
  theme(axis.text.x = element_text(face = "bold", size = 12), 
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title.x = element_text(face = "bold", size = 12), 
        axis.title.y = element_text(face = "bold", size = 12),
        legend.text = element_text(face = "bold", size = 12), 
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(face = "bold", size = 12))


#-----------------------------------------the survival plots------------------------------------------------------------#

prev_female <- prevalence.msm(msm_model_female)

# reshape observed prevalence

do1_f <-as_tibble(row.names(prev_female$Observed)) %>% rename(time = value) %>%  mutate(time = as.numeric(time))

do2_f <-as_tibble(prev_female$Observed) %>% mutate(type = "observed")
do_f <- cbind(do1_m, do2_m) %>% select(-Total)
do_l_f <- do_m %>% gather(state, number, -time, -type)


# reshape expected prevalence

de1_f <-as_tibble(row.names(prev_female$Expected)) %>% rename(time = value) %>% mutate(time = as.numeric(time))
de2_f <-as_tibble(prev_female$Expected) %>% mutate(type = "expected")
de_f <- cbind(de1_f,de2_f) %>% select(-Total) 
de_l_f <- de_f %>% gather(state, number, -time, -type) 


# bind into a single data frame
prev_l_f <-rbind(do_l_f, de_l_f) %>% mutate(type = factor(type),state = factor(state), time = round(time,3))


# plot for comparison
prev_gp_f <- prev_l_f %>% group_by(state)

prev_l_f %>% ggplot() +  geom_line(aes(time, number, color = type), linewidth = 1.2) +
  labs(title = "", x = "Days",   y = "") + theme_bw() +  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10, face = "bold"),,
    axis.text.y = element_text(size = 10, face = "bold"),
  ) +
  facet_wrap(~state, scales = "free_y", ncol = 2) +
  scale_y_continuous(limits = c(0, 12000), breaks = seq(0, 12000, by = 3000)) 


#------------------------------------------Forest plots-----------------------------------------------------------------#
#------------------Extract the hazard ratios (HRs), confidence intervals (CIs)------------------------------------------#


summary_model_female <- summary(msm_model_female)


#--------------------------------------Extract covariate effects--------------------------------------------------------#
hazard_data <- summary_model_female$hazard$`slum_area_in_NUHDSSViwandani`
hazard_data <- data.frame(
  Transition = rownames(hazard_data),
  HR = hazard_data[, "HR"],
  Lower_CI = hazard_data[, "L"],
  Upper_CI = hazard_data[, "U"],
  Covariate = "slum_area_in_NUHDSSViwandani"
)

rownames(hazard_data) <- NULL


#--------------------------------Combine data for multiple covariates----------------------------------------------------#
all_hazard_data <- do.call(rbind, lapply(names(summary_model_female$hazard), function(covariate) {
  hazard <- summary_model_female$hazard[[covariate]]
  data.frame(
    Transition = rownames(hazard),
    HR = hazard[, "HR"],
    Lower_CI = hazard[, "L"],
    Upper_CI = hazard[, "U"],
    Covariate = covariate
  )
}))

#----------------------------- Remove duplicate rows (if any)------------------------------------------------------------#
all_hazard_data <- unique(all_hazard_data)

#---------------------------------------------Save the dataset-----------------------------------------------------------#

save(all_hazard_data, file="~/Evans/female_all_hazard_data.RData")


write.csv(all_hazard_data, file="~/Evans/female_all_hazard_data.csv", row.names = FALSE)


#---------------------------------------------------------End-----------------------------------------------------------#