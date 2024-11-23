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


#------------------------------------load both male and female datasets ---------------------------------------------------#


load("D:\\APHRC\\APHRC-projects\\MSM\\MSM-Residence-2024\\Data\\male_all_hazard_data.RData")

load("D:\\APHRC\\APHRC-projects\\MSM\\MSM-Residence-2024\\Data\\male_all_hazard_data.RData")

#---------------------------------------remove the row names from the data-------------------------------------------------#
rownames(male_all_hazard_data) <- NULL

rownames(all_hazard_data) <- NULL

female_all_hazard_data <- all_hazard_data

#-------------------------------------------------------------------------------------------------------------------------#
#----------------------------------------Rename some of the labels--------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------#

#----------------------------------------Transitions----------------------------------------------------------------------#
male_all_hazard_data <- male_all_hazard_data %>%
  mutate(Transition = case_when(
    Transition == "State 1 - State 3" ~ "Enumeration - Exit",
    Transition == "State 1 - State 5" ~ "Enumeration - OutMigration",
    Transition == "State 1 - State 7" ~ "Enumeration - Death",
    Transition == "State 2 - State 3" ~ "Birth - Exit",
    Transition == "State 2 - State 5" ~ "Birth - OutMigration",
    Transition == "State 2 - State 7" ~ "Birth - Death",
    Transition == "State 3 - State 4" ~ "Exit - Entry",
    Transition == "State 4 - State 3" ~ "Entry - Exit",
    Transition == "State 4 - State 5" ~ "Entry - OutMigration",
    Transition == "State 4 - State 7" ~ "Entry - Death",
    Transition == "State 5 - State 6" ~ "OutMigration - InMigration",
    Transition == "State 6 - State 3" ~ "InMigration - Exit",
    Transition == "State 6 - State 5" ~ "InMigration - OutMigration",
    Transition == "State 6 - State 7" ~ "InMigration - Death",
    TRUE ~ Transition  # Retain other entries as is
  ))


female_all_hazard_data <- female_all_hazard_data %>%
  mutate(Transition = case_when(
    Transition == "State 1 - State 3" ~ "Enumeration - Exit",
    Transition == "State 1 - State 5" ~ "Enumeration - OutMigration",
    Transition == "State 1 - State 7" ~ "Enumeration - Death",
    Transition == "State 2 - State 3" ~ "Birth - Exit",
    Transition == "State 2 - State 5" ~ "Birth - OutMigration",
    Transition == "State 2 - State 7" ~ "Birth - Death",
    Transition == "State 3 - State 4" ~ "Exit - Entry",
    Transition == "State 4 - State 3" ~ "Entry - Exit",
    Transition == "State 4 - State 5" ~ "Entry - OutMigration",
    Transition == "State 4 - State 7" ~ "Entry - Death",
    Transition == "State 5 - State 6" ~ "OutMigration - InMigration",
    Transition == "State 6 - State 3" ~ "InMigration - Exit",
    Transition == "State 6 - State 5" ~ "InMigration - OutMigration",
    Transition == "State 6 - State 7" ~ "InMigration - Death",
    TRUE ~ Transition  # Retain other entries as is
  ))

#----------------------------------------covariates------------------------------------------------------------------------#

male_all_hazard_data <- male_all_hazard_data %>%
  mutate(Covariate = case_when(
    Covariate == "slum_area_in_NUHDSSViwandani" ~ "Viwandani",
    Covariate == "ethnicity_of_NUHDSS_individualKikuyu" ~ "Kikuyu",
    Covariate == "ethnicity_of_NUHDSS_individualLuhya" ~ "Luhya",
    Covariate == "ethnicity_of_NUHDSS_individualLuo" ~ "Luo",
    Covariate == "ethnicity_of_NUHDSS_individualOther" ~ "Other",
    Covariate == "age_in_completed_years" ~ "Age",
    Covariate == "type_of_area_in_Kenya_in_which_individual_was_bornOther places" ~ "Other places",
    Covariate == "type_of_area_in_Kenya_in_which_individual_was_bornRural Kenya" ~ "Rural Kenya",
    Covariate == "type_of_area_in_Kenya_in_which_individual_was_bornWithin same DSA slum" ~ "Within same DSA slum",
    TRUE ~ Covariate  # Retain other entries as is
  ))


female_all_hazard_data <- female_all_hazard_data %>%
  mutate(Covariate = case_when(
    Covariate == "slum_area_in_NUHDSSViwandani" ~ "Viwandani",
    Covariate == "ethnicity_of_NUHDSS_individualKikuyu" ~ "Kikuyu",
    Covariate == "ethnicity_of_NUHDSS_individualLuhya" ~ "Luhya",
    Covariate == "ethnicity_of_NUHDSS_individualLuo" ~ "Luo",
    Covariate == "ethnicity_of_NUHDSS_individualOther" ~ "Other",
    Covariate == "age_in_completed_years" ~ "Age",
    Covariate == "type_of_area_in_Kenya_in_which_individual_was_bornOther places" ~ "Other places",
    Covariate == "type_of_area_in_Kenya_in_which_individual_was_bornRural Kenya" ~ "Rural Kenya",
    Covariate == "type_of_area_in_Kenya_in_which_individual_was_bornWithin same DSA slum" ~ "Within same DSA slum",
    TRUE ~ Covariate  # Retain other entries as is
  ))


#-----------------------------------create gender and grouping variable for the covariates-------------------------------#

male_all_hazard_data$Gender <- "Male"


male_all_hazard_data <- male_all_hazard_data[, c("Gender", setdiff(names(male_all_hazard_data), "Gender"))]


female_all_hazard_data$Gender <- "Female"


female_all_hazard_data <- female_all_hazard_data[, c("Gender", setdiff(names(female_all_hazard_data), "Gender"))]


Group <- c(rep("Slum", 14), rep("Ethnicity", 56),  rep("Age", 14), rep("Birth place", 42))


male_all_hazard_data$Group <- Group


female_all_hazard_data$Group <- Group

#-----------Rename the variable gender to model----#

#------------------------------------------merge the datasets-------------------------------------------------------------#

final_data <- rbind(male_all_hazard_data, female_all_hazard_data)

final_data <- final_data %>% rename(Model = Gender)

#-------------------------------------------forest plot-------------------------------------------------------------------#

dotCOLS <- c("#a6d8f0", "#f9b282")
barCOLS <- c("#008fd5", "#de6b35")

ggplot(final_data, aes( x = HR, xmax = Upper_CI, xmin = Lower_CI, y = Transition, color = Gender, fill = Gender)) +
  geom_point(size = 3, shape = 18, position = position_dodge(width = 0.5)) +
  geom_linerange(position = position_dodge(width = 0.5), linewidth = 1) +
  geom_vline(xintercept = 1, size = 1) + facet_grid(Covariate ~ ., scales = "free_y", space = "free_y") +
  scale_alpha_identity() + scale_fill_manual(values = barCOLS) + scale_color_manual(values = dotCOLS) +
  scale_y_discrete(name = "Transitions")+ scale_x_continuous(name = "Hazard ratio", limits = c(0, 3)) +
  geom_text(aes(label = Covariate), x = -Inf, y = Inf, hjust = 1, vjust = 1, check_overlap = TRUE, color = "darkgreen") +
  coord_cartesian(clip = "off") +
  theme(
    panel.background = element_blank(),
    panel.spacing = unit(0, "pt"),
    axis.line.x.bottom = element_line(linewidth = 1),
    axis.text.y.left =  element_text(margin = margin(l = 20, unit = "pt")),
    strip.background = element_blank(), 
    strip.text = element_blank()
  )

#---------------------------------Separate the covariates to one at a time-----------------------------------------------#

slum_data <- subset(final_data, Group == "Slum")


dotCOLS <- c("#a6d8f0", "#f9b282")
barCOLS <- c("#008fd5", "#de6b35")

ggplot(slum_data, aes( x = HR, xmax = Upper_CI, xmin = Lower_CI, y = Transition, color = Gender, fill = Gender)) +
  geom_point(size = 3, shape = 18, position = position_dodge(width = 0.5)) +
  geom_linerange(position = position_dodge(width = 0.5), linewidth = 1) +
  geom_vline(xintercept = 1, size = 1) + facet_grid(Covariate ~ ., scales = "free_y", space = "free_y") +
  scale_alpha_identity() + scale_fill_manual(values = barCOLS) + scale_color_manual(values = dotCOLS) +
  scale_y_discrete(name = "Transitions") + 
  scale_x_continuous(name = "Hazard ratio (Reference: Korogocho)", limits = c(0.5, 1.8)) +
  geom_text(aes(label = Covariate), x = -Inf, y = Inf, hjust = 1, vjust = 1, check_overlap = TRUE, color = "darkgreen") +
  coord_cartesian(clip = "off") + theme_bw() +
  theme(
    panel.background = element_blank(),
    panel.spacing = unit(0, "pt"),
    axis.line.x.bottom = element_line(linewidth = 1),
    axis.text.y.left =  element_text(margin = margin(l = 20, unit = "pt")),
    strip.background = element_blank(), 
    strip.text = element_blank()
  )


