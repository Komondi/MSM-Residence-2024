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

load("D:\\APHRC\\APHRC-projects\\MSM\\MSM-Residence-2024\\Data\\female_all_hazard_data.RData")

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


#------------------------------------------merge the datasets-------------------------------------------------------------#

final_data <- rbind(male_all_hazard_data, female_all_hazard_data)

final_data <- final_data %>% rename(Model = Gender)

#-------------------------------------------forest plot-------------------------------------------------------------------#

dotCOLS <- c("#a6d8f0", "#f9b282")
barCOLS <- c("#008fd5", "#de6b35")

ggplot(final_data, aes( x = HR, xmax = Upper_CI, xmin = Lower_CI, y = Transition, color = Model, fill = Model)) +
  geom_point(size = 3, shape = 18, position = position_dodge(width = 0.5)) +
  geom_linerange(position = position_dodge(width = 0.5), linewidth = 1) +
  geom_vline(xintercept = 1, linewidth = 1) + facet_grid(Covariate ~ ., scales = "free_y", space = "free_y") +
  scale_alpha_identity() + scale_fill_manual(values = barCOLS) + scale_color_manual(values = dotCOLS) +
  scale_y_discrete(name = "Transitions")+ scale_x_continuous(name = "Hazard ratio", limits = c(0, 3)) +
  geom_text(aes(label = Covariate), x = -Inf, y = Inf, hjust = 1, vjust = 1, check_overlap = TRUE, color = "red",
            size = 4, fontface = "bold") +
  coord_cartesian(clip = "off") + theme_bw() +
  theme(
    panel.background = element_blank(),
    panel.spacing = unit(0, "pt"),
    axis.line.x.bottom = element_line(linewidth = 1),
    axis.text.y.left = element_text(size = 10, face = "bold", margin = margin(l = 20, unit = "pt")), 
    axis.title.y = element_text(size = 10, face = "bold"),  
    axis.title.x = element_text(size = 10, face = "bold"), 
    axis.text.x = element_text(size = 10, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"), 
    legend.text = element_text(size = 10, face = "bold"),
    strip.background = element_blank(), 
    strip.text = element_blank()
  )
#---------------------------------Separate the covariates to one at a time-----------------------------------------------#

#--------------------------------------------------Slum------------------------------------------------------------------#


slum_data <- subset(final_data, Group == "Slum")


dotCOLS <- c("#a6d8f0", "#f9b282")
barCOLS <- c("#008fd5", "#de6b35")

slum <- ggplot(slum_data, aes( x = HR, xmax = Upper_CI, xmin = Lower_CI, y = Transition, color = Model, fill = Model)) +
  geom_point(size = 3, shape = 18, position = position_dodge(width = 0.5)) +
  geom_linerange(position = position_dodge(width = 0.5), linewidth = 1) +
  geom_vline(xintercept = 1, size = 1) + facet_grid(Covariate ~ ., scales = "free_y", space = "free_y") +
  scale_alpha_identity() + scale_fill_manual(values = barCOLS) + scale_color_manual(values = dotCOLS) +
  scale_y_discrete(name = "Transitions") + 
  scale_x_continuous(name = "Hazard ratio (Reference: Korogocho)", limits = c(0.5, 1.8)) +
  geom_text(aes(label = Covariate), x = -Inf, y = Inf, hjust = 1, vjust = 1, check_overlap = TRUE, color = "red",
            size = 4, fontface = "bold") +
  coord_cartesian(clip = "off") + theme_bw() +
  theme(
    panel.background = element_blank(),
    panel.spacing = unit(0, "pt"),
    axis.line.x.bottom = element_line(linewidth = 1),
    axis.text.y.left = element_text(size = 10, face = "bold", margin = margin(l = 20, unit = "pt")), 
    axis.title.y = element_text(size = 10, face = "bold"),  
    axis.title.x = element_text(size = 10, face = "bold"), 
    axis.text.x = element_text(size = 10, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"), 
    legend.text = element_text(size = 10, face = "bold"),
    strip.background = element_blank(), 
    strip.text = element_blank()
  )

ggsave(
  filename = "D:\\APHRC\\APHRC-projects\\MSM\\MSM-Residence-2024\\Data\\slum.png",
  plot = slum,
  width =  10,
  height = 7,
  dpi = 300
)

ggsave(
  filename = "D:\\APHRC\\APHRC-projects\\MSM\\MSM-Residence-2024\\Data\\slum.pdf",
  plot = slum,
  width =  10,
  height = 7
)
#----------------------------------------------age----------------------------------------------------------------------#

Age_data <- subset(final_data, Group == "Age")


dotCOLS <- c("#a6d8f0", "#f9b282")
barCOLS <- c("#008fd5", "#de6b35")

Age <- ggplot(Age_data, aes( x = HR, xmax = Upper_CI, xmin = Lower_CI, y = Transition, color = Model, fill = Model)) +
  geom_point(size = 3, shape = 18, position = position_dodge(width = 0.5)) +
  geom_linerange(position = position_dodge(width = 0.5), linewidth = 1) +
  geom_vline(xintercept = 1, size = 1) + facet_grid(Covariate ~ ., scales = "free_y", space = "free_y") +
  scale_alpha_identity() + scale_fill_manual(values = barCOLS) + scale_color_manual(values = dotCOLS) +
  scale_y_discrete(name = "Transitions") + 
  scale_x_continuous(name = "Hazard ratio", limits = c(0.85, 1.18)) +
  geom_text(aes(label = Covariate), x = -Inf, y = Inf, hjust = 1, vjust = 1, check_overlap = TRUE, color = "red",
            size = 4, fontface = "bold") +
  coord_cartesian(clip = "off") + theme_bw() +
  theme(
    panel.background = element_blank(),
    panel.spacing = unit(0, "pt"),
    axis.line.x.bottom = element_line(linewidth = 1),
    axis.text.y.left = element_text(size = 10, face = "bold", margin = margin(l = 20, unit = "pt")), 
    axis.title.y = element_text(size = 10, face = "bold"),  
    axis.title.x = element_text(size = 10, face = "bold"), 
    axis.text.x = element_text(size = 10, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"), 
    legend.text = element_text(size = 10, face = "bold"),
    strip.background = element_blank(), 
    strip.text = element_blank()
  )

ggsave(
  filename = "D:\\APHRC\\APHRC-projects\\MSM\\MSM-Residence-2024\\Data\\Age.png",
  plot = Age,
  width =  10,
  height = 7,
  dpi = 300
)


#----------------------------------------------Ethnicity-----------------------------------------------------------------#

Ethnicity_data <- subset(final_data, Group == "Ethnicity")

dotCOLS <- c("#a6d8f0", "#f9b282")
barCOLS <- c("#008fd5", "#de6b35")

Ethnicity <- ggplot(Ethnicity_data, aes( x = HR, xmax = Upper_CI, xmin = Lower_CI, y = Transition, color = Model, fill = Model)) +
  geom_point(size = 3, shape = 18, position = position_dodge(width = 0.5)) +
  geom_linerange(position = position_dodge(width = 0.5), linewidth = 1) +
  geom_vline(xintercept = 1, size = 1) + facet_grid(Covariate ~ ., scales = "free_y", space = "free_y") +
  scale_alpha_identity() + scale_fill_manual(values = barCOLS) + scale_color_manual(values = dotCOLS) +
  scale_y_discrete(name = "Transitions") + 
  scale_x_continuous(name = "Hazard ratio (Reference: Kamba)", limits = c(0, 3.0)) +
  geom_text(aes(label = Covariate), x = -Inf, y = Inf, hjust = 1, vjust = 1, check_overlap = TRUE, color = "red",
            size = 2, fontface = "bold") +
  coord_cartesian(clip = "off") + theme_bw() +
  theme(
    panel.background = element_blank(),
    panel.spacing = unit(0, "pt"),
    axis.line.x.bottom = element_line(linewidth = 1),
    axis.text.y.left = element_text(size = 6, face = "bold", margin = margin(l = 20, unit = "pt")), 
    axis.title.y = element_text(size = 10, face = "bold"),  
    axis.title.x = element_text(size = 10, face = "bold"), 
    axis.text.x = element_text(size = 10, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"), 
    legend.text = element_text(size = 10, face = "bold"),
    strip.background = element_blank(), 
    strip.text = element_blank()
  )

ggsave(
  filename = "D:\\APHRC\\APHRC-projects\\MSM\\MSM-Residence-2024\\Data\\Ethnicity.png",
  plot = Ethnicity,
  width =  10,
  height = 7,
  dpi = 300
)

#----------------------------------------------Place of birth------------------------------------------------------------#

Birth_data <- subset(final_data, Group == "Birth place")

dotCOLS <- c("#a6d8f0", "#f9b282")
barCOLS <- c("#008fd5", "#de6b35")


Birth <- ggplot(Birth_data, aes( x = HR, xmax = Upper_CI, xmin = Lower_CI, y = Transition, color = Model, fill = Model)) +
  geom_point(size = 3, shape = 18, position = position_dodge(width = 0.5)) +
  geom_linerange(position = position_dodge(width = 0.5), linewidth = 1) +
  geom_vline(xintercept = 1, size = 1) +
  facet_grid(Covariate ~ ., scales = "free_y", space = "free_y") +
  scale_alpha_identity() +
  scale_fill_manual(values = barCOLS) +
  scale_color_manual(values = dotCOLS) +
  scale_y_discrete(name = "Transitions") + 
  scale_x_continuous(name = "Hazard ratio (Reference: Nairobi Non slum)", limits = c(0.2, 2.2)) +
  geom_text(aes(label = Covariate), x = -Inf, y = Inf, hjust = 1, vjust = 1, check_overlap = TRUE, 
            color = "red", size = 2, fontface = "bold") +  
  coord_cartesian(clip = "off") +
  theme_bw() +
  theme(
    panel.background = element_blank(),
    panel.spacing = unit(0, "pt"),
    axis.line.x.bottom = element_line(linewidth = 1),
    axis.text.y.left = element_text(size = 6, face = "bold", margin = margin(l = 20, unit = "pt")), 
    axis.title.y = element_text(size = 10, face = "bold"),  
    axis.title.x = element_text(size = 10, face = "bold"), 
    axis.text.x = element_text(size = 10, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"), 
    legend.text = element_text(size = 10, face = "bold"),
    strip.background = element_blank(), 
    strip.text = element_blank()
  )

ggsave(
  filename = "D:\\APHRC\\APHRC-projects\\MSM\\MSM-Residence-2024\\Data\\Birth.png",
  plot = Birth,
  width =  10,
  height = 7,
  dpi = 300
)
#-----------------------------------------------------End----------------------------------------------------------------#




