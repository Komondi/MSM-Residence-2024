#-------------------------------------------Loading the libraries-----------------------------------------------------#

libs <- c("haven", "mice", "dplyr", "tidyverse", "finalfit", "writexl", "lme4", "foreign", "readxl", "broom", 
          "gtsummary", "gt", "sjlabelled", "data.table", "DiagrammeRsvg", "DiagrammeR", "gridExtra", "grid", 
          "tidymodels", "msm", "flexsurv", "minqa", "survival", "mstate", "lattice", "latticeExtra", 
          "RColorBrewer", "diagram", "openxlsx", "janitor", "Hmisc", "magrittr", "devtools")


for(ilib in libs){
  if(!(ilib %in% installed.packages())){
    install.packages(ilib)
  }
  library(ilib, character.only = T)
}

#--------------------------------------------Residency Data, 2002 to 2018----------------------------------------------#

NUHDSS_Resi_Data  <- readRDS("D:\\APHRC\\APHRC-projects\\MSM\\MSM-Residence-2024\\Data\\NUHDSS_Final_Event_Data.rds")


#---------------------------------Convert unique HHIDs to numerical values---------------------------------------------#


NUHDSS_Resi_Data$HHID <- as.numeric(factor(NUHDSS_Resi_Data$NUHDSS_socialgroup,
                          levels = unique(NUHDSS_Resi_Data$NUHDSS_socialgroup)))

#make the ID begin from 300000 to make it different from other

NUHDSS_Resi_Data$HHID <- NUHDSS_Resi_Data$HHID + 300000 - min(NUHDSS_Resi_Data$HHID) + 1

#Make it the first column. 

NUHDSS_Resi_Data <- NUHDSS_Resi_Data[, c("HHID", setdiff(names(NUHDSS_Resi_Data), "HHID"))]


#drop the NUHDSS social ID

#------------------------------------------Drop some columns----------------------------------------------------------#

column_to_drop <-  "NUHDSS_socialgroup"


NUHDSS_Resi_Data <- NUHDSS_Resi_Data[, !(names(NUHDSS_Resi_Data) %in% column_to_drop)]


#----------------------------------------Remove the inconsistencies---------------------------------------------------#

NUHDSS_Resi_Data$residency_event[NUHDSS_Resi_Data$ID==55399 & NUHDSS_Resi_Data$residency_event_date=="2003-11-05"]<-"Entry"
NUHDSS_Resi_Data$residency_event[NUHDSS_Resi_Data$ID==77316 & NUHDSS_Resi_Data$residency_event_date=="2006-04-11"]<-"Entry"
NUHDSS_Resi_Data$residency_event[NUHDSS_Resi_Data$ID==34017 & NUHDSS_Resi_Data$residency_event_date=="2015-11-06"]<-"Inmigration"
NUHDSS_Resi_Data$residency_event[NUHDSS_Resi_Data$ID==11294 & NUHDSS_Resi_Data$residency_event_date=="2009-02-27"]<-"Inmigration"
NUHDSS_Resi_Data$residency_event[NUHDSS_Resi_Data$ID==55399 & NUHDSS_Resi_Data$residency_event_date=="2004-10-11"]<-"Inmigration"
NUHDSS_Resi_Data$residency_event[NUHDSS_Resi_Data$ID==59297 & NUHDSS_Resi_Data$residency_event_date=="2012-03-06"]<-"Inmigration"
NUHDSS_Resi_Data$residency_event[NUHDSS_Resi_Data$ID==77316 & NUHDSS_Resi_Data$residency_event_date=="2007-05-11"]<-"Inmigration"


#--------------------------------Remove the duplicated IDs and the event date-----------------------------------------#

NUHDSS_Resi_Data <- NUHDSS_Resi_Data[!duplicated(NUHDSS_Resi_Data[c("ID", "residency_event_date")]),]

#------------------------------Inconsistencies arising to all values NA in ruling govt--------------------------------#

NUHDSS_Resi_Data <- NUHDSS_Resi_Data[!is.na(NUHDSS_Resi_Data$ruling_government),]

#------------------------------------------------subset---------------------------------------------------------------#

NUHDSS_Resi_Data_2002_2015 <- NUHDSS_Resi_Data %>% 
  mutate(Calender_Year = as.numeric(as.character(Calender_Year))) %>% 
  subset(Calender_Year >= 2002 & Calender_Year <= 2015)


NUHDSS_Resi_Data_2002_2015 <- NUHDSS_Resi_Data_2002_2015 %>%   arrange(ID, residency_event_date)

#---------------------------------------------Create an event class---------------------------------------------------#

Events <- c("Enumeration","Birth", "Exit","Entry", "Outmigration","Inmigration", "Death")

NUHDSS_Resi_Data_2002_2015$Event <- factor(NUHDSS_Resi_Data_2002_2015$residency_event, levels = Events)



#-----------------------------------------------add age group---------------------------------------------------------#

breaks <- c(0, 15, 20, 36, 51, Inf)
labels <- c('0-14', '15-19', '20-35', '36-50', '51+')

NUHDSS_Resi_Data_2002_2015$age_cat <- cut(NUHDSS_Resi_Data_2002_2015$age_in_completed_years, 
                                          breaks = breaks, labels = labels, right = FALSE)


#----------------------------------------Convert  Kisii ethnic to others----------------------------------------------#

NUHDSS_Resi_Data_2002_2015 <- NUHDSS_Resi_Data_2002_2015 %>% mutate(ethnicity_of_NUHDSS_individual =  
    case_when(ethnicity_of_NUHDSS_individual == 'Kisii' ~ "Other",
    TRUE ~ as.character(ethnicity_of_NUHDSS_individual)
  ))


#-----------------------------------------Type of income generating activity------------------------------------------#

NUHDSS_Resi_Data_2002_2015 <- NUHDSS_Resi_Data_2002_2015 %>%
  mutate(type_of_income_generating_activity = case_when(
    type_of_income_generating_activity %in% c("Agriculture: Rural", "Agriculture: Urban") ~ "Agriculture",
    type_of_income_generating_activity %in% c("Formal: Casual", "Formal: Salaried") ~ "Formal",
    type_of_income_generating_activity %in% c("Informal: Casual", "Informal: Salaried") ~ "Informal",
    type_of_income_generating_activity %in% c("Own business: Established", "Own business: Unestablished") ~ 
      "Own Business",
    type_of_income_generating_activity %in% c("Other: Creative/Entertainer/Sports", 
      "Other: Illegal activities", "Other: Internship/Attachment/Volunteer", 
      "Other: Religious/Spiritual practice", "Other: Beggar/Donations") ~ "Others",
    TRUE ~ as.character(type_of_income_generating_activity)
  ))


#--------------------------------Type of area in Kenya in which someone was born--------------------------------------#

NUHDSS_Resi_Data_2002_2015 <- NUHDSS_Resi_Data_2002_2015 %>% 
  mutate(type_of_area_in_Kenya_in_which_individual_was_born =  
           case_when(type_of_area_in_Kenya_in_which_individual_was_born %in% 
  c("Non-DSA nairobi slum", "Other DSA slum", "Other urban Kenya", "Outside Kenya") ~ "Other places", 
  TRUE ~ as.character(type_of_area_in_Kenya_in_which_individual_was_born)))


#-----------------------------------------chain of movement for model-------------------------------------------------#

(smatrix<-statetable.msm(Event, ID, data=NUHDSS_Resi_Data_2002_2015)) # for modelling later


#----------------------------------------Descriptive statistics for constant variables--------------------------------#

# Select only constant variables

constant_vars <- c("ID", "gender_of_NUHDSS_individual", "ethnicity_of_NUHDSS_individual", 
                   "type_of_area_in_Kenya_in_which_individual_was_born")

#Subset the data with constant variables

NUHDSS_Residency_subset_dataset <- NUHDSS_Resi_Data_2002_2015 %>%filter(ID %in% unique(NUHDSS_Resi_Data_2002_2015$ID)) %>%
  distinct(ID, .keep_all = TRUE) %>% select(one_of(constant_vars))

#Set the theme to get more decimal points for %

set_gtsummary_theme(list(
  "tbl_summary-fn:percent_fun" = scales::label_percent(accuracy = 0.1)
))


# Obtain the summary statistics 

NUHDSS_Residency_subset_dataset %>%
  select(-c("ID") )%>% tbl_summary(by = gender_of_NUHDSS_individual,
    statistic = list(all_continuous() ~ "{median} ({IQR})", all_categorical() ~ "{n} / {N} ({p})"), #({p}%)
    digits = all_continuous() ~ 2
  ) %>%    modify_caption("NUHDSS Characteristics (N = {N})") %>%  as_gt() #-> tab_1

#-----------------------------------------including the years---------------------------------------------------------#

constant_vars_yr <- c("ID", "slum_area_in_NUHDSS", "gender_of_NUHDSS_individual", "ethnicity_of_NUHDSS_individual", 
                      "type_of_area_in_Kenya_in_which_individual_was_born", "age_in_completed_years", "Event",
                      "Calender_Year")

NUHDSS_Residency_subset_dataset_year <- NUHDSS_Resi_Data_2002_2015 %>%
  distinct(ID, Calender_Year, .keep_all = TRUE) %>%  select(one_of(constant_vars_yr))

tab_NUHDSS_individual <- NUHDSS_Residency_subset_dataset_year %>%  select(-c("ID") ) %>%  
  tbl_summary( by = Calender_Year, 
    statistic = list(all_continuous() ~ "{median} ({IQR})", all_categorical() ~ "{n} / {N} ({p})"),
    digits = all_continuous() ~ 2) %>% add_p() %>% 
  modify_caption("HDSS Characteristics  (N = {N})")

tab_NUHDSS_individual %>% as_gt() #-> tab_2




table_all_constants <-  NUHDSS_Residency_subset_dataset_year %>% select(-c("ID") ) %>% 
  tbl_summary(by =  Calender_Year, missing = "no") %>%  add_p() %>% 
  modify_header(label = "**Variable**") %>% bold_labels(); table_all_constants


table_all_time_var <-  tbl_summary(NUHDSS_Resi_Data_2002_2015, 
  include = c(type_of_income_generating_activity, age_in_completed_years), by  = Calender_Year) %>% 
  add_p() %>%  modify_header(label = "**Variable**") %>% bold_labels(); table_all_time_var




#--------------------------------Graph showing the trend of gender from 2002 to 2018----------------------------------#

# Convert calendar_year to factor with all levels from 2002 to 2018

NUHDSS_Resi_Data_2002_2015$Calender_Year <- factor(NUHDSS_Resi_Data_2002_2015$Calender_Year, levels = 2002:2015)

# Calculate the number of males and females each year
gender_data <- NUHDSS_Resi_Data_2002_2015 %>%   group_by(gender_of_NUHDSS_individual, Calender_Year) %>%
  dplyr::summarise(num_individuals = n_distinct(ID), .groups = "drop")



# Create a trend plot showing the number of males and females each year
ggplot(gender_data, aes(x = Calender_Year, y = num_individuals, color = gender_of_NUHDSS_individual, 
  group = gender_of_NUHDSS_individual)) + geom_line(linewidth = 1) +   geom_point(size = 2) +
  labs(x = "Calendar Year", y ="Number of Individuals", color = "Gender", title = NULL) + theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed")) +
  theme(panel.grid.minor.y = element_blank()) +
  scale_y_continuous(limits = c(0, max(gender_data$num_individuals) + 3000), 
  breaks = seq(0, max(gender_data$num_individuals) + 3000, 3000)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  geom_vline(xintercept = as.numeric(levels(gender_data$Calender_Year)[1]), linetype = "solid", color = "black")+
  theme(axis.text.x = element_text(face = "bold", size = 10), axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"),
        legend.text = element_text(face = "bold"), legend.title = element_text(face = "bold"))


#-------------------------------Graph showing the trend of ethnicity from 2002 to 2018--------------------------------#

Ethnicity_data <- NUHDSS_Resi_Data_2002_2015 %>% group_by(ethnicity_of_NUHDSS_individual, Calender_Year) %>%
  summarise(num_individuals = n_distinct(ID), .groups = "drop")


# Manually define a set of 19 distinct colors

ethnicity_colors <- c("#000000", "#8A2BE2", "#6495ED", "#00008B", "#8B0000")


# Create a trend plot showing the number of individual with different ethnicity per year
ggplot(Ethnicity_data, aes(x = Calender_Year, y = num_individuals, color = ethnicity_of_NUHDSS_individual, 
  group = ethnicity_of_NUHDSS_individual)) +  geom_line(linewidth = 1) +  geom_point(size = 2) +
  labs(x = "Calendar Year", y ="Number of Individuals", color = "Ethnicity", title = NULL) +   theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed")) +
  theme(panel.grid.minor.y = element_blank()) +
  scale_y_continuous(limits = c(0, max(Ethnicity_data$num_individuals) + 2000), 
  breaks = seq(0, max(Ethnicity_data$num_individuals) + 2000, 2000)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  geom_vline(xintercept = as.numeric(levels(Ethnicity_data$Calender_Year)[1]), linetype = "solid", color = "black")+
  scale_color_manual(values = ethnicity_colors)+
  theme(axis.text.x = element_text(face = "bold", size = 10), axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"),
        legend.text = element_text(face = "bold"), legend.title = element_text(face = "bold"))


#---------------------Graph showing the trend of type of area an individual was born from 2002 to 2018----------------#


# Calculate the number of individual born in different places per year

Area_data <- NUHDSS_Resi_Data_2002_2015 %>%
  group_by(type_of_area_in_Kenya_in_which_individual_was_born, Calender_Year) %>%
  dplyr::summarise(num_individuals = n_distinct(ID), .groups = "drop")

#Area_data


# Create a trend plot showing the number of individual born in different places per year

ggplot(Area_data, aes(x = Calender_Year, y = num_individuals, 
  color = type_of_area_in_Kenya_in_which_individual_was_born, 
  group = type_of_area_in_Kenya_in_which_individual_was_born)) + geom_line(linewidth = 1) + geom_point(size = 1) +
  labs(x = "Calendar Year", y ="Number of Individuals", color = "Area Type", title = NULL) +  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed")) +
  theme(panel.grid.minor.y = element_blank()) +
  scale_y_continuous(limits = c(0, max(Area_data$num_individuals) + 5000),
  breaks = seq(0, max(Area_data$num_individuals) + 5000, 5000)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  geom_vline(xintercept = as.numeric(levels(Area_data$Calender_Year)[1]), linetype = "solid", color = "black")+
  theme(axis.text.x = element_text(face = "bold", size = 10), axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"),
        legend.text = element_text(face = "bold"),  legend.title = element_text(face = "bold"))


#-----------------------trend of total number of individuals per year-------------------------------------------------#

# Calculate the total number of individuals per year

total_individuals_data <- NUHDSS_Resi_Data_2002_2015 %>%  group_by(Calender_Year) %>%
  dplyr::summarise(total_individuals = n_distinct(ID), .groups = "drop")


#  Create a trend plot showing the total number of individuals per year
ggplot(total_individuals_data, aes(x = Calender_Year, y = total_individuals)) +   geom_line(linewidth = 1 ) +
  geom_point(size = 1) +  geom_line(aes(group = 1), color = "red") +  # Add line connecting points
  labs(x = "Calendar Year", y = "Total Number of Individuals", title = NULL) +   theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed")) +
  theme(panel.grid.minor.y = element_blank()) +
  scale_y_continuous(limits = c(0, max(total_individuals_data$total_individuals) + 10000),
  breaks = seq(0, max(total_individuals_data$total_individuals) + 10000, 10000)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  geom_vline(xintercept = as.numeric(levels(total_individuals_data$Calender_Year)[1]),
             linetype = "solid", color = "black") +
  theme(axis.text.x = element_text(face = "bold", size = 10), axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"))

#-----------------------------------Population by age group-----------------------------------------------------------#


#percentage of population per gender per calender year
gender_percentage <- NUHDSS_Resi_Data_2002_2015 %>%
  group_by(Calender_Year, age_group, gender_of_NUHDSS_individual) %>%
  summarise(n = n(), .groups = "drop") %>%  group_by(Calender_Year, gender_of_NUHDSS_individual) %>%
  mutate(percentage = n / sum(n) * 100)

age_groupss <- c("<1","1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44","45-49", "50-54", 
                 "55-59",  "60-64", "65-69", "70-74", "75-79", "80+")

gender_percentage$age_group <- factor(gender_percentage$age_group,     levels = age_groupss)

#pyramid

ggplot(data = gender_percentage, 
  mapping = aes(x = ifelse(test = gender_of_NUHDSS_individual == "Male", yes = -percentage, no = percentage), 
  y = age_group, fill = gender_of_NUHDSS_individual)) + geom_col() + theme_bw()  +
  scale_x_continuous(labels = abs, breaks = c(-15, -10, -5, 0, 5, 10, 15)) +
  theme(legend.position = "bottom", legend.justification = "right",legend.title = element_blank(),
  axis.text = element_text(colour = "black", face = "bold"),axis.title.y = element_blank(), 
  axis.title.x = element_text(face = "bold", size = 12),strip.text = element_text(face = "bold")) +
  scale_fill_manual(values = c("blue", "darkred")) + labs(x ="Percentage (%)") +
  facet_wrap(~ Calender_Year, ncol = 5)


#using age category created  

#percentage of population per gender per calender year
gender_percentage_1 <- NUHDSS_Resi_Data_2002_2015 %>%
  group_by(Calender_Year, age_cat, gender_of_NUHDSS_individual) %>%
  summarise(n = n(), .groups = "drop") %>%  group_by(Calender_Year, gender_of_NUHDSS_individual) %>%
  mutate(percentage = n / sum(n) * 100)

age_groups_1 <- c("0-14", "15-19", "20-35", "36-50", "51+")


gender_percentage_1$age_cat <- factor(gender_percentage_1$age_cat,     levels = age_groups_1)

#pyramid

ggplot(data = gender_percentage_1, 
       mapping = aes(x = ifelse(test = gender_of_NUHDSS_individual == "Male", yes = -percentage, no = percentage), 
                     y = age_cat, fill = gender_of_NUHDSS_individual)) + geom_col() + theme_bw()  +
  scale_x_continuous(labels = abs, breaks = c(-40, -30,  -20,  -10,  0,  10,  20,  30, 40)) +
  theme(legend.position = "bottom", legend.justification = "right",legend.title = element_blank(),
        axis.text = element_text(colour = "black", face = "bold"),axis.title.y = element_blank(), 
        axis.title.x = element_text(face = "bold", size = 12),strip.text = element_text(face = "bold")) +
  scale_fill_manual(values = c("blue", "darkred")) + labs(x ="Percentage (%)") +
  facet_wrap(~ Calender_Year, ncol = 5)


#--------------------------------Residency Events of individuals per year---------------------------------------------#

# Filter data for specific residency events

filtered_data <- NUHDSS_Resi_Data_2002_2015 %>%
  filter(residency_event %in% c('Inmigration', 'Outmigration', 'Entry', 'Exit', 'Death', 'Birth'))

# Count unique individuals per residency event and year (this is helpful in showing inconsistencies)

event_counts <- filtered_data %>%  group_by(Calender_Year, residency_event) %>% 
  summarise(Count = n_distinct(ID), .groups = "drop")


# Pivot the data to have years as columns

event_counts_wide <- pivot_wider(event_counts, names_from = Calender_Year, values_from = Count)

# Create a gt table

table_gt <- gt(event_counts_wide) %>%  tab_header(
    title = "Number of Individuals by Residency Event and Year",
    subtitle = "Counts of In Migration, Out Migration, Entry, Exit, Death, and Birth"
  ); table_gt



#----------------------------------------Yearly average----------------------------------------------------------------#

event_counts_1 <- filtered_data %>% group_by(residency_event_date, Calender_Year, residency_event) %>% 
  summarise(Count = n(), .groups = "drop") %>%  ungroup() # replace ID with if uou need to see inconsistencies 

mean_per_year <- event_counts_1 %>% group_by(Calender_Year, residency_event) %>% summarise(
  Sum  =  sum(Count), n  =  n(),
  mean_count = mean(Count), SD = sd(Count),
  ci_lower = mean_count - qt(0.975, length(Count)-1) * sd(Count) / sqrt(length(Count)),
  ci_upper = mean_count + qt(0.975, length(Count)-1) * sd(Count) / sqrt(length(Count)), .groups = "drop"
  ) %>%  mutate_at(vars(mean_count, ci_lower, ci_upper), ~ sprintf("%.2f", .))

#= The data types  and then the gt table

mean_per_year <- mean_per_year %>% mutate_at(vars(Calender_Year), as.factor) %>% 
  mutate_at(vars(mean_count, ci_lower, ci_upper), as.numeric) %>%  gt(); mean_per_year


#=Report median instead of mean and Sd since the data is skewed 


median_per_year <- event_counts_1 %>% group_by(Calender_Year, residency_event) %>% summarise(Total  =  sum(Count), 
  Event_incidence  =  n(), Median  =  median(Count), q1 = quantile(Count, 0.25), q3 = quantile(Count, 0.75), 
  p_value = wilcox.test(Count)$p.value, .groups = "drop") %>%  
  mutate(Median_IQR = sprintf("%.0f[%.0f, %.0f]", Median, q1, q3)) %>% 
  mutate(P_value =  sprintf("%.4f", p_value)) %>% select(-Median, -q1, -q3, -p_value) %>% 
  mutate_at(vars(Calender_Year), as.factor) %>%  gt(); median_per_year


# Count the number of rounds per year

Rounds_per_year <- NUHDSS_Resi_Data_2002_2015 %>% group_by(Calender_Year) %>%  
  summarise(num_rounds = n_distinct(round_in_which_event_occurred))


# Create the bar plot
ggplot(Rounds_per_year, aes(x = factor(Calender_Year), y = num_rounds)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  geom_text(aes(label = num_rounds), vjust = -0.5, size = 4) +  # Add count labels
  labs(x = "Calendar year ", y = "Number of Rounds") +  theme_bw() +  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  +  theme(axis.text.x = element_text(face = "bold", 
  size = 10), axis.text.y = element_text(face = "bold", size = 10),
  axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"))


# Filter the data for round 0 and enumeration event

enumerations_round_0 <- subset(NUHDSS_Resi_Data_2002_2015, 
  round_in_which_event_occurred == 0 & residency_event == "Enumeration") %>% group_by(Calender_Year) %>%
  summarise(num_enumerations_round_0 = n())



#---------------------------------birth and death rates per slum per gender-------------------------------------------#

#Number of individuals per slum per year

total_ids_per_year_slum <- NUHDSS_Resi_Data_2002_2015 %>%  group_by(slum_area_in_NUHDSS, Calender_Year) %>%
  summarise(TotalUniqueIDs = n_distinct(ID), .groups = "drop")

# Filter data for birth and death events

birth_death_data <- NUHDSS_Resi_Data_2002_2015 %>% filter(residency_event %in% c('Birth', 'Death'))


# Calculate the number of births and deaths per year, slum area, gender, and event

birth_death_counts_slum_gender <- birth_death_data %>% 
  group_by(slum_area_in_NUHDSS, Calender_Year, gender_of_NUHDSS_individual, residency_event) %>%
  summarise(Count = n_distinct(ID), .groups = "drop") %>%
  pivot_wider(names_from = residency_event, values_from = Count) %>%
  left_join(total_ids_per_year_slum, by = c("slum_area_in_NUHDSS", "Calender_Year"))


# Calculate birth and death rates per year, slum area, gender, and event
rates_slum_gender <- birth_death_counts_slum_gender %>%
  mutate(BirthRate = (Birth / TotalUniqueIDs) * 1000,  DeathRate = (Death / TotalUniqueIDs) * 1000)

# Plot trend lines connecting birth rates per gender in two slum areas (separate graphs for male and female)

birth_trend_line_plot <- ggplot(rates_slum_gender, aes(x = factor(Calender_Year), y = BirthRate, 
  color = slum_area_in_NUHDSS, group = slum_area_in_NUHDSS)) +
  geom_line(linewidth = 1.2) +  geom_point(size = 1.2) +  labs(title = "",
       x = "", y = "Birth rates per 1000 individuals") +
  theme_bw() +  theme(
    legend.title = element_blank(),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10, face = "bold"),,
    axis.text.y = element_text(size = 10, face = "bold"),
  )+
  scale_color_discrete(name = "Slum Area") +
  facet_wrap(~gender_of_NUHDSS_individual, scales = "free_y", ncol = 1) +
  scale_x_discrete(breaks = unique(rates_slum_gender$Calender_Year)) +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 5)); birth_trend_line_plot





# Plot trend lines connecting death rates per gender in two slum areas (separate graphs for male and female)

death_trend_line_plot <- ggplot(rates_slum_gender, aes(x = factor(Calender_Year), y = DeathRate, 
  color = slum_area_in_NUHDSS, group = slum_area_in_NUHDSS)) +
  geom_line(linewidth = 1.2) +  geom_point(size = 1.2) +   labs(title = "",
       x = "",   y = "Death rate per 1000 individuals") +
  theme_bw() +  theme(
    legend.title = element_blank(),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10, face = "bold"),,
    axis.text.y = element_text(size = 10, face = "bold"),
  )+
  scale_color_discrete(name = "Slum Area") +
  facet_wrap(~gender_of_NUHDSS_individual, scales = "free_y", ncol = 1) +
  scale_x_discrete(breaks = unique(rates_slum_gender$Calender_Year)) +
  scale_y_continuous(limits = c(0, 14), breaks = seq(0, 14, by = 2)); death_trend_line_plot


#------------------------------------Birth and death rates per gender-------------------------------------------------#


total_ids_per_year <- NUHDSS_Resi_Data_2002_2015 %>%  group_by(Calender_Year) %>% 
  summarise(TotalUniqueIDs = n_distinct(ID), .groups = "drop")


# Calculate the number of births and deaths per year and gender
birth_death_counts_gender <- birth_death_data %>%
  group_by(Calender_Year, gender_of_NUHDSS_individual, residency_event) %>%
  summarise(Count = n_distinct(ID), .groups = "drop") %>%   
  pivot_wider(names_from = residency_event, values_from = Count) %>%
  left_join(total_ids_per_year, by = "Calender_Year")

# Calculate birth and death rates per year and gender
rates_gender <- birth_death_counts_gender %>%
  mutate(BirthRate = (Birth / TotalUniqueIDs) * 1000, DeathRate = (Death / TotalUniqueIDs) * 1000)

# Combine birth and death rates into a long format

rates_gender_long <- pivot_longer(rates_gender, cols = c("BirthRate", "DeathRate"), names_to = "RateType")

gt(rates_gender_long) %>% fmt_number(decimals = 1)

# Plot trend lines connecting birth and death rates per gender with facet wrap

rates_plot <- ggplot(rates_gender_long, aes(x = Calender_Year, y = value, color = gender_of_NUHDSS_individual, 
  group = interaction(gender_of_NUHDSS_individual, RateType))) +  geom_line(linewidth = 1) +  
  geom_point(size = 2) +  labs( x = "Calendar Year",  y = "Rate per 1000 individuals") +   theme_bw() +
  scale_color_manual(name = "Gender", values = c("Male" = "blue", "Female" = "pink")) +
  facet_wrap(~RateType, scales = "free_y", ncol = 1); rates_plot


#=slum area


birth_death_counts_slum <- birth_death_data %>%
  group_by(Calender_Year, slum_area_in_NUHDSS, residency_event) %>%
  summarise(Count = n_distinct(ID), .groups = "drop") %>%   
  pivot_wider(names_from = residency_event, values_from = Count) %>%
  left_join(total_ids_per_year, by = "Calender_Year")

rates_slum_area <- birth_death_counts_slum %>%
  mutate(BirthRate = (Birth / TotalUniqueIDs) * 1000, DeathRate = (Death / TotalUniqueIDs) * 1000)


rates_slum_long <- pivot_longer(rates_slum_area, cols = c("BirthRate", "DeathRate"), names_to = "RateType")

gt(rates_slum_long) %>% fmt_number(decimals = 1)


birth_death_counts_overall <- birth_death_data %>%
  group_by(Calender_Year,  residency_event) %>%
  summarise(Count = n_distinct(ID), .groups = "drop") %>%   
  pivot_wider(names_from = residency_event, values_from = Count) %>%
  left_join(total_ids_per_year, by = "Calender_Year")

rates_overall <- birth_death_counts_overall %>%
  mutate(BirthRate = (Birth / TotalUniqueIDs) * 1000, DeathRate = (Death / TotalUniqueIDs) * 1000)


rates_overall_long <- pivot_longer(rates_overall, cols = c("BirthRate", "DeathRate"), names_to = "RateType")

gt(rates_overall_long) %>% fmt_number(decimals = 1)

#---------------------------------------Analysis related to death-----------------------------------------------------#

deaths_data <- NUHDSS_Resi_Data_2002_2015 %>%  filter(residency_event == 'Death')

#-------------------------proportion of deaths per year per age group, #not death rates-------------------------------#

deaths_per_year_per_agegroup <- deaths_data %>% distinct(ID, Calender_Year, .keep_all = TRUE) %>%
  select(c("age_group","ID","Calender_Year"))

tab_NUHDSS_individual <- deaths_per_year_per_agegroup %>%  
  select(-c("ID") ) %>%  tbl_summary(    by = Calender_Year, 
    statistic = list(all_continuous() ~ "{median} ({IQR})", 
                     all_categorical() ~ "{n} / {N} ({p})"),
    digits = all_continuous() ~ 2) %>%  modify_caption("Death Data  (N = {N})")

tab_NUHDSS_individual %>% as_gt() 

# Group by year and age group, then count the number of deaths
deaths_by_year_age <- deaths_data %>%  group_by(Calender_Year, age_group) %>%  
  summarise(Deaths = n_distinct(ID), .groups = "drop" )

# Calculate the total deaths per year
deaths_per_year <- deaths_data %>%  group_by(Calender_Year) %>%   
  summarise(unique_ids_with_deaths = n_distinct(ID), .groups = "drop")

# Calculate proportion of deaths per age group
proportion_deaths_age <- deaths_by_year_age %>%  left_join(deaths_per_year, by = "Calender_Year") %>%
  mutate(Proportion = (Deaths / unique_ids_with_deaths) * 100)

# Manually define a set of 18 distinct colors

age_group_colors <- c("pink", "#8A2BE2", "#6495ED", "#00008B", "#A9A9A9", "#006400", "#BDB76B", "#8B008B", "#8B0000",
                      "#E9967A","#ADFF2F", "#0000FF","#FF0000", "#00FA9A","#AFEEEE", "#48D1CC", "#FAF0E6", "#FFD700")

proportion_deaths_age$age_group <- factor(proportion_deaths_age$age_group, levels = age_groupss)

ggplot(proportion_deaths_age, aes(x = Calender_Year, y = Proportion, fill = age_group)) + 
  geom_bar(stat = "identity", position = "stack", width = .7, alpha = .9) + 
  geom_text(aes(label = paste0(round(Proportion, 0), "%")), position = "stack", hjust = 1, size = 3) + 
  scale_fill_manual(values = setNames(age_group_colors, levels(proportion_deaths_age$age_group))) +
  labs(x = "Calendar Year", y = "Proportion of Deaths (%)", fill = "Age") +
  #scale_fill_brewer(palette = "Pastel1", na.value = "grey70") + #coord_flip() + 
  theme_bw(base_size = 12) + #theme_minimal(base_size = 12)
  theme(axis.title = element_blank(), panel.grid.major.y = element_blank(), legend.position = "right")



#What causes deaths among infacts (<1 years) as they are main people with death events (Infectious diseases)

# Filter death data for infants (<1 year old) and within the specified years, 
# and with specific causes

deaths_infants <- deaths_data %>% mutate(Calender_Year = as.numeric(as.character(Calender_Year))) %>%
  filter(age_group == "<1" &  Calender_Year >= 2002 & Calender_Year <= 2015 ) 
           
          
# Select relevant columns
deaths <- deaths_infants %>%  select(Calender_Year, broad_immediate_cause_of_death_final)

# Create summary table by year

tab_deaths_age <- deaths %>%  tbl_summary(by = Calender_Year,
    statistic = list(all_continuous() ~ "{median} ({IQR})", all_categorical() ~ "{n} / {N} ({p})"),
    digits = all_continuous() ~ 2) 

tab_deaths_age %>% as_gt()


#General main causes of infact mortality (neonatal causes,  respiratory tract infections,infectious diseases)

# Filter death data for infants (<1 year old) and within the specified years

deaths_infants <- deaths_data %>% mutate(Calender_Year = as.numeric(as.character(Calender_Year))) %>%
  filter(age_group == "<1" &  Calender_Year >= 2002 & Calender_Year <= 2015 )
           

# Select relevant columns

deaths <- deaths_infants %>% distinct(ID, Calender_Year, .keep_all = TRUE) %>%
  select(broad_immediate_cause_of_death_final)

# Create summary table

tab_deaths_age <- deaths %>%  tbl_summary(
    statistic = list(all_continuous() ~ "{median} ({IQR})", all_categorical() ~ "{n} / {N} ({p})"),
    digits = all_continuous() ~ 2)

# Print the summary table as a gt table

tab_deaths_age %>% as_gt()

#--------------------------------------Causes of death in general-----------------------------------------------------#


deaths_causes <- deaths_data %>% distinct(ID, Calender_Year, .keep_all = TRUE) %>%
  select(c("broad_underlying_cause_of_death_final","ID","Calender_Year"))

tab_NUHDSS_causes <- deaths_causes %>%  select(-c("ID", "Calender_Year") ) %>%  tbl_summary( #by = Calender_Year, 
  statistic = list(all_continuous() ~ "{median} ({IQR})", all_categorical() ~ "{n} / {N} ({p})"),
  digits = all_continuous() ~ 2) %>%  modify_caption("Death Data  (N = {N})")

tab_NUHDSS_causes %>% as_gt() #-> tab_3

#------------------------------------------------entry and exit-------------------------------------------------------#

total_ids_per_year_ent_exit <- NUHDSS_Resi_Data_2002_2015 %>%  group_by(Calender_Year, gender_of_NUHDSS_individual) %>% 
  summarise(TotalUniqueIDs = n_distinct(ID), .groups = "drop")


exit_entry_data <- NUHDSS_Resi_Data_2002_2015 %>%   filter(residency_event %in% c("Exit", "Entry"))


# Calculate the number of exit and entry per year and gender

Exit_Entry_counts_gender <- exit_entry_data %>% group_by(Calender_Year, gender_of_NUHDSS_individual, residency_event) %>%
  summarise(Count = n_distinct(ID), .groups = "drop") %>% pivot_wider(names_from = residency_event, values_from = Count) %>%
  left_join( total_ids_per_year_ent_exit, by = c("Calender_Year", "gender_of_NUHDSS_individual"))


# Calculate exit and entry rates per year and gender

rates_gender <- Exit_Entry_counts_gender %>%
  mutate(ExitRate = (Exit / TotalUniqueIDs) * 1000, EntryRate = (Entry / TotalUniqueIDs) * 1000)


# Separate data for male and female

male_data <- rates_gender %>%  filter(gender_of_NUHDSS_individual == "Male") %>%
  select(Calender_Year, ExitRate, EntryRate)

female_data <- rates_gender %>%   filter(gender_of_NUHDSS_individual == "Female") %>%
  select(Calender_Year, ExitRate, EntryRate)

# Rename columns for clarity

colnames(male_data) <- c("Year", "Male Exit Rate", "Male Entry Rate")
colnames(female_data) <- c("Year", "Female Exit Rate", "Female Entry Rate")

# Combine male and female rates by year

combined_rates_gender <- merge(male_data, female_data, by = "Year", all = TRUE)


# Create gt table
gt_combined <- gt(combined_rates_gender) %>% tab_spanner(label = "Male", columns = c(starts_with("Male"))) %>%
  tab_spanner(label = "Female", columns = c(starts_with("Female"))) %>% tab_header(title = "Gender") %>% 
  fmt_number(columns = c(starts_with("Male")), decimals = 1) %>%
  fmt_number(columns = c(starts_with("Female")), decimals = 1);  gt_combined


#ge groups 


total_ids_per_year_age_cat <- NUHDSS_Resi_Data_2002_2015 %>%  group_by(Calender_Year, age_cat) %>% 
  summarise(TotalUniqueIDs = n_distinct(ID), .groups = "drop")


exit_entry_data_age_cat <- NUHDSS_Resi_Data_2002_2015 %>%   filter(residency_event %in% c("Exit", "Entry"))


# Calculate the number of exit and entry per year and gender

Exit_Entry_counts_age_cat <- exit_entry_data_age_cat %>% group_by(Calender_Year, age_cat, residency_event) %>%
  summarise(Count = n_distinct(ID), .groups = "drop") %>% pivot_wider(names_from = residency_event, values_from = Count) %>%
  left_join( total_ids_per_year_age_cat, by = c("Calender_Year", "age_cat"))


# Calculate exit and entry rates per year and gender

rates_age_cat <- Exit_Entry_counts_age_cat %>%
  mutate(ExitRate = (Exit / TotalUniqueIDs) * 1000, EntryRate = (Entry / TotalUniqueIDs) * 1000)


# Separate data for  each age group

under_14_data <- rates_age_cat %>%  filter(age_cat == "0-14") %>% select(Calender_Year, ExitRate, EntryRate)

data_15_19 <- rates_age_cat %>%  filter(age_cat == "15-19") %>% select(Calender_Year, ExitRate, EntryRate)

data_20_35 <- rates_age_cat %>%  filter(age_cat == "20-35") %>% select(Calender_Year, ExitRate, EntryRate)

data_36_50 <- rates_age_cat %>%  filter(age_cat == "36-50") %>% select(Calender_Year, ExitRate, EntryRate)

data_50_over <- rates_age_cat %>%  filter(age_cat == "51+") %>% select(Calender_Year, ExitRate, EntryRate)


colnames(under_14_data) <- c("Year", "Under 14 Exit Rate", "Under 14 Entry Rate")

colnames(data_15_19) <- c("Year", "A 15-19 Exit Rate", "A 15-19 Entry Rate")

colnames(data_20_35) <- c("Year", "A 20-35 Exit Rate", "A 20-35 Entry Rate")

colnames(data_36_50) <- c("Year", "A 36-50 Exit Rate", "A 36-50 Entry Rate")

colnames(data_50_over) <- c("Year", "Over 50 Exit Rate", "Over 50 Entry Rate")

# List of datasets to merge
data_list <- list(under_14_data, data_15_19, data_20_35, data_36_50, data_50_over)

# Specify the common column for merging
by_column <- "Year"

# Merge datasets using Reduce function
combined_rates_age_cat <- Reduce(function(x, y) merge(x, y, by = by_column, all = TRUE), data_list)

# Create gt table
gt_combined <- gt(combined_rates_age_cat) %>% tab_spanner(label = "0-14", columns = c(starts_with("0-14"))) %>%
  tab_spanner(label = "15-19", columns = c(starts_with("15-19"))) %>% 
  tab_spanner(label = "20-35", columns = c(starts_with("20-35"))) %>% 
  tab_spanner(label = "36-50", columns = c(starts_with("36-50"))) %>% 
  tab_spanner(label = "51+", columns = c(starts_with("51+"))) %>% tab_header(title = "Age") %>% 
  fmt_number(columns = c(starts_with("0-14")), decimals = 2) %>%
  fmt_number(columns = c(starts_with("15-19")), decimals = 2)%>%
  fmt_number(columns = c(starts_with("20-35")), decimals = 2)%>%
  fmt_number(columns = c(starts_with("36-50")), decimals = 2)%>%
  fmt_number(columns = c(starts_with("51+")), decimals = 2);  gt_combined

#------------------------------------Entry and Exit per slum area------------------------------------------------------#

# Calculate total population per slum area and year

total_population_per_slum <- NUHDSS_Resi_Data_2002_2015 %>% group_by(Calender_Year, slum_area_in_NUHDSS) %>%
  summarise(TotalUniqueIDsPerSlum = n_distinct(ID), .groups = "drop")

# Calculate the number of exit and entry per year and slum
Exit_Entry_counts_slum <- exit_entry_data %>% group_by(Calender_Year, slum_area_in_NUHDSS, residency_event) %>%
  summarise(Count = n_distinct(ID), .groups = "drop")%>% pivot_wider(names_from = residency_event, values_from = Count) %>%
  left_join(total_population_per_slum, by = c("Calender_Year", "slum_area_in_NUHDSS")) 


# Calculate exit and entry rates per year and gender
rates_slum <- Exit_Entry_counts_slum %>%
  mutate(ExitRate = (Exit / TotalUniqueIDsPerSlum) * 1000, EntryRate = (Entry / TotalUniqueIDsPerSlum) * 1000)

# Separate data for V and k
Korogocho_data <- rates_slum %>%  filter(slum_area_in_NUHDSS == "Korogocho") %>%
  select(Calender_Year, ExitRate, EntryRate)

Viwandani_data <- rates_slum %>%  filter(slum_area_in_NUHDSS == "Viwandani") %>%
  select(Calender_Year, ExitRate, EntryRate)

# Rename columns for clarity
colnames(Korogocho_data) <- c("Year", "Korogocho Exit Rate", "Korogocho Entry Rate")
colnames(Viwandani_data) <- c("Year", "Viwandani Exit Rate", "Viwandani Entry Rate")

combined_rates <- merge(Korogocho_data, Viwandani_data, by = "Year", all = TRUE)


gt_combined <- gt(combined_rates) %>%
  tab_spanner(label = "Korogocho", columns = c(starts_with("Korogocho"))) %>%
  tab_spanner(label = "Viwandani", columns = c(starts_with("Viwandani"))) %>%
  tab_header(title = "Slum Area") %>%
  fmt_number(columns = c(starts_with("Korogocho")), decimals = 1) %>%
  fmt_number(columns = c(starts_with("Viwandani")), decimals = 1);  gt_combined

#overal

total_population_per_ov <- NUHDSS_Resi_Data_2002_2015 %>% group_by(Calender_Year) %>%
  summarise(TotalUniqueIDsOv = n_distinct(ID), .groups = "drop")

Exit_Entry_counts_ov <- exit_entry_data %>% group_by(Calender_Year,  residency_event) %>%
  summarise(Count = n_distinct(ID), .groups = "drop")%>% pivot_wider(names_from = residency_event, 
                                                                     values_from = Count) %>%left_join(total_population_per_ov, by = c("Calender_Year"))


# Calculate exit and entry rates per year 
rates_ov <- Exit_Entry_counts_ov %>%
  mutate(ExitRate = (Exit / TotalUniqueIDsOv) * 1000, EntryRate = (Entry / TotalUniqueIDsOv) * 1000)

gt(rates_ov) %>% fmt_number( decimals = 1)


#-------------------------------------------inmigrants analysis-------------------------------------------------------#


# Filter data for inmigration events

inmigration_data <- NUHDSS_Resi_Data_2002_2015 %>%  filter(residency_event == 'Inmigration')


demographic_characteristics <- c("ID", "gender_of_NUHDSS_individual", "current_marital_status", 
                                 "highest_level_of_school_ever_attended",  "Calender_Year") #"age_group",

Demographic_inmigration_data <- inmigration_data %>%  distinct(ID, Calender_Year, .keep_all = TRUE) %>%
  select(one_of(demographic_characteristics))

tab_NUHDSS_individual <- Demographic_inmigration_data %>% select(-c("ID") ) %>%
  tbl_summary(by = Calender_Year, 
    statistic = list(all_continuous() ~ "{median} ({IQR})", all_categorical() ~ "{n} / {N} ({p})"),
    digits = all_continuous() ~ 2) %>%
  modify_caption("Demographic Characteristics of Inmigrants  (N = {N})")

tab_NUHDSS_individual %>% as_gt() 


#Reasons for inmigrating to current slum area (to be with the family,expensive rent)

# Filter data for inmigration events and for the years 2002 to 2015

inmigration_data_02_015 <- NUHDSS_Resi_Data_2002_2015 %>% 
  mutate(Calender_Year = as.numeric(as.character(Calender_Year))) %>% 
  filter(residency_event == 'Inmigration', Calender_Year >= 2002, Calender_Year <= 2015 )
       
Inmigration_reasons <- inmigration_data_02_015 %>%  distinct(ID, Calender_Year, .keep_all = TRUE) %>%
  select(c("reason_for_inmigrating_to_Korogocho_or_Viwandani","ID","Calender_Year"))

tab_NUHDSS_individual <- Inmigration_reasons %>%  select(-c("ID") ) %>%
  tbl_summary(by = Calender_Year, statistic = list(all_continuous() ~ "{median} ({IQR})", 
                     all_categorical() ~ "{n} / {N} ({p})"), digits = all_continuous() ~ 2) %>%
  modify_caption("Inmigration Data (N = {N})")

tab_NUHDSS_individual %>% as_gt() #-> tab_2


#----------------------------------------Profiling Oumigrants----------------------------------------------------------#

#Demographic characteristics of outmigrants
# Filter data for outmogration events

outmigration_data <- NUHDSS_Resi_Data_2002_2015 %>%  filter(residency_event == 'Outmigration')


demographic_characteristics <- c("ID","slum_area_in_NUHDSS", "gender_of_NUHDSS_individual", 
                                 "current_marital_status", "highest_level_of_school_ever_attended",
                                 "age_group", "Calender_Year")

Demographic_outmigration_data <- outmigration_data %>%  distinct(ID, Calender_Year, .keep_all = TRUE) %>%
  select(one_of(demographic_characteristics))

tab_NUHDSS_individual <- Demographic_outmigration_data %>%  select(-c("ID") ) %>%
  tbl_summary(by = Calender_Year,  statistic = list(all_continuous() ~ "{median} ({IQR})", 
                     all_categorical() ~ "{n} / {N} ({p})"),
    digits = all_continuous() ~ 2) %>%
  modify_caption("Demographic Characteristics of Outmigrants  (N = {N})")

tab_NUHDSS_individual %>% as_gt() 

#Reasons for Outmigration 

outmigration_data_02_015 <- NUHDSS_Resi_Data_2002_2015 %>% 
  mutate(Calender_Year = as.numeric(as.character(Calender_Year))) %>%  filter(residency_event == 'Outmigration', 
                            Calender_Year >= 2002,    Calender_Year <= 2015 )
# Select relevant columns
Outmigration_reasons <- outmigration_data_02_015 %>%  select(reason_for_outmigrating, Calender_Year)

# Create summary table
tab_NUHDSS_individual <- Outmigration_reasons %>%  tbl_summary(
    by = Calender_Year,   statistic = list(all_continuous() ~ "{median} ({IQR})", 
                     all_categorical() ~ "{n} / {N} ({p})"),
    digits = all_continuous() ~ 2) %>%   modify_caption("Outmigration Data (2002-2015) (N = {N})")

tab_NUHDSS_individual %>% as_gt()


# Destination analysis of outmigrants 

Outmigration_reasons <- outmigration_data %>%   distinct(ID, Calender_Year, .keep_all = TRUE) %>%
  select(c("reason_for_outmigrating","ID","type_of_area_in_Kenya_to_which_individual_outmigrated_to"))

tab_NUHDSS_individual <- Outmigration_reasons %>% select(-c("ID") ) %>%
  tbl_summary(by = type_of_area_in_Kenya_to_which_individual_outmigrated_to, 
    statistic = list(all_continuous() ~ "{median} ({IQR})", all_categorical() ~ "{n} / {N} ({p})"),
    digits = all_continuous() ~ 2) %>%   modify_caption("Outmigration Data (N = {N})")

tab_NUHDSS_individual %>% as_gt() 

# Socioeconomic status of outmigrants 

# Group data by socioeconomic status indicators and count the number of unique outmigrant IDs

socioeconomic_status <- outmigration_data %>% 
  group_by(individual_has_had_an_income_generating_activity_in_past_30_days, type_of_income_generating_activity
  ) %>%  summarise(OutmigrantsCount = n_distinct(ID),  .groups = "drop") %>% as.data.frame()%>%
  gt()

#-------------------------------------------Population growth rate----------------------------------------------------#

Enumeration_start <- subset(NUHDSS_Resi_Data_2002_2015, 
round_in_which_event_occurred == 0 & residency_event == "Enumeration") %>% group_by(Calender_Year) %>%
  summarise(Enumeration_Start = n())



population_growth <- NUHDSS_Resi_Data_2002_2015 %>% group_by(Calender_Year) %>% summarise(
  Birth  = sum(residency_event == "Birth"),  Death  = sum (residency_event == "Death"),
  In_Migrations = sum(residency_event == "Inmigration"), Out_Migrations = sum(residency_event == "Outmigration")
)


population_growth$Pop_change <-  (population_growth$Birth - population_growth$Death) + 
  (population_growth$In_Migrations - population_growth$Out_Migrations) 

Enumeration_start <- Enumeration_start$Enumeration_Start


Enumeration_start <- tibble(Enumeration_start = Enumeration_start)

Enumeration_start <- Enumeration_start %>% slice(rep(1, nrow(population_growth)))

population_growth <-  bind_cols(population_growth, Enumeration_start)
  
population_growth <- population_growth %>% mutate(Growth_rate = (Pop_change/Enumeration_start)*1000)


# Plot population growth rate

ggplot(population_growth, aes(x = factor(Calender_Year), y = Growth_rate)) +
  geom_line(aes(group = 1)) +   geom_point() +  labs(x = "Calendar Year",
       y = "Population Growth Rate per 1000") +   theme_bw()


#--------------------------Profiling individuals by residency events using a rectangular plot--------------------------#

Enumeration_data <- NUHDSS_Resi_Data_2002_2015 %>%  filter(residency_event == 'Enumeration')



 Start_Date = min(Enumeration_data$residency_event_date)
 End_Date = max(Enumeration_data$residency_event_date)
 
 rec_1 <- ggplot(Enumeration_data, aes(x=residency_event_date, y=ID, group=ID))+
 geom_rect(aes(xmin=Start_Date, xmax=End_Date, ymin=0, ymax=Inf ), 
           fill="grey80", color=NA) +geom_line(linewidth=1.0, alpha=0.05) +
   geom_point(data=Enumeration_data[Enumeration_data$individual_has_ever_outmigrated%in%"Yes",], 
              aes(x=residency_event_date, y=ID, color="Outmigrated"),  size=0.5)+
   scale_x_date(date_breaks = "12 months", date_labels = "%b-%Y"); rec_1
 
 
 
 #rec_1 + geom_rect(aes(xmin=Start_Date, xmax=End_Date, ymin=0, ymax= 150000), fill = "blue", alpha = 0.3)
 
 #print(rec_1)

 Birth_data <- NUHDSS_Resi_Data_2002_2015 %>%  filter(residency_event == 'Birth')
 
 
 ggplot(data=Birth_data) + geom_point(aes(x = residency_event_date, y = ID))

 
 
 Inmigration_data <- NUHDSS_Resi_Data_2002_2015 %>%  filter(residency_event == 'Inmigration')
 
 
 ggplot(data=Inmigration_data) +
   geom_point(aes(x = residency_event_date, y = ID), size = 0.5, color = "blue")+
   geom_point(data=Inmigration_data[Inmigration_data$individual_has_ever_outmigrated%in%"Yes",], 
              aes(x=residency_event_date, y=ID, color="Outmigrated"),  size=0.5)+
   scale_x_date(date_breaks = "8 months", date_labels = "%b-%Y")
 
 
  
#------------------------------------------In migration and Outmigration-----------------------------------------------#
  
total_ids_per_year_in_out <- NUHDSS_Resi_Data_2002_2015 %>%  group_by(Calender_Year,gender_of_NUHDSS_individual ) %>% 
    summarise(TotalUniqueIDs = n_distinct(ID), .groups = "drop")
  
inmigration_outmigration_data <- NUHDSS_Resi_Data_2002_2015 %>% filter(residency_event %in% c("Inmigration", "Outmigration"))
  

Inmigration_Outmigration_counts_gender <- inmigration_outmigration_data %>%
  group_by(Calender_Year, gender_of_NUHDSS_individual, residency_event) %>%
  summarise(Count = n_distinct(ID), .groups = "drop") %>% pivot_wider(names_from = residency_event, values_from = Count) %>%
  left_join(total_ids_per_year_in_out, by = c("Calender_Year", "gender_of_NUHDSS_individual"))

Inmigration_Outmigration_counts_gender$Net_migration <- 
  Inmigration_Outmigration_counts_gender$Inmigration - Inmigration_Outmigration_counts_gender$Outmigration

 
rates_gender <- Inmigration_Outmigration_counts_gender %>%
  mutate(InmigrationRate = (Inmigration / TotalUniqueIDs) * 1000, OutmigrationRate = (Outmigration / TotalUniqueIDs) * 1000,
         Net_migrationRate = (Net_migration / TotalUniqueIDs) * 1000)


# Separate data for male and female
male_data <- rates_gender %>%  filter(gender_of_NUHDSS_individual == "Male") %>%
  select(Calender_Year, InmigrationRate, OutmigrationRate, Net_migrationRate)


female_data <- rates_gender %>%   filter(gender_of_NUHDSS_individual == "Female") %>%
  select(Calender_Year, InmigrationRate, OutmigrationRate, Net_migrationRate)

colnames(male_data) <- c("Year", "Male_Inmigration_Rate", "Male_Outmigration_Rate", "Male_Net_migration_Rate")
colnames(female_data) <- c("Year", "Female_Inmigration_Rate", "Female_Outmigration_Rate", "Female_Net_migration_Rate")

# Combine male and female rates by year
combined_rates_gender <- merge(male_data, female_data, by = "Year", all = TRUE)

# Create gt table
gt_combined <- gt(combined_rates_gender) %>%
  tab_spanner(label = "Male", columns = c(starts_with("Male"))) %>%
  tab_spanner(label = "Female", columns = c(starts_with("Female"))) %>%
  tab_header(title = "Gender") %>% fmt_number(columns = c(starts_with("Male")), decimals = 2) %>%
  fmt_number(columns = c(starts_with("Female")), decimals = 2); gt_combined


# Calculate total population per slum area and year

total_population_per_slum <- NUHDSS_Resi_Data_2002_2015 %>% group_by(Calender_Year, slum_area_in_NUHDSS) %>%
  summarise(TotalUniqueIDsPerSlum = n_distinct(ID), .groups = "drop")

Inmigration_Outmigration_data <- NUHDSS_Resi_Data_2002_2015 %>% filter(residency_event %in% c("Inmigration", "Outmigration"))


# Calculate t

Inmigration_Outmigration_counts_slum <- Inmigration_Outmigration_data %>%
  group_by(Calender_Year, slum_area_in_NUHDSS, residency_event) %>%  summarise(Count = n_distinct(ID), .groups = "drop")%>%   
  pivot_wider(names_from = residency_event, values_from = Count) %>%
  left_join(total_population_per_slum, by = c("Calender_Year", "slum_area_in_NUHDSS")) 

Inmigration_Outmigration_counts_slum$Net_migration <- 
  Inmigration_Outmigration_counts_slum$Inmigration - Inmigration_Outmigration_counts_slum$Outmigration


rates_slum <- Inmigration_Outmigration_counts_slum %>%
  mutate(InmigrationRate = (Inmigration / TotalUniqueIDsPerSlum) * 1000, 
         OutmigrationRate = (Outmigration / TotalUniqueIDsPerSlum) * 1000, 
         Net_migrationRate = (Net_migration / TotalUniqueIDsPerSlum) * 1000)

Korogocho_data <- rates_slum %>%  filter(slum_area_in_NUHDSS == "Korogocho") %>%
  select(Calender_Year, InmigrationRate, OutmigrationRate, Net_migrationRate)

Viwandani_data <- rates_slum %>%  filter(slum_area_in_NUHDSS == "Viwandani") %>%
  select(Calender_Year, InmigrationRate, OutmigrationRate, Net_migrationRate)

# Rename columns for clarity
colnames(Korogocho_data) <- c("Year", "Korogocho_Inmigration_Rate", "Korogocho_Outmigration_Rate", "Korogocho_Net_migration_Rate")
colnames(Viwandani_data) <- c("Year", "Viwandani_Inmigration_Rate", "Viwandani_Outmigration_Rate", "Viwandani_Net_migration_Rate")

# Combine male and female rates by year
combined_rates <- merge(Korogocho_data, Viwandani_data, by = "Year", all = TRUE)



# Create gt table
gt_combined <- gt(combined_rates) %>%
  tab_spanner(label = "Korogocho", columns = c(starts_with("Korogocho"))) %>%
  tab_spanner(label = "Viwandani", columns = c(starts_with("Viwandani"))) %>%
  tab_header(title = "Slum Area") %>% fmt_number(columns = c(starts_with("Korogocho")), decimals = 2) %>%
  fmt_number(columns = c(starts_with("Viwandani")), decimals = 2);gt_combined


#overall


total_population <- NUHDSS_Resi_Data_2002_2015 %>% group_by(Calender_Year) %>%
  summarise(TotalUniqueIDs = n_distinct(ID), .groups = "drop")

Inmigration_Outmigration_data <- NUHDSS_Resi_Data_2002_2015 %>% 
  filter(residency_event %in% c("Inmigration", "Outmigration"))


# Calculate the number 

Inmigration_Outmigration_counts <- Inmigration_Outmigration_data %>%
  group_by(Calender_Year,  residency_event) %>%  summarise(Count = n_distinct(ID), .groups = "drop")%>%   
  pivot_wider(names_from = residency_event, values_from = Count) %>%
  left_join(total_population, by = c("Calender_Year")) 


Inmigration_Outmigration_counts$Net_migration <- 
  Inmigration_Outmigration_counts$Inmigration - Inmigration_Outmigration_counts$Outmigration


rates_overall <- Inmigration_Outmigration_counts %>%
  mutate(InmigrationRate = (Inmigration / TotalUniqueIDs) * 1000, 
         OutmigrationRate = (Outmigration / TotalUniqueIDs) * 1000, 
         Overall_Net_migration_Rate = (Net_migration / TotalUniqueIDs) * 1000)

rates_overall <- rates_overall %>% rename(Year = Calender_Year)

gt(rates_overall) %>% fmt_number( decimals = 1)

# Select data
df_koch_viwa <- combined_rates %>% select(Year, Korogocho_Net_migration_Rate, Viwandani_Net_migration_Rate)

df_overall <- rates_overall %>% select(Year, Overall_Net_migration_Rate)


Net_rates <- merge(df_koch_viwa, df_overall, by = "Year", all = TRUE)


Net_rates <- Net_rates %>% rename(Korogocho = Korogocho_Net_migration_Rate,
                                  Viwandani = Viwandani_Net_migration_Rate,
                                  Overall = Overall_Net_migration_Rate)

#--------------------------------------------ggplot showing the net migration-----------------------------------------#

long_data_combined_rates <- Net_rates %>%
  pivot_longer(
    cols = -Year, # Select all columns except "Year"
    names_to = "NetRate", # The new column that stores the names of the original columns
    values_to = "Rate" # The new column that stores the values
  )


ggplot(long_data_combined_rates, aes(x = Year, y = Rate, color = NetRate, group = NetRate)) +
  geom_line(linewidth = 1.2) +
  labs(title = "",x = "", y = "Net migration rate per 1000 individuals") +
  scale_y_continuous(limits = c(-180, 100), breaks = seq(-180, 100, by = 20)) +
  theme_bw() + theme(legend.title = element_blank(),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10, face = "bold"),,
    axis.text.y = element_text(size = 10, face = "bold"),
  )


#gender

df_male_female <- combined_rates_gender %>% select(Year, Male_Net_migration_Rate, Female_Net_migration_Rate)

df_gen_Net_rates <- merge(df_male_female, df_overall, by = "Year", all = TRUE)

df_gen_Net_rates <- df_gen_Net_rates %>% rename(Male = Male_Net_migration_Rate,
                      Female = Female_Net_migration_Rate, Overall = Overall_Net_migration_Rate)

long_df_gen_Net_rates <- df_gen_Net_rates %>%
  pivot_longer(cols = -Year, names_to = "NetRate", values_to = "Rate" 
  )


ggplot(long_df_gen_Net_rates, aes(x = Year, y = Rate, color = NetRate, group = NetRate)) +
  geom_line(linewidth = 1.2) +
  labs(title = "",x = "", y = "Net migration rate per 1000 individuals") +
  scale_y_continuous(limits = c(-180, 100), breaks = seq(-180, 100, by = 20)) +
  theme_bw() + theme(legend.title = element_blank(),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10, face = "bold"),,
    axis.text.y = element_text(size = 10, face = "bold"),
  )


#-------------------------------------------Birth and death-----------------------------------------------------------#

total_ids_per_year_birth_death <- NUHDSS_Resi_Data_2002_2015 %>%  group_by(Calender_Year, gender_of_NUHDSS_individual) %>% 
  summarise(TotalUniqueIDs = n_distinct(ID), .groups = "drop")


birth_death_data <- NUHDSS_Resi_Data_2002_2015 %>%   filter(residency_event %in% c("Birth", "Death"))


# Calculate the number of exit and entry per year and gender

birth_death_counts_gender <- birth_death_data %>% group_by(Calender_Year, gender_of_NUHDSS_individual, residency_event) %>%
  summarise(Count = n_distinct(ID), .groups = "drop") %>% pivot_wider(names_from = residency_event, values_from = Count) %>%
  left_join( total_ids_per_year_birth_death, by = c("Calender_Year", "gender_of_NUHDSS_individual"))


# Calculate birth and death rates per year and gender

b_and_d_rates_gender <- birth_death_counts_gender %>%
  mutate(BirthRate = (Birth / TotalUniqueIDs) * 1000, DeathRate = (Death / TotalUniqueIDs) * 1000)


# Separate data for male and female

male_data <- b_and_d_rates_gender %>%  filter(gender_of_NUHDSS_individual == "Male") %>%
  select(Calender_Year, BirthRate, DeathRate)

female_data <- b_and_d_rates_gender %>%   filter(gender_of_NUHDSS_individual == "Female") %>%
  select(Calender_Year, BirthRate, DeathRate)

# Rename columns for clarity

colnames(male_data) <- c("Year", "Male_Birth_Rate", "Male_Death_Rate")
colnames(female_data) <- c("Year", "Female_Birth_Rate", "Female_Death_Rate")

# Combine male and female rates by year

combined_rates_gender <- merge(male_data, female_data, by = "Year", all = TRUE)


# Create gt table
gt_combined <- gt(combined_rates_gender) %>% tab_spanner(label = "Male", columns = c(starts_with("Male"))) %>%
  tab_spanner(label = "Female", columns = c(starts_with("Female"))) %>% tab_header(title = "Gender") %>% 
  fmt_number(columns = c(starts_with("Male")), decimals = 1) %>%
  fmt_number(columns = c(starts_with("Female")), decimals = 1);  gt_combined




#---------------------------------exit and entry rates per slum per gender-------------------------------------------#

#Number of individuals 

total_ids <- NUHDSS_Resi_Data_2002_2015 %>%  group_by( gender_of_NUHDSS_individual,slum_area_in_NUHDSS, Calender_Year) %>%
  summarise(TotalUniqueIDs = n_distinct(ID), .groups = "drop")



df_data <- NUHDSS_Resi_Data_2002_2015 %>% filter(residency_event %in% c('Entry', 'Exit'))


# Calculate the number 

df_counts <- df_data %>% 
  group_by(slum_area_in_NUHDSS, Calender_Year, gender_of_NUHDSS_individual, residency_event) %>%
  summarise(Count = n_distinct(ID), .groups = "drop") %>%
  pivot_wider(names_from = residency_event, values_from = Count) %>%
  left_join(total_ids, by = c("slum_area_in_NUHDSS", "Calender_Year", "gender_of_NUHDSS_individual"))


# Calculate  rates 
df_entry_exit_rates <- df_counts %>%
  mutate(EntryRate = (Entry/TotalUniqueIDs) * 1000,  ExitRate = (Exit/TotalUniqueIDs) * 1000)



# Plot trend 

 ggplot(df_entry_exit_rates, aes(x = factor(Calender_Year), y = EntryRate, 
      color = slum_area_in_NUHDSS, group = slum_area_in_NUHDSS)) +
  geom_line(linewidth = 1.2) +  geom_point(size = 1.2) +  
  labs(title = "", x = "", y = "Entry rates per 1000 individuals") +
  theme_bw() +  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10, face = "bold"),,
    axis.text.y = element_text(size = 10, face = "bold"),
  )+
  scale_color_discrete(name = "Slum Area") +
  facet_wrap(~gender_of_NUHDSS_individual, scales = "free_y", ncol = 1) +
  scale_x_discrete(breaks = unique(rates_slum_gender$Calender_Year)) +
  scale_y_continuous(limits = c(0, 400), breaks = seq(0, 400, by = 40))


 

ggplot(df_entry_exit_rates, aes(x = factor(Calender_Year), y = ExitRate, 
        color = slum_area_in_NUHDSS, group = slum_area_in_NUHDSS)) +
  geom_line(linewidth = 1.2) +  geom_point(size = 1.2) +   
  labs(title = "", x = "",   y = "Exit rate per 1000 individuals") +
  theme_bw() +  theme(
    legend.text = element_text(size = 14),
    legend.title = element_blank(),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10, face = "bold"),,
    axis.text.y = element_text(size = 10, face = "bold"),
  )+
  scale_color_discrete(name = "Slum Area") +
  facet_wrap(~gender_of_NUHDSS_individual, scales = "free_y", ncol = 1) +
  scale_x_discrete(breaks = unique(rates_slum_gender$Calender_Year)) +
  scale_y_continuous(limits = c(0, 400), breaks = seq(0, 400, by = 40))


#------------------------------------------------age specific death rates---------------------------------------------#

total_ids_fm <- NUHDSS_Resi_Data_2002_2015 %>%  group_by( Calender_Year, age_cat) %>%
  summarise(TotalUniqueIDs = n_distinct(ID), .groups = "drop")

# Filter data for birth and death events

death_age_data <- NUHDSS_Resi_Data_2002_2015 %>% filter(residency_event %in% c('Death'))


# Calculate the number of deaths per year, slum area, gender, age cat, and event

death_counts_data <- death_age_data %>% 
  group_by( Calender_Year, slum_area_in_NUHDSS, residency_event, age_cat) %>%
  summarise(Count = n_distinct(ID), .groups = "drop") %>%
  pivot_wider(names_from = residency_event, values_from = Count) %>%
  left_join(total_ids_fm, by = c( "Calender_Year", "age_cat"))


# Calculate birth and death rates per year, slum area, gender, age and event
rates_slum_gender_age_cat <- death_counts_data %>% mutate( DeathRate = (Death / TotalUniqueIDs) * 1000)

# plot

death_line_plot <- ggplot(rates_slum_gender_age_cat, aes(x = factor(Calender_Year), y = DeathRate, 
                 color = age_cat, group = age_cat)) + geom_line(linewidth = 1.2) +  geom_point(size = 1.2) +  
                  labs(title = "", x = "", y = "Death rates per 1000 individuals") + theme_bw() +  
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10, face = "bold"),,
    axis.text.y = element_text(size = 10, face = "bold"),
  )+
  scale_color_discrete(name = "Age") +
  facet_wrap(~gender_of_NUHDSS_individual, scales = "free_y", ncol = 2) +
  scale_x_discrete(breaks = unique(rates_slum_gender$Calender_Year)) +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 5)); death_line_plot


#slum_area_in_NUHDSS,
ggplot(rates_slum_gender_age_cat, aes(x = factor(Calender_Year), y = DeathRate, 
  color = age_cat, group = age_cat)) + geom_line(linewidth = 1.2) +  geom_point(size = 1.2) +  
  labs(title = "", x = "", y = "Death rates per 1000 individuals") + theme_bw() +  
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10, face = "bold"),,
    axis.text.y = element_text(size = 10, face = "bold"),
  )+
  scale_color_discrete(name = "Age") +
  facet_wrap(~slum_area_in_NUHDSS, scales = "free_y", ncol = 2) +
  scale_x_discrete(breaks = unique(rates_slum_gender$Calender_Year)) +
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 70, by = 5))

#-----------------------------------------age specific net-migration rates--------------------------------------------#

total_pop <- NUHDSS_Resi_Data_2002_2015 %>% group_by(Calender_Year, age_cat) %>%
  summarise(TotalUniqueIDs = n_distinct(ID), .groups = "drop")

age_migration_data <- NUHDSS_Resi_Data_2002_2015 %>% filter(residency_event %in% c("Inmigration", "Outmigration"))


# Calculate the number 

age_migration_counts <- age_migration_data %>% 
  group_by(Calender_Year,  residency_event, slum_area_in_NUHDSS, age_cat) %>%  
  summarise(Count = n_distinct(ID), .groups = "drop")%>%   
  pivot_wider(names_from = residency_event, values_from = Count) %>%
  left_join(total_pop, by = c("Calender_Year", "age_cat")) 


age_migration_counts$Net_migration <- age_migration_counts$Inmigration - age_migration_counts$Outmigration


age_rates_overall <- age_migration_counts %>%
  mutate(InmigrationRate = (Inmigration / TotalUniqueIDs) * 1000, 
         OutmigrationRate = (Outmigration / TotalUniqueIDs) * 1000, 
         Overall_NetRate = (Net_migration / TotalUniqueIDs) * 1000)

age_rates_overall <- age_rates_overall %>% rename(Year = Calender_Year)

gt(age_rates_overall) %>% fmt_number( decimals = 1)



ggplot(age_rates_overall, aes(x = factor(Year), y = Overall_NetRate, 
  color = age_cat, group = age_cat)) + geom_line(linewidth = 1.2) +  geom_point(size = 1.2) +  
  labs(title = "", x = "", y = "Overall net migration per 1000 individuals") + theme_bw() +  
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10, face = "bold"),,
    axis.text.y = element_text(size = 10, face = "bold"),
  )+
  scale_color_discrete(name = "Age") +
  facet_wrap(~slum_area_in_NUHDSS, scales = "free_y", ncol = 2) +
  scale_x_discrete(breaks = unique(age_rates_overall$Year)) +
  scale_y_continuous(limits = c(-140, 170), breaks = seq(-140, 170, by = 25))


#-----------------------------------------age specific exit/entry rates------------------------------------------------#

total_pop_int <- NUHDSS_Resi_Data_2002_2015 %>% group_by(Calender_Year, age_cat) %>%
  summarise(TotalUniqueIDs = n_distinct(ID), .groups = "drop")

age_internal_data <- NUHDSS_Resi_Data_2002_2015 %>% filter(residency_event %in% c("Exit", "Entry"))


# Calculate the number 

age_internal_counts <- age_internal_data %>% 
  group_by(Calender_Year,  residency_event, gender_of_NUHDSS_individual, age_cat) %>%  
  summarise(Count = n_distinct(ID), .groups = "drop")%>%   
  pivot_wider(names_from = residency_event, values_from = Count) %>%
  left_join(total_pop_int, by = c("Calender_Year", "age_cat")) 





age_rates_internal <- age_internal_counts %>%
  mutate(ExitRate = (Exit / TotalUniqueIDs) * 1000, 
         EntryRate = (Entry / TotalUniqueIDs) * 1000)

age_rates_internal <- age_rates_internal %>% rename(Year = Calender_Year)

gt(age_rates_internal) %>% fmt_number( decimals = 1)



ggplot(age_rates_internal, aes(x = factor(Year), y = EntryRate, 
  color = age_cat, group = age_cat)) + geom_line(linewidth = 1.2) +  geom_point(size = 1.2) +  
  labs(title = "", x = "", y = "Entry rates per 1000 individuals") + theme_bw() +  
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10, face = "bold"),,
    axis.text.y = element_text(size = 10, face = "bold"),
  )+
  scale_color_discrete(name = "Age") +
  facet_wrap(~gender_of_NUHDSS_individual, scales = "free_y", ncol = 2) +
  scale_x_discrete(breaks = unique(age_rates_internal$Year)) +
  scale_y_continuous(limits = c(0, 280), breaks = seq(0, 280, by = 40))


