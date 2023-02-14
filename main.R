library(tidyverse)
library(janitor)
library(ggbreak) 

#load data----------------------------------------------------------------------
data <- read_csv("data/20220816_KB_Rituximab_STrap_revised_02.csv") %>% clean_names()

#automate Skyline evaluation----------------------------------------------------
data_total_area <- data %>%
  group_by(file_name) %>%
  summarise(total_area = sum(total_area_ms1))


data$frac_abud <- 1
for (i in 1:length(data$file_name)) {
  for (j in 1:length(data_total_area$file_name))
  if (data$file_name[i] == data_total_area$file_name[j]) { 
    data$frac_abud[i] = ((data$total_area_ms1[i]*100)/data_total_area$total_area[j])
    }
}

data_frac_abud <- data %>%
  group_by(file_name) %>%
  summarise(frac_abud = sum(frac_abud))

data_charge_states <- data %>%
  group_by(file_name, peptide_modified_sequence, modified_sequence_full_names) %>%
  summarise(frac_abud_charge_states = sum(frac_abud))

data_charge_states_summary <- data_charge_states %>%
  group_by(peptide_modified_sequence, modified_sequence_full_names) %>%
  summarise(mean = mean(frac_abud_charge_states), 
            sd = sd(frac_abud_charge_states), 
            rsd = (sd(frac_abud_charge_states)/mean(frac_abud_charge_states)*100))


#Plot data-----------------------------------------------------------------------
ggplot(data = data_charge_states_summary, mapping = aes(y = modified_sequence_full_names, x = mean)) +
  geom_col() +
  geom_errorbar(mapping = aes(xmin = mean - sd, xmax = mean + sd), width = 0.2) +
  scale_x_break(breaks = c(10, 35), space = 0.5)

ggplot(data = data_charge_states_summary, mapping = aes(y = modified_sequence_full_names, x = mean)) +
  geom_col() +
  geom_errorbar(mapping = aes(xmin = mean - rsd, xmax = mean + rsd), width = 0.2) +
  scale_x_break(breaks = c(10, 35), space = 0.5)
