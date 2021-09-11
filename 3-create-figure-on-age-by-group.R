# Andrew Boslett
# University of Rochester Medical Center
# andrew_boslett@urmc.rochester.edu

rm(list = ls())

options(scipen = 999)

# Goal: Examine distribution of ages for drug overdoses and drug-related deaths

# Import data --------------------------

overdose_deaths <- readRDS("Opioids_CCs/Scratch/Overdoses.rds") # overdose
drug_deaths <- readRDS("Opioids_CCs/Scratch/Drug_related_Deaths.rds") # drug-related

# Clean data ------------------------

# Tabulate age for < 2003

drug_deaths %>% filter(year <= 2002) %>% statar::tab(age)

drug_deaths %<>% filter(!(age > 105 & year <= 2002))
overdose_deaths %<>% filter(!(age > 105 & year <= 2002))

# Tabulate age for >= 2003

overdose_deaths %>% filter(year >= 2003) %>% statar::tab(age)

overdose_deaths %<>% filter(!(age > 1107 & year >= 2003)) 
drug_deaths %<>% filter(!(age > 1107 & year >= 2003))

# Replace age with age - 1000 if year 2003 and above

overdose_deaths %<>% mutate(age = ifelse(year >= 2003, age - 1000, age))
drug_deaths %<>% mutate(age = ifelse(year >= 2003, age - 1000, age))

# Calculate quantiles of age by year --------------------------------

age_data <- data.frame()

for(fff in c('overdose_deaths', 'drug_deaths')) {
  
  temp <- get(fff)
  
  # Calculate median and quantile of age
  
  temp_summary <- temp %>% group_by(year) %>%
    summarise(median_age = median(age, na.rm = TRUE),
              age_25 = quantile(age, probs = 0.25),
              age_75 = quantile(age, probs = 0.75)) %>%
    ungroup() %>% mutate(variable = fff)
  
  # Bind to data frame
  
  age_data %<>% bind_rows(temp_summary)
  
  # Remove files
  
  rm(temp, fff, temp_summary)
  
}

# Plot change in age over time

age_data %<>% mutate(variable = ifelse(variable != 'drug_deaths', 'Drug overdoses', 'Drug-related disorders'))

temp_plot <- ggplot() + 
  theme_classic() + 
  theme(legend.title = element_blank()) + 
  geom_line(data = age_data,
            aes(x = year, y = median_age, color = variable), linetype = 'solid') + 
  geom_line(data = age_data,
            aes(x = year, y = age_25, color = variable), linetype = 'dashed') + 
  geom_line(data = age_data,
            aes(x = year, y = age_75, color = variable), linetype = 'dashed') + 
  labs(x = 'Year', y = 'Age (years)') + 
  geom_point(data = age_data,
             aes(x = year, y = median_age, color = variable)) + 
  theme(legend.position = c(0.2, 0.9)) + 
  scale_colour_manual(values = c('tomato', 'darkblue')) + 
  xlim(1999, 2020)

temp_plot

ggsave('Opioids_CCs/Figures/Figure_X_Age_IQR.jpg')
