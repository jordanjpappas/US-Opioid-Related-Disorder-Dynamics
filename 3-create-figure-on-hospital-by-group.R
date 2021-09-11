# Jordan Pappas, Andrew Boslett, Elaine Hill, Alina Denham, Meredrith Adams
# University of Rochester Medical Center, Hill Lab for Health Economics
# jordan.pappas@rochester.edu

rm(list = ls())

options(scipen = 999)

# Import data ----------------------------

overdose_deaths <- readRDS("Opioids_CCs/Scratch/Overdoses.rds") # overdose
drug_deaths <- readRDS("Opioids_CCs/Scratch/Drug_related_Deaths.rds") # drug-related

# Clean data -----------------------
# Notes: In this part of the script, we loop a couple functions that summarize the drug overdose and drug-related disorder
# data by race groups.

hospital_data <- data.frame()

for(fff in c('drug_deaths', 'overdose_deaths')) {
  
  # Get data 
  
  temp <- get(fff)

  # Add hospital indicator
  
  temp %<>% mutate(hospital = ifelse(dead_hospital_in == 1 | dead_hospital_out, 1, 0))
  
  # Group by year
  
  temp %>% 
    group_by(year) %>% 
    summarise_at(vars(hospital), funs(mean(., na.rm = TRUE))) %>% 
    ungroup() -> temp_summary  
  
  # Add male
  
  temp_summary %<>% mutate(non_hospital = 1 - hospital, 
                           category = ifelse(fff == 'overdose_deaths', 
                                             'Drug Overdoses', 'Drug-Related Deaths'))
  
  # Bind to gender data
  
  hospital_data %<>% bind_rows(temp_summary)
  
  rm(temp_summary)
  
}

hospital_data %<>% gather(variable, value, -year, -category)

hospital_data %<>% mutate(variable = ifelse(variable == 'hospital', 'Hospital', 'Non-Hospital'))

hospital_plot <- ggplot(data = hospital_data,
                      aes(x = year, y = value, linetype = variable, colour = category,
                          group = interaction(variable, category))) + 
  geom_line() +
  theme_classic() + 
  labs(x = 'Year', y = '% of deaths') + 
  scale_colour_manual(values=c('#c51b7d', '#4d9221')) +
  theme(legend.title=element_blank()) +
  xlim(1998, 2020)

hospital_plot

ggsave('Opioids_CCs/Figures/Figure_X_Hospital.jpg')