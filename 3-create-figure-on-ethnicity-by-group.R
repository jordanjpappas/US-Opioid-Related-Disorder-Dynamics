# Andrew Boslett
# University of Rochester Medical Center
# andrew_boslett@urmc.rochester.edu

rm(list = ls())

options(scipen = 999)

# Goal: Examine distribution of ages for drug overdoses and drug-related deaths

# Import data --------------------------

overdose_deaths <- readRDS("Opioids_CCs/Scratch/Overdoses.rds") # overdose
drug_deaths <- readRDS("Opioids_CCs/Scratch/Drug_related_Deaths.rds") # drug-related

# Calculate quantiles of age by year --------------------------------

ethnicity_data <- data.frame()

for(fff in c('drug_deaths', 'overdose_deaths')) {
  
  # Get data 
  
  temp <- get(fff)
  
  temp %>% 
    group_by(year) %>% 
    summarise_at(vars(hispanic), funs(mean(., na.rm = TRUE))) %>% 
    ungroup() -> temp_summary  
  
  # Add male
  
  temp_summary %<>% mutate(non_hispanic = 1 - hispanic, 
                           category = ifelse(fff == 'overdose_deaths', 
                                             'Drug Overdoses', 'Drug-Related Deaths'))
  
  # Bind to gender data
  
  ethnicity_data %<>% bind_rows(temp_summary)
  
  rm(temp_summary)
  
}

ethnicity_data %<>% gather(variable, value, -year, -category)

ethnicity_data %<>% mutate(variable = ifelse(variable == 'hispanic', 'Hispanic', 'Non-Hispanic'))

ethnicity_plot <- ggplot(data = ethnicity_data,
                      aes(x = year, y = value, linetype = variable, colour = category,
                          group = interaction(variable, category))) + 
  geom_line() +
  theme_classic() + 
  labs(x = 'Year', y = '% of deaths') + 
  scale_colour_manual(values=c('#f46d43', '#4575b4')) +
  theme(legend.title=element_blank()) +
  xlim(1998, 2020)

ethnicity_plot

ggsave('Opioids_CCs/Figures/Figure_X_Ethnicity.jpg')



