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

education_data <- data.frame()

for(fff in c('drug_deaths', 'overdose_deaths')) {
  
  # Get data 
  
  temp <- get(fff)
  
  # Add less_than_hs for overdose deaths
  
  if(fff == 'overdose_deaths') {
    
    temp %<>% mutate(less_than_hs = 1 - (hs + college + some_college + educ_unknown))
    
  } else {
    print('Other')
  }
  
  # Group by year
  
  temp %>% 
    group_by(year) %>% 
    summarise_at(vars(less_than_hs, hs, some_college, college), funs(mean(., na.rm = TRUE))) %>% 
    ungroup() -> temp_summary  
  
  # Add male
  
  temp_summary %<>% mutate(category = ifelse(fff == 'overdose_deaths', 
                                             'Drug Overdoses', 'Drug-Related Deaths'))
  
  # Bind to education data
  
  education_data %<>% bind_rows(temp_summary)
  
  rm(temp_summary)
  
}

education_data %<>% gather(variable, value, -year, -category)

education_data %<>% mutate(variable = case_when(
  variable == 'less_than_hs' ~ 'Less than High School',
  variable == 'hs' ~ 'High School',
  variable == 'some_college' ~ 'Some College',
  variable == 'college' ~ 'College'
))

education_plot <- ggplot(data = education_data,
                      aes(x = year, y = value, linetype = variable, colour = category,
                          group = interaction(variable, category))) + 
  geom_line() +
  theme_classic() + 
  labs(x = 'Year', y = '% of deaths') + 
  scale_colour_manual(values=c('#c51b7d', '#4d9221')) +
  theme(legend.title=element_blank()) +
  xlim(1998, 2020)

education_plot

ggsave('Opioids_CCs/Figures/Figure_X_Education.jpg')