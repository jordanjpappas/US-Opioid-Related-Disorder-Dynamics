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

gender_data <- data.frame()

for(fff in c('drug_deaths', 'overdose_deaths')) {
  
  # Get data 
  
  temp <- get(fff)

  temp %>% 
    group_by(year) %>% 
    summarise_at(vars(female), funs(mean(., na.rm = TRUE))) %>% 
    ungroup() -> temp_summary  
  
  # Add male
  
  temp_summary %<>% mutate(male = 1 - female, 
                           category = ifelse(str_detect(fff, pattern = 'overdose') == TRUE, 
                                             'Drug Overdoses', 'Drug-Related Deaths'))
  
  # Bind to gender data
  
  gender_data %<>% bind_rows(temp_summary)
  
  rm(temp_summary)
  
}

gender_data %<>% gather(variable, value, -year, -category)

gender_data$variable %<>% str_to_title()

gender_plot <- ggplot(data = gender_data,
                      aes(x = year, y = value, linetype = variable, colour = category,
                          group = interaction(variable, category))) + 
  geom_line() +
  theme_classic() + 
  labs(x = 'Year', y = '% of deaths') + 
  scale_colour_manual(values=c('#a50026', '#313695')) +
  theme(legend.title=element_blank()) +
  theme(legend.position = c(0.2, 0.85)) + 
  ylim(0.2, 0.9) + xlim(1998, 2020)

gender_plot

ggsave('Opioids_CCs/Figures/Figure_X_Gender.jpg')



