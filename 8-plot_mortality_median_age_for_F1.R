# Jordan Pappas, Andrew Boslett, Elaine Hill, Alina Denham, Meredrith Adams
# University of Rochester Medical Center, Hill Lab for Health Economics
# jordan.pappas@rochester.edu



# (8) Variation in age, sex, race, and ethnicity of decedents with F1* UCOD or at least 1 F1* contributing cause from 1999-2019.





# Load data

all_deaths <- readRDS("Opioids_CCs/Scratch/Deaths_with_F1_code.rds")



# Clean data

  # all_deaths
    
    # NA to 0 for all record causes
    all_deaths %<>% mutate_at(vars(F110:F154, F135:F116),
                              funs(ifelse(is.na(.) == TRUE, 0, .)))
    
    # Create variables indicating incidence of any related drug codes
    all_deaths %>% select(starts_with('F1')) %>%
      rowSums() -> all_deaths$F1_all
    all_deaths %<>% mutate(F1_all = ifelse(F1_all > 0, 1, 0))
    
    # Group by year and filter on drug code and age bin
    all_deaths %>% 
      filter(F1_all == 1) %>% 
      group_by(year) %>% 
      summarise_at(vars(age_20, age_30, age_40, age_50, age_60, age_70, age_80, age_125), funs(sum(.))) %>% 
      ungroup() -> summary_age
    
    # Format data-long
    summary_age %>% 
      select(year, age_20, age_30, age_40, age_50, age_60, age_70, age_80, age_125) %>% 
      gather(variable, value, -year) %>% 
      mutate(variable = str_to_title(variable)) -> summary_age_long
    
    summary_age_long$variable = factor(summary_age_long$variable, levels = c('Age_20','Age_30','Age_40','Age_50','Age_60','Age_70','Age_80','Age_125'))



# Plot data

png('Opioids_CCs/Figures/Figure_(7)_2.png',width=600,height=600)

yearly_plot <- ggplot(data = summary_age_long, aes(x = year, y = value, colour = variable)) + 
  geom_point() + geom_line() + theme_classic() + 
  labs(x = 'Year', y = '# of Deaths') + 
  scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#000000")) +
  theme(legend.justification=c(1,1), legend.position=c(0.2,1.0)) + theme(legend.title=element_blank())

yearly_plot

dev.off()
