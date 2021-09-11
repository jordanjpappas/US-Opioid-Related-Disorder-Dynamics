# Jordan Pappas, Andrew Boslett, Elaine Hill, Alina Denham, Meredrith Adams
# University of Rochester Medical Center, Hill Lab for Health Economics
# jordan.pappas@rochester.edu



# (7) Understand how age; sex; race; and ethnicity of decedents from both types of deaths vary from 1999-2019. We can imagine a separate plot for each variable, perhaps with some additional variations for each variable. For example, one could imagine a figure where average/median age of decedent from drug overdoses vs. drug-related disorders (focus on those with underlying cause of death as F1*, for now) varies across time. Another slightly different version would be a binned approach, where we identify the #/proportion of deaths in various age bins (your current figure in Box).

# demographic trends of drug related disorders and drug overdoses from 1999-2019
  # X: Year
  # Y: 
    # % black drug related disorders 
    # % asian drug related disorders 
    # % hispanic drug related disorders 
    # % white drug related disorders 
    # % female drug related disorders
    # % black drug overdoses
    # % asian drug overdoses
    # % hispanic drug overdoses
    # % white drug overdoses
    # % female drug overdoses





# Import data

all_deaths <- readRDS("Opioids_CCs/Scratch/Deaths_with_F1_code.rds")
overdose_deaths <- readRDS("Opioids_CCs/Scratch/Overdoses.rds")



# Clean data

  # all_deaths

    # Group by year and aggregate by mean of race
    all_deaths %>% 
      group_by(year) %>% 
      summarise_at(vars(black, hispanic, indian, white, female), funs(mean(., na.rm = TRUE))) %>% 
      ungroup() -> summary_disorders
    
    # Format data-long
    summary_disorders %>% 
      select(year, black, indian, white, hispanic, female) %>% 
      gather(variable, value, -year) %>% 
      mutate(variable = str_to_title(variable)) -> summary_disorders_long
    
    summary_disorders_long %<>% mutate(variable = factor(variable, levels = c('White', 'Black', 'Indian', 'Hispanic', 'Female')))
    summary_disorders_long %<>% mutate(death_type = 'Drug-Related Disorders')

  # overdose_deaths

    # Group by year and aggregate by mean of race
    overdose_deaths %>% 
      group_by(year) %>% 
      summarise_at(vars(black, hispanic, indian, white, female), funs(mean(., na.rm = TRUE))) %>% 
      ungroup() -> summary_overdose
    
    # Format data-long
    summary_overdose %>% 
      select(year, black, indian, white, hispanic, female) %>% 
      gather(variable, value, -year) %>% 
      mutate(variable = str_to_title(variable)) -> summary_overdose_long
    
    summary_overdose_long %<>% mutate(variable = factor(variable, levels = c('White', 'Black', 'Indian', 'Hispanic', 'Female')))
    summary_overdose_long %<>% mutate(death_type = 'Drug Overdose')


    
# Bind data

temp <- bind_rows(summary_disorders_long,summary_overdose_long)



# Plot data

png('Opioids_CCs/Figures/Figure_(7)_1.png',width=600,height=600)

temp_gender <- temp %>% filter(variable == 'Female')

temp_gender %<>% dplyr::rename(Female = value) %>%
  mutate(Male = 1 - Female) %>%
  dplyr::select(year, death_type, Male, Female) %>%
  gather(variable, value, -year, -death_type)

race_plot <- ggplot(data = temp_gender, 
                    aes(x = year, y = value, colour = variable, linetype = death_type)) + 
  geom_point() + geom_line() + theme_classic() + 
  labs(x = 'Year', y = '% of deaths by gender') + 
  scale_colour_manual(values=c('#d8b365', '#5ab4ac')) +
  theme(legend.justification=c(1,1), legend.position=c(0.3,0.5)) + theme(legend.title=element_blank()) +
  theme(text=element_text(size=15,  family = 'serif'))

race_plot

ggsave('Opioids_CCs/Figures/Figure_X_Sex.jpg')

dev.off()

# Race

temp_race <- temp %>% 
  mutate(variable = as.character(variable)) %>%
  filter(variable %in% c('Black', 'White', 'Indian')) %>%
  mutate(variable = ifelse(variable == 'Indian', 'Native American', variable)) %>%
  dcast(year + death_type ~ variable, value.var = c('value')) %>%
  mutate(Asian = 1 - (Black + White + `Native American`)) %>%
  gather(variable, value, -year, -death_type)

race_plot <- ggplot(data = temp_race, 
                    aes(x = year, y = value, colour = death_type)) + 
  geom_point() + geom_line() + theme_classic() + 
  facet_wrap(~variable) + 
  labs(x = 'Year', y = '% of deaths') + 
  scale_colour_manual(values=c('#a50026', '#313695')) +
  theme(legend.title=element_blank()) +
  theme(text=element_text(size=15,  family = 'serif'))

race_plot

ggsave('Opioids_CCs/Figures/Figure_X_Race.jpg', width = 10, height = 5)



