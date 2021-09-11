# Jordan Pappas, Andrew Boslett, Elaine Hill, Alina Denham, Meredrith Adams
# University of Rochester Medical Center, Hill Lab for Health Economics
# jordan.pappas@rochester.edu



# (7b) I want to understand how age; sex; race; and ethnicity of decedents from both types of deaths vary from 1999-2019. We can imagine a separate plot for each variable, perhaps with some additional variations for each variable. For example, one could imagine a figure where average/median age of decedent from drug overdoses vs. drug-related disorders (focus on those with underlying cause of death as F1*, for now) varies across time. Another slightly different version would be a binned approach, where we identify the #/proportion of deaths in various age bins (your current figure in Box).

# demographic trends of drug related disorders and drug overdoses from 1999-2019
  # X: Year
  # Y: 
    # % male drug related disorders 
    # % female drug related disorders
    # % male drug overdoses
    # % female drug overdoses





# Import data

all_deaths <- readRDS("Opioids_CCs/Scratch/Deaths_with_F1_code.rds")
overdose_deaths <- readRDS("Opioids_CCs/Scratch/Overdoses.rds")



# Clean data

# all_deaths

all_deaths$male <- ifelse(all_deaths$female==1, 0, 1)

# Group by year and aggregate by mean of race
all_deaths %>% 
  group_by(year) %>% 
  summarise_at(vars(male, female), funs(mean(., na.rm = TRUE))) %>% 
  ungroup() -> summary_disorders

# Format data-long
summary_disorders %>% 
  select(year, male, female) %>% 
  gather(variable, value, -year) %>% 
  mutate(variable = str_to_title(variable)) -> summary_disorders_long

summary_disorders_long %<>% mutate(variable = factor(variable, levels = c('Male', 'Female')))
summary_disorders_long %<>% mutate(death_type = 'Drug-Related Disorders')

# overdose_deaths

overdose_deaths$male <- ifelse(overdose_deaths$female==1, 0, 1)

# Group by year and aggregate by mean of race
overdose_deaths %>% 
  group_by(year) %>% 
  summarise_at(vars(male,female), funs(mean(., na.rm = TRUE))) %>% 
  ungroup() -> summary_overdose

# Format data-long
summary_overdose %>% 
  select(year, male, female) %>% 
  gather(variable, value, -year) %>% 
  mutate(variable = str_to_title(variable)) -> summary_overdose_long

summary_overdose_long %<>% mutate(variable = factor(variable, levels = c('Male', 'Female')))
summary_overdose_long %<>% mutate(death_type = 'Drug Overdose')



# Bind data

temp <- bind_rows(summary_disorders_long,summary_overdose_long)



# Plot data

png('Opioids_CCs/Figures/Figure_(7b).png',width=600,height=600)

race_plot <- ggplot(data = temp, aes(x = year, y = value, colour = variable, linetype = death_type)) + 
  geom_point() + geom_line() + theme_classic() + 
  labs(x = 'Year', y = '% of drug overdoses') + 
  scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#000000")) +
  theme(plot.margin=unit(c(1,4,1,1),"cm")) +
  theme(legend.justification=c(.15,.15), legend.position=c(1.021,0.35)) + theme(legend.title=element_blank())

race_plot

dev.off()




