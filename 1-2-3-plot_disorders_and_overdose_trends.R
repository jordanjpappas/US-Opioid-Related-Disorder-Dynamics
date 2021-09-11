# Jordan Pappas, Andrew Boslett, Elaine Hill, Alina Denham, Meredrith Adams
# University of Rochester Medical Center, Hill Lab for Health Economics
# jordan.pappas@rochester.edu



# (1) A figure of trends from 1999-2019 in
  # (a) # of drug overdoses, total;
  # (b) # of drug overdoses, with filters on F1* contributing causes of death (SKIP FOR NOW - AJB); and
  # (c) # of total non-drug overdose deaths with either CC/underlying cause of death with a F1* cause.

# (2) A figure of trends from 1999-2019 in
  # (a) # of drug overdoses, total;
  # (b) # of drug overdoses, with filters on F1* contributing causes of death; and
  # (c) # of total non-drug overdose deaths with either CC/underlying cause of death with a F1* cause.
  # (d) # of opioid overdoses, total;
  # (e) # of opioid overdoses, with filters on F1* contributing causes of death.

# (3) A figure of trends from 1999-2019 with 
  # (a) # of total non-drug overdose deaths with an underlying cause of death (UCOD) with a F1* cause; and 
  # (b) # of total non-drug overdose deaths with a contributing, but not underlying, cause of death with a F1* cause.





# Import data ------------------------------------

all_deaths <- readRDS("Opioids_CCs/Scratch/Deaths_with_F1_code.rds")
overdose_deaths <- readRDS("Opioids_CCs/Scratch/Overdoses.rds")

# Summarise data by year ----------------------------

# Drug overdoses

overdoses_per_year <- overdose_deaths %>% group_by(year) %>%
  summarise(value = n()) %>%
  ungroup() %>%
  mutate(variable = 'Drug overdose')

# Drug-related disorders

disorders_per_year <- all_deaths %>% 
  mutate(variable = ifelse(
    str_detect(string = ucod, pattern = '^F1') == TRUE,
    'Drug-related disorder, UCOD', 'Drug-related disorder, CC')) %>%
  group_by(year, variable) %>%
  summarise(value = n()) %>%
  ungroup()

disorders_total <- all_deaths %>%
  group_by(year) %>%
  summarise(value = n()) %>%
  ungroup() %>%
  mutate(variable = 'Drug-related disorder')

bound_data <- overdoses_per_year %>% bind_rows(disorders_per_year) %>%
  bind_rows(disorders_total)

bound_data$variable <- factor(bound_data$variable, levels = c('Drug overdose',
                                                              'Drug-related disorder',
                                                              'Drug-related disorder, CC',
                                                              'Drug-related disorder, UCOD'))

bound_plot <- ggplot(data = bound_data, aes(x = year, y = value, colour = variable, group = variable)) + 
  geom_line() + geom_point() + theme_classic() + labs(x = 'Year', y = '# of deaths') + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = c(0.3, 0.8)) + 
  scale_color_manual(values = c('#a50026', '#fee090', '#abd9e9', '#313695')) +
  theme(text=element_text(size=13,  family = 'serif')) +
  xlim(1998, 2022)

bound_plot

getwd()

ggsave('Opioids_CCs/Figures/Figure_X_Death_Counts_by_Category.jpg')

all_deaths_all <- all_deaths %>%
  filter(str_detect(string = ucod, pattern = '^F1') == TRUE) %>%
  #filter(F1_all == 1 | str_detect(string = ucod, pattern = '^F1') == TRUE) %>%
  group_by(year) %>%
  summarise(n_n = n()) %>%
  ungroup()
all_deaths_all$ICD_code <- 'all deaths'


# Clean data

  # overdose_deaths

    # NA to 0 for all record causes
    overdose_deaths %<>% mutate_at(vars(F101:O961),
                                   funs(ifelse(is.na(.) == TRUE, 0, .)))
    
    # NA to 0 for all record causes
    all_deaths %<>% mutate_at(vars(F110:F154, F135:F116),
                              funs(ifelse(is.na(.) == TRUE, 0, .)))

    # (a) # of drug overdoses, total

      # Group by year
      overdose_all <- overdose_deaths %>%
        group_by(year) %>%
        summarise(n_n = n()) %>%
        ungroup()
      overdose_all$ICD_code <- 'overdoses all'

    # (b) # of drug overdoses, with filters on F1* contributing causes of death

      # Create variables indicating incidence of any related drug codes
      overdose_deaths %>% select(starts_with('F1')) %>%
        rowSums() -> overdose_deaths$F1_all
      overdose_deaths %<>% mutate(F1_all = ifelse(F1_all > 0, 1, 0))

      # Group by year and filter in F1* CC
      overdose_F1_CC <- overdose_deaths %>%
        filter(F1_all == 1) %>%
        group_by(year) %>%
        summarise(n_n = n()) %>%
        ungroup()
      overdose_F1_CC$ICD_code <- 'overdoses with F1* CC'

    # (c) # of total non-drug overdose deaths with either CC/underlying cause of death with a F1* cause

      # Create a variable indicating incidence of any F1 related to opioids
      all_deaths %>% select(starts_with('F1')) %>%
        rowSums() -> all_deaths$F1_all
      all_deaths %<>% mutate(F1_all = ifelse(F1_all > 0, 1, 0))
      
      # Group by UCOD and filter in F1* CC and filter out F1* UCOD
      all_deaths_all <- all_deaths %>%
        filter(F1_all == 1 | str_detect(string = ucod, pattern = '^F1') == TRUE) %>%
        group_by(year) %>%
        summarise(n_n = n()) %>%
        ungroup()
      all_deaths_all$ICD_code <- 'all deaths'

    # (d) # of opioid overdoses, total

      # Group by year and filter in opioids
      overdose_opioid_all <- overdose_deaths %>%
        filter(any_opioid == 1) %>%
        group_by(year) %>%
        summarise(n_n = n()) %>%
        ungroup()
      overdose_opioid_all$ICD_code <- 'overdoses opioid all'

    # (e) # of opioid overdoses, with filters on F1* contributing causes of death

      # Create variables indicating incidence of any related drug codes
      overdose_deaths %>% select(starts_with('F1')) %>%
        rowSums() -> overdose_deaths$F1_all
      overdose_deaths %<>% mutate(F1_all = ifelse(F1_all > 0, 1, 0))

      # Group by year and filter in F1* CC
      overdose_opioid_F1_CC <- overdose_deaths %>%
        filter(any_opioid == 1 & F1_all == 1) %>%
        group_by(year) %>%
        summarise(n_n = n()) %>%
        ungroup()
      overdose_opioid_F1_CC$ICD_code <- 'overdoses opioid with F1* CC'



# Bind data

bind <- bind_rows(overdose_all,overdose_F1_CC,all_deaths_all,overdose_opioid_all,overdose_opioid_F1_CC)



# Plot data

png('Opioids_CCs/Figures/Figure_(2).png',width=600,height=600)

yearly_plot <- ggplot(data = bind,
                      aes(x = year, y = n_n, colour=ICD_code)) +
  labs(x = 'Year', y = '# of Deaths') +
  geom_line() + geom_point(size = 3) +
  scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
                      breaks=c("overdoses all", "overdoses with F1* CC", "all deaths", "overdoses opioid all", "overdoses opioid with F1* CC"),
                      labels=c("# of drug overdoses, total", "# of drug overdoses, with filters on F1* contributing causes of death", "# of total non-drug overdose deaths with either CC/underlying cause of death with a F1* cause", "# of opioid overdoses, total", "# of opioid overdoses, with filters on F1* contributing causes of death")) +
  theme_classic() +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) + theme(legend.title=element_blank())

yearly_plot

dev.off()





# Clean data

bind <- subset(bind, ICD_code=="overdoses all" | ICD_code=="overdoses with F1* CC" | ICD_code=="all deaths")



# Plot data

png('Opioids_CCs/Figures/Figure_(1).png',width=600,height=600)

yearly_plot <- ggplot(data = bind,
                      aes(x = year, y = n_n, colour=ICD_code)) +
  labs(x = 'Year', y = '# of Deaths') +
  geom_line() + geom_point(size = 3) +
  scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
                      breaks=c("overdoses all", "overdoses with F1* CC", "all deaths"),
                      labels=c("# of drug overdoses, total", "# of drug overdoses, with filters on F1* contributing causes of death", "# of total non-drug overdose deaths with either CC/underlying cause of death with a F1* cause")) +
  theme_classic() +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) + theme(legend.title=element_blank())

yearly_plot

dev.off()








# Import data

all_deaths <- readRDS("Opioids_CCs/Scratch/Deaths_with_F1_code.rds")



# Clean data
  
  # NA to 0 for all record causes
  all_deaths %<>% mutate_at(vars(F110:F154, F135:F116),
                            funs(ifelse(is.na(.) == TRUE, 0, .)))
  
  # (a) # of total non-drug overdose deaths with an underlying cause of death (UCOD) with a F1* cause
    
    # Group by UCOD and filter in F1* CC and filter out F1* UCOD
    all_deaths_UCOD <- all_deaths %>%
      filter(str_detect(string = ucod, pattern = '^F1') == TRUE) %>%
      group_by(year) %>%
      summarise(n_n = n()) %>%
      ungroup()
    all_deaths_UCOD$ICD_code <- 'all deaths UCOD'
    
  # (b) # of total non-drug overdose deaths with a contributing, but not underlying, cause of death with a F1* cause
    
    # Create a variable indicating incidence of any F1 related to opioids
    all_deaths %>% select(starts_with('F1')) %>%
      rowSums() -> all_deaths$F1_all
    all_deaths %<>% mutate(F1_all = ifelse(F1_all > 0, 1, 0))
    
    # Group by UCOD and filter in F1* CC and filter out F1* UCOD
    all_deaths_CC <- all_deaths %>%
      filter(F1_all == 1 | str_detect(string = ucod, pattern = '^F1') == FALSE) %>%
      group_by(year) %>%
      summarise(n_n = n()) %>%
      ungroup()
    all_deaths_CC$ICD_code <- 'all deaths CC'
  


# Bind data

bind <- bind_rows(all_deaths_UCOD,all_deaths_CC)



# Plot data

png('Opioids_CCs/Figures/Figure_(3).png',width=600,height=600)

yearly_plot <- ggplot(data = bind,
                      aes(x = year, y = n_n, colour=ICD_code)) +
  labs(x = 'Year', y = '# of Deaths') +
  geom_line() + geom_point(size = 3) +
  scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
                      breaks=c("all deaths CC", "all deaths UCOD"),
                      labels=c("UCOD with a F1* cause","CC, but not UCOD, with a F1* cause")) +
  theme_classic() +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) + theme(legend.title=element_blank())

yearly_plot

dev.off()





# Import data

all_deaths <- readRDS("Opioids_CCs/Scratch/Deaths_with_F1_code.rds")
overdose_deaths <- readRDS("Opioids_CCs/Scratch/Overdoses.rds")



# Clean data

# overdose_deaths

# NA to 0 for all record causes
overdose_deaths %<>% mutate_at(vars(F101:O961),
                               funs(ifelse(is.na(.) == TRUE, 0, .)))

# NA to 0 for all record causes
all_deaths %<>% mutate_at(vars(F110:F154, F135:F116),
                          funs(ifelse(is.na(.) == TRUE, 0, .)))



# (a) # of drug-related disorders, with filters on F1* contributing causes of death

# Create variables indicating incidence of any related drug codes
all_deaths %>% select(starts_with('F1')) %>%
  rowSums() -> all_deaths$F1_all
all_deaths %<>% mutate(F1_all = ifelse(F1_all > 0, 1, 0))

# Group by year and filter in F1* CC
all_F1_CC <- all_deaths %>%
  filter(F1_all == 1) %>%
  group_by(year) %>%
  summarise(n_n = n()) %>%
  ungroup()
all_F1_CC$ICD_code <- 'disorders with F1* CC'



# (b) # of drug-related disorders, with filters on F11* contributing causes of death

# Create variables indicating incidence of any related drug codes
all_deaths %>% select(starts_with('F11')) %>%
  rowSums() -> all_deaths$F11_all
all_deaths %<>% mutate(F11_all = ifelse(F11_all > 0, 1, 0))

# Group by year and filter in F11* CC
all_F11_CC <- all_deaths %>%
  filter(F11_all == 1) %>%
  group_by(year) %>%
  summarise(n_n = n()) %>%
  ungroup()
all_F11_CC$ICD_code <- 'disorders with F11 CC'



# (c) # of drug overdoses, with filters on F1* contributing causes of death

# Create variables indicating incidence of any related drug codes
overdose_deaths %>% select(starts_with('F1')) %>%
  rowSums() -> overdose_deaths$F1_all
overdose_deaths %<>% mutate(F1_all = ifelse(F1_all > 0, 1, 0))

# Group by year and filter in F1* CC
overdose_F1_CC <- overdose_deaths %>%
  filter(F1_all == 1) %>%
  group_by(year) %>%
  summarise(n_n = n()) %>%
  ungroup()
overdose_F1_CC$ICD_code <- 'overdoses with F1* CC'



# (d) # of drug overdoses, with filters on F11* contributing causes of death

# Create variables indicating incidence of any related drug codes
overdose_deaths %>% select(starts_with('F11')) %>%
  rowSums() -> overdose_deaths$F11_all
overdose_deaths %<>% mutate(F11_all = ifelse(F11_all > 0, 1, 0))

# Group by year and filter in F11* CC
overdose_F11_CC <- overdose_deaths %>%
  filter(F11_all == 1) %>%
  group_by(year) %>%
  summarise(n_n = n()) %>%
  ungroup()
overdose_F11_CC$ICD_code <- 'overdoses with F11 CC'



# Bind data

bind <- bind_rows(all_F1_CC,all_F11_CC,overdose_F1_CC,overdose_F11_CC)



# Plot data

png('Opioids_CCs/Figures/Figure_(4).png',width=600,height=600)

yearly_plot <- ggplot(data = bind,
                      aes(x = year, y = n_n, colour=ICD_code)) +
  labs(x = 'Year', y = '# of Deaths') +
  geom_line() + geom_point(size = 3) +
  scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
                      breaks=c('disorders with F1* CC', 'disorders with F11 CC','overdoses with F1* CC','overdoses with F11 CC'),
                      labels=c("Drug-related disorders, F1* CC","Drug-related disorders, F11 CC","Drug overdoses, F1* CC","Drug overdoses, F11 CC")) +
  theme_classic() +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) + theme(legend.title=element_blank())

yearly_plot

dev.off()


