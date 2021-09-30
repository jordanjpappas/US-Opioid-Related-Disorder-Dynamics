# Elaine Hill, Andrew Boslett, Alina Denham, Meredith Adams, Jordan Pappas
# University of Rochester Medical Center, Health and Environmental Economics Lab (HEEL)


########################### Preamble ###########################

rm(list = ls())
options(scipen = 999)

########################### Figure: create-figure-on-disorders-and-overdose-trends ###########################

# Goal: A figure of trends from 1999-2019 in
# (a) # of drug overdoses, total;
# (b) # of drug overdoses, with filters on F1* contributing causes of death (SKIP FOR NOW - AJB); and
# (c) # of total non-drug overdose deaths with either CC/underlying cause of death with a F1* cause.

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


########################### Figure: create-figure-on-disorders-and-overdose-trends ###########################

# Goal: A figure of trends from 1999-2019 in
# (a) # of drug overdoses, total;
# (b) # of drug overdoses, with filters on F1* contributing causes of death; and
# (c) # of total non-drug overdose deaths with either CC/underlying cause of death with a F1* cause.
# (d) # of opioid overdoses, total;
# (e) # of opioid overdoses, with filters on F1* contributing causes of death.

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

########################### Figure: create-figure-on-disorders-and-overdose-trends ###########################

# Goal: A figure of trends from 1999-2019 with 
# (a) # of total non-drug overdose deaths with an underlying cause of death (UCOD) with a F1* cause; and 
# (b) # of total non-drug overdose deaths with a contributing, but not underlying, cause of death with a F1* cause.

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

########################### Figure: create-figure-on-#-of-deaths-by-year ###########################

# Goal: # of deaths by year ----------------------

# Import data ----------------------

all_deaths <- readRDS("Opioids_CCs/Scratch/Deaths_with_F1_code.rds")

# Clean data -----------------------

names(all_deaths)

# NA to 0 for all record causes

all_deaths %<>% mutate_at(vars(F110:F154, F135:F116),
                          funs(ifelse(is.na(.) == TRUE, 0, .)))

# Create a variable indicating incidence of any F1 and F11 code related to opioids

all_deaths %>% select(starts_with('F11')) %>%
  rowSums() -> all_deaths$F11_all

all_deaths %<>% mutate(F11_all = ifelse(F11_all > 0, 1, 0))

all_deaths %>% select(starts_with('F1')) %>%
  rowSums() -> all_deaths$F1_all

all_deaths %<>% mutate(F1_all = ifelse(F1_all > 0, 1, 0))

temp <- all_deaths %>% group_by(year) %>%
  summarise(n_n = n()) %>%
  ungroup()

yearly_plot <- ggplot(data = temp,
                      aes(x = year, y = n_n)) +
  theme_classic() + labs(x = 'Year', y = '# of deaths with drug-related disorder ICD-10 codes') +
  geom_line() + geom_point(size = 3, color = 'dodgerblue')

yearly_plot

ggsave('Opioids_CCs/Figures/Figure_X_Annual_Deaths_Non_Drug_Overdoses_F1.jpg')

########################### Figure: create-figure-on-#-of-deaths-by-year-F11-category ###########################

# Goal: # of deaths by year, by F11 category -------------------------

temp <- all_deaths %>%
  filter(F11_all == 1) %>%
  group_by(year) %>%
  summarise(n_n = n()) %>%
  ungroup()

yearly_plot <- ggplot(data = temp,
                      aes(x = year, y = n_n)) +
  theme_classic() + labs(x = 'Year', y = '# of deaths with opioid-related disorders ICD-10 codes') +
  geom_line() + geom_point(size = 3, color = '#35978f') + theme(legend.title = element_blank()) +
  theme(legend.position = c(0.2, 0.8))

yearly_plot

ggsave('Opioids_CCs/Figures/Figure_X_Annual_Deaths_Non_Drug_Overdoses_F11.jpg')

########################### Figure: create-figure-on-#-of-deaths-by-year-F1*-as-UCOD ###########################

# Goal: # of deaths by year, F1* as UCOD -----------------------

temp <- all_deaths %>%
  filter(str_detect(string = ucod, pattern = '^F1') == TRUE) %>%
  group_by(year) %>%
  summarise(n_n = n()) %>%
  ungroup()

yearly_plot <- ggplot(data = temp,
                      aes(x = year, y = n_n)) +
  theme_classic() + labs(x = 'Year', y = '# of deaths with drug-related disorders ICD-10 codes, UCOD') +
  geom_line() + geom_point(size = 3, color = 'tomato3') + theme(legend.title = element_blank()) +
  theme(legend.position = c(0.2, 0.8))

yearly_plot

ggsave('Opioids_CCs/Figures/Figure_X_Annual_Deaths_Non_Drug_Overdoses_F1_UCOD.jpg')

########################### Figure: create-figure-on-#-of-deaths-by-year-F11-as-UCOD ###########################

# Goal: # of deaths by year, F11 as UCOD -----------------------

temp <- all_deaths %>%
  filter(str_detect(string = ucod, pattern = '^F11') == TRUE) %>%
  group_by(year) %>%
  summarise(n_n = n()) %>%
  ungroup()

yearly_plot <- ggplot(data = temp,
                      aes(x = year, y = n_n)) +
  theme_classic() + labs(x = 'Year', y = '# of deaths with opioid-related disorders ICD-10 codes, UCOD') +
  geom_line() + geom_point(size = 3, color = 'tomato1') + theme(legend.title = element_blank()) +
  theme(legend.position = c(0.2, 0.8))

yearly_plot

ggsave('Opioids_CCs/Figures/Figure_X_Annual_Deaths_Non_Drug_Overdoses_F11_UCOD.jpg')

########################### Figure: create-figure-on-#-of-deaths-by-year-F11-as-CC ###########################

# Goal: # of deaths by year, F11 as CC -----------------------

temp <- all_deaths %>%
  filter(str_detect(string = ucod, pattern = '^F11') == FALSE &
           F11_all == 1) %>%
  group_by(year) %>%
  summarise(n_n = n()) %>%
  ungroup()

yearly_plot <- ggplot(data = temp,
                      aes(x = year, y = n_n)) +
  theme_classic() + labs(x = 'Year', y = '# of deaths with opioid-related disorders ICD-10 codes, CC') +
  geom_line() + geom_point(size = 3, color = 'seagreen') + theme(legend.title = element_blank()) +
  theme(legend.position = c(0.2, 0.8))

yearly_plot

ggsave('Opioids_CCs/Figures/Figure_X_Annual_Deaths_Non_Drug_Overdoses_F11_CC.jpg')

########################### Figure: create-figure-on-most-common-UCODs-with-opioid-related-disorders-F11*-as-CC ###########################

# Goal: Most common UCODs with opioid-related disorders F11* as CC -------------------

temp <- all_deaths %>%
  filter(F11_all == 1 & str_detect(string = ucod, pattern = '^F1') == FALSE) %>%
  group_by(ucod) %>%
  summarise(n_n = n()) %>%
  ungroup()

# Top-25

temp %<>% arrange(-n_n) %>%
  filter(row_number() <= 25)

temp %<>% arrange(n_n) %>% mutate(ucod = factor(ucod, unique(ucod)))

fig_6_ucod_vector <- as.vector(temp$ucod)

# Plot

dot_plot <- ggplot(data = temp,
                   aes(x = ucod, y = n_n)) +
  geom_point(col="dodgerblue", size=3) +   # Draw points
  geom_segment(aes(x = ucod,
                   xend = ucod,
                   y = 0,
                   yend = max(n_n)),
               linetype="dashed",
               size=0.1) +   # Draw dashed lines
  labs(x = 'Underlying cause of death',
       y = '# of deaths with opioid-related disorder as CC') +
  coord_flip() + theme_classic()

dot_plot

ggsave('Opioids_CCs/Figures/Figure_X_Top_25_UCOD_F11.jpg')

########################### Figure: create-figure-on-most-common-UCODs-with-opioid-related-disorders-F1*-as-CC ###########################

# Goal: Most common UCODs with opioid-related disorders F1* as CC -------------------

temp <- all_deaths %>%
  filter(F1_all == 1 & str_detect(string = ucod, pattern = '^F1') == FALSE) %>%
  group_by(ucod) %>%
  summarise(n_n = n()) %>%
  ungroup()

# Top-25

temp %<>% arrange(-n_n) %>%
  filter(row_number() <= 25)

temp %<>% arrange(n_n) %>% mutate(ucod = factor(ucod, unique(ucod)))

fig_7_ucod_vector <- as.vector(temp$ucod)

# Plot

dot_plot <- ggplot(data = temp,
                   aes(x = ucod, y = n_n)) +
  geom_point(col="dodgerblue", size=3) +   # Draw points
  geom_segment(aes(x = ucod,
                   xend = ucod,
                   y = 0,
                   yend = max(n_n)),
               linetype="dashed",
               size=0.1) +   # Draw dashed lines
  labs(x = 'Underlying cause of death',
       y = '# of deaths with drug-related disorder as CC') +
  coord_flip() + theme_classic()

dot_plot

ggsave('Opioids_CCs/Figures/Figure_X_Top_25_UCOD_F1.jpg')

########################### Figure: create-figure-on-age-by-group ###########################

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

########################### Figure: create-figure-on-education-by-group ###########################

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

########################### Figure: create-figure-on-ethnicity-by-group ###########################

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

########################### Figure: create-figure-on-gender-by-group ###########################

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

########################### Figure: create-figure-on-hospital-by-group ###########################

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

########################### Figure: create-figure-on-number-of-contributing-causes-by-state-policy ###########################

# Import data ----------------------------

death_data <- data.frame()

for(fff in 2015:2019) {
  
  temp <- readRDS(paste0('//smdnas/Hill_Lab/Mortality/RDS/mort', as.character(fff), '.rds'))
  
  # Select relevant data
  
  temp %<>% select(id_var, county_fips_res, year, ucod, contains('record'),
                   drug_poisoning_death)
  
  # Bind rows to death data
  
  death_data %<>% bind_rows(temp)
  
  # Remove file and print
  
  print(fff)
  
  rm(fff, temp)
  
}

# Clean data -----------------------
# Notes: In this part of the script, we loop a couple functions that summarize the drug overdose and drug-related disorder
# data by race groups.

# Grab policy data 
# Note: Copied from list in Slack (provided by Alina Denham in Opioid_CCs channel)

mdi_data <- data.frame(state_name = state.name)

mdi_data %<>% mutate(policy = case_when(
  state_name %in% c('Maine', 'Alaska', 'Connecticut', 'Delaware',
                    'Maryland', 'Massachusetts', 'New Hampshire',
                    'New Mexico', 'North Carolina', 'Oklahoma',
                    'Oregon', 'Rhode Island', 'Utah', 'Vermont',
                    'Virginia', 'West Virginia', 'District of Columbia') ~ 'Centralized State Medical Examiner',
  state_name %in% c('Arizona', 'Florida', 'Iowa', 'Michigan', 'New Jersey',
                    'Tennessee') ~ 'Decentralized County/District-Based ME',
  state_name %in% c('Alabama', 'California', 'Georgia', 'Hawaii', 'Illinois',
                    'Minnesota', 'Mississippi', 'Missouri', 'New York', 'Ohio',
                    'Pennsylvania', 'Texas', 'Washington', 'Wisconsin') ~ 'Hybrid System',
  state_name %in% c('Arkansas', 'Colorado', 'Idaho', 'Indiana',
                    'Kansas', 'Kentucky', 'Louisiana', 'Montana',
                    'Nebraska', 'Nevada', 'North Dakota', 'South Carolina',
                    'South Dakota', 'Wyoming') ~ 'Decentralized County/District-Based'
))

# Get state name in data 

death_data %<>% mutate(state_fips_res = str_sub(county_fips_res, end = 2))

join_data <- state.fips

join_data$polyname %<>% str_to_title()

join_data %<>% mutate(state_fips_code = ifelse(fips < 10, paste0('0', as.character(fips)),
                                               as.character(fips)))

join_data %<>% dplyr::select(polyname, state_fips_code)

join_data$polyname %<>% str_replace_all(pattern = '\\:.*$', replacement = '')

join_data %<>% unique()

mdi_data %<>% left_join(join_data, by = c('state_name' = 'polyname'))

rm(join_data)

mdi_data %<>% mutate(state_fips_code = case_when(
  state_name == 'Alaska' ~ '02', 
  state_name == 'Hawaii' ~ '15',
  state_name != 'Alaska' & state_name != 'Hawaii' ~ state_fips_code
))

# Join MDI data with drug overdoses and drug-related deaths data

death_data %<>% left_join(mdi_data, by = c('state_fips_res' = 'state_fips_code'))

# Fill in DC data (not available in state.fips)

death_data %<>% mutate(policy = ifelse(state_fips_res == '11', 'Centralized State Medical Examiner',
                                       policy))

# Calculate number of contributing causes

death_data %>% dplyr::select(contains('record')) %>%
  mutate_at(vars(contains('record')),
            funs(ifelse(. == '', NA, .))) %>%
  mutate_at(vars(contains('record')),
            funs(ifelse(!is.na(.) == TRUE, 1, 0))) %>%
  dplyr::select(-record_1) %>%
  rowSums() -> death_data$number_of_record_causes

# Plot number of contributing causes --------------------------

violin_plot <- ggplot(data = death_data,
                      aes(policy, number_of_record_causes)) + 
  theme_classic() + geom_boxplot(varwidth = TRUE, fill = 'plum')

violin_plot

# Line graph over time ----------------------------

temp <- death_data %>% group_by(year, policy) %>%
  summarise(mean_number = mean(number_of_record_causes, na.rm = TRUE),
            median_number = median(number_of_record_causes, na.rm = TRUE),
            sd_number = sd(number_of_record_causes, na.rm = TRUE)) %>%
  ungroup()

line_plot <- ggplot(data = temp, aes(x = year, y = mean_number, colour = policy, group = policy)) + 
  theme_classic() + labs(x = 'Year', y = 'Mean number of contributing causes', 
                         title = '# of contributing causes in death record by state MDI policy',
                         subtitle = '2015-2019') + 
  geom_line() + geom_point() + 
  theme(legend.title = element_blank())

line_plot

ggsave('Opioids_CCs/Figures/Figure_X_Number_of_Contributing_Causes.jpg')


# Line graph over time ----------------------------

temp <- death_data %>% filter(drug_poisoning_death == 1) %>%
  group_by(year, policy) %>%
  summarise(mean_number = mean(number_of_record_causes, na.rm = TRUE),
            median_number = median(number_of_record_causes, na.rm = TRUE),
            sd_number = sd(number_of_record_causes, na.rm = TRUE)) %>%
  ungroup()

line_plot <- ggplot(data = temp, aes(x = year, y = mean_number, colour = policy, group = policy)) + 
  theme_classic() + labs(x = 'Year', y = 'Mean number of contributing causes', 
                         title = '# of contributing causes in death record by state MDI policy',
                         subtitle = '2015-2019') + 
  geom_line() + geom_point() + 
  theme(legend.title = element_blank())

line_plot

ggsave('Opioids_CCs/Figures/Figure_X_Number_of_Contributing_Causes_Drugs.jpg')




g <- ggplot(mpg, aes(class, cty))
g + geom_violin() + 
  labs(title="Violin plot", 
       subtitle="City Mileage vs Class of vehicle",
       caption="Source: mpg",
       x="Class of Vehicle",
       y="City Mileage")

########################### Figure: create-figure-on-race-by-group ###########################

# Import data ----------------------------

overdose_deaths <- readRDS("Opioids_CCs/Scratch/Overdoses.rds") # overdose
drug_deaths <- readRDS("Opioids_CCs/Scratch/Drug_related_Deaths.rds") # drug-related

# Clean data -----------------------
# Notes: In this part of the script, we loop a couple functions that summarize the drug overdose and drug-related disorder
# data by race groups.

race_data <- data.frame()

for(fff in c('drug_deaths', 'overdose_deaths')) {
  
  # Get data 
  
  temp <- get(fff)
  
  temp %>% 
    group_by(year) %>% 
    summarise_at(vars(white), funs(mean(., na.rm = TRUE))) %>% 
    ungroup() -> temp_summary  
  
  # Add male
  
  temp_summary %<>% mutate(non_white = 1 - white, 
                           category = ifelse(str_detect(fff, pattern = 'overdose') == TRUE, 
                                             'Drug Overdoses', 'Drug-Related Deaths'))
  
  # Bind to race data
  
  race_data %<>% bind_rows(temp_summary)
  
  rm(temp_summary)
  
}

race_data %<>% gather(variable, value, -year, -category)

race_data$variable %<>% str_to_title() %>%
  str_replace_all(pattern = 'Non_white', replacement = 'Non-White')

race_plot <- ggplot(data = race_data,
                    aes(x = year, y = value, linetype = variable, colour = category,
                        group = interaction(variable, category))) + 
  geom_line() +
  theme_classic() + 
  labs(x = 'Year', y = '% of deaths') + 
  scale_colour_manual(values=c('#8c510a', '#01665e')) +
  theme(legend.title=element_blank()) +
  theme(legend.position = c(0.8, 0.5)) + 
  ylim(0, 1) + xlim(1998, 2020)

race_plot

ggsave('Opioids_CCs/Figures/Figure_X_Race.jpg')

########################### Figure: create-figure-on-state-policy-by-group ###########################

# Import data ----------------------------

overdose_deaths <- readRDS("Opioids_CCs/Scratch/Overdoses.rds") # overdose
drug_deaths <- readRDS("Opioids_CCs/Scratch/Drug_related_Deaths.rds") # drug-related

# Clean data -----------------------
# Notes: In this part of the script, we loop a couple functions that summarize the drug overdose and drug-related disorder
# data by race groups.

# Grab policy data 
# Note: Copied from list in Slack (provided by Alina Denham in Opioid_CCs channel)

mdi_data <- data.frame(state_name = state.name)

mdi_data %<>% mutate(policy = case_when(
  state_name %in% c('Maine', 'Alaska', 'Connecticut', 'Delaware',
                    'Maryland', 'Massachusetts', 'New Hampshire',
                    'New Mexico', 'North Carolina', 'Oklahoma',
                    'Oregon', 'Rhode Island', 'Utah', 'Vermont',
                    'Virginia', 'West Virginia', 'District of Columbia') ~ 'Centralized State Medical Examiner',
  state_name %in% c('Arizona', 'Florida', 'Iowa', 'Michigan', 'New Jersey',
                    'Tennessee') ~ 'Decentralized County/District-Based ME',
  state_name %in% c('Alabama', 'California', 'Georgia', 'Hawaii', 'Illinois',
                    'Minnesota', 'Mississippi', 'Missouri', 'New York', 'Ohio',
                    'Pennsylvania', 'Texas', 'Washington', 'Wisconsin') ~ 'Hybrid System',
  state_name %in% c('Arkansas', 'Colorado', 'Idaho', 'Indiana',
                    'Kansas', 'Kentucky', 'Louisiana', 'Montana',
                    'Nebraska', 'Nevada', 'North Dakota', 'South Carolina',
                    'South Dakota', 'Wyoming') ~ 'Decentralized County/District-Based'
))

# Get state name in data 

drug_deaths %<>% mutate(state_fips_res = str_sub(county_fips_res, end = 2))

join_data <- state.fips

join_data$polyname %<>% str_to_title()

join_data %<>% mutate(state_fips_code = ifelse(fips < 10, paste0('0', as.character(fips)),
                                               as.character(fips)))

join_data %<>% dplyr::select(polyname, state_fips_code)

join_data$polyname %<>% str_replace_all(pattern = '\\:.*$', replacement = '')

join_data %<>% unique()

mdi_data %<>% left_join(join_data, by = c('state_name' = 'polyname'))

rm(join_data)

mdi_data %<>% mutate(state_fips_code = case_when(
  state_name == 'Alaska' ~ '02', 
  state_name == 'Hawaii' ~ '15',
  state_name != 'Alaska' & state_name != 'Hawaii' ~ state_fips_code
))

# Join MDI data with drug overdoses and drug-related deaths data

overdose_deaths %<>% left_join(mdi_data, by = c('state_fips_res' = 'state_fips_code'))
drug_deaths %<>% left_join(mdi_data, by = c('state_fips_res' = 'state_fips_code'))

# Fill in DC data (not available in state.fips)

overdose_deaths %<>% mutate(policy = ifelse(state_fips_res == '11', 'Centralized State Medical Examiner',
                                            policy))

drug_deaths %<>% mutate(policy = ifelse(state_fips_res == '11', 'Centralized State Medical Examiner',
                                        policy))

# Loop it

policy_data <- data.frame()

for(fff in c('drug_deaths', 'overdose_deaths')) {
  
  # Get data 
  
  temp <- get(fff)
  
  # Get policy data and widen 
  
  temp %<>% dplyr::select(year, policy) %>%
    mutate(value = 1) %>% 
    dcast(year ~ policy, value.var = c('value'))
  
  temp$total_deaths <- temp %>% dplyr::select(-year) %>%
    rowSums()
  
  temp %<>% mutate_at(vars(`Centralized State Medical Examiner`:`Hybrid System`),
                      funs(. / total_deaths))
  
  temp %<>% dplyr::select(-total_deaths)
  
  temp %<>% gather(variable, value, -year)
  
  # Add category
  
  temp %<>% mutate(category = ifelse(str_detect(fff, pattern = 'overdose') == TRUE, 
                                     'Drug Overdoses', 'Drug-Related Deaths'))
  
  # Bind to policy data
  
  policy_data %<>% bind_rows(temp)
  
  rm(temp)
  
}


policy_plot <- ggplot(data = policy_data,
                      aes(x = year, y = value, linetype = variable, colour = category,
                          group = interaction(variable, category))) + 
  geom_line() +
  theme_classic() + 
  labs(x = 'Year', y = '% of deaths') + 
  scale_colour_manual(values=c('#a50026', '#313695')) +
  theme(legend.title=element_blank())

policy_plot

ggsave('Opioids_CCs/Figures/Figure_X_Policy.jpg')

########################### Figure: plot_frequency_of_common_UCODs_where_CC_F1 ###########################

# Goal: What are the top-15 most common underlying causes of death with non-drug overdose deaths with a F1* contributing, but not underlying, cause of death? 2015-2019 for this one + one for the entire time period

# Load data

all_deaths <- readRDS("Opioids_CCs/Scratch/Deaths_with_F1_code.rds")


# Clean data

# NA to 0 for all record causes
all_deaths %<>% mutate_at(vars(F110:F154, F135:F116),
                          funs(ifelse(is.na(.) == TRUE, 0, .)))

# Create a variable indicating incidence of any F1 related to opioids
all_deaths %>% select(starts_with('F1')) %>%
  rowSums() -> all_deaths$F1_all
all_deaths %<>% mutate(F1_all = ifelse(F1_all > 0, 1, 0))

# Group by UCOD and filter in F1* CC and filter out F1* UCOD
temp <- all_deaths %>%
  filter(F1_all == 1 & str_detect(string = ucod, pattern = '^F1') == FALSE) %>%
  group_by(ucod) %>%
  summarise(n_n = n()) %>%
  ungroup()

# Filter in top 15 rows by count
temp %<>% arrange(-n_n) %>%
  filter(row_number() <= 15)

temp %<>% arrange(n_n) %>% mutate(ucod = factor(ucod, unique(ucod)))



# Plot data

png('Opioids_CCs/Figures/Figure_(4)_all.png',width=600,height=600)

dot_plot <- ggplot(data = temp,
                   aes(x = ucod, y = n_n)) +
  geom_point(col="dodgerblue", size=3) +   # Draw points
  geom_segment(aes(x = ucod,
                   xend = ucod,
                   y = 0,
                   yend = max(n_n)),
               linetype="dashed",
               size=0.1) +   # Draw dashed lines
  labs(x = 'Underlying Cause of Death',
       y = '# of Deaths with Drug-Related Disorder as CC (F1*)') +
  coord_flip() + 
  scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
                      #breaks=c("rate"),
                      labels=c("F1* CC")) +
  theme_classic() + 
  theme(legend.justification=c(0,1), legend.position=c(0,1), legend.title=element_blank(), 
        axis.text.x = element_text(size = 20), 
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

dot_plot

dev.off()






# Load data

all_deaths <- readRDS("Opioids_CCs/Scratch/Deaths_with_F1_code.rds")


# Clean data

# Filter years
all_deaths <- subset(all_deaths, year >= 2015 & year <= 2019)

# NA to 0 for all record causes
all_deaths %<>% mutate_at(vars(F110:F154, F135:F116),
                          funs(ifelse(is.na(.) == TRUE, 0, .)))

# Create a variable indicating incidence of any F1 related to opioids
all_deaths %>% select(starts_with('F1')) %>%
  rowSums() -> all_deaths$F1_all
all_deaths %<>% mutate(F1_all = ifelse(F1_all > 0, 1, 0))

# Group by UCOD and filter in F1* CC and filter out F1* UCOD
temp <- all_deaths %>%
  filter(F1_all == 1 & str_detect(string = ucod, pattern = '^F1') == FALSE) %>%
  group_by(ucod) %>%
  summarise(n_n = n()) %>%
  ungroup()

# Filter in top 15 rows by count
temp %<>% arrange(-n_n) %>%
  filter(row_number() <= 15)

temp %<>% arrange(n_n) %>% mutate(ucod = factor(ucod, unique(ucod)))


# Plot data

png('Opioids_CCs/Figures/Figure_(4)_2014_2019.png',width=600,height=600)

dot_plot <- ggplot(data = temp,
                   aes(x = ucod, y = n_n)) +
  geom_point(col="dodgerblue", size=3) +   # Draw points
  geom_segment(aes(x = ucod,
                   xend = ucod,
                   y = 0,
                   yend = max(n_n)),
               linetype="dashed",
               size=0.1) +   # Draw dashed lines
  labs(x = 'Underlying Cause of Death',
       y = '# of Deaths with Drug-Related Disorder as CC (F1*)') +
  coord_flip() + 
  scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
                      #breaks=c("rate"),
                      labels=c("F1* CC")) +
  theme_classic() + 
  theme(legend.justification=c(0,1), legend.position=c(0,1), legend.title=element_blank(), 
        axis.text.x = element_text(size = 20), 
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

dot_plot

dev.off()

########################### Figure: plot_frequency_of_common_CCs_where_UCOD_F1 ###########################


# Goal: What are the top-15 most common contributing causes of death with non-drug overdose deaths with a F1* underlying cause of death? Same as above for timelines.

# Load data

all_deaths <- readRDS("Opioids_CCs/Scratch/Deaths_with_F1_code.rds")


# Clean data

# NA to 0 for all record causes

all_deaths %<>% mutate_at(vars(F110:F154, F135:F116),
                          funs(ifelse(is.na(.) == TRUE, 0, .)))

# Create a variable indicating incidence of any F1 and F11 code related to opioids

all_deaths %>% select(starts_with('F11')) %>%
  rowSums() -> all_deaths$F11_all

all_deaths %<>% mutate(F11_all = ifelse(F11_all > 0, 1, 0))

all_deaths %>% select(starts_with('F1')) %>%
  rowSums() -> all_deaths$F1_all

all_deaths %<>% mutate(F1_all = ifelse(F1_all > 0, 1, 0))



all_deaths %>% select(record_2:record_20) %>% t %>% c %>% unique 


# record_2:record_20 unique values -> dummy variables
# sum dummy variable columns and arrange by size




temp <- all_deaths %>%
  filter(F1_all == 1 & str_detect(string = ucod, pattern = '^F1') == TRUE) %>%
  group_by(ucod) %>%
  summarise(n_n = n()) %>%
  ungroup()

temp %<>% arrange(-n_n) %>%
  filter(row_number() <= 25)

temp %<>% arrange(n_n) %>% mutate(ucod = factor(ucod, unique(ucod)))


# Plot data

png('Opioids_CCs/Figures/Figure_(5)_Frequency_of_Common_CCs_F1_UCOD_from_2015-2019.png',width=600,height=600)

dot_plot <- ggplot(data = temp,
                   aes(x = ucod, y = n_n)) +
  geom_point(col="dodgerblue", size=3) +   # Draw points
  geom_segment(aes(x = ucod,
                   xend = ucod,
                   y = 0,
                   yend = max(n_n)),
               linetype="dashed",
               size=0.1) +   # Draw dashed lines
  labs(x = 'Contributing Cause of Death',
       y = '# of Deaths with Drug-Related Disorder as UCOD (F1*)') +
  coord_flip() + 
  scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
                      #breaks=c("rate"),
                      labels=c("F1* UCOD")) +
  theme_classic() + 
  theme(legend.justification=c(0,1), legend.position=c(0,1), legend.title=element_blank(), 
        axis.text.x = element_text(size = 20), 
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

dot_plot

dev.off()

########################### Figure: plot_age_unadjusted_mortality_rates_two_dots_by_state ###########################

# Goal: State-by-state dot-plots for 2019 with mortality rates (unadjusted, for now) per 100,000 people for 
# (a) drug overdoses; and 
# (b) drug-related disorders? I think we want two versions of this because I'm not sure which is best: one version where (b) is measured with only those deaths with F1* underlying causes of death; and another version where (b) includes those with F1* contributing causes of death.

all_deaths <- data.frame()

for (dataset in c('Drug-Related Disorders','Drug Overdoses','Drug-Related Disorders UCOD','Drug-Related Disorders UCOD and CC')){
  
  # Load data
  if (dataset == "Drug-Related Disorders") {
    deaths <- readRDS("Opioids_CCs/Scratch/Deaths_with_F1_code.rds")
  }
  else if (dataset == "Drug-Related Disorders UCOD") {
    deaths <- readRDS("Opioids_CCs/Scratch/Deaths_with_F1_code.rds")
  }
  else if (dataset == "Drug-Related Disorders UCOD and CC") {
    deaths <- readRDS("Opioids_CCs/Scratch/Deaths_with_F1_code.rds")
  }
  else if (dataset == "Drug Overdoses") {
    deaths <- readRDS("Opioids_CCs/Scratch/Overdoses.rds")
  }
  state_to_fips <- read_excel("Opioids_CCs/Data/USCB_State_FIPS_Codes_2019.xlsx", col_names = TRUE)
  us_pop <- read_fwf('Opioids_CCs/Data/us.1969_2019.19ages.adjusted.txt',
                     col_positions = fwf_widths(
                       c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8),
                       c('year', 'state', 'state_fips', 'county_fips',
                         'registry', 'race', 'origin', 'sex', 'age', 
                         'population')))
  
  # Clean data
  
  # Clean deaths
  
  # Filter out F1 CCs for drug overdoses and F11 CCs for opioid overdoses
  if (dataset == "Drug-Related Disorders") {
    deaths %<>% mutate_at(vars(F110:F154, F135:F116),
                          funs(ifelse(is.na(.) == TRUE, 0, .)))
    
    deaths %>% select(starts_with('F1')) %>%
      rowSums() -> deaths$F1_all
    deaths %<>% mutate(F1_all = ifelse(F1_all > 0, 1, 0))
    deaths <- deaths %>% filter(F1_all == 1)
  }
  
  else if (dataset == "Drug-Related Disorders UCOD") {
    deaths %<>% mutate_at(vars(F110:F154, F135:F116),
                          funs(ifelse(is.na(.) == TRUE, 0, .)))
    
    deaths <- deaths %>% filter(str_detect(string = ucod, pattern = '^F1') == TRUE)
  }  
  
  else if (dataset == "Drug-Related Disorders UCOD and CC") {
    deaths %<>% mutate_at(vars(F110:F154, F135:F116),
                          funs(ifelse(is.na(.) == TRUE, 0, .)))
    
    deaths %>% select(starts_with('F1')) %>%
      rowSums() -> deaths$F1_all
    deaths %<>% mutate(F1_all = ifelse(F1_all > 0, 1, 0))
    
    deaths <- deaths %>% filter(F1_all == 1 & str_detect(string = ucod, pattern = '^F1') == TRUE)
  }   
  
  else if (dataset == "Drug Overdoses") {
    print("pass")
  } 
  
  
  # Create state column
  deaths$state <- NA
  deaths$state <- substr(deaths$county_fips_occ, 0, 2)
  temp <- deaths
  # Select relevant variables in drug overdoses database
  temp %<>% select(year, state)
  # Summarise drug overdoses by year and state
  temp_summary <- temp %>% 
    group_by(year, state) %>%
    summarise(n = n())
  # Resummarise using new definition 
  temp_summary %<>%
    group_by(year, state) %>%
    summarise_at(vars(contains('n')),
                 funs(sum(., na.rm = TRUE))) %>%
    ungroup()
  
  
  # Clean state_to_fips
  # Pad 0s on FIPS column
  state_to_fips$`FIPS State Numeric Code` <- str_pad(state_to_fips$`FIPS State Numeric Code`, 2, side="left",0)
  
  # Clean us_pop
  # Make population numeric
  us_pop %<>% mutate_at(vars(population),
                        funs(as.numeric(.)))
  # Summarise US population by year and state
  pop_summary <- us_pop %>% 
    group_by(year, state_fips) %>%
    summarise_at(vars(population),
                 funs(sum(., na.rm = TRUE))) %>%
    ungroup()
  
  
  # Merge data
  # Merge population data to drug overdoses data
  temp_summary <- 
    pop_summary %<>% left_join(temp_summary, by = c('year', 
                                                    'state_fips' = 'state'))
  
  # Clean data
  # Clean merged
  # Rearrange data
  temp_summary %<>% arrange(state_fips, year)
  # Add total population data for each year
  temp_summary %<>% group_by(year, state_fips) %>%
    mutate(population_total = sum(population)) %>%
    ungroup()
  # Add group-specific rates
  temp_summary %<>% mutate_at(
    vars(n),
    funs(rate = (. / population_total) * 100000)
  )
  # Add age-specific weights and weighted rates
  #temp_summary %<>% mutate(
  #  weight = population / population_total
  #) %>%
  #  mutate_at(
  #    vars(n),
  #    funs(weighted = . * weight)
  #  )
  # Generate opioid overdose rates across groups
  temp_summary %>% group_by(year, state_fips) %>%
    summarise_at(vars(contains('rate')),
                 funs(sum(., na.rm = TRUE))) %>%
    ungroup() -> state_rates
  state_rates <- subset(state_rates, 1999 <= year & year <= 2019)
  state_rates <- merge(state_rates, state_to_fips, by.x='state_fips', by.y='FIPS State Numeric Code')
  state_rates <- state_rates[order(state_rates$rate),]
  #state_rates$`Official USPS Code` = factor(state_rates$`Official USPS Code`,levels=state_rates$`Official USPS Code`)
  print("test")
  state_rates$type <- dataset
  
  
  
  # Bind data
  
  all_deaths %<>% bind_rows(state_rates)
  
  print(dataset)
  
}



# Clean data

# all_deaths
all_deaths <- all_deaths[order(all_deaths$rate),]

disorder_deaths <- subset(all_deaths, type=='Drug-Related Disorders')
overdose_deaths <- subset(all_deaths, type=='Drug Overdoses')

#all_deaths$`Official USPS Code` = factor(all_deaths$`Official USPS Code`,levels=all_deaths$`Official USPS Code`)

# Graph data
# Dot plot

png('Opioids_CCs/Figures/Figure_(6a)_disorders.png',width=600,height=600)

dot_plot <- ggplot(data = all_deaths,
                   aes(x = `Official USPS Code`, y = rate, colour=type)) +
  geom_point() +   # Draw points
  geom_segment(aes(x = `Official USPS Code`,
                   xend = `Official USPS Code`,
                   y = 0,
                   yend = max(rate)),
               linetype="dashed",
               size=0.1) +   # Draw dashed lines
  labs(x = 'State',
       y = 'Unadjusted Mortality Rates') +
  scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#000000")) +
  coord_flip() +
  theme_classic() +
  theme(plot.margin=unit(c(1,4,1,1),"cm")) +
  theme(legend.justification=c(.15,.15), legend.position=c(1,0.15)) + theme(legend.title=element_blank())

dot_plot

dev.off()


# Save data

# all_deaths
all_deaths_UCOD <- subset(all_deaths, type=='Drug-Related Disorders UCOD')
all_deaths_UCOD <- all_deaths_UCOD[order(all_deaths_UCOD$rate),]
#all_deaths_UCOD <- tail(all_deaths_UCOD,n=20)
#all_deaths_UCOD$state_county_name = factor(all_deaths_UCOD$state_county_name,levels=all_deaths_UCOD$state_county_name)
write.csv(all_deaths_UCOD,'/Users/Jordan/Box/Opioids_CCs/Scratch/all_deaths_UCOD_state.csv')

# all_deaths
all_deaths_UCOD_CC <- subset(all_deaths, type=='Drug-Related Disorders UCOD and CC')
all_deaths_UCOD_CC <- all_deaths_UCOD_CC[order(all_deaths_UCOD_CC$rate),]
#all_deaths_UCOD_CC <- tail(all_deaths_UCOD_CC,n=20)
#all_deaths_UCOD_CC$state_county_name = factor(all_deaths_UCOD_CC$state_county_name,levels=all_deaths_UCOD_CC$state_county_name)
write.csv(all_deaths_UCOD_CC,'/Users/Jordan/Box/Opioids_CCs/Scratch/all_deaths_UCOD_CC_state.csv')

# all_deaths
overdose_deaths <- subset(all_deaths, type=='Drug Overdoses')
overdose_deaths <- overdose_deaths[order(overdose_deaths$rate),]
#overdose_deaths <- tail(overdose_deaths,n=20)
#overdose_deaths$state_county_name = factor(overdose_deaths$state_county_name,levels=overdose_deaths$state_county_name)
write.csv(overdose_deaths,'/Users/Jordan/Box/Opioids_CCs/Scratch/all_deaths_overdose_state.csv')

########################### Figure: plot_age_unadjusted_mortality_rates_two_dots_by_county ###########################

# Goal: What are the top-15 or 20 counties in the U.S. in terms of drug-related disorder deaths for 2019. Two versions of this figure would suffice, for review: 
# (a) one where drug-related disorders are only based on those with underlying causes of death with an F1*; and 
# (b) one which includes those + contributing causes of death with an F1*.

all_deaths <- data.frame()

for (dataset in c('Drug-Related Disorders UCOD','Drug-Related Disorders UCOD and CC')){
  
  # Load data
  deaths <- readRDS("Opioids_CCs/Scratch/Deaths_with_F1_code.rds")
  state_to_fips <- read_excel("Opioids_CCs/Data/USCB_State_FIPS_Codes_2019.xlsx", col_names = TRUE)
  county_state_fips <- read_xlsx('Opioids_CCs/Data/county_fips_code.xlsx')
  us_pop <- read_fwf('Opioids_CCs/Data/us.1969_2019.19ages.adjusted.txt',
                     col_positions = fwf_widths(
                       c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8),
                       c('year', 'state', 'state_fips', 'county_fips',
                         'registry', 'race', 'origin', 'sex', 'age', 
                         'population')))
  
  # Clean data
  # Clean deaths
  # Filter out F1 CCs for drug overdoses and F11 CCs for opioid overdoses
  if (dataset == "Drug-Related Disorders UCOD") {
    deaths %<>% mutate_at(vars(F110:F154, F135:F116),
                          funs(ifelse(is.na(.) == TRUE, 0, .)))
    
    deaths <- deaths %>% filter(str_detect(string = ucod, pattern = '^F1') == TRUE)
  }  
  
  else if (dataset == "Drug-Related Disorders UCOD and CC") {
    deaths %<>% mutate_at(vars(F110:F154, F135:F116),
                          funs(ifelse(is.na(.) == TRUE, 0, .)))
    
    deaths %>% select(starts_with('F1')) %>%
      rowSums() -> deaths$F1_all
    deaths %<>% mutate(F1_all = ifelse(F1_all > 0, 1, 0))
    
    deaths <- deaths %>% filter(F1_all == 1 & str_detect(string = ucod, pattern = '^F1') == TRUE)
  } 
  
  # Create state column
  deaths$state <- NA
  deaths$state <- substr(deaths$county_fips_occ, 0, 2)
  temp <- deaths
  # Select relevant variables in drug overdoses database
  temp %<>% select(year, county_fips_occ)
  # Summarise drug overdoses by year and state
  temp_summary <- temp %>% 
    group_by(year, county_fips_occ) %>%
    summarise(n = n())
  # Resummarise using new definition 
  temp_summary %<>%
    group_by(year, county_fips_occ) %>%
    summarise_at(vars(contains('n')),
                 funs(sum(., na.rm = TRUE))) %>%
    ungroup()
  
  # Clean state_to_fips
  # Pad 0s on FIPS column
  state_to_fips$`FIPS State Numeric Code` <- str_pad(state_to_fips$`FIPS State Numeric Code`, 2, side="left", 0)
  
  # Clean county_fips
  county_state_fips$state_fips <- str_pad(county_state_fips$state_fips, 2, side="left", 0)
  county_state_fips$county_fips <- str_pad(county_state_fips$county_fips, 3, side="left", 0)
  county_state_fips$state_county <- paste0(county_state_fips$state_fips,county_state_fips$county_fips)
  
  # Clean us_pop
  # Create state_county column
  us_pop$state_county <- NA
  us_pop$state_county <- paste0(us_pop$state_fips,us_pop$county_fips)
  # Make population numeric
  us_pop %<>% mutate_at(vars(population),
                        funs(as.numeric(.)))
  # Summarise US population by year and state
  pop_summary <- us_pop %>% 
    group_by(year, state_county) %>%
    summarise_at(vars(population),
                 funs(sum(., na.rm = TRUE))) %>%
    ungroup()
  
  
  # Merge data
  # Merge population data to drug overdoses data
  temp_summary <- 
    pop_summary %<>% left_join(temp_summary, by = c('year', 
                                                    'state_county' = 'county_fips_occ'))
  
  # Clean data
  # Clean merged
  # Rearrange data
  temp_summary <- subset(temp_summary, population > 250000)
  temp_summary %<>% arrange(state_county, year)
  # Add total population data for each year
  temp_summary %<>% group_by(year, state_county) %>%
    mutate(population_total = sum(population)) %>%
    ungroup()
  # Add group-specific rates
  temp_summary %<>% mutate_at(
    vars(n),
    funs(rate = (. / population_total) * 100000)
  )
  # Add age-specific weights and weighted rates
  #temp_summary %<>% mutate(
  #  weight = population / population_total
  #) %>%
  #  mutate_at(
  #    vars(n),
  #    funs(weighted = . * weight)
  #  )
  # Generate opioid overdose rates across groups
  temp_summary %>% group_by(year, state_county) %>%
    summarise_at(vars(contains('rate')),
                 funs(sum(., na.rm = TRUE))) %>%
    ungroup() -> county_rates
  county_rates <- subset(county_rates, year==2019)
  
  merged_county_rates <- merge(county_rates, county_state_fips, by.x='state_county', by.y='state_county')
  merged_county_rates$state_county_name <- paste0(merged_county_rates$full_name,', ',merged_county_rates$state)
  merged_county_rates <- merged_county_rates[order(merged_county_rates$rate),]
  merged_county_rates <- tail(merged_county_rates,n=20)
  merged_county_rates$state_county_name = factor(merged_county_rates$state_county_name,levels=merged_county_rates$state_county_name)
  merged_county_rates$type <- dataset
  
  
  
  # Bind data
  
  all_deaths %<>% bind_rows(merged_county_rates)
  
  print(dataset)
  
}



# Clean data

# all_deaths
all_deaths_UCOD <- subset(all_deaths, type=='Drug-Related Disorders UCOD')
all_deaths_UCOD <- all_deaths_UCOD[order(all_deaths_UCOD$rate),]
all_deaths_UCOD <- tail(all_deaths_UCOD,n=20)
all_deaths_UCOD$state_county_name = factor(all_deaths_UCOD$state_county_name,levels=all_deaths_UCOD$state_county_name)
write.csv(all_deaths_UCOD,'/Users/Jordan/Box/Opioids_CCs/Scratch/all_deaths_UCOD.csv')


# Graph data

# Dot plot

png('Opioids_CCs/Figures/Figure_(8)_UCOD.png',width=600,height=600)

dot_plot <- ggplot(data = all_deaths_UCOD,
                   aes(x = state_county_name, y = rate, colour = type)) +
  geom_point() +   # Draw points
  geom_segment(aes(x = state_county_name,
                   xend = state_county_name,
                   y = 0,
                   yend = max(rate)),
               linetype="dashed",
               size=0.1) +   # Draw dashed lines
  labs(x = 'County',
       y = 'Unadjusted Mortality Rates') +
  scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#000000")) +
  coord_flip() +
  theme_classic() + 
  theme(legend.justification=c(1,1), legend.position=c(1,0.15)) + theme(legend.title=element_blank())

dot_plot

dev.off()





# Clean data

# all_deaths
all_deaths_UCOD_CC <- subset(all_deaths, type=='Drug-Related Disorders UCOD and CC')
all_deaths_UCOD_CC <- all_deaths_UCOD_CC[order(all_deaths_UCOD_CC$rate),]
all_deaths_UCOD_CC <- tail(all_deaths_UCOD_CC,n=20)
all_deaths_UCOD_CC$state_county_name = factor(all_deaths_UCOD_CC$state_county_name,levels=all_deaths_UCOD_CC$state_county_name)
write.csv(all_deaths_UCOD_CC,'/Users/Jordan/Box/Opioids_CCs/Scratch/all_deaths_UCOD_CC.csv')


# Graph data

# Dot plot

png('Opioids_CCs/Figures/Figure_(8)_UCOD_and_CC.png',width=600,height=600)

dot_plot <- ggplot(data = all_deaths_UCOD_CC,
                   aes(x = state_county_name, y = rate, colour = type)) +
  geom_point() +   # Draw points
  geom_segment(aes(x = state_county_name,
                   xend = state_county_name,
                   y = 0,
                   yend = max(rate)),
               linetype="dashed",
               size=0.1) +   # Draw dashed lines
  labs(x = 'County',
       y = 'Unadjusted Mortality Rates') +
  scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#000000")) +
  coord_flip() +
  theme_classic() + 
  theme(legend.justification=c(1,1), legend.position=c(1,0.15)) + theme(legend.title=element_blank())

dot_plot

dev.off()

########################### Figure: plot_disorders_and_overdoses_demograhic_trends ###########################

# Goal: Understand how age; sex; race; and ethnicity of decedents from both types of deaths vary from 1999-2019. We can imagine a separate plot for each variable, perhaps with some additional variations for each variable. For example, one could imagine a figure where average/median age of decedent from drug overdoses vs. drug-related disorders (focus on those with underlying cause of death as F1*, for now) varies across time. Another slightly different version would be a binned approach, where we identify the #/proportion of deaths in various age bins (your current figure in Box).

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

########################### Figure: plot_disorders_and_overdoses_sex_trends ###########################

# Goal: I want to understand how age; sex; race; and ethnicity of decedents from both types of deaths vary from 1999-2019. We can imagine a separate plot for each variable, perhaps with some additional variations for each variable. For example, one could imagine a figure where average/median age of decedent from drug overdoses vs. drug-related disorders (focus on those with underlying cause of death as F1*, for now) varies across time. Another slightly different version would be a binned approach, where we identify the #/proportion of deaths in various age bins (your current figure in Box).

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

########################### Figure: plot_mortality_median_age_for_F1 ###########################

# Goal: Variation in age, sex, race, and ethnicity of decedents with F1* UCOD or at least 1 F1* contributing cause from 1999-2019.

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

########################### Figure: plot_disorders_and_overdoses_trends ###########################

# Goal: Trends of drug overdoses and drug-related disorders from 1999-2019
# X: Year
# Y: 
# drug overdose
# drug-related disorder
# drug-related disorder with CC
# drug-related disorder with UCOD

# Import data

overdose_deaths <- readRDS("Opioids_CCs/Scratch/Overdoses.rds") # overdose
drug_deaths <- readRDS("Opioids_CCs/Scratch/Drug_related_Deaths.rds") # drug-related



# Clean data

overdose_deaths %<>% mutate_at(vars(F101:O961),
                               funs(ifelse(is.na(.) == TRUE, 0, .)))

# drug overdose
overdose_deaths <- overdose_deaths

# Group by year
temp_overdose <- overdose_deaths %>%
  group_by(year) %>%
  summarise(n_n = n()) %>%
  ungroup()
temp_overdose$ICD_code <- 'drug overdoses'


# drug-related disorder
drug_deaths_new <- drug_deaths

# Group by year
temp_drug <- drug_deaths_new %>%
  group_by(year) %>%
  summarise(n_n = n()) %>%
  ungroup()
temp_drug$ICD_code <- 'drug-related disorders'


# drug-related disorder with F1* UCOD and without F1* CC
drug_deaths_UCOD <- drug_deaths

# Group by year and filter out F1* CC and filter in F1* UCOD
temp_drug_UCOD <- drug_deaths_UCOD %>%
  filter(str_detect(string = ucod, pattern = '^F1') == TRUE) %>%
  group_by(year) %>%
  summarise(n_n = n()) %>%
  ungroup()
temp_drug_UCOD$ICD_code <- 'drug-related disorders UCOD'


# drug-related disorder with F1* CC and without F1* UCOD
drug_deaths_CC <- drug_deaths

drug_deaths_CC %<>% mutate_at(vars(F111:F154, F135:F116),
                              funs(ifelse(is.na(.) == TRUE, 0, .)))

# Create variables indicating incidence of any related drug codes
drug_deaths_CC %>% select(starts_with('F1')) %>%
  rowSums() -> drug_deaths_CC$F1_all
drug_deaths_CC %<>% mutate(F1_all = ifelse(F1_all > 0, 1, 0))

# Group by year and filter in F1* CC and filter out F1* UCOD
temp_drug_CC <- drug_deaths_CC %>%
  filter(F1_all == 1) %>%
  group_by(year) %>%
  summarise(n_n = n()) %>%
  ungroup()
temp_drug_CC$ICD_code <- 'drug-related disorders CC'



# Bind data

bind <- bind_rows(temp_overdose,temp_drug,temp_drug_UCOD,temp_drug_CC)



# Plot data

png('Opioids_CCs/Figures/Figure_(9)_Trends_of_Drug_Overdoses_and_Disorders_from_1999-2019.png',width=1000,height=800)

yearly_plot <- ggplot(data = bind,
                      aes(x = year, y = n_n, colour=ICD_code)) +
  theme_classic() + labs(x = 'Year', y = '# of Deaths') +
  geom_line() + geom_point(size = 3) + 
  scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
                      breaks=c("drug overdoses", "drug-related disorders", "drug-related disorders UCOD", "drug-related disorders CC"),
                      labels=c("Drug Overdoses", "Drug-related Disorders", "Drug-related Disorders, UCOD", "Drug-related Disorders, CC")) +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) + theme(legend.title=element_blank())

yearly_plot

dev.off()
