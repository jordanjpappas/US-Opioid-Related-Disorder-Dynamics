# Elaine Hill, Andrew Boslett, Alina Denham, Meredith Adams, Jordan Pappas
# University of Rochester Medical Center, Health and Environmental Economics Lab (HEEL)

################################################################
########################### Preamble ###########################
################################################################

rm(list = ls())
options(scipen = 999)

##############################################################################################################
########################### Figure 2: create-figures-on-disorders-and-overdose-trends ########################
##############################################################################################################

# Goal: A figure of trends from 1999-2019 in
# (a) # of drug overdoses, total;
# (b) # of drug overdoses, with filters on F1* contributing causes of death (SKIP FOR NOW - AJB); and
# (c) # of total non-drug overdose deaths with either CC/underlying cause of death with a F1* cause.

# Import data

drug_related_deaths <- readRDS("Opioids_CCs/Scratch/Drug_Related.rds")
overdose_deaths <- readRDS("Opioids_CCs/Scratch/Overdoses.rds")

# Summarise data by year

# Drug overdoses

overdoses_per_year <- overdose_deaths %>% group_by(year) %>%
  summarise(value = n()) %>%
  ungroup() %>%
  mutate(variable = 'Drug overdoses')

# Drug-related disorders

disorders_per_year <- drug_related_deaths %>% 
  mutate(variable = ifelse(
    str_detect(string = ucod, pattern = '^F1') == TRUE,
    'Drug-related disorders, UCOD', 'Drug-related disorders, CC')) %>%
  group_by(year, variable) %>%
  summarise(value = n()) %>%
  ungroup()

disorders_total <- drug_related_deaths %>%
  group_by(year) %>%
  summarise(value = n()) %>%
  ungroup() %>%
  mutate(variable = 'Drug-related disorders')

bound_data <- overdoses_per_year %>% bind_rows(disorders_per_year) %>%
  bind_rows(disorders_total)

bound_data$variable <- factor(bound_data$variable, levels = c('Drug overdoses',
                                                              'Drug-related disorders',
                                                              'Drug-related disorders, CC',
                                                              'Drug-related disorders, UCOD'))

bound_plot <- ggplot(data = bound_data, aes(x = year, y = value, colour = variable, group = variable)) + 
  geom_line() + geom_point() + theme_classic() + labs(x = 'Year', y = '# of deaths') + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = c(0.3, 0.8)) + 
  scale_color_manual(values = c('#a50026', '#fee090', '#abd9e9', '#313695')) +
  theme(text=element_text(size=13,  family = 'Arial')) +
  xlim(1998, 2022)

bound_plot

getwd()

ggsave('Opioids_CCs/Figures/Figure_2_Death_Counts_by_Category.jpg')



#####################################################################################################################
########################### Figure 3: plot_age_unadjusted_mortality_rates_two_dots_by_state #########################
#####################################################################################################################

# Goal: State-by-state dot-plots for 2019 with mortality rates (unadjusted, for now) per 100,000 people for 
# (a) drug overdoses; and 
# (b) drug-related disorders? I think we want two versions of this because I'm not sure which is best: one version where (b) is measured with only those deaths with F1* underlying causes of death; and another version where (b) includes those with F1* contributing causes of death.

drug_related_deaths <- data.frame()

for (dataset in c('Drug-related disorders','Drug overdoses')){
  
  # Load data
  if (dataset == "Drug-related disorders") {
    deaths <- readRDS("Opioids_CCs/Scratch/Drug_Related.rds")
  }
  else if (dataset == "Drug overdoses") {
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
  if (dataset == "Drug-related disorders") {
    deaths %<>% mutate_at(vars(F110:F154, F135:F116),
                          funs(ifelse(is.na(.) == TRUE, 0, .)))
    
    deaths %>% select(starts_with('F1')) %>%
      rowSums() -> deaths$F1_all
    deaths %<>% mutate(F1_all = ifelse(F1_all > 0, 1, 0))
    deaths <- deaths %>% filter(F1_all == 1)
  }   
  
  else if (dataset == "Drug overdoses") {
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
  state_rates$type <- dataset
  
  # Bind data
  
  drug_related_deaths %<>% bind_rows(state_rates)
  
  print(dataset)
  
}

# Clean data

drug_related_deaths <- subset(drug_related_deaths, year==2019)
drug_related_deaths <- drug_related_deaths[order(drug_related_deaths$rate),]

# Graph data

dot_plot <- ggplot(data = drug_related_deaths,
                   aes(x = reorder(`Official USPS Code`,rate), y = rate, colour=type)) +
  geom_point() +   # Draw points
  geom_segment(aes(x = `Official USPS Code`,
                   xend = `Official USPS Code`,
                   y = 0,
                   yend = max(rate)),
               linetype="dashed",
               size=0.1) +   # Draw dashed lines
  labs(x = 'State',
       y = 'Mortality Rates') +
  scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#000000")) +
  coord_flip() +
  theme_classic() +
  theme(plot.margin=unit(c(1,4,1,1),"cm")) +
  theme(legend.justification=c(.15,.15), legend.position=c(1,0.15)) + theme(legend.title=element_blank())

dot_plot

ggsave('Opioids_CCs/Figures/Figure_3_Disorders.jpg')



#############################################################################################
########################### Figure 4: create-figure-on-age-by-group #########################
#############################################################################################

# Goal: Examine distribution of ages for drug overdoses and drug-related deaths

# Import data 

overdose_deaths <- readRDS("Opioids_CCs/Scratch/Overdoses.rds") # overdose
drug_deaths <- readRDS("Opioids_CCs/Scratch/Drug_Related.rds") # drug-related

# Clean data

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

ggsave('Opioids_CCs/Figures/Figure_4_Age_IQR.jpg')



##############################################################################################################
################### Figure A1: create-figure-on-correlations-for-disorders-and-overdose-trends ###############
##############################################################################################################

# Goal: A figure of Pearson correlation coefficients from 1999-2019 between state-level drug overdose and drug-related disorder mortality rates

# Import data

overdose_deaths <- readRDS("Opioids_CCs/Scratch/Overdoses.rds") # overdose
drug_deaths <- readRDS("Opioids_CCs/Scratch/Drug_Related.rds") # drug-related

# Clean data

# drug overdose
overdose_deaths %<>% mutate(state_fips_res = str_sub(county_fips_res, end = 2))

# group by county-year
temp_overdose <- overdose_deaths %>%
  group_by(state_fips_res,year) %>%
  summarise(number_of_overdoses = n()) %>%
  ungroup()

# drug-related disorder
drug_deaths %<>% mutate(state_fips_res = str_sub(county_fips_res, end = 2))

# group by county-year
temp_drug <- drug_deaths %>%
  group_by(state_fips_res,year) %>%
  summarise(number_of_drug_deaths = n()) %>%
  ungroup()

# Merge data

# Drug-related deaths and overdose deaths
merged_drug_overdose <- temp_drug %>% left_join(temp_overdose, by = c('year','state_fips_res'))

# SEER population data
# Notes: From this link: https://seer.cancer.gov/popdata/yr1969_2019.19ages/us.1969_2019.19ages.adjusted.txt.gz
seer_data <- read_fwf('Opioids_CCs/Data/us.1969_2019.19ages.adjusted.txt',
                   col_positions = fwf_widths(
                     c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8),
                     c('year', 'state', 'state_fips', 'county_fips',
                       'registry', 'race', 'origin', 'sex', 'age',
                       'population')))

seer_data %<>% dplyr::select(year, state_fips, population) 

seer_data %<>% mutate_at(vars(year, population),
                         funs(as.numeric(.)))

# Calculate population by race, year, and county FIPS code
seer_data %<>% group_by(state_fips, year) %>%
  summarise(population = sum(population)) %>%
  ungroup()

# Filter post-2000
seer_data %<>% filter(year >= 2000)

# Join with death data
merged_drug_overdose %<>% left_join(seer_data, by = c('state_fips_res' = 'state_fips', 'year'))

rm(list = setdiff(ls(), c('seer_data', 'merged_drug_overdose')))

# Generate rates of death per 100,000
merged_drug_overdose %<>% mutate_at(vars(number_of_overdoses:number_of_drug_deaths),
                                    funs(rate = (. / population) * 100000))

# Calculate correlation by year

correlation_df <- data.frame()

for(fff in unique(merged_drug_overdose$year)) {
  
  # Get yearly data
  temp <- merged_drug_overdose %>% filter(year == fff) 
  
  # Generate correlation coefficients
  temp_corr_ods <- cor(temp$number_of_overdoses_rate, temp$number_of_drug_deaths_rate)
  
  # Create data frame
  temp_correlation <- data.frame(year = fff, corr = temp_corr_ods)
  
  # Bind to data frame
  correlation_df %<>% bind_rows(temp_correlation)
  
  # Remove files
  print(fff)
  rm(temp_correlation, temp, fff)
  
}

# Plot data

yearly_plot <- ggplot(data = correlation_df,
                      aes(x = year, y = corr)) + 
  theme_classic() + labs(x = 'Year', y = 'Pearson Correlation Coefficient') + 
  geom_line(color = 'black') + geom_point(fill = 'dodgerblue', shape = 23, size = 2) +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) + theme(legend.title=element_blank())

yearly_plot

ggsave('Opioids_CCs/Figures/Figure_A1_Correlation_of_Drug_Overdoses_and_Disorders_from_1999-2019.jpg')



################################################################################################
########################### Figure A3: create-figure-on-gender-by-group ########################
################################################################################################

# Import data

overdose_deaths <- readRDS("Opioids_CCs/Scratch/Overdoses.rds") # overdose
drug_deaths <- readRDS("Opioids_CCs/Scratch/Drug_Related.rds") # drug-related

# Clean data
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
                                             'Drug overdoses', 'Drug-related disorders'))
  
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

ggsave('Opioids_CCs/Figures/Figure_A3_Gender.jpg')



##############################################################################################
########################### Figure A4: create-figure-on-race-by-group ########################
##############################################################################################

# Import data

overdose_deaths <- readRDS("Opioids_CCs/Scratch/Overdoses.rds") # overdose
drug_deaths <- readRDS("Opioids_CCs/Scratch/Drug_Related.rds") # drug-related

# Clean data
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
                                             'Drug overdoses', 'Drug-related disorders'))
  
  # Bind to race data
  
  race_data %<>% bind_rows(temp_summary)
  
  rm(temp_summary)
  
}

race_data %<>% gather(variable, value, -year, -category)

race_data$variable %<>% str_to_title() %>%
  str_replace_all(pattern = 'Non_white', replacement = 'Non-white')

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

ggsave('Opioids_CCs/Figures/Figure_A4_Race.jpg')



###################################################################################################
########################### Figure A5: create-figure-on-ethnicity-by-group ########################
###################################################################################################

# Goal: Examine distribution of ages for drug overdoses and drug-related deaths

# Import data

overdose_deaths <- readRDS("Opioids_CCs/Scratch/Overdoses.rds") # overdose
drug_deaths <- readRDS("Opioids_CCs/Scratch/Drug_Related.rds") # drug-related

# Calculate quantiles of age by year

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
                                             'Drug overdoses', 'Drug-related disorders'))
  
  # Bind to gender data
  
  ethnicity_data %<>% bind_rows(temp_summary)
  
  rm(temp_summary)
  
}

ethnicity_data %<>% gather(variable, value, -year, -category)

ethnicity_data %<>% mutate(variable = ifelse(variable == 'hispanic', 'Hispanic', 'Non-hispanic'))

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

ggsave('Opioids_CCs/Figures/Figure_A5_Ethnicity.jpg', width=7)



##################################################################################################
########################### Figure A6: create-figure-on-hospital-by-group ########################
##################################################################################################

# Import data

overdose_deaths <- readRDS("Opioids_CCs/Scratch/Overdoses.rds") # overdose
drug_deaths <- readRDS("Opioids_CCs/Scratch/Drug_Related.rds") # drug-related

# Clean data
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
                                             'Drug overdoses', 'Drug-related disorders'))
  
  # Bind to gender data
  
  hospital_data %<>% bind_rows(temp_summary)
  
  rm(temp_summary)
  
}

hospital_data %<>% gather(variable, value, -year, -category)

hospital_data %<>% mutate(variable = ifelse(variable == 'hospital', 'Hospital', 'Non-hospital'))

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

ggsave('Opioids_CCs/Figures/Figure_A6_Hospital.jpg', width=7)



##########################################################################################################
########################### Figure A7: plot_frequency_of_common_UCODs_where_CC_F1 #########################
##########################################################################################################

# Goal: What are the top-15 most common underlying causes of death with non-drug overdose deaths with a F1* contributing, but not underlying, cause of death? 2015-2019 for this one + one for the entire time period

# Load data

drug_related_deaths <- readRDS("Opioids_CCs/Scratch/Drug_Related.rds")

# Clean data

# NA to 0 for all record causes
drug_related_deaths %<>% mutate_at(vars(F110:F154, F135:F116),
                                   funs(ifelse(is.na(.) == TRUE, 0, .)))

# Create a variable indicating incidence of any F1 related to opioids
drug_related_deaths %>% select(starts_with('F1')) %>%
  rowSums() -> drug_related_deaths$F1_all
drug_related_deaths %<>% mutate(F1_all = ifelse(F1_all > 0, 1, 0))

# Group by UCOD and filter in F1* CC and filter out F1* UCOD
temp <- drug_related_deaths %>%
  filter(F1_all == 1 & str_detect(string = ucod, pattern = '^F1') == FALSE) %>%
  group_by(ucod) %>%
  summarise(n_n = n()) %>%
  ungroup()

# Filter in top 15 rows by count
temp %<>% arrange(-n_n) %>%
  filter(row_number() <= 15)

temp %<>% arrange(n_n) %>% mutate(ucod = factor(ucod, unique(ucod)))

# Plot data

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

ggsave('Opioids_CCs/Figures/Figure_A7_UCOD_F1_all.jpg')

# Load data

drug_related_deaths <- readRDS("Opioids_CCs/Scratch/Drug_Related.rds")

# Clean data

# Filter years
drug_related_deaths <- subset(drug_related_deaths, year >= 2015 & year <= 2019)

# NA to 0 for all record causes
drug_related_deaths %<>% mutate_at(vars(F110:F154, F135:F116),
                                   funs(ifelse(is.na(.) == TRUE, 0, .)))

# Create a variable indicating incidence of any F1 related to opioids
drug_related_deaths %>% select(starts_with('F1')) %>%
  rowSums() -> drug_related_deaths$F1_all
drug_related_deaths %<>% mutate(F1_all = ifelse(F1_all > 0, 1, 0))

# Group by UCOD and filter in F1* CC and filter out F1* UCOD
temp <- drug_related_deaths %>%
  filter(F1_all == 1 & str_detect(string = ucod, pattern = '^F1') == FALSE) %>%
  group_by(ucod) %>%
  summarise(n_n = n()) %>%
  ungroup()

# Filter in top 15 rows by count
temp %<>% arrange(-n_n) %>%
  filter(row_number() <= 15)

temp %<>% arrange(n_n) %>% mutate(ucod = factor(ucod, unique(ucod)))

# Plot data

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

ggsave('Opioids_CCs/Figures/Figure_A7_UCOD_F1_2015-2019.jpg')