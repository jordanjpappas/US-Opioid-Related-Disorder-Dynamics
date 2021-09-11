# Jordan Pappas, Andrew Boslett, Elaine Hill, Alina Denham, Meredrith Adams
# University of Rochester Medical Center, Hill Lab for Health Economics
# jordan.pappas@rochester.edu

rm(list = ls())

options(scipen = 999)

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

