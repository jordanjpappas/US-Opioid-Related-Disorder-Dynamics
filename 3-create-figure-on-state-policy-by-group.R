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



