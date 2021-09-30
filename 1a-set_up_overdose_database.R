# Jordan Pappas, Andrew Boslett, Elaine Hill, Alina Denham, Meredrith Adams
# University of Rochester Medical Center, Hill Lab for Health Economics
# andrew_boslett@urmc.rochester.edu



# Set options ---------------------

rm(list = ls())

options(scipen = 999) 

# Import data --------------------------

all_overdoses <- data.frame()

for(fff in 1999:2019) {
  
  # Import data
  
  temp <- readRDS(paste0('Opioids_CCs/Data/mort_drug_overdoses_', as.character(fff), '_inter.rds'))
  
  # Bind data to drug overdoses data frame
  
  all_overdoses %<>% bind_rows(temp)
  
  # Remove file
  
  rm(temp)
  
  print(fff)
  
}

all_overdoses %>% saveRDS("Opioids_CCs/Scratch/Overdoses.rds")




temp <- all_overdoses






temp_summary <- temp %>%
  select(id_overdose, year,
         contains('record')) %>%
  gather(variable, value, -id_overdose, -year) %>%
  filter(variable != 'record_1') %>%
  filter(!is.na(variable) & variable != '') %>%
  filter(str_detect(string = value, pattern = '^F1'))

# Total drug overdoses for year: F1*
  # Note to Jordan/Andy: Need to drop Alcohol and Tobacco-related F1*

temp_summary %>% 
  filter(str_detect(string = variable, pattern = '^F10') == FALSE & 
           str_detect(string = variable, pattern = '^F17') == FALSE) %>%
  select(id_overdose, year) %>% 
  unique() %>% nrow()/nrow(temp)

# Total drug overdoses for year: F11*

temp_summary %>% 
  filter(str_detect(string = value, pattern = '^F11') == TRUE) %>%
  select(id_overdose, year) %>% 
  unique() %>% nrow()/nrow(temp)
  




#write.csv(all_overdoses,'/Users/Jordan/Documents/Work/Hill Lab/Opioids ML/opioids.csv')
