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
  

