# Elaine Hill, Andrew Boslett, Alina Denham, Meredith Adams, Jordan Pappas
# University of Rochester Medical Center, Health and Environmental Economics Lab (HEEL)
# andrew_boslett@urmc.rochester.edu



# Set options ---------------------

rm(list = ls())

options(scipen = 999)

# Goal: Find deaths that involved drug use but weren't drug overdoses.

# Import files -----------------------

# Set ICD codes of interest

icd_codes <- c(paste0('F', seq(from = 110, to = 169, by = 1)),
               paste0('F', seq(from = 180, to = 199, by = 1)),
               paste0('T', seq(from = 400, to = 499, by = 1)),
               'T509')

# Set total data frame of deaths to bind deaths

drug_related_deaths <- data.frame()

for(zzz in 1999:2019) {
  
  # Import file 
  
  temp <- readRDS(paste0('Opioids_CCs/Data/mort', as.character(zzz), '.rds'))
  
  # Factor to character all records
  
  temp %<>% mutate_at(vars(contains('record'), ucod),
                      funs(as.character(.)))
  
  # Filter out deaths with drug-related record causes
  
  temp_records <- temp %>% select(id_var, contains('record'), ucod) %>%
    mutate_at(vars(contains('record'), ucod), funs(ifelse(. == '', NA, .))) %>%
    melt(id.vars = 'id_var') %>%
    select(-variable) %>%
    filter(value %in% icd_codes)
  
  # Unique
  
  temp_records %<>% unique()
  
  # Keep only those deaths with a ICD-10 code of interest
  
  temp %<>% filter(id_var %in% temp_records$id_var)
  
  # Drop deaths from drug overdoses
  
  temp %<>% filter(!ucod %in% c('X40', 'X41', 'X42', 'X43', 'X44', 'X60', 'X61', 'X62',
                                'X63', 'X64', 'X85', 'Y10', 'Y11', 'Y12', 'Y13', 'Y14'))
  
  # Reshape wide
  
  temp_records %<>% mutate(count = 1) %>%
    dcast(id_var ~ value, value.var = c('count'))
  
  # Join back to deaths data
  
  temp %<>% left_join(temp_records, by = c('id_var'))
  
  # Bind rows to data frame
  
  drug_related_deaths %<>% bind_rows(temp)
  
  # Timestamp and remove files
  
  print(zzz)
  
  rm(temp, zzz)
  
}

# Save data

drug_related_deaths %>% saveRDS("Opioids_CCs/Scratch/Drug_Related.rds")

