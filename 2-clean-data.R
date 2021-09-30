# Elaine Hill, Andrew Boslett, Alina Denham, Meredith Adams, Jordan Pappas
# University of Rochester Medical Center, Health and Environmental Economics Lab (HEEL)


########################### Preamble ###########################

rm(list = ls())
options(scipen = 999)


# Goal: Find deaths that involved drug use but weren't drug overdoses.

# Import files -----------------------

# Set ICD codes of interest

icd_codes <- c(paste0('F', seq(from = 111, to = 169, by = 1)),
               paste0('F', seq(from = 181, to = 199, by = 1)),
               paste0('T', seq(from = 400, to = 499, by = 1)),
               'T509')
#icd_codes <- F1_vector
#icd_codes <- F11_vector # What is this? AB
#icd_codes <- suicide_vector

#F11_vector <- c(paste0('F', seq(from = 110, to = 169, by = 1)),
#                paste0('F', seq(from = 180, to = 199, by = 1)),
#               paste0('F', seq(from = 10, to = 19, by = 1)))
#icd_codes <- F11_vector

#all_deaths_vector <- c()
#icd_codes <- all_deaths_vector

# Set total data frame of deaths to bind deaths

all_deaths <- data.frame()

for(zzz in 1999:2019) {
  
  # Import file 
  
  temp <- readRDS(paste0('Opioids_CCs/Data/mort', as.character(zzz), '.rds'))
  
  # Factor to character all records
  
  temp %<>% mutate_at(vars(contains('record'), ucod),
                      funs(as.character(.)))
  
  # Filter out deaths with drug-related record causes
  # Note: I added ucod here. ucod == record_1 so I don't know why I did it but I guess it's because it clarifies the code a bit?
  
  temp_records <- temp %>% select(id_var, contains('record'), ucod) %>%
    mutate_at(vars(contains('record'), ucod), funs(ifelse(. == '', NA, .))) %>%
    melt(id.vars = 'id_var') %>%
    select(-variable) %>%
    filter(value %in% icd_codes)
  
  # Unique?
  
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
  
  # Note: We'll have to fill these new indicators as 0s, not NAs, once the data file is constructed for all years.
  
  # Bind rows to data frame
  
  all_deaths %<>% bind_rows(temp)
  
  # Timestamp and remove files
  
  print(zzz)
  
  rm(temp, zzz)
  
}

# Save data
# Note: I moved this outside of the loop because I think it was slowing it down. To save over the same file, over and over again,
# is probably not the best look here. So it is what it is.

#all_deaths %>% saveRDS("Opioids_CCs/Scratch/Deaths_with_F1_code.rds")
all_deaths %>% saveRDS("Opioids_CCs/Scratch/Drug_related_Deaths.rds")
