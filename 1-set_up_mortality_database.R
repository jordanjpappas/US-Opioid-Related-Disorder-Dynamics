# Jordan Pappas, Andrew Boslett, Elaine Hill, Alina Denham, Meredrith Adams
# University of Rochester Medical Center, Hill Lab for Health Economics
# andrew_boslett@urmc.rochester.edu



rm(list = ls())

options(scipen = 999)

# Goal: Overarching goal is to set up the drug overdoses database

# Set up individual box directory link -------------

box_directory <- 'D:/Box Sync/'

# Download CSV files from the NBER website (1995-2004) ----------------------
# DEPRECATED

# data_source <- '//smdnas/EHill_InfantHealth/NCHS/Mortality'
# 
# tempDir  <- tempdir()
# 
# years  <- as.character(1995:2004)
# 
# urls   <- paste0("http://www.nber.org/mortality/",
#                  years, "/mort", years, ".csv.zip")
# 
# files  <- paste(data_source, basename(urls), sep = "/")
# 
# map2(urls, files, function(urls, files)
#   if(!file.exists(files)) download.file(urls, files))

# Unzip folders from NCHS --------------------

# library(plyr)
# 
# zipF <- list.files(path = 'C:/Users/aboslett/Documents/Opioids',
#                    pattern = "*csv.zip",
#                    full.names = TRUE)
# 
# ldply(.data = zipF, .fun = unzip, exdir = 'C:/Users/aboslett/Documents/Opioids')

# (1) Import and clean data from 1999-2002 ---------------------
# Note 1: 1999-2002 data had a data dictionary that was different from the later vintages. We execute
# two loops, one for the 1999-2002 and the other for the 2003-2016 data.

# Note 2: As I recall, all data from 1999-2016 was converted to a CSV file from a SAS file 
# received from NCHS. This was done by Alina. We started all cleaning efforts using the resulting CSV
# files, as shown below. I don't have the code that did the conversion into this file structure. 

for(fff in 1999:2002) {
  
  temp <- read.csv(file = paste0('//smdnas/EHill_InfantHealth/Mortality/CSV', '/', 'mort', as.character(fff), '.csv'))
  
  names(temp)
  
  # Drop deaths to foreign residents -----------------------
  
  temp %<>% filter(restatus != 4)
  
  # Add relevant variables ----------------------
  
  temp %<>% mutate(
    id_var = 1:nrow(temp),
    year = fff,
    female = sex - 1, # works for 1999-2002
    # for 2003 and later: female = ifelse(sex == "F", 1, 0) # no code for not recorded, but check frequency of missing
    white = ifelse(race == 1, 1, 0), # seems consistent across years
    black = ifelse(race == 2, 1, 0), # seems consistent across years
    indian = ifelse(race == 3, 1, 0), # seems consistent across years
    asian = ifelse(race > 3, 1, 0), # seems consistent across years
    married = ifelse(marstat == 2, 1, 0), # works for 1999-2002
    # for 2003 and later: married = ifelse(marstat == 'M', 1, 0)
    less_than_hs = ifelse(educ < 12, 1, 0),
    hs = ifelse(educ == 12, 1, 0),
    some_college = ifelse(educ > 12 & educ < 16, 1, 0),
    college = ifelse(educ == 16 | educ == 17, 1, 0),
    educ_unknown = ifelse(educ == 99, 1, 0),
    # the above code for education works for 1999-2002
    # for 2003-on use the following:
    # less_than_his = ifelse((educ89 < 12) | (educ == 1 | educ == 2), 1, 0),
    # hs = ifelse((educ89 == 12) | (educ == 3), 1, 0),
    # some_college = ifelse((educ89 > 12 & educ89 < 16) | (educ == 4 | educ == 5), 1, 0), # includes some college and associate degree
    # college = ifelse((educ89 == 16 | educ89 == 17) | (educ > 5 & educ < 9), 1, 0),
    # educ_unknown = ifelse((educ89 == 99) |  (educ == 9), 1, 0),
    # the reason for using both educ89 and educ - some areas use one 1989 version and then 2003 version is blank, and vice versa
    dead_hospital_in = ifelse(placdth == 1, 1, 0), # seems consistent across years
    dead_hospital_out = ifelse(placdth == 2, 1, 0), # seems consistent across years
    dead_hospital_doa = ifelse(placdth == 3, 1, 0), # seems consistent across years
    # dead_hospital_unk = ifelse(placdth == 4, 1, 0), # unknown patient status; category doesn't exist 2003-on; drop?
    dead_nursing_home = ifelse(placdth == 5, 1, 0), # seems consistent across years
    dead_residence = ifelse(placdth == 6, 1, 0), #works for 1999-2002; in 2003-on: placdth == 4
    dead_unknown = ifelse(placdth > 6, 1, 0), #7=other, 0=unknown; want to group them together into unknown? maybe not?
    age_20 = ifelse(ager52 < 30, 1, 0), # seems consistent across years
    age_30 = ifelse(ager52 == 30 | ager52 == 31, 1, 0), # seems consistent across years
    age_40 = ifelse(ager52 == 32 | ager52 == 33, 1, 0), # seems consistent across years
    age_50 = ifelse(ager52 == 34 | ager52 == 35, 1, 0), # seems consistent across years
    age_60 = ifelse(ager52 == 36 | ager52 == 37, 1, 0), # seems consistent across years
    age_70 = ifelse(ager52 == 38 | ager52 == 39, 1, 0), # seems consistent across years
    age_80 = ifelse(ager52 == 40 | ager52 == 41, 1, 0), # seems consistent across years
    age_125 = ifelse(ager52 > 41 & ager52 <= 51, 1, 0), # seems consistent across years
    age_unknown = ifelse(ager52 == 52, 1, 0), # seems consistent across years
    hispanic = ifelse(hispanic != 0 & hispanic != 99, 1, 0), # works for 1999-2002
    # for 2003 and later: hispanic = ifelse(hispanic >= 200 & hispanic <= 299, 1, 0)
    work_injury = ifelse(injwork == 1, 1, 0), # works for 1999-2002
    # for 2003 and later: work_injury = ifelse(injwork == "Y", 1, 0)
    weekday = ifelse(weekday == 9, NA, weekday) # seems consistent across years
  )
  
  # Select only relevant variables --------------------------
  
  temp %<>% select(id_var, rectype, restatus, metro, educ, monthdth, sex, race, age, ager52, placdth, marstat, hispanic, 
                   weekday, year, state_fips_occ = fipssto, county_fips_occ = fipsctyo, 
                   state_fips_res = fipsstr, county_fips_res = fipsctyr, injwork, mandeath, injury, ucod, ucr358,
                   ucr113, ucr130, ucr39, contains('record'), female, white, black, indian, asian, married,
                   less_than_hs, hs, contains('college'), educ_unknown, contains('dead_'), contains('age_'), work_injury)
  
  temp %<>% mutate_at(
    vars(county_fips_occ, county_fips_res),
    funs(as.character(.))
  )
  
  temp %<>% mutate_at(
    vars(county_fips_res, county_fips_occ),
    funs(ifelse(str_length(.) == 4,
                paste0("0", .), .))
  )

  temp %<>% select(-sex, -marstat, -state_fips_occ, -state_fips_res, -injwork)
  
  # Save as RDS file ----------------
  # Note: Save all other mortality as a yearly file
  
  temp %>% saveRDS(file = paste0('Opioids_CCs/Data/', 'mort', as.character(fff), '.rds'))
  
  rm(temp, temp_drug)
  
  print(fff)
  
}

# (2) Import and clean 2003-2016 ---------------------

for(fff in 2003:2019) {
  
  temp <- read.csv(file = paste0('//smdnas/EHill_InfantHealth/Mortality/CSV', '/', 'mort', as.character(fff), '.csv'))
  
  # Drop deaths to foreign residents ----------------------
  
  temp %<>% filter(restatus != 4)
  
  # Add relevant variables ----------------------
  
  temp %<>% mutate(
    id_var = 1:nrow(temp),
    year = fff,
    female = ifelse(sex == "F", 1, 0),  # no code for not recorded, but we should check frequency of missing
    white = ifelse(race == 1, 1, 0),
    black = ifelse(race == 2, 1, 0),
    indian = ifelse(race == 3, 1, 0),
    asian = ifelse(race > 3, 1, 0),
    married = ifelse(marstat == "M", 1, 0),
    # the reason for using both educ89 and educ - some areas use one 1989 version and then 2003 version is blank, and vice versa
    less_than_hs = ifelse((educ89 < 12) | (educ == 1 | educ == 2), 1, 0),
    hs = ifelse((educ89 == 12) | (educ == 3), 1, 0),
    some_college = ifelse((educ89 > 12 & educ89 < 16) | (educ == 4 | educ == 5), 1, 0), # includes some college and associate degree
    college = ifelse((educ89 == 16 | educ89 == 17) | (educ > 5 & educ < 9), 1, 0),
    educ_unknown = ifelse((educ89 == 99) |  (educ == 9), 1, 0),
    dead_hospital_in = ifelse(placdth == 1, 1, 0), # seems consistent across years
    dead_hospital_out = ifelse(placdth == 2, 1, 0), # seems consistent across years
    dead_hospital_doa = ifelse(placdth == 3, 1, 0), # seems consistent across years
    dead_hospice = ifelse(placdth == 5, 1, 0), # seems consistent across years
    dead_nursing_home = ifelse(placdth == 6, 1, 0), # seems consistent across years
    dead_residence = ifelse(placdth == 4, 1, 0), # residence (home) =4 in 2003-on
    dead_unknown = ifelse(placdth > 6, 1, 0), #7=other, 0=unknown; want to group them together into unknown? maybe not?
    age_20 = ifelse(ager52 < 30, 1, 0),
    age_30 = ifelse(ager52 == 30 | ager52 == 31, 1, 0),
    age_40 = ifelse(ager52 == 32 | ager52 == 33, 1, 0),
    age_50 = ifelse(ager52 == 34 | ager52 == 35, 1, 0),
    age_60 = ifelse(ager52 == 36 | ager52 == 37, 1, 0),
    age_70 = ifelse(ager52 == 38 | ager52 == 39, 1, 0),
    age_80 = ifelse(ager52 == 40 | ager52 == 41, 1, 0),
    age_125 = ifelse(ager52 > 41 & ager52 <= 51, 1, 0),
    age_unknown = ifelse(ager52 == 52, 1, 0),
    hispanic = ifelse(hispanic > 199 & hispanic < 300, 1, 0),
    work_injury = ifelse(injwork == "Y", 1, 0),
    weekday = ifelse(weekday == 9, NA, weekday),
    autopsy_done = ifelse(autopsy == "Y", 1, 0)
  )
  
  # Select relevant variables -----------------------
  
  # we need both educ and educ89, hence the change; also deleted rectype - is not here
  temp %<>% select(id_var, restatus, metro, educ, educ89, monthdth, sex, race, age, ager52, placdth, marstat, hispanic, 
                   weekday, year, state_fips_occ = stateoc, county_fips_occ = countyoc, 
                   state_fips_res = staters, county_fips_res = countyrs, injwork, mandeath, injury, ucod, ucr358,
                   ucr113, ucr130, ucr39, contains('record'), female, white, black, indian, asian, married,
                   less_than_hs, hs, contains('college'), educ_unknown, contains('dead_'), contains('age_'), work_injury,
                   autopsy_done)
  
  # Replace county fips codes as the last three digits in the field ----------------------
  
  temp %<>% mutate_at(
    vars(county_fips_occ, county_fips_res),
    funs(str_sub(., start = -3))
  )
  
  # Replace state fips codes with abbreviations as the actual code --------------------------
  # Note: Should have used maps package for this one.
  
  temp$state_fips_occ %<>%  str_replace_all(replacement = "02", pattern = "AK") %>%
    str_replace_all(replacement = "01", pattern = "AL") %>%
    str_replace_all(replacement = "05", pattern = "AR") %>%
    str_replace_all(replacement = "04", pattern = "AZ") %>%
    str_replace_all(replacement = "06", pattern = "CA") %>%	
    str_replace_all(replacement = "08", pattern = "CO") %>%	
    str_replace_all(replacement = "09", pattern = "CT") %>%	
    str_replace_all(replacement = "11", pattern = "DC") %>%	
    str_replace_all(replacement = "10", pattern = "DE") %>%	
    str_replace_all(replacement = "12", pattern = "FL") %>%	
    str_replace_all(replacement = "13", pattern = "GA") %>%	
    str_replace_all(replacement = "15", pattern = "HI") %>%
    str_replace_all(replacement = "19", pattern = "IA") %>%	
    str_replace_all(replacement = "16", pattern = "ID") %>%
    str_replace_all(replacement = "17", pattern = "IL") %>%
    str_replace_all(replacement = "18", pattern = "IN") %>%
    str_replace_all(replacement = "20", pattern = "KS") %>%
    str_replace_all(replacement = "21", pattern = "KY") %>%
    str_replace_all(replacement = "22", pattern = "LA") %>%
    str_replace_all(replacement = "25", pattern = "MA") %>%
    str_replace_all(replacement = "24", pattern = "MD") %>%
    str_replace_all(replacement = "23", pattern = "ME") %>%
    str_replace_all(replacement = "26", pattern = "MI") %>%
    str_replace_all(replacement = "27", pattern = "MN") %>%
    str_replace_all(replacement = "29", pattern = "MO") %>%
    str_replace_all(replacement = "28", pattern = "MS") %>%
    str_replace_all(replacement = "30", pattern = "MT") %>%	
    str_replace_all(replacement = "37", pattern = "NC") %>%
    str_replace_all(replacement = "38", pattern = "ND") %>%
    str_replace_all(replacement = "31", pattern = "NE") %>%
    str_replace_all(replacement = "33", pattern = "NH") %>%
    str_replace_all(replacement = "34", pattern = "NJ") %>%
    str_replace_all(replacement = "35", pattern = "NM") %>%
    str_replace_all(replacement = "32", pattern = "NV") %>%
    str_replace_all(replacement = "36", pattern = "NY") %>%
    str_replace_all(replacement = "39", pattern = "OH") %>%
    str_replace_all(replacement = "40", pattern = "OK") %>%
    str_replace_all(replacement = "41", pattern = "OR") %>%
    str_replace_all(replacement = "42", pattern = "PA") %>%
    str_replace_all(replacement = "44", pattern = "RI") %>%
    str_replace_all(replacement = "45", pattern = "SC") %>%
    str_replace_all(replacement = "46", pattern = "SD") %>%
    str_replace_all(replacement = "47", pattern = "TN") %>%
    str_replace_all(replacement = "48", pattern = "TX") %>%
    str_replace_all(replacement = "49", pattern = "UT") %>%
    str_replace_all(replacement = "51", pattern = "VA") %>%
    str_replace_all(replacement = "50", pattern = "VT") %>%
    str_replace_all(replacement = "53", pattern = "WA") %>%
    str_replace_all(replacement = "55", pattern = "WI") %>%
    str_replace_all(replacement = "54", pattern = "WV") %>%
    str_replace_all(replacement = "56", pattern = "WY")
  
  temp$state_fips_res %<>%  str_replace_all(replacement = "02", pattern = "AK") %>%
    str_replace_all(replacement = "01", pattern = "AL") %>%
    str_replace_all(replacement = "05", pattern = "AR") %>%
    str_replace_all(replacement = "04", pattern = "AZ") %>%
    str_replace_all(replacement = "06", pattern = "CA") %>%	
    str_replace_all(replacement = "08", pattern = "CO") %>%	
    str_replace_all(replacement = "09", pattern = "CT") %>%	
    str_replace_all(replacement = "11", pattern = "DC") %>%	
    str_replace_all(replacement = "10", pattern = "DE") %>%	
    str_replace_all(replacement = "12", pattern = "FL") %>%	
    str_replace_all(replacement = "13", pattern = "GA") %>%	
    str_replace_all(replacement = "15", pattern = "HI") %>%
    str_replace_all(replacement = "19", pattern = "IA") %>%	
    str_replace_all(replacement = "16", pattern = "ID") %>%
    str_replace_all(replacement = "17", pattern = "IL") %>%
    str_replace_all(replacement = "18", pattern = "IN") %>%
    str_replace_all(replacement = "20", pattern = "KS") %>%
    str_replace_all(replacement = "21", pattern = "KY") %>%
    str_replace_all(replacement = "22", pattern = "LA") %>%
    str_replace_all(replacement = "25", pattern = "MA") %>%
    str_replace_all(replacement = "24", pattern = "MD") %>%
    str_replace_all(replacement = "23", pattern = "ME") %>%
    str_replace_all(replacement = "26", pattern = "MI") %>%
    str_replace_all(replacement = "27", pattern = "MN") %>%
    str_replace_all(replacement = "29", pattern = "MO") %>%
    str_replace_all(replacement = "28", pattern = "MS") %>%
    str_replace_all(replacement = "30", pattern = "MT") %>%	
    str_replace_all(replacement = "37", pattern = "NC") %>%
    str_replace_all(replacement = "38", pattern = "ND") %>%
    str_replace_all(replacement = "31", pattern = "NE") %>%
    str_replace_all(replacement = "33", pattern = "NH") %>%
    str_replace_all(replacement = "34", pattern = "NJ") %>%
    str_replace_all(replacement = "35", pattern = "NM") %>%
    str_replace_all(replacement = "32", pattern = "NV") %>%
    str_replace_all(replacement = "36", pattern = "NY") %>%
    str_replace_all(replacement = "39", pattern = "OH") %>%
    str_replace_all(replacement = "40", pattern = "OK") %>%
    str_replace_all(replacement = "41", pattern = "OR") %>%
    str_replace_all(replacement = "42", pattern = "PA") %>%
    str_replace_all(replacement = "44", pattern = "RI") %>%
    str_replace_all(replacement = "45", pattern = "SC") %>%
    str_replace_all(replacement = "46", pattern = "SD") %>%
    str_replace_all(replacement = "47", pattern = "TN") %>%
    str_replace_all(replacement = "48", pattern = "TX") %>%
    str_replace_all(replacement = "49", pattern = "UT") %>%
    str_replace_all(replacement = "51", pattern = "VA") %>%
    str_replace_all(replacement = "50", pattern = "VT") %>%
    str_replace_all(replacement = "53", pattern = "WA") %>%
    str_replace_all(replacement = "55", pattern = "WI") %>%
    str_replace_all(replacement = "54", pattern = "WV") %>%
    str_replace_all(replacement = "56", pattern = "WY")
  
  # Create new county fips codes -------------------
  
  temp %<>% mutate_at(
    vars(county_fips_res, county_fips_occ),
    funs(as.character(.))
  )
  
  temp %<>% mutate_at(
    vars(county_fips_res, county_fips_occ),
    funs(ifelse(str_length(.) == 1, paste0("00", .), 
                ifelse(str_length(.) == 2, paste0("0", .), .)))
  )
  
  temp %<>% mutate(county_fips_res = paste0(state_fips_res, county_fips_res),
                   county_fips_occ = paste0(state_fips_occ, county_fips_occ))
  
  temp %<>% mutate_at(
    vars(less_than_hs:educ_unknown),
    funs(ifelse(is.na(.) == TRUE, FALSE, .))
  )
  
  temp %<>% select(-sex, -marstat, -state_fips_occ, -state_fips_res, -injwork)

  # Save as RDS file ----------------
  
  temp %>% saveRDS(file = paste0('Opioids_CCs/Data', '/', 'mort', as.character(fff), '.rds'))
  
  rm(temp, temp_drug, temp_drug_deaths)
  
  print(fff)
  
}

