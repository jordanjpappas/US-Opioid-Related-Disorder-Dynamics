# Jordan Pappas, Andrew Boslett, Elaine Hill, Alina Denham, Meredrith Adams
# University of Rochester Medical Center, Hill Lab for Health Economics
# andrew_boslett@urmc.rochester.edu



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

# Read file again ----------------------

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

# Figures ---------------------

# (1) # of deaths by year ----------------------

temp <- all_deaths %>% group_by(year) %>%
  summarise(n_n = n()) %>%
  ungroup()

yearly_plot <- ggplot(data = temp,
                      aes(x = year, y = n_n)) +
  theme_classic() + labs(x = 'Year', y = '# of deaths with drug-related disorder ICD-10 codes') +
  geom_line() + geom_point(size = 3, color = 'dodgerblue')

yearly_plot

ggsave('Opioids_CCs/Figures/Figure_X_Annual_Deaths_Non_Drug_Overdoses_F1.jpg')

# (2) # of deaths by year, by F11 category -------------------------

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

# (3) # of deaths by year, F1* as UCOD -----------------------

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

# (4) # of deaths by year, F11 as UCOD -----------------------

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

# (5) # of deaths by year, F11 as CC -----------------------

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

# (6) Most common UCODs with opioid-related disorders F11* as CC -------------------

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

# (7) Most common UCODs with opioid-related disorders F1* as CC -------------------

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
