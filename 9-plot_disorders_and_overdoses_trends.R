# trends of drug overdoses and drug-related disorders from 1999-2019
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


