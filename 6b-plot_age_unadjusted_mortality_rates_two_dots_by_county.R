# Jordan Pappas, Andrew Boslett, Elaine Hill, Alina Denham, Meredrith Adams
# University of Rochester Medical Center, Hill Lab for Health Economics
# jordan.pappas@rochester.edu



# (6b) What are the top-15 or 20 counties in the U.S. in terms of drug-related disorder deaths for 2019. Two versions of this figure would suffice, for review: 
  # (a) one where drug-related disorders are only based on those with underlying causes of death with an F1*; and 
  # (b) one which includes those + contributing causes of death with an F1*.

  



all_deaths <- data.frame()

for (dataset in c('Drug-Related Disorders UCOD','Drug-Related Disorders UCOD and CC')){
  
  # Load data
  deaths <- readRDS("Opioids_CCs/Scratch/Deaths_with_F1_code.rds")
  state_to_fips <- read_excel("Opioids_CCs/Data/USCB_State_FIPS_Codes_2019.xlsx", col_names = TRUE)
  county_state_fips <- read_xlsx('Opioids_CCs/Data/county_fips_code.xlsx')
  us_pop <- read_fwf('Opioids_CCs/Data/us.1969_2019.19ages.adjusted.txt',
                     col_positions = fwf_widths(
                       c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8),
                       c('year', 'state', 'state_fips', 'county_fips',
                         'registry', 'race', 'origin', 'sex', 'age', 
                         'population')))
  
  # Clean data
  # Clean deaths
  # Filter out F1 CCs for drug overdoses and F11 CCs for opioid overdoses
  if (dataset == "Drug-Related Disorders UCOD") {
    deaths %<>% mutate_at(vars(F110:F154, F135:F116),
                          funs(ifelse(is.na(.) == TRUE, 0, .)))
    
    deaths <- deaths %>% filter(str_detect(string = ucod, pattern = '^F1') == TRUE)
  }  
  
  else if (dataset == "Drug-Related Disorders UCOD and CC") {
    deaths %<>% mutate_at(vars(F110:F154, F135:F116),
                          funs(ifelse(is.na(.) == TRUE, 0, .)))
    
    deaths %>% select(starts_with('F1')) %>%
      rowSums() -> deaths$F1_all
    deaths %<>% mutate(F1_all = ifelse(F1_all > 0, 1, 0))
    
    deaths <- deaths %>% filter(F1_all == 1 & str_detect(string = ucod, pattern = '^F1') == TRUE)
  } 
  
  # Create state column
  deaths$state <- NA
  deaths$state <- substr(deaths$county_fips_occ, 0, 2)
  temp <- deaths
  # Select relevant variables in drug overdoses database
  temp %<>% select(year, county_fips_occ)
  # Summarise drug overdoses by year and state
  temp_summary <- temp %>% 
    group_by(year, county_fips_occ) %>%
    summarise(n = n())
  # Resummarise using new definition 
  temp_summary %<>%
    group_by(year, county_fips_occ) %>%
    summarise_at(vars(contains('n')),
                 funs(sum(., na.rm = TRUE))) %>%
    ungroup()
  
  # Clean state_to_fips
  # Pad 0s on FIPS column
  state_to_fips$`FIPS State Numeric Code` <- str_pad(state_to_fips$`FIPS State Numeric Code`, 2, side="left", 0)
  
  # Clean county_fips
  county_state_fips$state_fips <- str_pad(county_state_fips$state_fips, 2, side="left", 0)
  county_state_fips$county_fips <- str_pad(county_state_fips$county_fips, 3, side="left", 0)
  county_state_fips$state_county <- paste0(county_state_fips$state_fips,county_state_fips$county_fips)
  
  # Clean us_pop
  # Create state_county column
  us_pop$state_county <- NA
  us_pop$state_county <- paste0(us_pop$state_fips,us_pop$county_fips)
  # Make population numeric
  us_pop %<>% mutate_at(vars(population),
                        funs(as.numeric(.)))
  # Summarise US population by year and state
  pop_summary <- us_pop %>% 
    group_by(year, state_county) %>%
    summarise_at(vars(population),
                 funs(sum(., na.rm = TRUE))) %>%
    ungroup()
  
  
  # Merge data
  # Merge population data to drug overdoses data
  temp_summary <- 
    pop_summary %<>% left_join(temp_summary, by = c('year', 
                                                    'state_county' = 'county_fips_occ'))
  
  # Clean data
  # Clean merged
  # Rearrange data
  temp_summary <- subset(temp_summary, population > 250000)
  temp_summary %<>% arrange(state_county, year)
  # Add total population data for each year
  temp_summary %<>% group_by(year, state_county) %>%
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
  temp_summary %>% group_by(year, state_county) %>%
    summarise_at(vars(contains('rate')),
                 funs(sum(., na.rm = TRUE))) %>%
    ungroup() -> county_rates
  county_rates <- subset(county_rates, year==2019)
  
  merged_county_rates <- merge(county_rates, county_state_fips, by.x='state_county', by.y='state_county')
  merged_county_rates$state_county_name <- paste0(merged_county_rates$full_name,', ',merged_county_rates$state)
  merged_county_rates <- merged_county_rates[order(merged_county_rates$rate),]
  merged_county_rates <- tail(merged_county_rates,n=20)
  merged_county_rates$state_county_name = factor(merged_county_rates$state_county_name,levels=merged_county_rates$state_county_name)
  merged_county_rates$type <- dataset
  
  
  
  # Bind data
  
  all_deaths %<>% bind_rows(merged_county_rates)
  
  print(dataset)
  
}



# Clean data

  # all_deaths
  all_deaths_UCOD <- subset(all_deaths, type=='Drug-Related Disorders UCOD')
  all_deaths_UCOD <- all_deaths_UCOD[order(all_deaths_UCOD$rate),]
  all_deaths_UCOD <- tail(all_deaths_UCOD,n=20)
  all_deaths_UCOD$state_county_name = factor(all_deaths_UCOD$state_county_name,levels=all_deaths_UCOD$state_county_name)
  write.csv(all_deaths_UCOD,'/Users/Jordan/Box/Opioids_CCs/Scratch/all_deaths_UCOD.csv')
  

# Graph data
  
  # Dot plot
  
  png('Opioids_CCs/Figures/Figure_(8)_UCOD.png',width=600,height=600)
  
  dot_plot <- ggplot(data = all_deaths_UCOD,
                     aes(x = state_county_name, y = rate, colour = type)) +
    geom_point() +   # Draw points
    geom_segment(aes(x = state_county_name,
                     xend = state_county_name,
                     y = 0,
                     yend = max(rate)),
                 linetype="dashed",
                 size=0.1) +   # Draw dashed lines
    labs(x = 'County',
         y = 'Unadjusted Mortality Rates') +
    scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#000000")) +
    coord_flip() +
    theme_classic() + 
    theme(legend.justification=c(1,1), legend.position=c(1,0.15)) + theme(legend.title=element_blank())
  
  dot_plot
  
  dev.off()





# Clean data

  # all_deaths
  all_deaths_UCOD_CC <- subset(all_deaths, type=='Drug-Related Disorders UCOD and CC')
  all_deaths_UCOD_CC <- all_deaths_UCOD_CC[order(all_deaths_UCOD_CC$rate),]
  all_deaths_UCOD_CC <- tail(all_deaths_UCOD_CC,n=20)
  all_deaths_UCOD_CC$state_county_name = factor(all_deaths_UCOD_CC$state_county_name,levels=all_deaths_UCOD_CC$state_county_name)
  write.csv(all_deaths_UCOD_CC,'/Users/Jordan/Box/Opioids_CCs/Scratch/all_deaths_UCOD_CC.csv')
  

# Graph data
  
  # Dot plot
  
  png('Opioids_CCs/Figures/Figure_(8)_UCOD_and_CC.png',width=600,height=600)
  
  dot_plot <- ggplot(data = all_deaths_UCOD_CC,
                     aes(x = state_county_name, y = rate, colour = type)) +
    geom_point() +   # Draw points
    geom_segment(aes(x = state_county_name,
                     xend = state_county_name,
                     y = 0,
                     yend = max(rate)),
                 linetype="dashed",
                 size=0.1) +   # Draw dashed lines
    labs(x = 'County',
         y = 'Unadjusted Mortality Rates') +
    scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#000000")) +
    coord_flip() +
    theme_classic() + 
    theme(legend.justification=c(1,1), legend.position=c(1,0.15)) + theme(legend.title=element_blank())
  
  dot_plot
  
  dev.off()



