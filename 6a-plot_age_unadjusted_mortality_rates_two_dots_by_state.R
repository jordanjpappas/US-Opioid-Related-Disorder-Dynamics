# Jordan Pappas, Andrew Boslett, Elaine Hill, Alina Denham, Meredrith Adams
# University of Rochester Medical Center, Hill Lab for Health Economics
# jordan.pappas@rochester.edu



# (6a) A messy but potentially interesting figure: state-by-state dot-plots for 2019 with mortality rates (unadjusted, for now) per 100,000 people for 
  # (a) drug overdoses; and 
  # (b) drug-related disorders? I think we want two versions of this because I'm not sure which is best: one version where (b) is measured with only those deaths with F1* underlying causes of death; and another version where (b) includes those with F1* contributing causes of death.





all_deaths <- data.frame()

for (dataset in c('Drug-Related Disorders','Drug Overdoses','Drug-Related Disorders UCOD','Drug-Related Disorders UCOD and CC')){
  
  # Load data
  if (dataset == "Drug-Related Disorders") {
    deaths <- readRDS("Opioids_CCs/Scratch/Deaths_with_F1_code.rds")
  }
  else if (dataset == "Drug-Related Disorders UCOD") {
    deaths <- readRDS("Opioids_CCs/Scratch/Deaths_with_F1_code.rds")
  }
  else if (dataset == "Drug-Related Disorders UCOD and CC") {
    deaths <- readRDS("Opioids_CCs/Scratch/Deaths_with_F1_code.rds")
  }
  else if (dataset == "Drug Overdoses") {
    deaths <- readRDS("Opioids_CCs/Scratch/Overdoses.rds")
  }
  state_to_fips <- read_excel("Opioids_CCs/Data/USCB_State_FIPS_Codes_2019.xlsx", col_names = TRUE)
  us_pop <- read_fwf('Opioids_CCs/Data/us.1969_2019.19ages.adjusted.txt',
                     col_positions = fwf_widths(
                       c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8),
                       c('year', 'state', 'state_fips', 'county_fips',
                         'registry', 'race', 'origin', 'sex', 'age', 
                         'population')))
  
  # Clean data
  
  # Clean deaths
  
  # Filter out F1 CCs for drug overdoses and F11 CCs for opioid overdoses
  if (dataset == "Drug-Related Disorders") {
    deaths %<>% mutate_at(vars(F110:F154, F135:F116),
                          funs(ifelse(is.na(.) == TRUE, 0, .)))
    
    deaths %>% select(starts_with('F1')) %>%
      rowSums() -> deaths$F1_all
    deaths %<>% mutate(F1_all = ifelse(F1_all > 0, 1, 0))
    deaths <- deaths %>% filter(F1_all == 1)
  }
    
    else if (dataset == "Drug-Related Disorders UCOD") {
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
    
  else if (dataset == "Drug Overdoses") {
    print("pass")
  } 
  
  
  # Create state column
  deaths$state <- NA
  deaths$state <- substr(deaths$county_fips_occ, 0, 2)
  temp <- deaths
  # Select relevant variables in drug overdoses database
  temp %<>% select(year, state)
  # Summarise drug overdoses by year and state
  temp_summary <- temp %>% 
    group_by(year, state) %>%
    summarise(n = n())
  # Resummarise using new definition 
  temp_summary %<>%
    group_by(year, state) %>%
    summarise_at(vars(contains('n')),
                 funs(sum(., na.rm = TRUE))) %>%
    ungroup()
  
  
  # Clean state_to_fips
  # Pad 0s on FIPS column
  state_to_fips$`FIPS State Numeric Code` <- str_pad(state_to_fips$`FIPS State Numeric Code`, 2, side="left",0)
  
  # Clean us_pop
  # Make population numeric
  us_pop %<>% mutate_at(vars(population),
                        funs(as.numeric(.)))
  # Summarise US population by year and state
  pop_summary <- us_pop %>% 
    group_by(year, state_fips) %>%
    summarise_at(vars(population),
                 funs(sum(., na.rm = TRUE))) %>%
    ungroup()
  
  
  # Merge data
  # Merge population data to drug overdoses data
  temp_summary <- 
    pop_summary %<>% left_join(temp_summary, by = c('year', 
                                                    'state_fips' = 'state'))
  
  # Clean data
  # Clean merged
  # Rearrange data
  temp_summary %<>% arrange(state_fips, year)
  # Add total population data for each year
  temp_summary %<>% group_by(year, state_fips) %>%
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
  temp_summary %>% group_by(year, state_fips) %>%
    summarise_at(vars(contains('rate')),
                 funs(sum(., na.rm = TRUE))) %>%
    ungroup() -> state_rates
  state_rates <- subset(state_rates, 1999 <= year & year <= 2019)
  state_rates <- merge(state_rates, state_to_fips, by.x='state_fips', by.y='FIPS State Numeric Code')
  state_rates <- state_rates[order(state_rates$rate),]
  #state_rates$`Official USPS Code` = factor(state_rates$`Official USPS Code`,levels=state_rates$`Official USPS Code`)
  print("test")
  state_rates$type <- dataset
  
  
  
  # Bind data
  
  all_deaths %<>% bind_rows(state_rates)
  
  print(dataset)
  
}



# Clean data

  # all_deaths
  all_deaths <- all_deaths[order(all_deaths$rate),]

  disorder_deaths <- subset(all_deaths, type=='Drug-Related Disorders')
  overdose_deaths <- subset(all_deaths, type=='Drug Overdoses')
  
  #all_deaths$`Official USPS Code` = factor(all_deaths$`Official USPS Code`,levels=all_deaths$`Official USPS Code`)

# Graph data
  # Dot plot
  
  png('Opioids_CCs/Figures/Figure_(6a)_disorders.png',width=600,height=600)
  
  dot_plot <- ggplot(data = all_deaths,
                     aes(x = `Official USPS Code`, y = rate, colour=type)) +
    geom_point() +   # Draw points
    geom_segment(aes(x = `Official USPS Code`,
                     xend = `Official USPS Code`,
                     y = 0,
                     yend = max(rate)),
                     linetype="dashed",
                     size=0.1) +   # Draw dashed lines
    labs(x = 'State',
         y = 'Unadjusted Mortality Rates') +
    scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#000000")) +
    coord_flip() +
    theme_classic() +
    theme(plot.margin=unit(c(1,4,1,1),"cm")) +
    theme(legend.justification=c(.15,.15), legend.position=c(1,0.15)) + theme(legend.title=element_blank())
  
  dot_plot
  
  dev.off()

  
# Save data
  
  # all_deaths
  all_deaths_UCOD <- subset(all_deaths, type=='Drug-Related Disorders UCOD')
  all_deaths_UCOD <- all_deaths_UCOD[order(all_deaths_UCOD$rate),]
  #all_deaths_UCOD <- tail(all_deaths_UCOD,n=20)
  #all_deaths_UCOD$state_county_name = factor(all_deaths_UCOD$state_county_name,levels=all_deaths_UCOD$state_county_name)
  write.csv(all_deaths_UCOD,'/Users/Jordan/Box/Opioids_CCs/Scratch/all_deaths_UCOD_state.csv')
  
  # all_deaths
  all_deaths_UCOD_CC <- subset(all_deaths, type=='Drug-Related Disorders UCOD and CC')
  all_deaths_UCOD_CC <- all_deaths_UCOD_CC[order(all_deaths_UCOD_CC$rate),]
  #all_deaths_UCOD_CC <- tail(all_deaths_UCOD_CC,n=20)
  #all_deaths_UCOD_CC$state_county_name = factor(all_deaths_UCOD_CC$state_county_name,levels=all_deaths_UCOD_CC$state_county_name)
  write.csv(all_deaths_UCOD_CC,'/Users/Jordan/Box/Opioids_CCs/Scratch/all_deaths_UCOD_CC_state.csv')
  
  # all_deaths
  overdose_deaths <- subset(all_deaths, type=='Drug Overdoses')
  overdose_deaths <- overdose_deaths[order(overdose_deaths$rate),]
  #overdose_deaths <- tail(overdose_deaths,n=20)
  #overdose_deaths$state_county_name = factor(overdose_deaths$state_county_name,levels=overdose_deaths$state_county_name)
  write.csv(overdose_deaths,'/Users/Jordan/Box/Opioids_CCs/Scratch/all_deaths_overdose_state.csv')
  