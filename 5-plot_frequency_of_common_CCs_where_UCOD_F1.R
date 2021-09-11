# Jordan Pappas, Andrew Boslett, Elaine Hill, Alina Denham, Meredrith Adams
# University of Rochester Medical Center, Hill Lab for Health Economics
# jordan.pappas@rochester.edu



# (5) What are the top-15 most common contributing causes of death with non-drug overdose deaths with a F1* underlying cause of death? Same as above for timelines.





# Load data

all_deaths <- readRDS("Opioids_CCs/Scratch/Deaths_with_F1_code.rds")


# Clean data

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
  
  
  
  all_deaths %>% select(record_2:record_20) %>% t %>% c %>% unique 
  
  
  # record_2:record_20 unique values -> dummy variables
  # sum dummy variable columns and arrange by size
  
  
  
  
  temp <- all_deaths %>%
    filter(F1_all == 1 & str_detect(string = ucod, pattern = '^F1') == TRUE) %>%
    group_by(ucod) %>%
    summarise(n_n = n()) %>%
    ungroup()
  
  temp %<>% arrange(-n_n) %>%
    filter(row_number() <= 25)
  
  temp %<>% arrange(n_n) %>% mutate(ucod = factor(ucod, unique(ucod)))


# Plot data

png('Opioids_CCs/Figures/Figure_(5)_Frequency_of_Common_CCs_F1_UCOD_from_2015-2019.png',width=600,height=600)

dot_plot <- ggplot(data = temp,
                   aes(x = ucod, y = n_n)) +
  geom_point(col="dodgerblue", size=3) +   # Draw points
  geom_segment(aes(x = ucod,
                   xend = ucod,
                   y = 0,
                   yend = max(n_n)),
                   linetype="dashed",
                   size=0.1) +   # Draw dashed lines
  labs(x = 'Contributing Cause of Death',
       y = '# of Deaths with Drug-Related Disorder as UCOD (F1*)') +
  coord_flip() + 
  scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
                      #breaks=c("rate"),
                      labels=c("F1* UCOD")) +
  theme_classic() + 
  theme(legend.justification=c(0,1), legend.position=c(0,1), legend.title=element_blank(), 
        axis.text.x = element_text(size = 20), 
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

dot_plot

dev.off()


