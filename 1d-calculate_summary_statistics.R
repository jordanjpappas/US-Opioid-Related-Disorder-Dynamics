# Jordan Pappas, Andrew Boslett, Elaine Hill, Alina Denham, Meredrith Adams
# University of Rochester Medical Center, Hill Lab for Health Economics
# jordan.pappas@rochester.edu



# Summ Stats

for (year in c(1,2)){
  rds_means <- data.frame(0,1:28)
  for (vector in c("F1","F11")){
    
    # Load data
  rds <- readRDS(paste0("Opioids_CCs/Data/",vector,"_rds"))
    # Subset data based on year
    if (year == 1) {
      rds <- subset(rds, year<=1999 | year>=2019)
    } else {
      rds <- subset(rds, year<=2014 | year>=2019)
    }
  rds_binary <- rds[apply(rds,2,function(x) { all(x %in% 0:1) }) == TRUE]
  rds <- rds_binary
    # Clean data
  rds[is.na(rds)] <- 0
    # Calculate means of selected variables
  temp <- colMeans(rds)
  temp <- as.data.frame(temp)
    # Rename column as vector group
  names(temp)[names(temp) == "temp"] <- vector
    # Bind results together
  rds_means %<>% cbind(temp)
  rds_means$X0 <- NULL
  rds_means$X1.28 <- NULL
  rds_means <- round(rds_means,3)
  }
    # Saves 2 tables, 1 table for each year range
  saveRDS(rds_means,paste0("Opioids_CCs/Data/rds_means_",year))
  
}



# Read in RDS
rds <- readRDS(paste0("Opioids_CCs/Data/F1_rds"))
rds_means_1 <- readRDS("Opioids_CCs/Data/rds_means_1")
rds_means_2 <- readRDS("Opioids_CCs/Data/rds_means_2")



