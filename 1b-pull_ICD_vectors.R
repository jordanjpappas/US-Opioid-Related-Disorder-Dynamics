# Jordan Pappas, Andrew Boslett, Elaine Hill, Alina Denham, Meredrith Adams
# University of Rochester Medical Center, Hill Lab for Health Economics
# jordan.pappas@rochester.edu



# Load data

my_data <- read.delim("Opioids_CCs/Data/icd10cm_codes_2021.txt", header = FALSE, sep = " ")
# Concat and subset description columns
my_data$description <- unite(my_data[,2:12],'description')
my_data <- my_data[,c(1,13)]

avector <- as.vector(unlist(my_data$description))
aframe <- data.frame(my_data, avector)

aframe$description <- NULL
# Make sure all ICD-10 codes are the same length
aframe$V1 <- stri_sub(aframe$V1, 1, -3)
aframe$V1 <- stri_pad_right(aframe$V1, 4, 0)




# Create ICD-10 vector given prefixes and keywords
# F1
code_prefix_1 <- "F1"
description_keyword_remove_1 <- "Alcohol"
description_keyword_remove_2 <- "Nicotine"
description_keyword_remove_3 <- "Tobacco"

aframe_F1 <- subset(aframe, grepl(code_prefix_1, aframe$V1) == TRUE)
aframe_F1 <- subset(aframe_F1, grepl(description_keyword_remove_1, aframe_F1$avector) == FALSE & grepl(tolower(description_keyword_remove_1), aframe_F1$avector) == FALSE & grepl(description_keyword_remove_2, aframe_F1$avector) == FALSE & grepl(tolower(description_keyword_remove_2), aframe_F1$avector) == FALSE & grepl(description_keyword_remove_3, aframe_F1$avector) == FALSE & grepl(tolower(description_keyword_remove_3), aframe_F1$avector) == FALSE)

# Save ICD-10 codes vector
F1_vector <- as.vector(unique(aframe_F1$V1))



# F11
code_prefix_1 <- "F11"
description_keyword_keep_1 <- "Opioid"

aframe_F11 <- subset(aframe, grepl(code_prefix_1, aframe$V1) == TRUE)
aframe_F11 <- subset(aframe_F11, grepl(description_keyword_keep_1, aframe_F11$avector) == TRUE | grepl(tolower(description_keyword_keep_1), aframe_F11$avector) == TRUE)

# Save ICD-10 codes vector
F11_vector <- as.vector(unique(aframe_F11$V1))



# Suicide
code_prefix_1 <- "T1"
code_prefix_2 <- "Y3"
description_keyword_keep_1 <- "Suicide"

aframe_1 <- subset(aframe, grepl(code_prefix_1, aframe$V1) == TRUE)
aframe_2 <- subset(aframe, grepl(code_prefix_2, aframe$V1) == TRUE)
aframe_suicide <- bind_rows(aframe_1,aframe_2)
aframe_suicide <- subset(aframe_suicide, grepl(description_keyword_keep_1, aframe_suicide$avector) == TRUE | grepl(tolower(description_keyword_keep_1), aframe_suicide$avector) == TRUE)

# Save ICD-10 codes vector
suicide_vector <- as.vector(unique(aframe_suicide$V1))



# Drugs
description_keyword_keep_1 <- "Drug"

aframe_drug <- subset(aframe, grepl(description_keyword_keep_1, aframe$avector) == TRUE | grepl(tolower(description_keyword_keep_1), aframe$avector) == TRUE)

drug_list <- unique(aframe_drug)
# Save ICD-10 codes data frame
write.csv(drug_list,'Opioids_CCs/Data/drug_list.csv')







# Pull descriptions from given ICD-10 vector
#fig_6_7_ucod_vector <- c(fig_6_ucod_vector, fig_7_ucod_vector)
#fig_6_7_ucod_vector <- unique(fig_6_7_ucod_vector)

#fig_6_7_df <- subset(aframe, aframe$V1 %in% fig_6_7_ucod_vector)





