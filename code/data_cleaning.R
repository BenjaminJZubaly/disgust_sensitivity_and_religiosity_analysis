# Data Cleaning -----------------------------------------------------------
# This code is called at the beginning of 2024_USRA_analysis_disgust_and_religiosity.qmd,
# so there is no reason to run it on its own.

# Defining the packages to use to clean the data
pkg <- c("tidyverse")

# Loading the groundhog package to install packages from a certain date
library(groundhog)

# Reading in the packages with the groundhog package for reproducibility
  # Message suppressed in order to allow for rendering of quarto document
suppressMessages(groundhog.library(pkg = pkg, date = "2024-06-01"))

# Reading in the unclean data as data.unclean
data.unclean <- read.csv(file = "./data/prepared_data.csv")

# Format data types
  # IDs as strings
data.unclean$ID <- as.character(data.unclean$ID)
  # Start and finish time as date format
data.unclean <- data.unclean %>%
  mutate(start_time = parse_date_time(start_time, orders = "mdy HM", tz = "UTC")) %>%
  mutate(start_time = as.POSIXct(start_time, origin = "1970-01-01", tz = "UTC"))
data.unclean <- data.unclean %>%
  mutate(finish_time = parse_date_time(finish_time, orders = "mdy HM", tz = "UTC")) %>%
  mutate(finish_time = as.POSIXct(finish_time, origin = "1970-01-01", tz = "UTC"))

# Calculate age (drop down started at 18)
data.unclean$age <- data.unclean$age + 17

# Code sex variable as factor
  # Make them the right numbers
data.unclean$sex <- data.unclean$sex - 1
  # Make it a factor
data.unclean$sex <- factor(data.unclean$sex, 
                           levels = c("0", "1"),
                           labels = c("Male", "Female"))

# Turning gender responses into standardized labels
data.unclean <- data.unclean %>%
  mutate(
    gender = str_trim(gender),  # Remove leading and trailing spaces
    gender = str_to_lower(gender),  # Convert all to lowercase
    gender = case_when(
      gender %in% c("male", "man", "masculine", "xy-male", "boy", "men", "answered above already") ~ "Male",
      gender %in% c("female", "woman", "she/her") ~ "Female",
      gender %in% c("non-binary", "non binary", "nonbinary") ~ "Non-binary",
      gender %in% c("non-binary (agender)", "agender") ~ "Agender",
      gender %in% c("gender fluid") ~ "Gender Fluid",
      gender %in% c("queer") ~ "Gender Queer",
      TRUE ~ NA_character_  # Assign NA to any unclassified responses
    )
  )
  # Make it a factor
data.unclean$gender <- as.factor(data.unclean$gender)

# Combine ethnicity columns
data.unclean$ethnicity <- as.character(data.unclean$ethnicity)
  # Put "Other" responses in the ethnicity column
data.unclean <- data.unclean %>%
  mutate(
    ethnicity = ifelse(ethnicity_other != "", ethnicity_other, ethnicity)
  )
  # Remove the ethnicity_other column
data.unclean <- data.unclean %>% 
  select(-ethnicity_other)
  # Put in the right labels and make factor
data.unclean$ethnicity <- factor(data.unclean$ethnicity, 
                                             levels = c("1", 
                                                        "2", 
                                                        "3",
                                                        "4",
                                                        "5",
                                                        "6",
                                                        "7",
                                                        "8",
                                                        "9",
                                                        "10",
                                                        "11",
                                                        "12",
                                                        "13",
                                                        "14",
                                                        "15",
                                                        "16",
                                                        "Indian",
                                                        "NZ Māori / Pākeha (European) mixed",
                                                        "Asian-Mexican"), 
                                             labels = c("African",
                                                        "Black or African American",
                                                        "Caribbean",
                                                        "East Asian",
                                                        "Latino or Hispanic",
                                                        "Middle Eastern",
                                                        "Mixed",
                                                        "Native American or Alaskan Native",
                                                        "South Asian",
                                                        "White or Causasian",
                                                        "White or Sapharic Jew",
                                                        "Black British",
                                                        "White Mexican",
                                                        "Romani or Traveller",
                                                        "South East Asian",
                                                        "Rather Not Say",
                                                        "South Asian",
                                                        "Mixed",
                                                        "Mixed"))

# Combine nationality columns
data.unclean$nationality <- as.character(data.unclean$nationality)
# Put "Other" responses in the ethnicity column
data.unclean <- data.unclean %>%
  mutate(
    nationality = ifelse(nationality_other != "", nationality_other, nationality)
  )
  # Remove the nationality_other column
data.unclean <- data.unclean %>% 
  select(-nationality_other)
  # Standardize labels
data.unclean <- data.unclean %>%
  mutate(nationality = str_to_lower(nationality)) %>%
  mutate(nationality = str_trim(nationality)) %>%
  mutate(nationality = case_when(
    nationality %in% c("1") ~ "United Kingdom", 
    nationality %in% c("2") ~ "United States", 
    nationality %in% c("3") ~ "Ireland", 
    nationality %in% c("4") ~ "Germany", 
    nationality %in% c("5") ~ "France", 
    nationality %in% c("6") ~ "Spain", 
    nationality %in% c("7") ~ "Canada", 
    nationality %in% c("8") ~ "Mexico", 
    nationality %in% c("9") ~ "Italy",
    nationality %in% c("chile", "chilean") ~ "Chile",
    nationality %in% c("portugal", "portuguese") ~ "Portugal",
    nationality %in% c("south africa", "south african") ~ "South Africa",
    nationality %in% c("greece", "greek") ~ "Greece",
    nationality %in% c("czech republic", "czechia", "czech") ~ "Czech Republic",
    nationality %in% c("australia", "australian") ~ "Australia",
    nationality %in% c("kenya", "kenyan") ~ "Kenya",
    nationality %in% c("brazil", "brazilian") ~ "Brazil",
    nationality %in% c("sweden", "swedish") ~ "Sweden",
    nationality %in% c("poland") ~ "Poland",
    nationality %in% c("austria") ~ "Austria",
    nationality %in% c("romania") ~ "Romania",
    nationality %in% c("israel", "israeli") ~ "Israel",
    nationality %in% c("nz european", "new zealand", "new zealander") ~ "New Zealand",
    nationality %in% c("belgium") ~ "Belgium",
    nationality %in% c("columbian", "colombian", "colombia") ~ "Columbia",
    nationality %in% c("zimbabwe") ~ "Zimbabwe",
    nationality %in% c("turkey") ~ "Turkey",
    nationality %in% c("eritrea") ~ "Eritrea",
    nationality %in% c("slovakia") ~ "Slovakia",
    nationality %in% c("egypt") ~ "Egypt",
    nationality %in% c("tunisian") ~ "Tunisia",
    nationality %in% c("india") ~ "India",
    nationality %in% c("hungarian", "hungary") ~ "Hungary",
    nationality %in% c("saudi arabja") ~ "Saudi Arabia",
    nationality %in% c("chinese") ~ "China",
    nationality %in% c("portugual") ~ "Portugal",
    nationality %in% c("finnish", "finland") ~ "Finland",
    nationality %in% c("swiss", "Swiss") ~ "Sweden",
    nationality %in% c("vietnamese") ~ "Vietnam",
    nationality %in% c("algeria") ~ "Algeria",
    nationality %in% c("japanese") ~ "Japan",
    nationality %in% c("venezuelan") ~ "Venezuela",
    nationality %in% c("ghanaian") ~ "Ghana",
    TRUE ~ nationality # Keep the original if no match
  ))
  # Make it a factor
data.unclean$nationality <- as.factor(data.unclean$nationality)

# If a person wrote in Catholic or Roman Catholic as their religious affiliation, write this in as their Christian affiliation
data.unclean <- data.unclean %>% 
  mutate(
    christian_affiliation = NA_character_
  ) %>% 
  mutate(
    christian_affiliation = case_when(
      rel_aff_other %in% c("Catholic", "Roman Catholic") ~ "Catholic",
      TRUE ~ christian_affiliation # Set to NA if otherwise
    )
  )

# Combine religious affiliation columns
data.unclean$religious_affiliation <- as.character(data.unclean$religious_affiliation)
  # Put "Other" responses in the religious_affiliation column
data.unclean <- data.unclean %>%
  mutate(
    religious_affiliation = ifelse(rel_aff_other != "", rel_aff_other, religious_affiliation)
  )
# Remove the rel_aff_other column
data.unclean <- data.unclean %>% 
  select(-rel_aff_other)
# Standardize labels
data.unclean <- data.unclean %>%
  mutate(religious_affiliation = str_to_lower(religious_affiliation)) %>%
  mutate(religious_affiliation = str_trim(religious_affiliation)) %>%
  mutate(religious_affiliation = case_when(
    religious_affiliation %in% c("1") ~ "Christian", 
    religious_affiliation %in% c("2") ~ "Muslim", 
    religious_affiliation %in% c("3") ~ "Hindu", 
    religious_affiliation %in% c("4") ~ "Buddhist", 
    religious_affiliation %in% c("5") ~ "Buddhist", 
    religious_affiliation %in% c("6") ~ "None", 
    religious_affiliation %in% c("anti-religions") ~ "Anti-religious",
    religious_affiliation %in% c("agnostic", "agnostic ") ~ "Agnostic",
    religious_affiliation %in% c("atheist") ~ "Atheist",
    religious_affiliation %in% c("catholic", "roman catholic") ~ "Christian",
    religious_affiliation %in% c("spiritual", "spiritual ") ~ "Spiritual",
    religious_affiliation %in% c("spiritualist") ~ "Spiritualist",
    religious_affiliation %in% c("deism") ~ "Deist",
    religious_affiliation %in% c("pagan") ~ "Pagan",
    TRUE ~ religious_affiliation # Keep the original if no match
  ))
# Make it a factor
data.unclean$religious_affiliation <- as.factor(data.unclean$religious_affiliation)

# Collapse Christian affiliation into one variable
data.unclean <- data.unclean %>%
  mutate(christian_affiliation = case_when(
    ca_catholic == 1 ~ "Catholic",
    ca_protestant == 2 ~ "Protestant",
    ca_orthodox == 3 ~ "Orthodox",
    ca_pentecostal == 4 ~ "Pentecostal",
    ca_lutheran == 5 ~ "Lutheran",
    ca_methodist == 6 ~ "Methodist",
    ca_anglican == 7 ~ "Anglican",
    ca_baptist == 8 ~ "Baptist",
    ca_presbyterian == 9 ~ "Presbyterian",
    TRUE ~ christian_affiliation  # Handle cases where none are selected
  ))
  # Integrate the write-ins
data.unclean <- data.unclean %>%
  mutate(
    christian_affiliation = ifelse(ca_other != "", ca_other, christian_affiliation)
  )
  # Consolodate the categories
data.unclean <- data.unclean %>%
  mutate(christian_affiliation = str_to_lower(christian_affiliation)) %>%
  mutate(christian_affiliation = str_trim(christian_affiliation)) %>%
  mutate(christian_affiliation = case_when(
    christian_affiliation %in% c("catholic") ~ "Catholic",
    christian_affiliation == "protestant" ~ "Protestant",
    christian_affiliation == "lutheran" ~ "Lutheran",
    christian_affiliation %in% c("evangelical", "grace community life", "full gospel", "new creation church christian !") ~ "Evangelical",
    christian_affiliation == "congregationalist" ~ "Congregationalist",
    christian_affiliation == "apostolic" ~ "Apostolic",
    christian_affiliation == "non religious christian" ~ "Non-religious Christian",
    christian_affiliation == "orthodox" ~ "Orthodox",
    christian_affiliation == "seventh day adventist" ~ "Seventh Day Adventist",
    christian_affiliation == "jehovah’s witness" ~ "Jehovah’s Witness",
    christian_affiliation == "pentecostal" ~ "Pentecostal",
    christian_affiliation == "baptist" ~ "Baptist",
    christian_affiliation == "methodist" ~ "Methodist",
    christian_affiliation == "presbyterian" ~ "Presbyterian",
    christian_affiliation == "anglican" ~ "Anglican",
    christian_affiliation %in% c("none", "i don’t know", "nondenominational", "christian") ~ "None",
    TRUE ~ christian_affiliation # Keep the original if no match
  ))
  # Make it a factor
data.unclean$christian_affiliation <- as.factor(data.unclean$christian_affiliation)
  # Get rid of the other variables
data.unclean <- data.unclean %>%
  select(-starts_with("ca_"))
  # Reorder so christian_affiliation is in a logical spot
data.unclean <- data.unclean %>%
  select(ID, start_time, finish_time, age, sex, gender, ethnicity, nationality,
         religious_affiliation, christian_affiliation, religious_importance, everything())

# Reverse the item scores of the CRS, so that higher scores indicate more religiosity
data.unclean <- data.unclean %>% 
  mutate(
    CRS.1 = 6 - CRS.1,
    CRS.2 = 6 - CRS.2,
    CRS.3 = 8 - CRS.3,
    CRS.4 = 8 - CRS.4,
    CRS.5 = 6 - CRS.5
  )

# Manipulate raw CRS scale item scores to fit scoring instructions
data.unclean <- data.unclean %>% 
  mutate(
    CRS.3 = recode(CRS.3,
                   `1` = 1,
                   `2` = 2,
                   `3` = 3,
                   `4` = 4,
                   `5` = 4,
                   `6` = 5,
                   `7` = 5),
    CRS.4 = recode(CRS.4,
                   `1` = 1,
                   `2` = 2,
                   `3` = 3,
                   `4` = 4,
                   `5` = 4,
                   `6` = 5,
                   `7` = 5)
  )

# Calculate the full CRS score
data.unclean <- data.unclean %>% 
  mutate(
    CRS = rowMeans(select(., starts_with("CRS")))
  )

# Adjust the range of TDDS items from 1-6 to 0-5
data.unclean <- data.unclean %>%
  mutate(across(starts_with("TDDS"), ~ . - 1))

# Calculate scores for the TDDS full and subscales
data.unclean <- data.unclean %>% 
  mutate(
    TDDS_f = rowSums(select(., starts_with("TDDS"))),
    TDDS_p = rowSums(select(., c("TDDS.12", "TDDS.15", "TDDS.9", "TDDS.3", "TDDS.21", "TDDS.18", "TDDS.6"))),
    TDDS_s = rowSums(select(., c("TDDS.8", "TDDS.5", "TDDS.20", "TDDS.2", "TDDS.14", "TDDS.14", "TDDS.11", "TDDS.17"))),
    TDDS_m = rowSums(select(., c("TDDS.4", "TDDS.19", "TDDS.13", "TDDS.10", "TDDS.7", "TDDS.1", "TDDS.16")))
  )

# Reverse the only SOI-R reverse scored item
data.unclean <- data.unclean %>% 
  mutate(
    SOI.6 = 10 - SOI.6
  )

# Calculate scores for the SOI-R and subscales
data.unclean <- data.unclean %>% 
  mutate(
    SOI_f = rowMeans(select(., starts_with("SOI"))),
    SOI_b = rowMeans(select(., c("SOI.1", "SOI.2", "SOI.3"))),
    SOI_a = rowMeans(select(., c("SOI.4", "SOI.5", "SOI.6"))),
    SOI_d = rowMeans(select(., c("SOI.7", "SOI.8", "SOI.9")))
  )

# Reverse score items 5 and 6 for the GENE
data.unclean <- data.unclean %>% 
  mutate(
    GENE.5 = 6 - GENE.5,
    GENE.6 = 6 - GENE.6
  )

# Score the GENE and subscales
data.unclean <- data.unclean %>% 
  mutate(
    GENE_f = rowSums(select(., starts_with("GENE"))),
    GENE_p = rowSums(select(., c("GENE.1", "GENE.2", "GENE.3"))),
    GENE_s = rowSums(select(., c("GENE.4", "GENE.5", "GENE.6", "GENE.7")))
  )

# Reverse score the CONV and TRAD items
data.unclean <- data.unclean %>%
  mutate(
    CONV.1 = 6 - CONV.1, # People emphasize tradition too much.
    CONV.4 = 6 - CONV.4, # Traditions interfere with progress.
    CONV.5 = 6 - CONV.5, # People should challenge social traditions in order to advance society.
    TRAD.1 = 6 - TRAD.1, # Society should not resist innovation.
    TRAD.3 = 6 - TRAD.3  # People are too often stuck in the ways of the past.
  )

# Calculate the CONV score
data.unclean <- data.unclean %>%
  mutate(
    CONV = rowSums(select(., CONV.1, CONV.2, CONV.3, CONV.4, CONV.5, CONV.6), na.rm = TRUE)
  )

# Calculate the TRAD score
data.unclean <- data.unclean %>%
  mutate(
    TRAD = rowSums(select(., TRAD.1, TRAD.2, TRAD.3, TRAD.4, TRAD.5, TRAD.6), na.rm = TRUE)
  )

# Calculate the TRAD_f score by summing both CONV and TRAD items
data.unclean <- data.unclean %>%
  mutate(
    CONV_f = rowSums(select(., CONV.1, CONV.2, CONV.3, CONV.4, CONV.5, CONV.6,
                                  TRAD.1, TRAD.2, TRAD.3, TRAD.4, TRAD.5, TRAD.6), na.rm = TRUE)
  )

# Remove observations with NA values for crutial variables
data.unclean <- data.unclean %>%
  filter(complete.cases(CRS, TDDS_p, SOI_a, GENE_p, CONV_f))

# Make the data frame data instead of data.unclean
data <- data.unclean

# Remove everything but the clean data from the environment
rm(data.unclean, pkg)

write.csv(data, file = "./data/data_clean.csv")


