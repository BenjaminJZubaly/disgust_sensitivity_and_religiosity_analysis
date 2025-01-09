
# Create Correlation Table ------------------------------------------------

# Bring the data in
source("code/data_cleaning.R")

# Load packages
library(psych)
library(apaTables)

# Creating the GENE_p_2 variable as the sum of items 2 and 3
data <- data %>% 
  mutate(
    GENE_p_2 = rowSums(select(., c(GENE.2, GENE.3)))
  )

# Calculate correlations for numerical variables
apa.cor.table(data = data %>% select("religious_importance",
                                     "CRS",
                                     "TDDS_p",
                                     "TDDS_s",
                                     "SOI_a",
                                     "GENE_p",
                                     "GENE_p_2",
                                     "CONV_f"),
              filename = "tables/correlation_table.doc")

