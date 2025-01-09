
# Script to Create Nationality Frequency Table ----------------------------

# Load packages
library("flextable")
library("officer")
library("dplyr")

# Read in the finalized data
nationality_data <- read.csv(file = "data/data_clean.csv")

# Calculate frequencies and percentages
nationality_freq_table <- nationality_data %>%
  count(nationality) %>%
  mutate(percentage = round(n / sum(n) * 100, 2))

# Create the flextable
nationality_apa_table <- nationality_freq_table %>%
  flextable() %>%
  set_header_labels(
    nationality = "Nationality",
    n = "Frequency",
    percentage = "Percentage (%)"
  ) %>%
  autofit() %>%
  theme_box() # Optional for a clean APA-like style

# Create a Word document and add the table
nationality_table_doc <- read_docx() %>%
  body_add_flextable(nationality_apa_table) %>%
  body_add_par(value = "Note: This table shows the frequency and percentage of nationalities.", style = "Normal")

# Save the document
print(nationality_table_doc, target = "tables/nationality_table.docx")