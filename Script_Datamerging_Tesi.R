rm(list=ls()) 
## ------------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

listofpackages <- c("ellipse","reshape2","ggplot2","dygraphs", "dplyr","readr", "Hmisc", "rvest", "readxl", "psych")

for (j in listofpackages){
  if(sum(installed.packages()[, 1] == j) == 0) {
    install.packages(j)
  }
  library(j, character.only = T)
}



# Load necessary library
library(dplyr)

# Define the path to the folder containing the CSV files
folder_path <- "/Users/paolodu/Desktop/Dati_Tesi_Paolo"

# List all CSV files in the folder
file_list <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)

# Function to read and return a data frame from a file
read_csv_file <- function(file) {
  read.csv(file, stringsAsFactors = FALSE)
}

# Read and merge all CSV files
merged_data <- lapply(file_list, read_csv_file) %>%
  bind_rows()

# Check the merged data
head(merged_data)



# Load the readxl package
library(readxl)


excel_file_1 <- "/Users/paolodu/Desktop/Dati_Tesi_Paolo/Sponsored_Products_adgroup_targeting_giu_25_2024 (2).xlsx"
csv_file_1 <- "/Users/paolodu/Desktop/Dati_Tesi_Paolo/Sponsored_Products_adgroup_targeting_giu_25_2024 (2).csv"
data_1 <- read_excel(excel_file_1)
write.csv(data_1, file = csv_file_1, row.names = FALSE)

excel_file_2 <- "/Users/paolodu/Desktop/Dati_Tesi_Paolo/Sponsored_Products_adgroup_targeting_giu_25_2024 (4).xlsx"
csv_file_2 <- "/Users/paolodu/Desktop/Dati_Tesi_Paolo/Sponsored_Products_adgroup_targeting_giu_25_2024 (4).csv"
data_2 <- read_excel(excel_file_2)
write.csv(data_2, file = csv_file_2, row.names = FALSE)

excel_file_3 <- "/Users/paolodu/Desktop/Dati_Tesi_Paolo/Sponsored_Products_adgroup_targeting_giu_25_2024 (12).xlsx"
csv_file_3 <- "/Users/paolodu/Desktop/Dati_Tesi_Paolo/Sponsored_Products_adgroup_targeting_giu_25_2024 (12).csv"
data_3 <- read_excel(excel_file_3)
write.csv(data_3, file = csv_file_3, row.names = FALSE)




# Load necessary library
library(dplyr)

# Define the new header
new_header <- c("Stato", "Parola chiave", "Tipo di corrispondenza", "Stato", 
                "Offerta suggerita (basso)(EUR)", "Offerta suggerita (medio)(EUR)", 
                "Offerta suggerita (alto)(EUR)", "Offerta per parola chiave(EUR)", 
                "QI in cima ai risultati di ricerca", "Impressioni", "Clic", 
                "Tasso di clic (CTR)", "Spesa(EUR)", "CPC(EUR)", "DPV", "Ordini", 
                "Vendite(EUR)", "ACOS", "ROAS", "Ordini nuovi clienti", 
                "% ord. nuovi clienti", "Vendite, nuovi clienti(EUR)", 
                "% vend. a nuovi clienti")

# Generate the sequence of numbers from 7 to 109
file_numbers <- 1:109

# Define the base path
base_path <- "/Users/paolodu/Desktop/Dati_Tesi_Paolo/Sponsored_Products_adgroup_targeting_giu_25_2024 "

# Create the list of file paths
files <- paste0(base_path, "(", file_numbers, ").csv")

# Print the list to verify
print(files)


# Function to read, modify header, and return a data frame
read_and_modify_csv <- function(file) {
  data <- read.csv(file, header = FALSE, stringsAsFactors = FALSE)
  colnames(data) <- new_header
  return(data)
}

read_and_modify_csv <- function(file) {
  # Print file name for debugging
  cat("Processing file:", file, "\n")
  
  # Try to read the file
  data <- tryCatch({
    read_csv(file, col_names = FALSE)
  }, error = function(e) {
    cat("Error reading file:", file, "\n")
    return(NULL)
  })
  
  # If data is NULL, return
  if (is.null(data)) {
    return(NULL)
  }
  
  num_cols <- ncol(data)
  cat("Number of columns in the file:", num_cols, "\n")
  
  # Check if the data has 22 columns
  if (num_cols == 22) {
    # Add an empty column after the second column
    data <- cbind(data[, 1:2], "Tipo di corrispondenza" = NA, data[, 3:num_cols])
    num_cols <- ncol(data)  # Update the number of columns
    cat("Added 'Tipo di corrispondenza' column, new number of columns:", num_cols, "\n")
  }
  
  # Ensure the number of columns matches the new header length
  if (num_cols == length(new_header)) {
    colnames(data) <- new_header
  } else {
    cat(sprintf("File %s has %d columns after adjustment, expected 23\n", file, num_cols))
    return(NULL)
  }
  
  return(data)
}



# Read and modify all CSV files
modified_data <- lapply(files, read_and_modify_csv) %>%
  bind_rows()

# Check the merged data
head(modified_data)
names(modified_data)

cleaned_data <- modified_data %>%
  filter(`Stato...1` != "State" & `Stato...1` != "Stato") %>%
  select(-`Tipo di corrispondenza`, -`Stato...4`) %>%
  mutate_at(vars(`Offerta suggerita (basso)(EUR)`:`% vend. a nuovi clienti`), function(x) as.numeric(as.character(x)))



summary(cleaned_data)
describe(cleaned_data)
write_csv(cleaned_data, file = "/Users/paolodu/Desktop/Cleaned_Data.csv")
final_data <- na.omit(cleaned_data$`Offerta per parola chiave(EUR)`)












