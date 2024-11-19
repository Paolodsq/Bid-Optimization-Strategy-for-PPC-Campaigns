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
dataset <- read.csv("/Users/paolodu/Desktop/Dataset_pulito.csv", sep = ";")

###########################
#FEATURE ENGINEERING
###########################

#1. Target Type
dataset <- dataset %>%
  mutate(`Target Type` = NA) %>%  # Crei la nuova colonna "Target Type"
  relocate(`Target Type`, .after = 3)

dataset <- dataset %>%
  mutate(`Target Type` = ifelse(grepl("PT", `Campaign.Name`), "prodotto", "keyword"))

count_prodotto <- sum(dataset$`Target Type` == "prodotto")
count_prodotto

describe(dataset)
summary(dataset)

unique_values <- unique(dataset$`Campaign.Name`)
print(unique_values)

#2. Targeting
dataset <- dataset %>%
  mutate(`Targeting` = NA) %>%  
  relocate(`Targeting`, .after = 2)

dataset <- dataset %>%
  mutate(`Targeting` = ifelse(grepl("Auto", `Campaign.Name`), "Auto", "Manual"))

#3. Strategia
dataset <- dataset %>%
  mutate(`Strategia` = NA) %>%  
  relocate(`Strategia`, .after = 2)

dataset <- dataset %>%
  mutate(`Strategia` = ifelse(grepl("Brand", `Campaign.Name`), "Brand Protection", "Performance"))


#4. Tool Automazione
dataset <- dataset %>%
  mutate(`Tool Automazione` = NA) %>%  
  relocate(`Tool Automazione`, .after = 7)

dataset <- dataset %>%
  mutate(`Tool Automazione` = ifelse(grepl("TN|Perpetua", `Campaign.Name`), "SI", "NO"))


#5. KW_Type
dataset <- dataset %>%
  mutate(`KW_Type` = NA) %>%  
  relocate(`KW_Type`, .after = 5)

dataset <- dataset %>%
  mutate(`KW_Type` = ifelse(grepl("\\S+ \\S+ \\S+", `keyword`), "Specifica", "Generica"))

dataset <- dataset %>%
  mutate(KW_Type = if_else(grepl("\\+", keyword), "Specifica", KW_Type))


#6. Eventi Speciali
merged_dataset <- merged_dataset %>%
  mutate(`Special Events` = NA) %>%  
  relocate(`Special Events`, .after = 2)

#merged_dataset$Week <- as.numeric(merged_dataset$Week)

merged_dataset <- merged_dataset %>%
  mutate(`Special Events` = ifelse(Week %in% c("18", "19", "20", "21"), "Yes", "No"))

summary(merged_dataset$Week)

#7. Vincoli di budget
merged_dataset <- merged_dataset %>%
  mutate(`Budget Constraints` = NA) %>%  
  relocate(`Budget Constraints`, .after = 10)

merged_dataset <- merged_dataset %>%
  mutate(`Budget Constraints` = ifelse(`Campaign.Name` %in% c("Risma", "Salviette Asciugamani", "Rotoloni Carta Asciugatutto",
                                                              "Polistirolo", "Sacchi", "Buste", "Pluriball", "Rotoli Carta Lettino",
                                                              "Scatole", "Kit Trasloco", "NastriImballaggio"), "No", "Yes"))

#8. Product Category
merged_dataset <- merged_dataset %>%
  mutate(`Product Category` = NA) %>%  
  relocate(`Product Category`, .after = 3)

merged_dataset <- merged_dataset %>%
  mutate(`Product Category` = case_when(
    grepl("185/65 R15 88 V", `Campaign.Name`) ~ "Auto e Moto",
    grepl("205/55 R16", `Campaign.Name`) ~ "Auto e Moto",
    grepl("Urban", `Campaign.Name`) ~ "Sport e Outdoor",
    grepl("Clima", `Campaign.Name`) ~ "Sport e Outdoor",
    grepl("Risma", `Campaign.Name`) ~ "Cancelleria e prodotti per ufficio",
    grepl("Accappatoio", `Campaign.Name`) ~ "Moda e Abbigliamento",
    grepl("Living", `Campaign.Name`) ~ "Casa e Cucina",
    grepl("Linea Enfant", `Campaign.Name`) ~ "Prima Infanzia",
    grepl("Salviette Asciugamani", `Campaign.Name`) ~ "Casa e Cucina",
    grepl("Rotoloni Carta Asciugatutto", `Campaign.Name`) ~ "Casa e Cucina",
    grepl("Topper", `Campaign.Name`) ~ "Casa e Cucina",
    grepl("Polistirolo", `Campaign.Name`) ~ "Imballaggi e Trasporto",
    grepl("Pouf", `Campaign.Name`) ~ "Casa e Cucina",
    grepl("Writing", `Campaign.Name`) ~ "Cancelleria e prodotti per ufficio",
    grepl("Sacchi", `Campaign.Name`) ~ "Imballaggi e Trasporto",
    grepl("Buste", `Campaign.Name`) ~ "Imballaggi e Trasporto",
    grepl("Pluriball", `Campaign.Name`) ~ "Imballaggi e Trasporto",
    grepl("lenzuola", `Campaign.Name`) ~ "Casa e Cucina",
    grepl("Trapunta", `Campaign.Name`) ~ "Casa e Cucina",
    grepl("Rotoli Carta Lettino", `Campaign.Name`) ~ "Casa e Cucina",
    grepl("Scatole", `Campaign.Name`) ~ "Imballaggi e Trasporto",
    grepl("Kit Trasloco", `Campaign.Name`) ~ "Imballaggi e Trasporto",
    grepl("Safes", `Campaign.Name`) ~ "Dispositivi di sicurezza",
    grepl("Max", `Campaign.Name`) ~ "Moda e Abbigliamento",
    grepl("Light Shock", `Campaign.Name`) ~ "Moda e Abbigliamento",
    grepl("Crema Corpo Profumata", `Campaign.Name`) ~ "Cosmetica e Cura del corpo",
    grepl("Cervical", `Campaign.Name`) ~ "Cosmetica e Cura del corpo",
    grepl("NastriImballaggio", `Campaign.Name`) ~ "Imballaggi e Trasporto",
    grepl("InCharge", `Campaign.Name`) ~ "Elettronica di consumo",
    grepl("Nurse", `Campaign.Name`) ~ "Moda e Abbigliamento",
    grepl("DDV", `Campaign.Name`) ~ "Dispositivi di sicurezza",
    TRUE ~ NA_character_  # Per tutti i casi non trovati, assegna NA
  ))


###########################
#DATA CLEANING
###########################

#convert to numeric
columns_to_convert <- c("AdSales", "AdSpend", "ACOS", "ROAS", "CPC", "Top.of.Search.Impression.Share", "Keyword.Bid")
dataset[columns_to_convert] <- lapply(dataset[columns_to_convert], function(x) gsub(",", ".", x))
columns_to_clean <- c("AdSales", "AdSpend")
dataset[columns_to_clean] <- lapply(dataset[columns_to_clean], function(x) gsub("€ ", "", x))
dataset[columns_to_convert] <- lapply(dataset[columns_to_convert], as.numeric)
cleaned_dataset <- dataset[!is.na(dataset$AdSpend), ]
cleaned_dataset <- cleaned_dataset[cleaned_dataset$AdSpend != 0.0, ]

describe(cleaned_dataset)
summary(cleaned_dataset)
unique_campaign_count <- length(unique(cleaned_dataset$Campaign.Name))

# Print the result
unique_campaign_count
unique_budget_count <- length(unique(cleaned_dataset$Campaign.Name))
print(unique(cleaned_dataset$Campaign.Name))

write.csv(cleaned_dataset, "cleaned_dataset.csv", row.names = FALSE)

subset_keywords <- cleaned_dataset[cleaned_dataset$Week == "2024-16", "keyword"]

# Mostra i risultati
print(subset_keywords)

# Trasforma subset_keywords in una lista
subset_keywords_list <- as.list(subset_keywords)

# Mostra la lista
print(subset_keywords_list)

subset_keywords_df <- data.frame(keyword = unlist(subset_keywords_list))

# Esporta il dataframe come file CSV
write.csv(subset_keywords_df, file = "subset_keywords_list.csv", row.names = FALSE)

# Rimuove i primi 5 caratteri dalla colonna "Week"
merged_dataset$Week <- substr(merged_dataset$Week, 6, nchar(merged_dataset$Week))

numeric_conversion <- c("Magnet.IQ.Score", "Search.Volume", "Search.Volume.Trend", "Competing.Products", "Title.Density")
merged_dataset[numeric_conversion] <- lapply(merged_dataset[numeric_conversion], as.numeric)
summary(merged_dataset)

# Rimuove le colonne specificate dal dataset
merged_dataset <- merged_dataset[, !names(merged_dataset) %in% c("Suggested.Bid", "Organic", "Smart.Complete", "Amazon.Recommended", "X", "X.1")]

write.csv(merged_dataset, file = "dataset_finale.csv", row.names = FALSE)

u <- unique(merged_dataset$`Campaign.Name`)
print(u)

uk <- unique(merged_dataset$keyword)
print(uk)


########################
# FINAL EXPORT
########################

write.csv(merged_dataset, file = "dataset_completo.csv", row.names = FALSE)


dataset_nuovo <- read.csv("/Users/paolodu/Desktop/dataset_completo.csv", sep = ",")

dataset_nuovo <- dataset_nuovo %>%
  mutate(`Budget Constraints` = NA) %>%  
  relocate(`Budget Constraints`, .after = 10)

dataset_nuovo <- dataset_nuovo %>%
  mutate(`Budget.Constraints` = ifelse(`Campaign.Name` %in% c("Risma", "Salviette Asciugamani", "Rotoloni Carta Asciugatutto",
                                                              "Polistirolo", "Sacchi", "Buste", "Pluriball", "Rotoli Carta Lettino",
                                                              "Scatole", "Kit Trasloco", "NastriImballaggio"), "No", "Yes"))


library(dplyr)
library(stringr)

dataset_nuovo <- dataset_nuovo %>%
  mutate(`Budget.Constraints` = ifelse(str_detect(`Campaign.Name`, paste(c("Risma", "Salviette Asciugamani", "Rotoloni Carta Asciugatutto",
                                                                           "Polistirolo", "Sacchi", "Buste", "Pluriball", "Rotoli Carta Lettino",
                                                                           "Scatole", "Kit Trasloco", "NastriImballaggio"), collapse = "|")),
                                       "No", "Yes"))

#show missing values of AdSales
missing_adsales_rows <- dataset_nuovo %>%
  filter(is.na(AdSales))
print(missing_adsales_rows)

#show missing values of ACOS
missing_ACOS_rows <- dataset_nuovo %>%
  filter(is.na(ACOS))
print(missing_ACOS_rows)

summary(dataset_nuovo)

#replace missing values with AdSpend/ACOS
dataset_nuovo <- dataset_nuovo %>%
  mutate(AdSales = ifelse(is.na(AdSales), AdSpend / ACOS, AdSales))

dataset_nuovo <- dataset_nuovo %>%
  distinct()


#8. CR
dataset_nuovo <- dataset_nuovo %>%
  mutate(`CR` = NA) %>%  
  relocate(`CR`, .after = 16)

dataset_nuovo <- dataset_nuovo %>%
  mutate(CR = Orders / Clicks)


print(unique(dataset_nuovo$Product.Category))


#9. CPC, CR, ROAS settore
dataset_nuovo <- dataset_nuovo %>%
  mutate(`CR_settore` = NA) %>%  
  relocate(`CR_settore`, .after = 17) 

dataset_nuovo <- dataset_nuovo %>%
  mutate(`ROAS_settore` = NA) %>%  
  relocate(`ROAS_settore`, .after = 19)

dataset_nuovo <- dataset_nuovo %>%
  mutate(`CPC_settore` = NA) %>%  
  relocate(`CPC_settore`, .after = 21)

dataset_nuovo <- dataset_nuovo %>%
  mutate(CPC_settore = case_when(
    Product.Category == "Auto e Moto" ~ 0.29,
    Product.Category == "Sport e Outdoor" ~ 0.42,
    Product.Category == "Cancelleria e prodotti per ufficio" ~ 0.29,
    Product.Category == "Moda e Abbigliamento" ~ 0.27,
    Product.Category == "Prima Infanzia" ~ 0.38,
    Product.Category == "Casa e Cucina" ~ 0.39,
    Product.Category == "Imballaggi e Trasporto" ~ 0.29,
    Product.Category == "Dispositivi di sicurezza" ~ 0.46,
    Product.Category == "Cosmetica e Cura del corpo" ~ 0.38,
    Product.Category == "Elettronica di consumo" ~ 0.46,
    TRUE ~ NA_real_  # Assign NA for any other categories
  ))

dataset_nuovo <- dataset_nuovo %>%
  mutate(CR_settore = case_when(
    Product.Category == "Auto e Moto" ~ 0.0631,
    Product.Category == "Sport e Outdoor" ~ 0.0616,
    Product.Category == "Cancelleria e prodotti per ufficio" ~ 0.0631,
    Product.Category == "Moda e Abbigliamento" ~ 0.0401,
    Product.Category == "Prima Infanzia" ~ 0.0861,
    Product.Category == "Casa e Cucina" ~ 0.0902,
    Product.Category == "Imballaggi e Trasporto" ~ 0.0631,
    Product.Category == "Dispositivi di sicurezza" ~ 0.0395,
    Product.Category == "Cosmetica e Cura del corpo" ~ 0.0861,
    Product.Category == "Elettronica di consumo" ~ 0.0395,
    TRUE ~ NA_real_  # Assign NA for any other categories
  ))


dataset_nuovo <- dataset_nuovo %>%
  mutate(ROAS_settore = case_when(
    Product.Category == "Auto e Moto" ~ 4.28,
    Product.Category == "Sport e Outdoor" ~ 4.15,
    Product.Category == "Cancelleria e prodotti per ufficio" ~ 4.28,
    Product.Category == "Moda e Abbigliamento" ~ 3.79,
    Product.Category == "Prima Infanzia" ~ 3.34,
    Product.Category == "Casa e Cucina" ~ 3.92,
    Product.Category == "Imballaggi e Trasporto" ~ 4.28,
    Product.Category == "Dispositivi di sicurezza" ~ 3.44,
    Product.Category == "Cosmetica e Cura del corpo" ~ 3.34,
    Product.Category == "Elettronica di consumo" ~ 3.44,
    TRUE ~ NA_real_  # Assign NA for any other categories
  ))

#8. RACR
dataset_nuovo <- dataset_nuovo %>%
  mutate(`RACR` = NA) %>%  
  relocate(`RACR`, .after = 32)

dataset_nuovo <- dataset_nuovo %>%
  mutate(RACR = ROAS*CR / CPC)

dataset_nuovo <- dataset_nuovo %>%
  mutate(ROAS_CPC = ROAS / CPC)

summary(dataset_nuovo)
describe(dataset_nuovo)
write.csv(dataset_nuovo, file = "dataset_nuovo.csv", row.names = FALSE)


model_home1 <- lm(RACR ~ ROAS+CPC+CR, data=dataset_nuovo) 
summary(model_home1)


###################################################
#FURTHER ANALYSIS: LINEAR REGRESSION and ELASTICITY
###################################################

dataset_Further_Analysis <- read.csv("/Users/paolodu/Desktop/Dataset_Further_Analysis.csv", sep = ",")
describe(dataset_Further_Analysis)
summary(dataset_Further_Analysis)

#modello baseline
model_baseline <- lm(RACR_weighted ~ `Top.of.Search.Impression.Share`+`Keyword.Bid`+`Ratio_Bid_CPC`, data=dataset_Further_Analysis) 
summary(model_baseline)

#regressione lineare con RACR_weighted anziché RACC
model_feature_eng <- lm(RACR_weighted ~ `Top.of.Search.Impression.Share`+`Keyword.Bid`+`Ratio_Bid_CPC`+dataset_Further_Analysis$Special.Events+
                          dataset_Further_Analysis$Product.Category+dataset_Further_Analysis$Strategia+dataset_Further_Analysis$Budget.Constraints+
                          dataset_Further_Analysis$Sponsored.ASINs+dataset_Further_Analysis$Title.Density, data=dataset_Further_Analysis) 
summary(model_feature_eng)

model_feature_all <- lm(RACR_weighted ~ `Top.of.Search.Impression.Share`+`Keyword.Bid`+`Ratio_Bid_CPC`+dataset_Further_Analysis$Special.Events+
                          dataset_Further_Analysis$Product.Category+dataset_Further_Analysis$Strategia+dataset_Further_Analysis$Budget.Constraints+
                          dataset_Further_Analysis$Sponsored.ASINs+dataset_Further_Analysis$Title.Density+dataset_Further_Analysis$Targeting+
                          dataset_Further_Analysis$KW_Type+dataset_Further_Analysis$Target.Type+dataset_Further_Analysis$MatchType, data=dataset_Further_Analysis) 
summary(model_feature_all)

#regressione lineare con interazioni tra bid e altre variabili

model_bid_interactions <- lm(RACR_weighted ~ `Top.of.Search.Impression.Share`+`Keyword.Bid`+`Ratio_Bid_CPC`+dataset_Further_Analysis$Special.Events+
                          dataset_Further_Analysis$Product.Category+dataset_Further_Analysis$Strategia+dataset_Further_Analysis$Budget.Constraints+
                          dataset_Further_Analysis$Sponsored.ASINs+dataset_Further_Analysis$Title.Density+
                            dataset_Further_Analysis$Keyword.Bid*dataset_Further_Analysis$Special.Events+
                            dataset_Further_Analysis$Keyword.Bid*dataset_Further_Analysis$Product.Category, data=dataset_Further_Analysis) 
summary(model_bid_interactions)


#####################
#ELASTICITÀ
#####################

# Caricamento dei pacchetti necessari
library(dplyr)

######
#1. Calibrare il Modello di Regressione Logistica
# Addestramento del modello di regressione logistica
model_elasticity <- glm(dataset_Further_Analysis$RACC_weighted ~ `Top.of.Search.Impression.Share`+`Keyword.Bid`+`Ratio_Bid_CPC`+dataset_Further_Analysis$Special.Events+
                          dataset_Further_Analysis$Product.Category+dataset_Further_Analysis$Strategia+dataset_Further_Analysis$Budget.Constraints+
                          dataset_Further_Analysis$Sponsored.ASINs+dataset_Further_Analysis$Title.Density,
             data = dataset_Further_Analysis, family = "binomial")
summary(model_elasticity)

##########
#2. Ottenere le Probabilità Predette
# Ottenere le probabilità predette
dataset_Further_Analysis$predicted_probabilities <- predict(model_elasticity, dataset_Further_Analysis, type = "response")

##########
#3. Eseguire Variazioni Incrementali su Bid
# Creiamo versioni del dataset con variazioni di bid (esempio: +/- 5%)
dataset_increment <- dataset_Further_Analysis %>% mutate(`Keyword.Bid` = `Keyword.Bid` * 1.05)
dataset_decrement <- dataset_Further_Analysis %>% mutate(`Keyword.Bid` = `Keyword.Bid` * 0.95)

#########
#4. Calcolare l’Elasticità del Bid
#Per ogni bid originale, calcola la variazione percentuale della probabilità di successo 
#rispetto alla variazione percentuale del bid, utilizzando la formula dell’elasticità. 
#Di seguito un esempio per un singolo record (ma è possibile applicarlo a tutti i record).

# Calcoliamo le probabilità predette per ogni variazione di bid
dataset_increment$predicted_probabilities <- predict(model_elasticity, dataset_increment, type = "response")
dataset_decrement$predicted_probabilities <- predict(model_elasticity, dataset_decrement, type = "response")

# Selezioniamo un record come esempio
P0 <- dataset_Further_Analysis$predicted_probabilities[1]  # Probabilità con il bid originale
B0 <- dataset_Further_Analysis$`Keyword.Bid`[1]                      # Bid originale
P1 <- dataset_increment$predicted_probabilities[1]  # Probabilità con bid aumentato
B1 <- dataset_increment$`Keyword.Bid`[1]             # Bid incrementato

# ESEMPIO: Calcolo dell'elasticità
elasticity <- ((P1 - P0) / P0) / ((B1 - B0) / B0)
elasticity

#aggiungere i valori di elasticità al dataset principale
dataset_Further_Analysis$elasticity <- ((dataset_increment$predicted_probabilities - dataset_Further_Analysis$predicted_probabilities) / dataset_Further_Analysis$predicted_probabilities) / ((dataset_increment$`Keyword.Bid` - dataset_Further_Analysis$`Keyword.Bid`) / dataset_Further_Analysis$`Keyword.Bid`)



#####################
#CURVA DI ELASTICITÀ
#####################

#creo una copia del dataset originale
dataset_curve <- dataset_Further_Analysis

#imposto le variazioni incrementali
#dataset_curve$Bid_0.05 <- `Keyword.Bid` * 0.05

########################################################
# Vettore degli incrementi
incrementi <- seq(0.05, 2, by = 0.05)

# Loop per ogni incremento
for (inc in incrementi) {
  # Crea il nome della colonna dinamicamente
  col_name <- paste0("Bid_", sprintf("%.2f", inc))
  
  # Calcola la nuova colonna come Keyword.Bid moltiplicato per l'incremento
  dataset_curve[[col_name]] <- dataset_curve$Keyword.Bid * inc
}

# Visualizza il dataset con le nuove colonne
head(dataset_curve)
########################################################

########################################################
# Crea una lista per salvare i modelli
modelli_elasticity <- list()

# Loop per ogni incremento
for (inc in incrementi) {
  # Crea il nome della colonna dinamicamente e del modello
  col_name <- paste0("Bid_", sprintf("%.2f", inc))
  model_name <- paste0("model_elasticity_", sprintf("%.2f", inc))
  
  # Esegui il modello di regressione logistica per ogni colonna Bid
  modelli_elasticity[[model_name]] <- glm(dataset_curve$RACC_weighted ~ 
                                            `Top.of.Search.Impression.Share` + dataset_curve[[col_name]] + 
                                            `Ratio_Bid_CPC` + dataset_curve$Special.Events +
                                            dataset_curve$Product.Category + dataset_curve$Strategia + 
                                            dataset_curve$Budget.Constraints + dataset_curve$Sponsored.ASINs + 
                                            dataset_curve$Title.Density,
                                          data = dataset_curve, family = "binomial")
}

summary(modelli_elasticity$model_elasticity_0.05)
########################################################

########################################################
# Vettore degli incrementi
incrementi <- seq(0.05, 0.95, by = 0.05)

# Crea una lista per salvare i modelli
modelli_elasticity <- list()

# Loop per ogni incremento per creare i modelli
for (inc in incrementi) {
  # Crea il nome della colonna dinamicamente
  col_name <- paste0("Bid_", sprintf("%.2f", inc))
  
  # Crea la formula dinamicamente
  formula <- as.formula(paste("RACC_weighted ~ `Top.of.Search.Impression.Share` +", col_name, 
                              "+ `Ratio_Bid_CPC` + Special.Events + Product.Category + Strategia +",
                              "Budget.Constraints + Sponsored.ASINs + Title.Density"))
  
  # Esegui il modello di regressione logistica per ogni colonna Bid
  modelli_elasticity[[col_name]] <- glm(formula, data = dataset_curve, family = "binomial")
}

# Calcolo delle probabilità previste per ogni modello
for (inc in incrementi) {
  # Crea il nome della colonna dinamicamente
  col_name <- paste0("predicted_probabilities_", sprintf("%.2f", inc))
  model_name <- paste0("Bid_", sprintf("%.2f", inc))
  
  # Calcola le probabilità previste usando il modello corrispondente
  dataset_curve[[col_name]] <- predict(modelli_elasticity[[model_name]], dataset_curve, type = "response")
}
########################################################

########################################################
dataset_Further_Analysis$elasticity <- ((dataset_increment$predicted_probabilities - dataset_Further_Analysis$predicted_probabilities) / dataset_Further_Analysis$predicted_probabilities) / ((dataset_increment$`Keyword.Bid` - dataset_Further_Analysis$`Keyword.Bid`) / dataset_Further_Analysis$`Keyword.Bid`)

dataset_curve$elasticity_0.05 <- ((dataset_curve$predicted_probabilities_0.05 - dataset_curve$predicted_probabilities) / dataset_curve$predicted_probabilities) / ((dataset_curve$`Bid_0.05` - dataset_curve$`Keyword.Bid`) / dataset_curve$`Keyword.Bid`)

# Loop per ogni incremento per calcolare l'elasticità
for (inc in incrementi) {
  # Crea i nomi delle colonne dinamicamente
  col_pred_prob <- paste0("predicted_probabilities_", sprintf("%.2f", inc))
  col_bid <- paste0("Bid_", sprintf("%.2f", inc))
  col_elasticity <- paste0("elasticity_", sprintf("%.2f", inc))
  
  # Calcola l'elasticità per ogni incremento
  dataset_curve[[col_elasticity]] <- ((dataset_curve[[col_pred_prob]] - dataset_curve$predicted_probabilities) / dataset_curve$predicted_probabilities) / 
    ((dataset_curve[[col_bid]] - dataset_curve$Keyword.Bid) / dataset_curve$Keyword.Bid)
}

# Visualizza il dataset con le nuove colonne di elasticità
head(dataset_curve)
########################################################

#row297
# Crea un nuovo dataset con solo la riga 297
dataset_row297 <- dataset_curve[297, ]

# Trova i nomi delle colonne comuni per Bid, predicted_probabilities ed elasticity
col_bid <- grep("^Bid_", names(dataset_row297), value = TRUE)
col_probs <- grep("^predicted_probabilities_", names(dataset_row297), value = TRUE)
col_elasticity <- grep("^elasticity_", names(dataset_row297), value = TRUE)

# Identifica gli incrementi comuni in tutte e tre le categorie
incrementi_comuni <- Reduce(intersect, list(col_bid, col_probs, col_elasticity))

# Ordina i nomi delle colonne per avere un ordine coerente
col_bid <- sort(grep(paste(incrementi_comuni, collapse = "|"), col_bid, value = TRUE))
col_probs <- sort(grep(paste(incrementi_comuni, collapse = "|"), col_probs, value = TRUE))
col_elasticity <- sort(grep(paste(incrementi_comuni, collapse = "|"), col_elasticity, value = TRUE))

# Crea il dataset trasposto
trasposto <- data.frame(
  bid = unlist(dataset_row297[, col_bid]),
  predicted_probs = unlist(dataset_row297[, col_probs]),
  elasticità = unlist(dataset_row297[, col_elasticity])
)

# Azzera i nomi delle righe
row.names(trasposto) <- NULL

# Visualizza il dataset trasposto
print(trasposto)


write.csv(dataset_curve, file = "dataset_elasticity_curve.csv", row.names = FALSE)

# Supponendo che il tuo dataframe si chiami "df"
df_filtrato <- dataset_curve[abs(dataset_curve$elasticity) > 1, ]



# Carica il pacchetto ggplot2 per la visualizzazione
library(ggplot2)

# Crea la curva di elasticità
ggplot(campaign_data, aes(x = bid, y = elasticity)) +
  geom_line(color = "blue") +
  labs(
    title = "Curva di Elasticità della Probabilità di Successo rispetto al Bid",
    x = "Bid",
    y = "Elasticità"
  ) +
  theme_minimal()

########################
#PANEL REGRESSION
########################

# Installazione e caricamento del pacchetto plm
install.packages("plm")
library(plm)

# Strutturazione dei dati panel: identifica campagna e periodo come dimensioni panel
panel_data <- pdata.frame(all_campaign_data, index = c("campaign_id", "time"))

# Modello a effetti fissi per stimare l'impatto del bid sulla probabilità di successo
fixed_effects_model <- plm(dataset_Further_Analysis$RACR_weighted ~ `Top.of.Search.Impression.Share`+`Keyword.Bid`+`Ratio_Bid_CPC`+dataset_Further_Analysis$Special.Events+
                             dataset_Further_Analysis$Product.Category+dataset_Further_Analysis$Strategia+dataset_Further_Analysis$Budget.Constraints+
                             dataset_Further_Analysis$Sponsored.ASINs+dataset_Further_Analysis$Title.Density, 
                           data = dataset_Further_Analysis, 
                           model = "within")  # ‘within’ specifica gli effetti fissi

# Sintesi dei risultati
summary(fixed_effects_model)



model_RACR <- glm(dataset_Further_Analysis$RACR_weighted ~ `Top.of.Search.Impression.Share`+`Keyword.Bid`+`Ratio_Bid_CPC`+dataset_Further_Analysis$Special.Events+
                          dataset_Further_Analysis$Product.Category+dataset_Further_Analysis$Strategia+dataset_Further_Analysis$Budget.Constraints+
                          dataset_Further_Analysis$Sponsored.ASINs+dataset_Further_Analysis$Title.Density,
                        data = dataset_Further_Analysis, family = "poisson")

summary(model_RACR)


model_RACR_gaussian <- glm(dataset_Further_Analysis$RACR_weighted ~ `Top.of.Search.Impression.Share`+`Keyword.Bid`+`Ratio_Bid_CPC`+dataset_Further_Analysis$Special.Events+
                    dataset_Further_Analysis$Product.Category+dataset_Further_Analysis$Strategia+dataset_Further_Analysis$Budget.Constraints+
                    dataset_Further_Analysis$Sponsored.ASINs+dataset_Further_Analysis$Title.Density,
                  data = dataset_Further_Analysis, family = "gaussian")

summary(model_RACR_gaussian)






data_lineplot <- read.csv("/Users/paolodu/Desktop/Grafico_Elasticità_2.csv", sep=" ,""",)


data_lineplot <- read.csv("/Users/paolodu/Desktop/Grafico_Elasticità_2.csv", sep = ";")


data_lineplot <- read_xlsx("/Users/paolodu/Desktop/Grafico_Elasticità_2.xlsx")

ggplot(data_lineplot, aes(x = Bid_Value, y = Elasticity)) +
  geom_line(color = "blue") +
  labs(
    title = "Curva di Elasticità della Probabilità di Successo rispetto al Bid",
    x = "Bid",
    y = "Elasticità"
  ) +
  theme_minimal()


summary(dataset)





