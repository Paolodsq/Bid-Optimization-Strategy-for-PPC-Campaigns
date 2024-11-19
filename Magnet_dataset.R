setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

magnet_1 <- read.csv("/Users/paolodu/Desktop/magnet_parte1.csv", sep = ",")
magnet_2 <- read.csv("/Users/paolodu/Desktop/magnet_parte2.csv", sep = ",")
combined_magnet <- rbind(magnet_1, magnet_2)

unique_kw <- read.csv("/Users/paolodu/Desktop/unique_keywords_list.csv", sep = ";")


subset_magnet_kw <- combined_magnet["Keyword.Phrase"]
subset_magnet_kw_list <- as.list(subset_magnet_kw)
unique_kw_list <- as.list(unique_kw)
diff_list <- setdiff(unique_kw_list, subset_magnet_kw_list)

filtered_unique_kw <- unique_kw[!(unique_kw$Keyword.Phrase %in% subset_magnet_kw$Keyword.Phrase), ]

remaining_keywords_df <- data.frame("Keyword.Phrase" = unlist(filtered_unique_kw))
write.csv(remaining_keywords_df, file = "remaining_keywords.csv", row.names = FALSE)

magnet_3 <- read.csv("/Users/paolodu/Desktop/magnet_parte3.csv", sep = ",")



combine_2 <- rbind(magnet_1, magnet_2, magnet_3)
sub_combine_2 <- combine_2["Keyword.Phrase"]
rimanenti <- unique_kw[!(unique_kw$Keyword.Phrase %in% sub_combine_2$Keyword.Phrase), ]
rimanenti_df <- data.frame("Keyword.Phrase" = unlist(rimanenti))
write.csv(rimanenti_df, file = "rimanenti.csv", row.names = FALSE)



dataset_filtrato <- cleaned_dataset[(cleaned_dataset$keyword %in% combine_2$Keyword.Phrase), ]
write.csv(dataset_filtrato, file = "dataset_finale.csv", row.names = FALSE)
write.csv(combine_2, file = "magnet_finale.csv", row.names = FALSE)


merged_dataset <- merge(dataset_filtrato, combine_2, by.x = "keyword", by.y = "Keyword.Phrase", all.x = TRUE)
describe(merged_dataset)
summary(merged_dataset)








