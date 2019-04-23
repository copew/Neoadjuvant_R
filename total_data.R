library(readr)
library(ggplot2)

#loading the image files
extractor_output <- read_csv("~/Documents/OneDrive - University Of Cambridge/Documents/PhD/Neoadjuvant/Analysis/neoadj_20180511.csv")
colnames(extractor_output)[1] <- "ImageID"


#loading the annotation files
annotation <- read_csv("~/Documents/OneDrive - University Of Cambridge/Documents/PhD/Neoadjuvant/Scanned Images/annotation.csv")

#putting them together
total_data <- merge(annotation, extractor_output, by="ImageID")


#the "extra" ones from each data sheet
#extra_extractoroutput <- extractor_output$ImageID[-which(total_data$ImageID %in% extractor_output$ImageID)]
#extra_annotationoutput <- annotation$ImageID[-which(total_data$ImageID %in% annotation$ImageID)]



#add in clinical data/path response info

#loading clinical data
TransNeo_RCB <- read_csv("~/Documents/OneDrive - University Of Cambridge/Documents/PhD/Neoadjuvant/Pathology info/TransNeo_RCB.csv")

#now combine the path data
combined_data <- merge(TransNeo_RCB, total_data, by="Trial_ID")



#compare lymphocyte density in TransNeo
combined_data$median_lymph_KDE_knn_50 <- as.numeric(combined_data$median_lymph_KDE_knn_50)
combined_data$mean_lymph_KDE_knn_50 <- as.numeric(combined_data$mean_lymph_KDE_knn_50)
ggplot(combined_data, aes(x=combined_data$`RCB category`, y=combined_data$median_lymph_KDE_knn_50))+
  geom_boxplot()

#looking at biopsies only
biopsy_data <- combined_data[combined_data$Procedure=="Biopsy", ]

#keeping only H&E
biopsy_data <- biopsy_data[biopsy_data$Stain=="H&E", ]

pCR <- biopsy_data[biopsy_data$`RCB category`=="pCR", ]

ggplot(biopsy_data, aes(x=biopsy_data$`RCB category`, y=(biopsy_data$mean_lymph_KDE_knn_50)))+
  geom_violin()+
  theme_bw()




