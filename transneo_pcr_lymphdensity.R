#rcb vs not rcb at lymphocyte density

#load data
library(readr)
library(ggpubr)

#who has pCR
transneo_rcb_only <- read_csv("~/Documents/OneDrive - University Of Cambridge/Documents/PhD/Neoadjuvant/Pathology info/TransNeo_RCB.csv")

#transneo er her2 info etc
transneo_ErHer2 <- read_csv("~/Documents/OneDrive - University Of Cambridge/Documents/PhD/Neoadjuvant/Transneo_er_her2.csv")

transneo_rcb <- merge(transneo_rcb_only, transneo_ErHer2, by="Trial_ID")

#list of cases and image ID
combined_ffpe <- read_csv("~/Documents/OneDrive - University Of Cambridge/Documents/PhD/Neoadjuvant/Scanned Images/Combined_ffpe.csv")

#density
catalogue_output <- read_csv("/Users/cope01/Documents/OneDrive - University Of Cambridge/Documents/PhD/Neoadjuvant/Image Data/NeoAdjuvant--20180808-145608_level5.csv")


#subset into biopsy of breast and transneo and H&E

transneo<- combined_ffpe[which(combined_ffpe$Trial=="TransNeo"), ]
transneo_biopsy <- transneo[which(transneo$Procedure=="Biopsy"), ]
transneo_biopsy_HandE <- transneo_biopsy[which(transneo_biopsy$Stain=="H&E"), ]
transneo_biopsy_HandE_breast <- transneo_biopsy_HandE[which(transneo_biopsy_HandE$Tissue=="Breast"), ]

#remove NA (bilateral tumours - need to be sorted later)
transneo_rcb <- na.omit(transneo_rcb)

#tidyup catalogue output
catalogue_output$filename <- sub(".fits", "", catalogue_output$filename)
colnames(catalogue_output)[1] <- "Image_ID"


#make a new dataframe
test_df <- data.frame(cbind(transneo_biopsy_HandE_breast$Trial_ID, transneo_biopsy_HandE_breast$`Image ID`))
colnames(test_df) <- c("Trial_ID", "Image_ID")
test_df_rcb <- merge(test_df, transneo_rcb, by='Trial_ID')
test_df_rcb <- merge(test_df_rcb, catalogue_output, by="Image_ID") 
test_df_rcb$median_lymph_KDE_knn_50 <- as.numeric(test_df_rcb$median_lymph_KDE_knn_50)

test_df_rcb$pCR <- ifelse(test_df_rcb$RCB == "pCR", 'pCR', "RD")

#subsetting into Her2 +, Her2neg (with ER+ and ER-)

test_df_rcb_her2pos <- test_df_rcb[which(test_df_rcb$HER2.status=="POS"),]
test_df_rcb_erpos <- test_df_rcb[which(test_df_rcb$HER2.status=="NEG" & test_df_rcb$ER.status=="POS"),]
test_df_rcb_erneg <- test_df_rcb[which(test_df_rcb$HER2.status=="NEG" & test_df_rcb$ER.status=="NEG"),]

#or, make a column of her2+, her2-ER+ and her2-ER-
test_df_rcb$HER2_ER_status <- ifelse(test_df_rcb$HER2.status=="POS", "HER2+", ifelse(test_df_rcb$ER.status=="POS", "HER2-ER+", "HER2-ER-"))


ggplot(test_df_rcb, aes(x=pCR, y=exp(median_lymph_KDE_knn_50),fill=pCR))+
  geom_boxplot(show.legend=F)+
  stat_compare_means(size=6)+
  theme_bw(base_size = 20)+
  ylab("Median Lymphocyte Density")+
  xlab("Response to neoadjuvant chemotherapy")+
  ggtitle("Median Lymphocyte Density correlates with Response to Neoadjuvant Chemotherapy")
#+
  facet_wrap(~HER2_ER_status)

ggplot(test_df_rcb_erneg, aes(x=pCR, y=median_lymph_KDE_knn_50))+
  geom_boxplot()+
  stat_compare_means()+
  theme_bw(base_size = 16)+
  ylab("Median Lymphocyte Density (log)")

table(test_df_rcb$ERHER2.status)

#comparing median lymphocyte density with matlab extracted features
subset_biopsy <- test_df_rcb[which(test_df_rcb$Image_ID %in% c("603288", "625951", "619872", "619905", "619857")), ]
#using merged_features
subset_biopsy <- merge(subset_biopsy, merged_features, by="Trial_ID")



#right, let's plot

ggplot(test_df_rcb, aes(x=RCB, y=median_lymph_KDE_knn_50))+
  geom_boxplot()+
  scale_x_discrete(limits=c('pCR', "I", "II", "III"))+theme_bw()


test_df_rcb$lowandhigh <- ifelse(test_df_rcb$RCB=="pCR", "LowVolume", ifelse(test_df_rcb$RCB=="I", "LowVolume", "HighVolume"))
test_df_rcb$lowmedhigh <- ifelse(test_df_rcb$RCB=="pCR", "LowVolume", ifelse(test_df_rcb$RCB=="I", "LowVolume", ifelse(test_df_rcb$RCB =="II", "MedVolume", "HighVolume")))

ggplot(test_df_rcb, aes(x=lowmedhigh, y=median_lymph_KDE_knn_50))+
  geom_boxplot()+
  stat_compare_means()+
  scale_x_discrete(limits=c("LowVolume", "MedVolume", "HighVolume"))



my_comparisons <- list(c("LowVolume", "MedVolume"), c("MedVolume", "HighVolume"), c("LowVolume", "HighVolume"))
ggboxplot(test_df_rcb, x="lowmedhigh", y="median_lymph_KDE_knn_50",legend.position=FALSE, size=1, ylab="Median Lymphocyte Density", xlab=FALSE,fill = "lowmedhigh", order =c("LowVolume", "MedVolume", "HighVolume") , font.label = list(size = 16, color = "black"))+
  stat_compare_means(comparisons=my_comparisons, size=4)+
  stat_compare_means(label.y = -1.8, label.x = 0.7, size=6)

#now remove duplicate slides
test_df_rcb_unique <- subset(test_df_rcb, !duplicated(test_df_rcb$Trial_ID)) 
ggboxplot(test_df_rcb_unique, x="lowmedhigh", y="median_lymph_KDE_knn_50",legend.position=FALSE, size=1, ylab="Median Lymphocyte Density", xlab=FALSE,fill = "lowmedhigh", order =c("LowVolume", "MedVolume", "HighVolume") , font.label = list(size = 16, color = "black"))+
  stat_compare_means(comparisons=my_comparisons, size=4)+
  stat_compare_means(label.y = -1.8, label.x = 0.7, size=6)
ggplot(test_df_rcb_unique, aes(x=pCR, y=(median_lymph_KDE_knn_50), fill=pCR))+
  geom_boxplot()+
  theme_bw(base_size=20)+
  stat_compare_means(size=8, label.y=-2.7)+
  xlab("")+
  ylab("Median Lymphocyte Density (log)")
wilcox.test(test_df_rcb$median_lymph_KDE_knn_50[which(test_df_rcb$pCR=="pCR")],test_df_rcb$median_lymph_KDE_knn_50[which(test_df_rcb$pCR=="not pCR")], )
