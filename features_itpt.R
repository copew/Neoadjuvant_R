#merged_features
merged_features <- readRDS("merged_features.RDS")
dim(merged_features)
p_values <- data.frame(matrix(NA, nrow=76, ncol=2))
for (j in 3:78){
  p <- wilcox.test(merged_features[,j] ~ merged_features$pcr_rd)
  p_values[j-2, 2] <- p$p.value
  p_values[j-2,1] <- colnames(merged_features)[j]
}
merged_features$pcr_rd <- as.factor(merged_features$pcr_rd)
merged_features <- na.omit(merged_features)
#merged_features_selected <- merged_features[, c(1,2, 4, 10, 13, 19, 20,21, 26,27, 28, 34, 35, 40, 41, 52, 58, 61, 67, 68, 69, 74, 75, 76, 82, 83, 88, 89, 42:50)]

merged_features_density <- merged_features_nona
merged_features_density$buffer_15 <- (merged_features_density$knn_median_15_buffer)^(-2)
merged_features_density$in_15 <- (merged_features_density$knn_median_15_in)^(-2)
merged_features_density$difference_15 <- merged_features_density$buffer_15-merged_features_density$in_15
merged_features_density$distance_15 <- merged_features_density$knn_median_15_buffer-merged_features_density$knn_median_15_in
merged_features_density$lymph_countdifference <- merged_features_density$lymphcount_ninetieth_buffer-merged_features_density$lymphcount_ninetieth_in

#just changing the knn

#merged_features_selected$knn_median_15_buffer <- log((merged_features_selected$knn_median_15_buffer)^(-2))
#merged_features_selected$knn_median_15_in <- log((merged_features_selected$knn_median_15_in)^(-2))

p_values_selected <- data.frame(matrix(NA, nrow=26, ncol=2))
for (j in 3:28){
  p <- wilcox.test(merged_features_selected[,j] ~ merged_features_selected$pcr_rd)
  p_values_selected[j-2, 2] <- p$p.value
  p_values_selected[j-2,1] <- colnames(merged_features_selected)[j]
}

#difference dataframe
merged_features_selected$difference_knn <- merged_features_selected$knn_median_15_buffer-merged_features_selected$knn_median_15_in

merged_features_nona$lowmedhigh <- ifelse(merged_features_nona$RCB=="pCR", "LowVolume", ifelse(merged_features_nona$RCB=="I", "LowVolume", ifelse(merged_features_nona$RCB =="II", "MedVolume", "HighVolume")))
merged_features_nona$lowmedhigh <- ifelse(merged_features_nona$RCB=="pCR", "LowVolume", ifelse(merged_features_nona$RCB=="I", "LowVolume", ifelse(merged_features_nona$RCB =="II", "MedVolume", "HighVolume")))

merged_features_selected_nona <- merged_features_selected[-which(is.na(merged_features_selected$pcr_rd)), ]
merged_features_nona <- merged_features[-which(is.na(merged_features$pcr_rd)), ]

merged_features_selected$lymphcount_differenth <-  merged_features_selected$lymphcount_ninetieth_buffer-merged_features_selected$lymphcount_ninetieth_in
ggplot(merged_features_density, aes(x=lowmedhigh, y=log(in_15)))+ #, fill=pcr_rd))+
  geom_boxplot()+
  stat_compare_means()


(size=8, comparisons = list(c("pCR", "RD")))+
  theme_bw(base_size=16)+
  ylab("Difference in Lymphocyte Count")+
  xlab("")

ggboxplot(merged_features_density, x="HER2_ER_status", y="buffer_15",legend.position=FALSE, size=1, ylab="Median Lymphocyte Density", xlab=FALSE, fill = "HER2_ER_status")+ #, order =c("LowVolume", "MedVolume", "HighVolume") , font.label = list(size = 16, color = "black"))+
  stat_compare_means(label.y=2.7e-05, label.x = 0.7, size=6)+
  stat_compare_means(comparison=list(c("HER2-ER-", "HER2-ER+"), c("HER2-ER+", "HER2+"), c("HER2-ER-", "HER2+")))
merged_features_density$HER2_ER_status <- ifelse(merged_features_density$HER2.status=="POS", "HER2+", ifelse(merged_features_density$ER.status=="POS", "HER2-ER+", "HER2-ER-"))
ggplot(merged_features_density, aes(x=HER2_ER_status, y=buffer_15, fill=pcr_rd))+ #, fill=pcr_rd))+
  geom_boxplot()+
  stat_compare_means(method = "wilcox.test")+
  theme_bw()

  #stat_compare_means(comparisons=my_comparisons, size=4)




merged_features_selected$pcr_rd <- as.factor(merged_features_selected$pcr_rd )
#get a logistic regression type of univariate
model <- glm(pcr_rd ~.,family=binomial(link = "logit"),merged_features[, c(3:78, 87)], control = list(maxit=50))
summary(model)                                                       




model1 <- glm(pcr_rd ~tumourcount_stdev_in,family=binomial(link = "logit"),merged_features_selected[, c(3:28, 37)])
summary(model1)



ggplot(merged_features, aes(pcr_rd,tumourcount_mean_in))+
  geom_boxplot()
    
  