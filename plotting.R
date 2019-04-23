#to plot p values

merged_features_plot <- merged_features[, c(1, 2,  4, 11, 14, 22, 23, 30, 31, 38, 57, 64, 67, 75, 76, 83, 84, 91, 107)]
colnames(merged_features_plot) <- c("ImageID","Trial_ID", "tumourcount_median_buffer", "tumourcount_iqr_buffer","lymphocyte_density_median_buffer" ,"lymphocyte_density_iqr_buffer" ,"lymphcount_median_buffer" ,  "lymphcount_iqr_buffer","area_median_buffer","area_iqr_buffer","tumourcount_median_in", "tumourcount_iqr_in", "lymphocyte_density_median_15_in","lymphocyte_density_iqr_in","lymphcount_median_in","lymphcount_iqr_in","area_median_in","area_iqr_in","pcr_rd")   



plotting_df <- data.frame(matrix(NA, nrow=16))
plotting_df[,1] <- colnames(merged_features_plot[3:18])
colnames(plotting_df) <- "Feature"
colnames(merged_features_plot)
merged_features_plot$pcr_rd <- as.factor(merged_features_plot$pcr_rd)
#p <- glm(pcr_rd ~ tumourcount_median_buffer, data=merged_features_plot[,3:23], family="binomial")


#this is done with logistic regression
# for (i in 1:20) {
#   var <- as.formula(paste0("pcr_rd~", plotting_df[i,1]))
#   fit <- glm(var, data = merged_features_plot[, 3:25], family="binomial")
#   plotting_df[i, 2] <- unname(coef(summary(fit))[, 'Pr(>|z|)'][2])
# }


for (j in 1:16){
  p <- wilcox.test(merged_features_plot[, j+2]~ merged_features_plot$pcr_rd, conf.int=T)
  plotting_df[j,2] <- p$p.value
  plotting_df[j,3] <- -log10(p$p.value)
  plotting_df[j,4] <- p$conf.int[1]
  plotting_df[j,5] <- p$conf.int[2]
  
}
colnames(plotting_df)[2] <- "p_value"


#try plotting confidence interval for plotting_df
ggplot(plotting_df, aes(x=p_value, y=Feature))+
  geom_point()+
  geom_linerangeh(aes(xmin=V4, xmax=V5, y=Feature))



plotting_df$log_p <- -log10(plotting_df$p_value)
#plotting_df$location <- substr(plotting_df$Feature, regexpr("\\_[^\\_]*$", plotting_df$Feature)+1, nchar(plotting_df$Feature))
#plotting_df$Feature <- substr(plotting_df$Feature, 1, regexpr("\\_[^\\_]*$", plotting_df$Feature)-1 )
write.csv(plotting_df, file="plotting_df.csv")

difference_pvalues <- data.frame(matrix(NA, nrow=36))
difference_pvalues[,1] <- colnames(difference_features_rcb)[3:38]
for (j in 3:38){
  p <- wilcox.test(difference_features_rcb[, j]~ difference_features_rcb$pcr_rd)
  difference_pvalues[j-2,2] <- p$p.value
}
colnames(difference_pvalues)[2] <- "p_value"
colnames(difference_pvalues)[1] <- "Features"
# 
# difference_over_in_pvalues <- data.frame(matrix(NA, nrow=33))
# difference_over_in_pvalues[,1] <- colnames(difference_feature_rcb_over_in)[3:35]
# for (j in 3:35){
#   y_values <- difference_feature_rcb_over_in[, j][is.finite(difference_feature_rcb_over_in[, j])]
#   p <- wilcox.test(difference_feature_rcb_over_in[, j][subset]~ difference_feature_rcb_over_in$pcr_rd[subset])
#   difference_over_in_pvalues[j-2,2] <- p$p.value
# }
# colnames(difference_over_in_pvalues)[2] <- "p_value"
# colnames(difference_over_in_pvalues)[1] <- "Features"




difference_pvalues$log_p <- -log10(difference_pvalues$p_value)
write.csv(difference_pvalues, file="difference_pvalues.csv")


ggplot(plotting_df, aes(x=Feature, y=log_p))+
  geom_point()+theme_bw()+
  theme(axis.text.x = element_text(angle=45, vjust = -0.1))


ggplot(plotting_df, aes(x=log_p, y=Feature))+
  geom_point()+theme_bw()+
  geom_vline(xintercept = -log10(0.05),linetype="dotted", color = "blue", size=1.5)

combined_pvalues1 <- read.csv("combined1.csv")
combined_pvalues <- read.csv("pvalues_all.csv")

#combined_pvalues$Location <- sub("In", "Within", combined_pvalues$Location)
#combined_pvalues$Feature <- sub("LymphocyteDensity", "lymphocyte_density", combined_pvalues$Feature)
combined_pvalues_mini <- combined_pvalues[-which(combined_pvalues$Measurement=="iqr"), ]

ggplot(combined_pvalues_mini, aes(x=log_p, y=Measurement, colour=Location))+
  geom_point(shape=23,  size=10, position = position_dodgev(height=0.5), aes(fill=Location), colour="black")+
  theme_bw(base_size = 30)+
  scale_shape_manual(values=c(15, 17, 19))+
  theme(element_blank(), axis.title = element_text(face="bold"), legend.position = "top", strip.text = element_blank())+
  geom_hline(yintercept=c(seq(15, 105, 10)/10), color="grey")+
  ylab("")+
  xlab(expression("-log"[10]*"p"))+
  facet_grid(Features~., switch = "both")+
  scale_y_discrete(position = "right", limits=c("stdev", "median", "mean"))+
  scale_x_continuous(breaks=c(0, 1, -log10(0.05), 2, 3), labels = scales::number_format(accuracy = 0.01))
  

combined_pvalues$adjusted <- p.adjust(combined_pvalues$p_value, method = "bonferroni", n = length(combined_pvalues$p_value))



#position = position_dodgev(height=0.8), strip.text.y = element_blank()

# geom_vline(xintercept = -log10(0.05),linetype="dotted", color = "purple", size=0.75)


cor.test(merged_features$lymphcount_ninetieth_buffer, merged_features$lymphcount_stdev_buffer, method = "spearman")

ggplot(merged_features, aes(x=lymphcount_ninetieth_buffer, y=lymphcount_stdev_buffer))+
  geom_point()



ggplot(na.omit(merged_features), aes(x=Grade.pre.chemotherapy, y=lymphcount_stdev_buffer, colour=pcr_rd))+
  geom_boxplot()+
  stat_compare_means()




merged_features_tidy <- merged_features[, -c( 3, 47:57 )]
pvalues_all <- data.frame(matrix(NA, nrow=86))
pvalues_all$features <- colnames(merged_features_tidy)[3:88]
for (j in 3:88){
  p <- wilcox.test(merged_features_tidy[, j]~ merged_features_tidy$pcr_rd)
  pvalues_all[j-2,3] <- p$p.value
}
pvalues_all <- pvalues_all[, -1]
write.csv(pvalues_all, "pvalues_all.csv")
