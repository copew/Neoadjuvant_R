merged_features

sample_slides <- merged_features[, c(1,2, 12, 19, 87)]
colnames(test_df_rcb)[1] <- "ImageID"

sample_slides <- merge(sample_slides, test_df_rcb, by="ImageID")
sample_slides <- sample_slides[,c(1, 2, 3, 4, 60)]
sample_slides$median_lymph_buffer <- 15/(sample_slides$knn_median_15_buffer^(2))
cor.test(sample_slides$median_lymph_KDE_knn_50, sample_slides$median_lymph_buffer)


whole_slide_sample <- sample_slides[, c(1, 2, 3, 4, 5, 61, 65)]


density_625951<- as.numeric(knn_files$`625951_buffer`[16, ])
density_625951<- density_625951[!is.na(density_625951)]
density_595806<- as.numeric(knn_files$`595806_buffer`[16, ])
density_595806<- density_595806[!is.na(density_595806)]

length(density_595806) <- length(density_625951)

distribution_plot <- data.frame(cbind(density_595806, density_625951))
distribution_plot_melted <- melt(distribution_plot)
distribution_plot_melted$density <- 15*(distribution_plot_melted$value^(-2))
ggplot(distribution_plot_melted, aes(x=density, colour=variable))+
  geom_density()


density_625911<- as.numeric(knn_files$`625911_buffer`[16, ])^(-2)
density_625911<- data.frame(density_625911[!is.na(density_625911)])
colnames(density_625911) <- "density"
ggplot(density_625911, aes(x=density))+
  geom_histogram()

density_625951<- as.numeric(knn_files$`625911_buffer`[16, ])^(-2)
density_625951<- data.frame(density_625951[!is.na(density_625951)])
colnames(density_625951) <- "density"
ggplot(density_625951, aes(x=density))+
  geom_histogram()

density_619460<- as.numeric(knn_files$`619460_buffer`[16, ])^(-2)
density_619460<- data.frame(density_619460[!is.na(density_619460)])
colnames(density_619460) <- "density"
ggplot(density_619460, aes(x=density))+
  geom_density()+
  geom_density(density_625951, aes(x=density))

test_merge <- merge(density_625911, density_619460, by="row.names", all.x=T)
test_merge_melt <- melt(test_merge)
ggplot(test_merge_melt, aes(x=value,  fill=variable))+
  geom_histogram(alpha=0.75, colour="black")+
  theme_bw(base_size=30)+
  xlab("Density of Lymphocytes around Tumour Clusters")+
  ylab("Count")+
  theme(legend.position = "none", strip.text = element_blank())+
  facet_grid(variable~.)
