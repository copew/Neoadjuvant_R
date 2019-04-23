#packages
library(factoextra)
library(ggplot2)
library(psych)

#trying to do pca with data
all_features_rcb <- readRDS("all_features_rcb.RDS")
buffer_features_rcb <- readRDS("buffer_features_rcb.RDS")
in_features_rcb <- readRDS("in_features_rcb.RDS")

#modify the dataframes so that it matches
buffer_variables <- buffer_features_rcb[4:36]
rownames(buffer_variables) <- buffer_variables$ImageID
buffer.pca <- prcomp(na.omit(buffer_variables), scale=TRUE)
fviz_eig(buffer.pca)

variables <- get_pca_var(buffer.pca)

fviz_pca_var(buffer.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_ind(buffer.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#nwo to do the rotation
buffer_pca_rotated <- principal(buffer_variables, rotate="varimax", nfactors=4, scores=TRUE)
print(buffer_pca_rotated)
biplot.psych(buffer_pca_rotated)




#now use merged features and for pca
merged_data <- merged_features[, c(4:42, 52:89)]
merged_data <- merged_data[,which(apply(merged_data, 2, var) != 0)]
merged_data <- na.omit(merged_data)
scaling_01 <- function(x){(x-min(x))/(max(x)-min(x))}
merged_data01 <- apply(merged_data, 2, scaling_01)
merged.pca01 <- prcomp(merged_data01, scale=F)
fviz_eig(merged.pca01)

variables <- get_pca_var(merged.pca01)

fviz_pca_var(merged.pca01,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_ind(merged.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#nwo to do the rotation
merged_pca01_rotated <- principal(merged_data01, rotate="varimax", nfactors=5, scores=TRUE)
print(merged_pca01_rotated)
#biplot.psych(merged_pca01_rotated) #stupid plot
colnames(merged_df[, 3:78])

pca_rotation <- as.matrix(merged_pca01_rotated$loadings)
pca_rotation_cutoff <- ifelse(abs(pca_rotation)>=0.7, pca_rotation, 0)
components <- data.frame(as.matrix(merged_data01) %*% pca_rotation)
#scale_merged_data <- scale(merged_data)
components_cutoff <- data.frame(as.matrix(merged_data01) %*% pca_rotation_cutoff)
#components$pcr_rd <- merged_df$pcr_rd
components_cutoff$pcr_rd <- merged_features$pcr_rd


components_cutoffmelted <- melt(components_cutoff)
ggplot(components_cutoffmelted, aes(x=pcr_rd, y=value))+
  geom_boxplot()+
  facet_wrap(~variable)

  
#now do it on training_df data
training_data <- training_df[, 3:78]
training_data <- training_data[,apply(training_df[,3:78], 2, var) != 0]
training.pca <- prcomp(training_data, scale=TRUE)
fviz_eig(training.pca)

variables <- get_pca_var(training.pca)

fviz_pca_var(training.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_ind(training.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#nwo to do the rotation
training_pca_rotated <- principal(training_data, rotate="varimax", nfactors=4, scores=TRUE)
print(training_pca_rotated)
biplot.psych(training_pca_rotated)
colnames(training_df[, 3:78])


pca_rotation <- as.matrix(training_pca_rotated$loadings)
pca_rotation_cutoff <- ifelse(abs(pca_rotation)>=0.7, pca_rotation, 0)
components <- data.frame(as.matrix(scale(training_data)) %*% pca_rotation)
scale_training_data <- scale(training_data)
components_cutoff <- data.frame(as.matrix(scale_training_data) %*% pca_rotation_cutoff)
#components$pcr_rd <- as.factor(training_df$pcr_rd)

components_cutoff$pcr_rd <- as.factor(training_df$pcr_rd)

#now do the 10 fold cross validation
mean_auc <- data.frame(matrix(NA, nrow=5))
for (k in 1:5){
  test_list <- createFolds(training_df$pcr_rd, k=10)
  cv_training <- list()
  cv_testing <- list()
  p_values <- list()
  features_list <- list()
  modelling_list <- list()
  model_fit <- list()
  prediction_glm <- list()
  roc.glmModel <- list()
  auc.glmModel <- list()

training_df$pcr_rd <- as.factor(training_df$pcr_rd)
test <- list()
for (i in 1:10){
    cv_training[[i]] <- components_cutoff[-test_list[[i]], ]
    cv_testing[[i]] <- components_cutoff[test_list[[i]], ]
    
    
    # features_list[[i]] <- p_values[[i]]$features[which(p_values[[i]]$p_value < p_value_cutoff[x])]
    # modelling_list[[i]] <- data.frame(cbind(cv_training[[i]]$pcr_rd, cv_training[[i]][, which(colnames(cv_training[[i]]) %in% features_list[[i]])]))
    # colnames(modelling_list[[i]])[1] <- "pcr_rd"
    # modelling_list[[i]]$pcr_rd <- as.factor(modelling_list[[i]]$pcr_rd)
    model_fit[[i]] <- glm(pcr_rd ~ .,  data=cv_training[[i]],family="binomial")
    prediction_glm[[i]] <- predict(model_fit[[i]], newdata=cv_testing[[i]], type="response")
    roc.glmModel[[i]] <- roc(cv_testing[[i]]$pcr_rd, prediction_glm[[i]])
    auc.glmModel[[i]] <- auc(roc.glmModel[[i]])
    #print(auc.glmModel[[i]]
  }
  auc_values <- do.call(rbind, auc.glmModel)
  mean_auc[k,1] <-mean(auc_values)
}
  

colnames(mean_auc)[1] <- "AUC"
ggplot(mean_auc, aes(x=p_value_cutoff, y=AUC))+
  geom_point(size=2)+geom_line(size=1.5)+
  ylim(c(0,1))+theme_bw(base_size = 22)

prediction_objects <- list()
prediction_glm_unnamed <- list()
response <- list()
for (i in 1:10){
  prediction_glm_unnamed[[i]] <- unname(prediction_glm[[i]])
  response[[i]] <- as.vector(cv_testing[[i]]$pcr_rd)
}
pred <- prediction(prediction_glm_unnamed, response)
perf <- performance(pred,measure="tpr",x.measure = "fpr")
plot(perf,col="grey82",lty=3)
plot(perf,lwd=3,avg="vertical",add=TRUE)#,spread.estimate="boxplot")



pca_model <- glm(pcr_rd ~. , data = components_cutoff, family=binomial(link = "logit"))
summary(pca_model)







pca_predict <- predict(pca_model, components_cutoff, type="response")


components_melted <- melt(components)
ggplot(components_melted, aes(x=pcr_rd, y=value))+
  geom_boxplot()+
  facet_wrap(~variable)


