##this is no longer necessary 


# library to read matlab data formats into R
library(R.matlab)

# read in our data
resultfolder <- "/Users/cope01/Documents/MATLAB/ImageAnalysis/results/knn_distance"
result_list <- list.files(resultfolder, pattern = ".mat", recursive = T, full.names = T)
all_mat_files <- lapply(result_list, function (i) readMat(i, header=FALSE))

names(all_mat_files) <- result_list

#attempted to remove empty ones but this is not really working
# remove_emptylist  <-  function(x.list){x.list[unlist(lapply(x.list, length) != 0)]}
# remove_emptylist(all_mat_files)

#all_mat_files: first list is the image, 2nd is just one list, 3rd list is the core, 4th is 1, 5th is cluster, 6th is finally getting to the actual matrix


#now separating imageID and location etc
split_mat_names <- strsplit(result_list, "/")
file_names <- vector('character')


for (i in 1:length(split_mat_names)){
  split_mat_names[[i]][1] <-sub("/Users/cope01/Documents/MATLAB/ImageAnalysis/results/knn", "", split_mat_names[[i]][1]) 
  file_names[i] <- paste(split_mat_names[[i]][1], "_", split_mat_names[[i]][3])
  }


knn_list <- list()
for (i in 1:length(all_mat_files)){
  knn_list[[i]] <- data.frame(matrix(NA, nrow = 51, ncol=1))
  names(knn_list[[i]]) <- paste0(split_mat_names[[i]][1], "_", split_mat_names[[i]][3])
  for (j in 1:length(all_mat_files[[i]][[1]])){
    for (k in 1:length(all_mat_files[[i]][[1]][[j]][[1]])){
      if (length(all_mat_files[[i]][[1]][[j]][[1]][[k]][[1]]) == 0){
        next
      }
      if (sum(all_mat_files[[i]][[1]][[j]][[1]][[k]][[1]])==0){
        next
      }
      knn_list[[i]] <- cbind.fill(knn_list[[i]], all_mat_files[[i]][[1]][[j]][[1]][[k]][[1]])
      knn_list[[i]]$ImageID <- split_mat_names[[i]][1]
      knn_list[[i]]$location <- split_mat_names[[i]][3]
    }
  }
}

names(knn_list) <- file_names


