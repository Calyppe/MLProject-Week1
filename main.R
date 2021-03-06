### Week 1
### MGI
### Regression for Spatial Data
### Margret Azuma & Chloé Girka

library(raster)
library(FNN) #library for KNN
library(ggplot2) #library for plotting
library(rgdal)

#clear env
rm(list=ls())

###read data###
#Set the directory where you put the data
#setwd("")
#read the csv
data <- as.data.frame(t(read.csv("J_SPARC_one_day.csv")))
#obtain the wavelenghts
wavelengths <- data[1,]
#remove the first row
data <- data[2:nrow(data),]
#put the right name for the rows
rownames(data) <- c(paste("Sample", seq_len(nrow(data)), sep = "_"))
#and for the columns
colnames(data) <- c("Chlorophyll", "LAI", "FCover", wavelengths[,4:65])

###Visualization histogram of chlorophyll###
graphics.off()
chlorophyll <- as.vector(data$Chlorophyll)
hist(chlorophyll)
#Some additional statistics on chlorophyll for a better insight
summary(chlorophyll)

###Show spectral values (unormalized)###
graphics.off()
sample_id = 11
spectral_val <- data.frame(t(wavelengths[1,4:ncol(wavelengths)]), t(data[sample_id,4:ncol(data)]), paste("Sample",sample_id,sep = " "))
colnames(spectral_val) <- c("Wavelengths", "Val", "ID")
sample_id = 100
spectral_val2 <- data.frame(t(wavelengths[1,4:ncol(wavelengths)]), t(data[sample_id,4:ncol(data)]), paste("Sample",sample_id,sep = " "))
colnames(spectral_val2) <- c("Wavelengths", "Val", "ID")
spectral_val <- rbind(spectral_val, spectral_val2)
ggplot(data=spectral_val, aes(x=Wavelengths, y=Val, color=ID, shape=ID)) + geom_point() + geom_line(linetype = "dashed")




###perform train/test split###
set.seed(90)
train_samples <- sample(seq_len(nrow(data)), size = 90)

train_ground_truth <- data[train_samples,-(2:3)]
test_ground_truth <- data[-train_samples,-(2:3)]


###Apply linear model###
linear_reg <- lm(Chlorophyll~., data=train_ground_truth)
pred_test <- predict(linear_reg, test_ground_truth)
RMSE_linear <- sqrt(mean((test_ground_truth$Chlorophyll-pred_test)^2))
MAE_linear <- mean(abs(test_ground_truth$Chlorophyll-pred_test))
R2_linear <- 1 - sum((test_ground_truth$Chlorophyll-pred_test)^2)/sum((test_ground_truth$Chlorophyll-mean(test_ground_truth$Chlorophyll))^2)

###normalize data###
data_norm <- data
data_norm[,4:65] <- data_norm[,4:65] / data_norm[,57]

###Show spectral values after normalization###
graphics.off()
sample_id = 11
spectral_val <- data.frame(t(wavelengths[1,4:ncol(wavelengths)]), t(data_norm[sample_id,4:ncol(data)]), paste("Sample",sample_id,sep = " "))
colnames(spectral_val) <- c("Wavelengths", "Val", "ID")
sample_id = 100
spectral_val2 <- data.frame(t(wavelengths[1,4:ncol(wavelengths)]), t(data_norm[sample_id,4:ncol(data)]), paste("Sample",sample_id,sep = " "))
colnames(spectral_val2) <- c("Wavelengths", "Val", "ID")
spectral_val <- rbind(spectral_val, spectral_val2)
ggplot(data=spectral_val, aes(x=Wavelengths, y=Val, color=ID, shape=ID)) + geom_point() + geom_line(linetype = "dashed")


###apply linear regression after normalization###
train_ground_truth_norm <- data_norm[train_samples,-(2:3)]
test_ground_truth_norm <- data_norm[-train_samples,-(2:3)]

linear_reg_norm <- lm(Chlorophyll~., data=train_ground_truth_norm)
pred_test_norm <- predict(linear_reg_norm, test_ground_truth_norm)
RMSE_linear_norm <- sqrt(mean((test_ground_truth_norm$Chlorophyll-pred_test_norm)^2))
MAE_linear_norm <- mean(abs(test_ground_truth_norm$Chlorophyll-pred_test_norm))
R2_linear_norm <- 1 - sum((test_ground_truth_norm$Chlorophyll-pred_test_norm)^2)/sum((test_ground_truth_norm$Chlorophyll-mean(test_ground_truth_norm$Chlorophyll))^2)


###Visualize linear model coeffs###
plot(linear_reg$coefficients)
plot(linear_reg_norm$coefficients)

###Make the scatter plot###
lm_scatter <- data.frame(rownames(test_ground_truth), test_ground_truth[,1], unname(pred_test), "LM")
colnames(lm_scatter) <- c("Sample_id", "GT", "Pred", "Method")
lm_scatter2 <- data.frame(rownames(test_ground_truth_norm), test_ground_truth_norm[,1], unname(pred_test_norm), "LM (Norm)")
colnames(lm_scatter2) <- c("Sample_id", "GT", "Pred", "Method")
lm_scatter <- rbind(lm_scatter, lm_scatter2)
ggplot(data=lm_scatter, aes(x=GT, y=Pred, color=Method, shape=Method)) + geom_point() + geom_abline(slope=1, intercept = 0)


###KNN reg###
set.seed(90)
knn <- knn.reg(train_ground_truth_norm[,-1], test_ground_truth_norm[,-1], train_ground_truth_norm$Chlorophyll, k=10)
pred_test = knn$pred
RMSE_knn_normalized <- sqrt(mean((test_ground_truth_norm$Chlorophyll-pred_test)^2))
MAE_knn_normalized <- mean(abs(test_ground_truth_norm$Chlorophyll-pred_test))
R2_knn_normalized <- 1 - sum((test_ground_truth_norm$Chlorophyll-pred_test)^2)/sum((test_ground_truth_norm$Chlorophyll-mean(test_ground_truth_norm$Chlorophyll))^2)
  
###find the best number of neighbors###
best_RMSE <- 10000000
best_k <- -1

for (k in seq(1:20)){
  knn <- knn.reg(train_ground_truth_norm[1:70,-1], train_ground_truth_norm[71:90,-1], train_ground_truth_norm$Chlorophyll[1:70], k=k)
  pred_test = knn$pred
  RMSE <- sqrt(mean((train_ground_truth_norm$Chlorophyll[71:90]-pred_test)^2))
  
  if (RMSE < best_RMSE) {
    best_RMSE <- RMSE
    best_k = k
    cat("New best model: ", k , " (", best_RMSE, ")\n")
  }
}

###Apply KNN with the best number of neighbors###
knn <- knn.reg(train_ground_truth_norm[,-1], test_ground_truth_norm[,-1], train_ground_truth_norm$Chlorophyll, k=best_k)
pred_test = knn$pred

###Show the scatter plot###
knn_scatter <- data.frame(rownames(test_ground_truth), test_ground_truth[,1], unname(pred_test), "KNN")
colnames(knn_scatter) <- c("Sample_id", "GT", "Pred", "Method")
lm_scatter2 <- rbind(lm_scatter2, knn_scatter)
ggplot(data=lm_scatter2, aes(x=GT, y=Pred, color=Method, shape=Method)) + geom_point() + geom_abline(slope=1, intercept = 0)

###Apply regression models on image###
raster_img <- stack("CHRIS.tif")

maxs <- cellStats(raster_img, stat='max')
par(mfrow=c(1,3))
plotRGB(raster_img/maxs*255, r=23,g=13,b=3, main='Input image')

###Apply lm###
img <- as.data.frame(raster_img, xy=TRUE)
colnames(img) <- c("x", "y", paste(wavelengths[1,4:ncol(wavelengths)]))
#Normalize
img[,3:64] <- img[,3:64]/img[,56]
#Predict
pred_img <- as.data.frame(predict(linear_reg_norm, img))
#Convert to matrix
mat_img <- matrix(unlist(pred_img), nrow=dim(raster_img)[1], ncol=dim(raster_img)[2],byrow = TRUE)
#Clip values
mat_img[mat_img<0] <- 0
mat_img[mat_img>55] <- 55
img_lm <- raster(mat_img)
plot(img_lm, axes=FALSE, main='Linear regression', asp=1)

#Apply knn
mat_img <- raster::as.matrix(raster_img)
#Normalize
mat_img[,] <- mat_img[,]/mat_img[,54]
#Give meaningful names to the column
colnames(mat_img) <- wavelengths[1,4:ncol(wavelengths)]
#Apply KNN
knn <- knn.reg(train_ground_truth_norm[,-1], mat_img, train_ground_truth_norm$Chlorophyll, k=best_k)
img_knn <- raster(matrix(knn$pred, nrow=dim(raster_img)[1], ncol=dim(raster_img)[2],byrow = TRUE))
plot(img_knn, axes=FALSE, main='KNN regression')

