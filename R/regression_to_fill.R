library(raster)
library(FNN) #library for KNN
library(ggplot2) #library for plotting

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
#?
#colnames(data) <-

###Visualization histogram of chlorophyll###
graphics.off()
#?
#Use hist() function

###Show spectral values (unormalized)###
graphics.off()
sample_id = #?
spectral_val <- data.frame(t(wavelengths[1,4:ncol(wavelengths)]), t(data[sample_id,4:ncol(data)]), paste("Sample",sample_id,sep = " "))
colnames(spectral_val) <- c("Wavelengths", "Val", "ID")
sample_id = #?
spectral_val2 <- data.frame(t(wavelengths[1,4:ncol(wavelengths)]), t(data[sample_id,4:ncol(data)]), paste("Sample",sample_id,sep = " "))
colnames(spectral_val2) <- c("Wavelengths", "Val", "ID")
spectral_val <- rbind(spectral_val, spectral_val2)
ggplot(data=spectral_val, aes(x=Wavelengths, y=Val, color=ID, shape=ID)) + geom_point() + geom_line(linetype = "dashed")




###perform train/test split###
train_samples <- #?
test_samples <- #?

train_ground_truth <- #?
test_ground_truth <- #?


###Apply linear model###
linear_reg <- #?
pred_test <- predict(linear_reg, test_samples)
RMSE_linear <- #?
MAE_linear <- #?
R2_linear <- #?

###normalize data###
data_norm <- data
data_norm[,4:65] <- #?

###Show spectral values after normalization###


###apply linear regression after normalization###


###Visualize linear model coeffs###
plot(linear_reg$coefficients)

###Make the scatter plot###
lm_scatter <- data.frame(rownames(test_samples), test_samples[,1], unname(#?), "LM")
colnames(lm_scatter) <- c("Sample_id", "GT", "Pred", "Method")
lm_scatter2 <- data.frame(rownames(test_samples), test_samples[,1], unname(#?), "LM (Norm)")
colnames(lm_scatter2) <- c("Sample_id", "GT", "Pred", "Method")
lm_scatter <- rbind(lm_scatter, lm_scatter2)
ggplot(data=lm_scatter, aes(x=GT, y=Pred, color=Method, shape=Method)) + geom_point() + geom_abline(slope=1, intercept = 0)


###KNN reg###
knn <- #?
pred_test = knn$pred
RMSE_knn_normalized <- #?
MAE_knn_normalized <- #?
R2_knn_normalized <- #?
  
###find the best number of neighbors###
best_RMSE <- 10000000
best_k <- -1

for (k in seq(#?)){
  knn <- #?
  pred_test = #?
  RMSE <- #?
  
  if (RMSE < best_RMSE) {
    best_RMSE <- RMSE
    best_k = k
    cat("New best model: ", k , " (", best_RMSE, ")\n")
  }
}

###Apply KNN with the best number of neighbors###
knn <- #?
pred_test = knn$pred

###Show the scatter plot###


###Apply regression models on image###
raster_img <- stack("~/regressionEx/CHRIS.tif")

maxs <- cellStats(raster_img, stat='max')
par(mfrow=c(1,3))
plotRGB(raster_img/maxs*255, r=#?,g=#?,b=#?, main='Input image')

###Apply lm###
img <- as.data.frame(raster_img, xy=TRUE)
colnames(img) <- c("x", "y", paste("Wavelength", wavelengths[1,4:ncol(wavelengths)], sep = "_"))
#Normalize
img[,3:64] <- #?
#Predict
pred_img <- as.data.frame(predict(#?))
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
mat_img[,] <- #?
#Give meaningful names to the column
colnames(mat_img) <- #?
#Apply KNN
knn <- #?
img_knn <- raster(matrix(#?, nrow=dim(raster_img)[1], ncol=dim(raster_img)[2],byrow = TRUE))
plot(img_knn, axes=FALSE, main='KNN regression')


