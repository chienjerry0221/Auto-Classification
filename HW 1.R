# Loading the necessary R packages.
library(ggplot2)
library(reshape2)
library(dplyr)
library(class)

# Read the data set and assign as a data frame, df. Then, rename the feature as abbreviation.
df = read.csv("DataAuto.csv")
colnames(df) = c("mpg", "cyl", "dis", "hor", "wei", "acc")  # Change the column names as instructed.
head(df)  # Examine the first few rows of data.
str(df)  # Examine the structure of the data.


# Problem 1
# Create empty data frame to record the Mean, Standard Deviation, and Range for each feature.
Table_1 = data.frame() 
for (i in colnames(df)[-1]){
  Table_1["Mean", i] = round(mean(df[, i]), 2)
  Table_1["Standard Deviation", i] = round(sd(df[, i]), 2)
  Table_1["Range", i] = paste(round(range(df[, i])[1], 2), "-", round(range(df[, i])[2], 2))
}


# Problem 2
# Plot the histogram of all the features separately in a row with normal density function  
# with their mean and standard deviation as red curve.
par(mfrow = c(1, 5))
for (i in colnames(df)[-1]){
  h = hist(df[, i], main = paste("Histogram of", i), xlab = i, probability = TRUE, col = "blue")
  xfit = seq(min(df[, i]), max(df[, i]), length = 40)
  yfit = dnorm(xfit, mean = mean(df[, i]), sd = sd(df[, i]))
  lines(xfit, yfit, col = "red", lwd = 2)
}
dev.off()

# Plot the histogram of response variable i.e., mpg with normal density function 
# with its mean and standard deviation as red curve.
h = hist(df[, "mpg"], main = "Histogram of mpg", xlab = "mpg", probability = TRUE, ylim = c(0, 0.06), col = "blue")
xfit = seq(min(df[, "mpg"]), max(df[, "mpg"]), length = 40)
yfit = dnorm(xfit, mean = mean(df[, "mpg"]), sd = sd(df[, "mpg"]))
lines(xfit, yfit, col = "red", lwd = 2)
dev.off()


# Problem 3
# Plot the scatter plot of all the features versus mpg in a row.
par(mfrow = c(1, 5))
for (i in colnames(df)[-1]){
  plot(df[, i], df[, "mpg"], xlab = i, ylab = "MPG", main = paste(i, "vs MPG"))
}
dev.off()

# Create an empty df to record correlation between each features with respect to MPG.
Table_3 = data.frame()
for (i in colnames(df)[-1]){
  Table_3["Correlation", paste("(", i, ", mpg )")] = round(cor(df[, i], df[, "mpg"]), 2)
}


# Problem 4
# Use cor() function to generate correlation matrix for all the features.
Table_4 = round(cor(df[, c(-1)]), 2)


# Problem 5
# Plot the Percentiles of MPG
quantile_mpg = quantile(df$mpg, probs = seq(0, 1, 0.01))
plot(quantile_mpg, at = seq(0, 100, 10), xlab = 'Percentiles', ylab = "MPG", main = "Quantiles of MPG", type = "l")


# Problem 6
# Calculate the linear regression model of each feature with respect to mpg.
cyl.lm = lm(mpg ~ cyl, data = df)
dis.lm = lm(mpg ~ dis, data = df)
hor.lm = lm(mpg ~ hor, data = df)
wei.lm = lm(mpg ~ wei, data = df)
acc.lm = lm(mpg ~ acc, data = df)
lm_list = list(cyl.lm, dis.lm, hor.lm, wei.lm, acc.lm)

# Create an empty data frame to record linear model coefficients, root mean squared error, and relative accuracy.
Table_6 = data.frame()
for (i in lm_list){
  Table_6["A (Intercept)", paste(row.names(summary(i)$coefficients)[2])] = round(summary(i)$coefficients[1, 1], 2)
  Table_6["B (Slope)", paste(row.names(summary(i)$coefficients)[2])] = round(summary(i)$coefficients[2, 1], 2)
  Table_6["Root Mean Squared Error", paste(row.names(summary(i)$coefficients)[2])] = round(sqrt(anova(i)["Residual", "Mean Sq"]), 2)
  Table_6["Relative Accuracy", paste(row.names(summary(i)$coefficients)[2])] = round(sqrt(anova(i)["Residual", "Mean Sq"]) / mean(df$mpg), 2)
}

# Plot the scatter plot in a row together with linear regression line ,respectively, as red. 
par(mfrow = c(1, 5))
plot(df$cyl, df$mpg, xlab = "cyl", ylab = "MPG", main = "cyl vs MPG")
abline(cyl.lm, col = "red")
plot(df$dis, df$mpg, xlab = "dis", ylab = "MPG", main = "dis vs MPG")
abline(dis.lm, col = "red")
plot(df$hor, df$mpg, xlab = "hor", ylab = "MPG", main = "hor vs MPG")
abline(hor.lm, col = "red")
plot(df$wei, df$mpg, xlab = "wei", ylab = "MPG", main = "wei vs MPG")
abline(wei.lm, col = "red")
plot(df$acc, df$mpg, xlab = "acc", ylab = "MPG", main = "acc vs MPG")
abline(acc.lm, col = "red")
dev.off()


# Problem 8
# Calculate the quantile for 33% and 66%. Then classified entire data frame into
# three classes. Filter those data have mpg smaller than 33% quantile as LOWmpg.
# Filter those data have mpg between 33% and 66% quantile as MEDmpg. Filter those 
# data have mpg larger than 66% quantile as HIGHmpg.
q = quantile(df$mpg, probs = c(0.33, 0.66))
LOWmpg = filter(df, mpg <= q["33%"])
MEDmpg = filter(df, mpg > q["33%"] & mpg <= q["66%"]) 
HIGHmpg = filter(df, mpg > q["66%"]) 


# Problem 9
# Plot the histogram of each features for both LOWmpg and HIGHmpg side by side.
par(mfrow = c(1, 2))
hist(LOWmpg$cyl, xlab = "cyl", main = "Histogram of cyl (LOWmpg)", col = "blue")
hist(HIGHmpg$cyl, xlab = "cyl", main = "Histogram of cyl (HIGHmpg)", col = "blue")
dev.off()

par(mfrow = c(1, 2))
hist(LOWmpg$dis, xlab = "dis", main = "Histogram of dis (LOWmpg)", col = "blue")
hist(HIGHmpg$dis, xlab = "dis", main = "Histogram of dis (HIGHmpg)", col = "blue")
dev.off()

par(mfrow = c(1, 2))
hist(LOWmpg$hor, xlab = "hor", main = "Histogram of hor (LOWmpg)", col = "blue")
hist(HIGHmpg$hor, xlab = "hor", main = "Histogram of hor (HIGHmpg)", col = "blue")
dev.off()

par(mfrow = c(1, 2))
hist(LOWmpg$wei, xlab = "wei", main = "Histogram of wei (LOWmpg)", col = "blue")
hist(HIGHmpg$wei, xlab = "wei", main = "Histogram of wei (HIGHmpg)", col = "blue")
dev.off()

par(mfrow = c(1, 2))
hist(LOWmpg$acc, xlab = "acc", main = "Histogram of acc (LOWmpg)", col = "blue")
hist(HIGHmpg$acc, xlab = "acc", main = "Histogram of acc (HIGHmpg)", col = "blue")
dev.off()


# Problem 10
# Create two empty data frames to record the Mean, Standard Deviation, and 
# Confidence Interval for both LOWmpg and HIGHmog for each feature
Table_10_Low = data.frame()
Table_10_High = data.frame()
for (i in colnames(df)[-1]){
  low_mean = round(mean(LOWmpg[, i]),2)
  low_sd = round(sd(LOWmpg[, i]), 2)
  low_error_margin = qnorm(0.95)*low_sd/sqrt(length(LOWmpg$mpg))
  Table_10_Low["Mean", i] = low_mean
  Table_10_Low["Standard Deviation", i] = low_sd
  Table_10_Low["Confidence Interval", i] = paste("[", round(low_mean - low_error_margin, 2), ",", round(low_mean + low_error_margin, 2), "]")
  
  high_mean = round(mean(HIGHmpg[, i]),2)
  high_sd = round(sd(HIGHmpg[, i]), 2)
  high_error_margin = qnorm(0.95)*high_sd/sqrt(length(HIGHmpg$mpg))
  Table_10_High["Mean", i] = high_mean
  Table_10_High["Standard Deviation", i] = high_sd
  Table_10_High["Confidence Interval", i] = paste("[", round(high_mean - high_error_margin, 2), ",", round(high_mean + high_error_margin, 2), "]")
}


# Problem 11
# Centralize and Rescale the numerical features and binary the categorical features
for (i in c("dis", "hor", "wei", "acc")){
  LOWmpg[, i] = (LOWmpg[, i] - mean(df[, i]))/sd(df[, i])
  MEDmpg[, i] = (MEDmpg[, i] - mean(df[, i]))/sd(df[, i])
  HIGHmpg[, i] = (HIGHmpg[, i] - mean(df[, i]))/sd(df[, i])
}

LOWmpg[, c("cyl_3", "cyl_4", "cyl_5", "cyl_6", "cyl_8")] = 0
MEDmpg[, c("cyl_3", "cyl_4", "cyl_5", "cyl_6", "cyl_8")] = 0
HIGHmpg[, c("cyl_3", "cyl_4", "cyl_5", "cyl_6", "cyl_8")] = 0
for (i in 1:nrow(LOWmpg)){
  if (LOWmpg[i, "cyl"] == 3){
    LOWmpg[i, "cyl_3"] = 1
  } else if (LOWmpg[i, "cyl"] == 4){
    LOWmpg[i, "cyl_4"] = 1
  } else if (LOWmpg[i, "cyl"] == 5){
    LOWmpg[i, "cyl_5"] = 1
  } else if (LOWmpg[i, "cyl"] == 6){
    LOWmpg[i, "cyl_6"] = 1
  } else {
    LOWmpg[i, "cyl_8"] = 1
  }
}
LOWmpg["cyl"] = NULL
for (i in 1:nrow(MEDmpg)){
  if (MEDmpg[i, "cyl"] == 3){
    MEDmpg[i, "cyl_3"] = 1
  } else if (MEDmpg[i, "cyl"] == 4){
    MEDmpg[i, "cyl_4"] = 1
  } else if (MEDmpg[i, "cyl"] == 5){
    MEDmpg[i, "cyl_5"] = 1
  } else if (MEDmpg[i, "cyl"] == 6){
    MEDmpg[i, "cyl_6"] = 1
  } else {
    MEDmpg[i, "cyl_8"] = 1
  }
}
MEDmpg["cyl"] = NULL
for (i in 1:nrow(HIGHmpg)){
  if (HIGHmpg[i, "cyl"] == 3){
    HIGHmpg[i, "cyl_3"] = 1
  } else if (HIGHmpg[i, "cyl"] == 4){
    HIGHmpg[i, "cyl_4"] = 1
  } else if (HIGHmpg[i, "cyl"] == 5){
    HIGHmpg[i, "cyl_5"] = 1
  } else if (HIGHmpg[i, "cyl"] == 6){
    HIGHmpg[i, "cyl_6"] = 1
  } else {
    HIGHmpg[i, "cyl_8"] = 1
  }
}
HIGHmpg["cyl"] = NULL

# Convert mpg from numerical to character for all three classes. 
LOWmpg$mpg = "Low"
MEDmpg$mpg = "Med"
HIGHmpg$mpg = "High"

# Randomly sample from each class. 80% as training set and 20% as test set.
# Set the seed so that we can generate the same result for each time we run the code.
set.seed(20210912)
CL1_sample = sample(c(1: dim(LOWmpg)[1]), size = round(dim(LOWmpg)[1] * 0.8))
CL2_sample = sample(c(1: dim(MEDmpg)[1]), size = round(dim(MEDmpg)[1] * 0.8))
CL3_sample = sample(c(1: dim(HIGHmpg)[1]), size = round(dim(HIGHmpg)[1] * 0.8))

LOWmpg_Training = LOWmpg[CL1_sample, ]
LOWmpg_Test = LOWmpg[-CL1_sample, ]
MEDmpg_Training = MEDmpg[CL2_sample, ]
MEDmpg_Test = MEDmpg[-CL2_sample, ]
HIGHmpg_Training = HIGHmpg[CL3_sample, ]
HIGHmpg_Test = HIGHmpg[-CL3_sample, ]

# Combined the training sets from three classes as one training sets data frame.
# And, do the same thing for the test test. Thus, we have equally pick from three 
# classes to minimize the bias toward any class. 
Training_Set = rbind(LOWmpg_Training, MEDmpg_Training, HIGHmpg_Training)
Test_Set = rbind(LOWmpg_Test, MEDmpg_Test, HIGHmpg_Test)

# kNN classifier and train at k = 5.
# First, use training set for train and training set for test. 
# Then, use training set for train and test set for test.
Training_result = knn(train = Training_Set[, -1], test = Training_Set[, -1], cl = Training_Set[, "mpg"], k = 5)
Test_result = knn(train = Training_Set[, -1], test = Test_Set[, -1], cl = Training_Set[, "mpg"], k = 5)

# Calculate accuracy of prediction for both training set and test set. Then, generate 
# a table and compare the accuracy values. 
AccTrain = round(sum(Training_result == Training_Set[, "mpg"])/dim(Training_Set)[1], 2)
AccTest = round(sum(Test_result == Test_Set[, "mpg"])/dim(Test_Set)[1], 2)
AccTable_k_5 = data.frame(AccTrain, AccTest)

# Create confusion matrix and calculate global performance for both training set and test set.
confusion_matrix_training = table(Training_result, Training_Set[,"mpg"])
rownames(confusion_matrix_training) = c("True High", "True Low", "Ture Med")
colnames(confusion_matrix_training) = c("Pred High", "Pred Low", "Pred Med")
confusion_matrix_training = dcast(as.data.frame(confusion_matrix_training), Training_result ~ Var2)
rownames(confusion_matrix_training) = confusion_matrix_training[, 1]
confusion_matrix_training[, 1] = NULL
for (i in 1:3){
  confusion_matrix_training[i, ] = round(confusion_matrix_training[i, ] / sum(confusion_matrix_training[i, ]), 3)
}
global_training_perf = (confusion_matrix_training[1, 1] + confusion_matrix_training[2, 2] + confusion_matrix_training[3, 3])/3

confusion_matrix_test=  table(Test_result, Test_Set[,"mpg"])
rownames(confusion_matrix_test) = c("True High", "True Low", "Ture Med")
colnames(confusion_matrix_test) = c("Pred High", "Pred Low", "Pred Med")
confusion_matrix_test = dcast(as.data.frame(confusion_matrix_test), Test_result ~ Var2)
rownames(confusion_matrix_test) = confusion_matrix_test[, 1]
confusion_matrix_test[, 1] = NULL
for (i in 1:3){
  confusion_matrix_test[i, ] = round(confusion_matrix_test[i, ] / sum(confusion_matrix_test[i, ]), 3)
}
global_test_perf = (confusion_matrix_test[1, 1] + confusion_matrix_test[2, 2] + confusion_matrix_test[3, 3])/3


# Problem 12
# Create two empty data frames to record accuracy prediction and Performance for 
# both training and test under different k values.
k = c(3, 5, 7, 9, 11, 13, 15, 17, 19, 29, 39)
AccTable = data.frame(k)
Performance_Table = data.frame(k)

# Train the data set under different k value.
for (i in c(1:length(k))){
  Training_result = knn(train = Training_Set[, -1], test = Training_Set[, -1], cl = Training_Set[, "mpg"], k = k[i])
  Test_result = knn(train = Training_Set[, -1], test = Test_Set[, -1], cl = Training_Set[, "mpg"], k = k[i])
  
  AccTrain = sum(Training_result == Training_Set[, "mpg"])/dim(Training_Set)[1]
  AccTest = sum(Test_result == Test_Set[, "mpg"])/dim(Test_Set)[1]

  AccTable[i, "AccTrain"] = AccTrain
  AccTable[i, "AccTest"] = AccTest
  
  confusion_matrix_test= table(Test_result, Test_Set[,"mpg"])
  rownames(confusion_matrix_test) = c("True High", "True Low", "Ture Med")
  colnames(confusion_matrix_test) = c("Pred High", "Pred Low", "Pred Med")
  confusion_matrix_test = dcast(as.data.frame(confusion_matrix_test), Test_result ~ Var2)
  rownames(confusion_matrix_test) = confusion_matrix_test[, 1]
  confusion_matrix_test[, 1] = NULL
  for (j in 1:3){
    confusion_matrix_test[j, ] = round(confusion_matrix_test[j, ] / sum(confusion_matrix_test[j, ]), 3)
  }

  global_test_perf = round((confusion_matrix_test[1, 1] + confusion_matrix_test[2, 2] + confusion_matrix_test[3, 3])/3, 3)
  Performance_Table[i, "Performance"] = global_test_perf
  Performance_Table[i, "Margin on Performance"] = round(sqrt(global_test_perf * (1 - global_test_perf) / dim(Test_Set)[1]), 3)
} 

# Plot the k value against accuracy of prediction.
melted_AccTable = melt(AccTable, id.vars = "k")
ggplot(melted_AccTable, aes(k, value)) + 
  geom_line(aes(colour = variable)) + 
  ylab("Accuracy Prediction") +
  geom_point() + 
  geom_text(aes(label = round(value, 2)), hjust = 0.5, vjust = -1) +
  scale_x_discrete(limits = c(3, 5, 7, 9, 11, 13, 15, 17, 19, 29, 39))
dev.off()
