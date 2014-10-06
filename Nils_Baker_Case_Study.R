library(xlsx)
library("psych")
library('xtable')
library('car')
library("lmtest")
library("MASS")
library('stats')

# load data
data <- read.xlsx("UV6486-XLS-ENG.xlsx", 2)
data <- data[2:length(data)]
data <- data[-nrow(data),]
data$dummy <- ifelse(data$Inside.Outside.Footprint=="Inside",1,0)


#univariate analysis
xtable(describe(data))
# boxplots
pdf("Boxplot_of_Total_Households_per_Area_all.pdf", width=5, height=7)
boxplot(data[1], main="Total Households per Area", ylab="Households per Area")
dev.off()

pdf("Boxplot_of_Households_with_Accounts_all.pdf", width=5, height=7)
boxplot(data[2], main="Households with Accounts", ylab="Households with Accounts")
dev.off()
#cor(data[1:2])

# plot col1, col2
pdf("Households_with_Accounts_vs_Total_Households_in_Area_all.pdf", width=7, height=5)
plot(x=data$Total.Households.in.Area, y=data$Households.with.Account, main='Households with Accounts vs. \nTotal Households in Area', xlab='Total Households in Area', ylab='Households with Accounts')
dev.off()

# linear regression
MLR <- lm(as.formula(paste(colnames(data)[2],'~ (',colnames(data)[1],'+', colnames(data)[4],')^2')),data=data)
outlierTest(MLR)
xtable(summary(MLR))

# Residual plots and QQ plots
pdf("Residuals_Plot_all.pdf", width=7, height=5)
residuals_vs_Y <- cbind(MLR$fitted.values, MLR$residuals)
plot(residuals_vs_Y,xlab="Fitted Values", ylab="Residuals", main="Residuals Plot")
abline(h=0)
dev.off()

pdf("Sequence_Plot_all.pdf", width=7, height=5)
plot(MLR$residuals, xlab="Entries", ylab="Residuals", main="Sequence Plot", type="o", col="black")
dev.off()

pdf("Studentized_Residuals_all.pdf", width=7, height=5)
plot(studres(MLR), main='Studentized Residuals',xlab='Observations',ylab='Studentized Residuals')
abline(h=0)
dev.off()

pdf("QQ_Plot_all.pdf", width=7, height=5)
qqPlot(MLR$residuals, distribution='norm', main='Normal Q-Q Plot', xlab='Expected Distribution', ylab='Theoretical Distribution')
dev.off()

# tests
bptest(MLR)
shapiro.test(MLR$residuals)


# box-cox transformation
source("Search_for_Lambda.R")
lambda <- bisectionBC(data)
data_transform <- cbind(data, data[2]^lambda)
names(data_transform)[5] <- 'transform_data'

pdf("Households_with_Accounts_vs_Total_Households_in_Area_transformed.pdf", width=7, height=5)
plot(x=data_transform$Total.Households.in.Area, y=data_transform$transform_data, main='Households with Accounts (Transformed) vs. \nTotal Households in Area', xlab='Total Households in Area', ylab='Households with Accounts')
dev.off()

# regression after transform
MLR_transform <- lm(as.formula(paste(colnames(data_transform)[5],'~ (',colnames(data_transform)[1],'+', colnames(data_transform)[4],')^2')),data=data_transform)
xtable(summary(MLR_transform))
anova(MLR_transform)

# plots
pdf("Residual_Plot_Transformed.pdf", width=7, height=5)
residuals_vs_Y_transform <- cbind(MLR_transform$fitted.values, MLR_transform$residuals)
plot(residuals_vs_Y_transform,xlab="Fitted Values", ylab="Residuals", main="Residuals Plot (Transformed Model)")
abline(h=0)
dev.off()

pdf("Sequence_Plot_Transformed.pdf", width=7, height=5)
plot(MLR_transform$residuals, xlab="Entries", ylab="Residuals", main="Sequence Plot (Transformed Model)", type="o", col="black")
dev.off()

pdf("Studentized_Residuals_transformed.pdf", width=7, height=5)
plot(studres(MLR_transform), main='Studentized Residuals (Transformed Model)',xlab='Observations',ylab='Studentized Residuals')
abline(h=0)
dev.off()

pdf("QQ_Plot_transformed.pdf", width=7, height=5)
qqPlot(MLR_transform$residuals, distribution='norm', main='Normal Q-Q (Transformed Model)', xlab='Expected Distribution', ylab='Theoretical Distribution')
dev.off()

# # tests
bptest(MLR_transform)
shapiro.test(MLR_transform$residuals)

# robust linear regression bc outliers
RMLR <- rlm(as.formula(paste(colnames(data)[2],'~ (',colnames(data)[1],'+', colnames(data)[4],')^2')),data=data)
summary(RMLR)


# Residual plots and QQ plots

pdf("Robust_Residuals_Plot.pdf", width=7, height=5)
roresiduals_vs_Y <- cbind(RMLR$fitted.values, RMLR$residuals)
plot(roresiduals_vs_Y,xlab="Fitted Values", ylab="Robust Residuals", main="Robust Residuals Plot")
abline(h=0)
dev.off()

pdf("Robust_Sequence_Plot.pdf", width=7, height=5)
plot(RMLR$residuals, xlab="Entries", ylab="Robust Residuals", main="Robust Sequence Plot", type="o", col="black")
dev.off()

pdf("Robust_QQ_Plot.pdf", width=7, height=5)
qqPlot(RMLR$residuals, distribution='norm', main='Robust Normal Q-Q Plot', xlab='Expected Distribution', ylab='Theoretical Distribution')
dev.off()

# tests
bptest(RMLR)
shapiro.test(RMLR$residuals)


# outlier detect and fix
outlierTest(MLR, cutoff=0.05)
dataNoOutliers <- data[-c(5,12,22,32),]
pdf("Households_with_Accounts_vs_Total_Households_in_Area_no_outliers.pdf", width=7, height=5)
plot(x=dataNoOutliers$Total.Households.in.Area, y=dataNoOutliers$Households.with.Account, main='Households with Accounts vs. \nTotal Households in Area (Excl. Outliers)', xlab='Total Households in Area', ylab='Households with Accounts')
dev.off()

MLR_nooutliers <- lm(Households.with.Account ~ (Total.Households.in.Area + dummy)^2, data=dataNoOutliers)
xtable(summary(MLR_nooutliers))


# Residual plots and QQ plots

pdf("Residual_Plot_no_outliers.pdf", width=7, height=5)
residuals_vs_Y <- cbind(MLR_nooutliers$fitted.values, MLR_nooutliers$residuals)
plot(residuals_vs_Y,xlab="Fitted Values", ylab="Residuals", main="Residuals Plot (Excl. Outliers)")
abline(h=0)
dev.off()

pdf("Sequence_Plot_no_outliers.pdf", width=7, height=5)
plot(MLR_nooutliers$residuals, xlab="Entries", ylab="Residuals", main="Sequence Plot (Excl. Outliers)", type="o", col="black")
abline(h=0)
dev.off()

pdf("Studentized_Residuals_no_outliers.pdf", width=7, height=5)
plot(studres(MLR_nooutliers), main='Studentized Residuals\n(Excl. Outliers)',xlab='Observations',ylab='Studentized Residuals')
abline(h=0)
dev.off()

pdf("QQ_Plot_no_outliers.pdf", width=7, height=5)
qqPlot(MLR_nooutliers$residuals, distribution='norm', main='Normal Q-Q Plot\n(Excl. Outliers)', xlab='Expected Distribution', ylab='Theoretical Distribution')
dev.off()


# boxcox on outlier removed
source("Search_for_Lambda.R")
lambda_nooutliers <- bisectionBC(dataNoOutliers)
dataNoOutliers_transform <- cbind(dataNoOutliers, dataNoOutliers[2]^lambda_nooutliers)
names(dataNoOutliers_transform)[5] <- 'transform_dataNooutliers'

pdf("Households_with_Accounts_vs_Total_Households_in_Area_no_outliers_transformed.pdf", width=7, height=5)
plot(x=dataNoOutliers_transform$Total.Households.in.Area, y=dataNoOutliers_transform$transform_dataNooutliers, main='Households with Accounts (Transformed) vs. \nTotal Households in Area (Excl. Outliers)', xlab='Total Households in Area', ylab='Households with Accounts')
dev.off()

# multiple linear regression
MLRNoOutliers_transform <- lm(as.formula(paste(colnames(dataNoOutliers_transform)[5],'~ (',colnames(dataNoOutliers_transform)[1],'+', colnames(dataNoOutliers_transform)[4],')^2')),data=dataNoOutliers_transform)

# plots
pdf("Residual_Plot_Transformed_No_outliers.pdf", width=7, height=5)
residuals_vs_Y_transform_noOutliers <- cbind(MLRNoOutliers_transform$fitted.values, MLRNoOutliers_transform$residuals)
plot(residuals_vs_Y_transform_noOutliers,xlab="Fitted Values", ylab="Residuals", main="Residuals Plot (Transformed Excl. Outliers)")
abline(h=0)
dev.off()

pdf("Sequence_Plot_Transformed_No_outliers.pdf", width=7, height=5)
plot(MLRNoOutliers_transform$residuals, xlab="Entries", ylab="Residuals", main="Sequence Plot (Transformed Excl. Outliers)", type="o", col="black")
dev.off()

pdf("Studentized_Residuals_transformed_No_outliers.pdf", width=7, height=5)
plot(studres(MLRNoOutliers_transform), main='Studentized Residuals (Transformed Excl. Outliers)',xlab='Observations',ylab='Studentized Residuals')
abline(h=0)
dev.off()

pdf("QQ_Plot_transformed_No_outliers.pdf", width=7, height=5)
qqPlot(MLRNoOutliers_transform$residuals, distribution='norm', main='Normal Q-Q (Transformed Excl. Outliers)', xlab='Expected Distribution', ylab='Theoretical Distribution')
dev.off()


# split data and analyze sepeartely
data_inside <- subset(data, dummy==1)
data_outside <- subset(data, dummy==0)

xtable(describe(data_inside))
pdf("Boxplot_Total_Households_inside_outside.pdf", width=5, height=7)
boxplot(data_inside$Total.Households.in.Area, data_outside$Total.Households.in.Area, main="Total Households in Area", names=c('Inside', 'Outside'))
dev.off()

pdf("Boxplot_Households_with_account_inside_outside.pdf", width=5, height=7)
boxplot(data_inside$Households.with.Account, data_outside$Households.with.Account, main="Households with Account", names=c('Inside', 'Outside'))
dev.off()

pdf("x1_vs_x2_inside.pdf", width=7, height=5)
plot(x=data_inside$Total.Households.in.Area, y=data_inside$Households.with.Account, main='Households with Accounts \nvs. Total Households in Area (Group 1)', xlab='Total Households in Area', ylab='Households with Accounts')
dev.off()

# correlation matrix
cor(data_inside[1:2])

xtable(describe(data_outside))

pdf("x1_vs_x2_outside.pdf", width=7, height=5)
plot(x=data_outside$Total.Households.in.Area, y=data_outside$Households.with.Account, main='Households with Accounts \nvs. Total Households in Area (Group 2)', xlab='Total Households in Area', ylab='Households with Accounts')
dev.off()

# correlation matrix
cor(data_outside[1:2])

data_inside <- cbind(data_inside, data_inside[2]/data_inside[1])
colnames(data_inside)[5] <- 'account_ratio'
mean1 <- mean(data_inside$account_ratio)
var1 <- var(data_inside$account_ratio)
n1 <- nrow(data_inside)

data_outside <- cbind(data_outside, data_outside[2]/data_outside[1])
colnames(data_outside)[5] <- 'account_ratio'
mean2 <- mean(data_outside$account_ratio)
var2 <- var(data_outside$account_ratio)
n2 <- nrow(data_outside)

# compute t statistic for an unpaired t test for two means with different variance
t <- (mean1-mean2)/sqrt((var1/n1)+(var2/n2))
v_up <- ((var1/n1)+(var2/n2))^2
v_low <- (((var1/n1)^2)/(n1-1))+(((var2/n2)^2)/(n2-1))
v <- v_up/v_low
t_crit <- qt(0.975,v)
t
t_crit