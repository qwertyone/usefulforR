install.packages("tables")
library(tables)
#df<-read.csv("TurkOpticonPay_3.19_3.21.csv",header=TRUE)
df<- structure(list(Sampling = c(2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), Task.Code = c(25L, 17L, 9L, 13L, 32L, 38L, 12L, 73L, 5L, 42L, 61L, 7L, 18L, 14L, 63L, 20L, 12L, 13L, 14L, 15L, 17L, 20L, 21L, 22L, 26L, 28L, 32L, 37L, 39L, 40L, 42L, 43L, 44L, 45L, 47L, 48L, 49L, 52L, 53L, 54L, 55L, 56L, 63L, 64L, 65L, 66L), Task.Type = structure(c(10L, 1L, 10L, 10L, 10L, 2L, NA, 10L, 6L, 10L, 10L, 10L, NA, 10L, 6L, 7L, 10L, 5L, NA, 7L, 5L, 10L, 10L, 8L, 10L, 10L, 10L, 10L, 10L, 10L, 10L, 10L, 4L, 10L, 10L, 10L, 10L, 10L, 3L, 10L, 10L, 3L, NA, 9L, 10L, 10L), .Label = c("ArtificialIntelligence", "AudioTranslation", "Interactive Decision Making", "Judgment", "Pattern Matching", "Perception", "Rating", "Search", "Selection", "Survey"), class = "factor"), Payment.Rating = c(1L, 2L, 2L, 1L, 1L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 4L, 4L, 4L, 5L, NA, 5L, 1L, 1L, 5L, NA, 4L, 3L, 5L, 4L, 5L, 5L, 4L, 5L, 5L, NA, 1L, 5L, 5L, 5L, NA, 1L, 2L, NA, 5L, 4L, 5L, 4L, 2L, 5L), Per.Per.Minute = c(0,0.000277778, 0.004545455, 0.02, 0.022727273, NA, 0.03, 0.045454545, 0.05, NA, 0.057142857, 0.058823529, 0.25, 0.5, 0.545454545, 1.083333333, 0.0625, 0.155, 0.05, 0.032352941, 0.111282051, 0.06, 0.114285714, 0.08, 0.125, 0.083333333, 0.1125, 0.15, 0.083333333, 0.18, 0.08, 0.18, 0.071428571, 0.1, 0.24516129, 0.316666667, 0.085714286, 0.05, 0.08, 0.09380863, 0.25, NA, 0.166666667, 0.72, 0.0625, NA), Pay.per.Hour = c(0, 0.016666667, 0.272727273, 1.2, 1.363636364, 1.5, 1.8, 2.727272727, 3, 3, 3.428571429, 3.529411765, 15, 30, 32.72727273, 65, 3.75, 9.3, 3, 1.941176471, 6.676923077, 3.6, 6.857142857, 4.8, 7.5, 5, 6.75, 9, 5, 10.8, 4.8, 10.8, 4.285714286,6, 14.70967742, 19, 5.142857143, 3, 4.8, 5.628517824, 15, 4.99,10, 43.2, 3.75, 15.08), Minutes = c(3, 900, 22, 20, 22, NA, NA,11, 15, NA, 7, 8.5, 2, 1, 5.5, 3, 8, 14, 10, 34, 19.5, 10, 3.5,5, 20, 12, 8, 6, 6, 5, 12.5, 5, 42, 4, 7.75, 6, 5.25, 30, 15, 5.33, 2, NA, 3, 3.5, 8, NA), Payment = c(0, 0.25, 0.1, 0.4, 0.5,NA, NA, 0.5, 0.75, NA, 0.4, 0.5, 0.5, 0.5, 3, 3.25, 0.5, 2.17,0.5, 1.1, 2.17, 0.6, 0.4, 0.4, 2.5, 1, 0.9, 0.9, 0.5, 0.9, 1, 0.9, 3, 0.4, 1.9, 1.9, 0.45, 1.5, 1.2, 0.5, 0.5, NA, 0.5, 0.36, 0.5, NA)), .Names = c("Sampling", "Task.Code", "Task.Type", "Payment.Rating", "Per.Per.Minute", "Pay.per.Hour", "Minutes", "Payment"), class = "data.frame", row.names = c(NA,-46L))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                -46L))
str(df)
df$Sampling<-as.factor(df$Sampling)
df$Task.Code<-as.factor(df$Task.Code)
df$Payment.Rating<-as.ordered(df$Payment.Rating)

with(df,
     table(Task.Type,Sampling))

tabular(Task.Type + Hline (2:6) + 1 ~ (Pay.per.Hour + Payment+Minutes)*(mean + sd),data=df)

barplot(table(df$Payment.Rating),main="Frequency of Payment Ratings", xlab = "Turker Rating")

Pay5 <- df[which(df$Payment.Rating == 5 & df$Task.Type == "Survey"),]
Pay4 <- df[which(df$Payment.Rating == 4 & df$Task.Type == "Survey"),]
Pay3 <- df[which(df$Payment.Rating == 3 & df$Task.Type == "Survey"),]
Pay2 <- df[which(df$Payment.Rating == 2 & df$Task.Type == "Survey"),]
Pay1 <- df[which(df$Payment.Rating == 1 & df$Task.Type == "Survey"),]
PayAll <- df[which(df$Task.Type == "Survey"),]
t.test(Pay5$Pay.per.Hour,AllOthers$Pay.per.Hour)
boxplot(list(Pay1$Pay.per.Hour,Pay2$Pay.per.Hour,Pay3$Pay.per.Hour,Pay4$Pay.per.Hour,Pay5$Pay.per.Hour),xlab="Turker Vote", ylab="$ per Hour",main="Comparison of Survey Pay Over Worker Vote")
#,PayAll$Pay.per.Hour,AllOthers$Pay.per.Hour
