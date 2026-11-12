sum(is.na(train))
dim(data)

sum(duplicated(data))
summary(train)
sapply(train, function(x)length(unique(x)))
descriptive <- data.frame(
  Mean = sapply(train, mean),
  Median = sapply(train, median),
  SD = sapply(train, sd),
  Variance = sapply(train, var),
  Minimum = sapply(train, min),
  Maximum = sapply(train, max),
  IQR = sapply(train, IQR)
)

round(descriptive, 3)
hist(train$medv,
     main = "Distribution of House Prices",
     xlab = "Median House Value",
     col = "lightblue",
     border = "white",
     breaks = 20)



plot(density(train$medv),
     main = "Density Plot of House Prices",
     xlab = "MEDV",
     col = "blue",
     lwd = 2)

boxplot(train$medv,
        main = "Boxplot of House Prices",
        ylab = "MEDV",
        col = "lightgreen")


par(mfrow = c(3, 5))

for (v in names(train)) {
  hist(train[[v]],
       main = v,
       xlab = v,
       col = "lightblue")
}

par(mfrow = c(1, 1))

plot(train$rm, train$medv,
     main = "House Price vs Average Number of Rooms",
     xlab = "Average Number of Rooms",
     ylab = "MEDV",
     pch = 19,
     col = "blue")

abline(lm(medv ~ rm, data = train),
       col = "red",
       lwd = 2)


plot(train$lstat, train$medv)
plot(train$crim, train$medv)
plot(train$ptratio, train$medv)
plot(train$nox, train$medv)



boxplot(medv ~ chas,
        data = train,
        main = "House Prices by Charles River Location",
        xlab = "CHAS",
        ylab = "MEDV",
        col = c("lightblue", "lightgreen"))


cor(train$rm, train$medv)

cor_matrix <- cor(train)

round(cor_matrix, 2)

cor_with_medv <- sort(
  cor_matrix[, "medv"],
  decreasing = TRUE
)

cor_with_medv

install.packages("corrplot")
library(corrplot)

corrplot(cor_matrix,
         method = "color",
         type = "full",
         addCoef.col = "black",
         tl.col = "black",
         number.cex = 0.6)


set.seed(123)

n <- nrow(train)

train_index <- sample(
  seq_len(n),
  size = floor(0.8 * n)
)

train_data <- train[train_index, ]

test_data <- train[-train_index, ]

# Check dimensions
dim(train_data)
dim(test_data)

cor.test(train_data$rm,
         train_data$medv,
         method = "pearson")
