### DATA PREPROCESSING ###

## READING THE DATASET

# Reading data
water <- read.csv("C:/Users/Nhu/Desktop/XSTK/water_probability/water_potability.csv")

# Basic info
cat("First 10 rows of the dataset:\n")
print(head(water, 10))
cat("\nStructure of the dataset:\n")
print(str(water))

## HANDLING MISSING VALUES

# Check missing values
cat("\nMissing values before handling:\n")
print(colSums(is.na(water)))

# Data visualization
par(mfrow = c(2, 3))
hist(water$ph, xlab = "ph", main = "Histogram of ph", col = "blue")
hist(water$Sulfate, xlab = "Sulfate",
     main = "Histogram of Sulfate",
     col = "orange")
hist(water$Trihalomethanes, xlab = "Trihalomethanes",
     main = "Histogram of Trihalomethanes",
     col = "green")
boxplot(ph ~ Potability, data = water,
        main = "Boxplot of pH for Potability",
        col = c("grey", "lightblue"))
boxplot(Sulfate ~ Potability, data = water,
        main = "Boxplot of Sulfate for Potability",
        col = c("grey", "lightblue"))
boxplot(Trihalomethanes ~ Potability, data = water,
        main = "Boxplot of Trihalomethanes for Potability",
        col = c("grey", "lightblue"))

# Replace missing values with median
cols <- c("ph", "Sulfate", "Trihalomethanes")
for (col in cols) {
  water[[col]][is.na(water[[col]])] <- median(water[[col]], na.rm = TRUE)
}

# Check missing values after handling
cat("\nMissing values after handling:\n")
print(colSums(is.na(water)))

## OUTLIER RATIO STATISTICS
outlier_ratio <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <-  q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  outliers <- x[x < lower | x > upper]
  n_outlier <- length(outliers)
  total <- sum(!is.na(x))
  ratio <- n_outlier / total
  c(outlier_count = n_outlier, total = total, outlier_rate = ratio)
}
cat("\nOutlier ratio statistics:\n")
outlier_table <- t(sapply(water[, 1:10], outlier_ratio))
print(outlier_table)

## POTABILITY FACTOR CONVERSION
water$Potability <- as.factor(water$Potability)

### DESCRIPTIVE STATISTICS ###

## GROUP-WISE DESCRIPTIVE ANALYSIS
describe <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  mean_val <- mean(x)
  sd_unbiased <- sd(x)
  sd_biased <- sqrt(sum((x - mean_val)^2) / n)
  q1 <- quantile(x, probs = 0.25)
  q2 <- median(x)
  q3 <- quantile(x, probs = 0.75)
  min_val <- min(x)
  max_val <- max(x)

  c(count = n,
    mean = mean_val,
    sd_unbiased = sd_unbiased,
    sd_biased = sd_biased,
    min = min_val,
    max = max_val,
    range = max_val - min_val,
    Q1 = q1,
    Q2_Median = q2,
    Q3 = q3)
}
cat("\nGroup-wise descriptive statistics (by Potability):\n")
result <- by(water[, 1:9], water$Potability, function(df) {
  t(sapply(df, describe))
})
print(result)

## POTABILITY DISTRIBUTION ANALYSIS
cat("\nPotability distribution:")
pot_table <- table(water$Potability)
print(pot_table)
par(mar = c(6, 6, 4, 2))
barplot(pot_table,
        main = "Potability Distribution",
        xlab = "Potability",
        ylab = "Frequency",
        col = c("lightblue", "pink"),
        cex.names = 1.5,
        cex.axis = 1.5,
        cex.lab = 1.5,
        cex.main = 1.8)

## FEATURE DISTRIBUTION BY POTABILITY CLASSES
select_data <- water[, 1:9]
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))
for (col in names(select_data)) {
  group0 <- select_data[water$Potability == 0, col]
  group1 <- select_data[water$Potability == 1, col]
  max_val <- max(c(group0, group1), na.rm = TRUE)
  min_val <- min(c(group0, group1), na.rm = TRUE)
  hist(group0,
       breaks = 20,
       col = rgb(1, 0, 0, 0.5),
       xlim = c(min_val, max_val),
       main = paste("Histogram of", col),
       xlab = col, ylab = "Frequency")
  hist(group1, breaks = 20, col = rgb(0, 0, 1, 0.5), add = TRUE)
  legend("topright", legend = c("0", "1"),
         fill = c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5)))
}

## FEATURE COMPARISION USING BOXPLOTS BY POTABILITY
features <- names(water)[1:9]
col_group <- c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5))
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))
for (feature in features) {
  boxplot(
    water[[feature]] ~ water$Potability,
    main = paste("Boxplot of", feature),
    col = col_group,
    xlab = "Potability",
    ylab = feature
  )
}

## CORRELATION MATRIX
par(mfrow = c(1, 1))
cor_matrix <- cor(water[, 1:9], use = "complete.obs")
cat("\nCorrelation matrix:\n")
print(round(cor_matrix, 4))
library(corrplot)
corrplot(cor_matrix,
         method = "color",
         type = "full",
         order = "hclust",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         diag = FALSE)

### ANOVA ###

## pH AND SULFATE LEVEL CATEGORIZATION
water$pH_group <- cut(water$ph,
                      breaks = c(-Inf, 6.5, 7.5, Inf),
                      labels = c("Low", "Medium", "High"))
water$Sulfate_group <- cut(water$Sulfate,
                           breaks = c(-Inf, 300, 340, Inf),
                           labels = c("Low", "Medium", "High"))
cat("\npH group distribution:")
print(table(water$pH_group))
cat("\nSulfate group distribution:")
print(table(water$Sulfate_group))
cat("\nCross table of pH and Sulfate groups:")
print(table(water$pH_group, water$Sulfate_group))

## NORMALITY ASSUMPTION CHECK
cat("\nShapiro-Wilk normality test results:")
normality_result <- by(water$Organic_carbon,
                       list(water$pH_group, water$Sulfate_group),
                       shapiro.test)
group_names <- expand.grid(
  pH = levels(water$pH_group),
  Sulfate = levels(water$Sulfate_group)
)
for (i in seq_along(normality_result)) {
  cat("\npH group:", group_names$pH[i], "\n")
  cat("Sulfate group:", group_names$Sulfate[i], "\n")
  w <- normality_result[[i]]$statistic
  p <- normality_result[[i]]$p.value
  cat("W =", round(w, 5), "\n")
  cat("p-value =", round(p, 5), "\n")
}

## HOMOGENEITY OF VARIANCE ASSESSMENT
library(car)
result <- leveneTest(Organic_carbon ~ pH_group * Sulfate_group, data = water)
cat("\n")
print(result)

## TWO-WAY ANOVA ON ORGANIC CARBON BY pH AND SULFATE GROUPS
anova_model <- aov(Organic_carbon ~ pH_group * Sulfate_group,
                   data = water)
cat("\nTwo-way ANOVA results for organic carbon (pH group - sulfate group):")
print(summary(anova_model))
cat("\n")

## TUKEY HSD FOR SULFATE GROUP
print(TukeyHSD(anova_model, "Sulfate_group"))

### LOGISTIC REGRESSION ###

## TRAIN-TEST SPLIT (70% TRAINING, 30% TESTING)
set.seed(8)
train_rows <- sample(rownames(water), dim(water)[1] * 0.7)
train_data <- water[train_rows, ]
test_rows <- setdiff(rownames(water), train_rows)
test_data <- water[test_rows, ]
cat("Training set dimensions:\n")
print(dim(train_data))
cat("\nTesting set dimensions:\n")
print(dim(test_data))

## POTABILITY FREQUENCY (TRAINING SET)
cat("\nPotability frequency in training set:")
print(table(train_data$Potability))
weights <- ifelse(train_data$Potability == 1, 1414 / 879, 1)

## MODEL SELECTION
# Model 1: Full logistic regression model
cat("\nModel 1: Full logistic regression model")
weights <- ifelse(train_data$Potability == 1, 1414 / 879, 1)
model_1 <- glm(Potability ~
                 ph + Hardness + Solids + Chloramines + Sulfate + Conductivity +
                   Organic_carbon + Trihalomethanes + Turbidity,
               data = train_data,
               family = "binomial",
               weights = weights)
print(summary(model_1))

# Model 2: Only solids and organic_carbon logistic regression model
cat("Model 2: Only solids and organic_carbon logistic regression model")
model_2 <- glm(Potability ~
                 Solids + Organic_carbon,
               data = train_data,
               family = "binomial",
               weights = weights)
print(summary(model_2))

## PREDICTION
prediction <- predict(model_2, test_data, type = "response")
test_data$prediction <- round(prediction)
cat("First 10 rows of test data with predictions:\n")
print(head(test_data, 10))

## CONFUSION MATRIX
library(caret)
confusion_matrix <- confusionMatrix(
                                    as.factor(test_data$prediction),
                                    as.factor(test_data$Potability),
                                    positive = "1")
cat("\n")
print(confusion_matrix)

## ROC CURVE AND AUC ANALYSIS
library(pROC)
prediction <- predict(model_2, test_data, type = "response")
roc_curve <- roc(test_data$Potability, prediction)
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve - Logistic Regression")
abline(a = 0, b = 1, lty = 2, col = "red")
auc_value <- auc(roc_curve)
cat("\nModel evaluation result:\n")
print(paste("AUC value:", round(auc_value, 3)))

## MULTICOLLINEARITY ASSESSMENT
library(car)
library(lmtest)
cat("\nVIF results:\n")
print(vif(model_2))

## LINEARITY OF THE LOGIT
plot(train_data$Solids,
     log(model_2$fitted.values / (1 - model_2$fitted.values)),
     main = "Linearity check: Solids vs log-odds",
     xlab = "Solids", ylab = "log-odds")

plot(train_data$Organic_carbon,
     log(model_2$fitted.values / (1 - model_2$fitted.values)),
     main = "Linearity check: Organic_carbon vs log-odds",
     xlab = "Organic_carbon", ylab = "log-odds")

## DETECTION OF INFLUENTIAL OUTLIERS
plot(model_2, which = 5)