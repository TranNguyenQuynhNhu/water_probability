### DATA PREPROCESSING ###

## READING THE DATASET

# Reading data
water <- read.csv("C:/Users/Nhu/Desktop/XSTK/water_potability.csv")

# Basic info
print(head(water, 10))
print(str(water))

## HANDLING MISSING VALUES

# Check missing values
print("Before handling missing values:")
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
print("After handling missing values:")
print(colSums(is.na(water)))

# DATA FORMATTING AND TYPE CONVERSION

# ADDING NEW VARIABLES (FEATURE CREATION)

# REMOVING UNNECESSARY VARIABLES

# TRANSFORMING VARIABLES