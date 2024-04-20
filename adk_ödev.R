### ÖDEV 1
## VERİ ÜRETME
# Veri oluşturmak için ilgili paketi indirme ve kütüphaneden çağırma
install.packages("MASS")
library(MASS)

# Veri seti oluşturma
set.seed(123) 
n <- 100
bagimsiz1 <- rnorm(n, mean = 10, sd = 2) 
bagimsiz2 <- rnorm(n, mean = 5, sd = 1) 
bagimli <- rnorm(n, mean = 3 + 2*bagimsiz1 - 1.5*bagimsiz2, sd = 2)

# Veri setini veri çerçevesine dönüştürme
data <- data.frame(bagimli, bagimsiz1, bagimsiz2)

# Veri setini gösterme
head(data)

# Değişkenleri sayısal/numerik değişkene dönüştürme
bagimsiz1_factor <- as.factor(bagimsiz1)
bagimsiz2_factor <- as.factor(bagimsiz2)
bagimli_factor <- as.factor(bagimli)

# Değişken türlerini kontrol etme
str(bagimsiz1_factor)
str(bagimsiz2_factor)
str(bagimli_factor)

# Eksik veri tespiti
na_bagimsiz1 <- is.na(bagimsiz1)
na_bagimsiz2 <- is.na(bagimsiz2)
na_bsagimli <- is.na(bagimli)

# Eksik verileri ortalama ile doldurma
mean_bagimsiz1 <- mean(bagimsiz1, na.rm = TRUE)
mean_bagimsiz2 <- mean(bagimsiz2, na.rm = TRUE)
mean_bagimli <- mean(bagimli, na.rm = TRUE)
bagimsiz1_filled <- ifelse(is.na(bagimsiz1), mean_bagimsiz1, bagimsiz1)
bagimsiz2_filled <- ifelse(is.na(bagimsiz2), mean_bagimsiz2, bagimsiz2)
bagimli_filled <- ifelse(is.na(bagimli), mean_bagimli, bagimli)

# Kutu grafiği oluşturma
boxplot(data)

# Z-puanı hesaplama
z_scores <- scale(data)

# Aykırı değerleri belirleme
outliers <- abs(z_scores) > 3

# Alt ve üst çeyreklikleri hesaplama
Q1 <- quantile(data, 0.25)
Q3 <- quantile(data, 0.75)

# Alt ve üst sınırı hesaplama
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Aykırı değerleri belirleme
outliers <- data < lower_bound | data > upper_bound

# Histogram oluşturma
hist(data)

# Kutu grafiği oluşturma
boxplot(data)

# Q-Q plot oluşturma
qqnorm(data)
qqline(data)

# Kantillerden yararlanma
summary(data)

# Korelasyon katsayısını hesaplama
correlation1 <- cor(bagimsiz1, bagimsiz2)
correlation2 <- cor(bagimsiz1, bagimli)
correlation3 <- cor(bagimsiz2, bagimli)

# Korelasyon katsayısını ekrana yazdırma
print(correlation1)
print(correlation2)
print(correlation3)

## Çoklu doğrusal regresyon modelini oluşturma
model <- lm(bagimli ~ bagimsiz1 + bagimsiz2)

# Model özetini alma
summary(model)

# Bağımsız değişkenler arasındaki korelasyonu hesaplama
correlation_matrix <- cor(data.frame(bagimsiz1, bagimsiz2, bagimli))
print(correlation_matrix)

# corrplot paketi ile grafik çizme 
library(corrplot)

# Korelasyon matrisini görselleştirme
corrplot(correlation_matrix, method = "color")

# Değişkenlerin grafiğini çizme
par(mfrow=c(1,3)) 
plot(bagimsiz1, bagimli, main = "Bagimsiz1 vs. Bagimli", xlab = "x1", ylab = "y", col = "blue", pch = 16)
plot(bagimsiz2, bagimli, main = "Bagimsiz2 vs Bagimli", xlab = "x2", ylab = "y", col = "red", pch = 16)
plot(bagimsiz1, bagimsiz2, main = "Bagimsiz1 vs Bagimsiz2", xlab = "x3", ylab = "y", col = "green", pch = 16)

# Veriyi standartlaştırma
x1_standardized <- scale(bagimsiz1)
x2_standardized <- scale(bagimsiz2)
y_standardized <- scale(bagimli)

# Standartlaştırılmış veriyi kontrol etme
summary(x1_standardized)
summary(x2_standardized)
summary(y_standardized)

## Son değişiklik
# Veriyi test ve eğitim alt kümelerine böleme
library(caTools)

split <- sample.split(bagimli, SplitRatio = 0.7) # 70% eğitim, 30% test
train_data <- subset(data.frame(bagimsiz1, bagimsiz2, bagimli), split == TRUE)
test_data <- subset(data.frame(bagimsiz1, bagimsiz2, bagimli), split == FALSE)

# Verinin boyutlarını kontrol etme
dim(train_data)
dim(test_data)

################################################################################
################################################################################
# ÖDEV 2 
# VERİ ÇEKME

#Çalışma dizini tespiti ve değişimi
getwd()
setwd("/Users/ahmetdogankuday/Desktop")

# Gerekli paketlerin çalıştırılması
library(caret)
library(corrplot)
library(ggplot2)
library(readxl)
library(readr)
library(tidyverse)
library(glmnet)

# Verinin okutulması
data <- read_excel("/Users/ahmetdogankuday/Desktop/data.xlsx")
View(data)
names(data)

# Değişkenleri sayısal/numerik değişkene dönüştürme
bagimsiz1_factor <- as.factor(data$bagimsiz1)
bagimsiz2_factor <- as.factor(data$bagimsiz2)
bagimli_factor <- as.factor(data$bagimli)

# Değişken türlerini kontrol etme
str(bagimsiz1_factor)
str(bagimsiz2_factor)
str(bagimli_factor)

# Eksik veri tespiti
na_bagimsiz1 <- is.na(data$bagimsiz1)
na_bagimsiz2 <- is.na(data$bagimsiz2)
na_bsagimli <- is.na(data$bagimli)

# Eksik verileri ortalama ile doldurma
mean_bagimsiz1 <- mean(data$bagimsiz1, na.rm = TRUE)
mean_bagimsiz2 <- mean(data$bagimsiz2, na.rm = TRUE)
mean_bagimli <- mean(data$bagimli, na.rm = TRUE)
bagimsiz1_filled <- ifelse(is.na(data$bagimsiz1), mean_bagimsiz1, data$bagimsiz1)
bagimsiz2_filled <- ifelse(is.na(data$bagimsiz2), mean_bagimsiz2, data$bagimsiz2)
bagimli_filled <- ifelse(is.na(data$bagimli), mean_bagimli, data$bagimli)

# Kutu grafiği oluşturma
boxplot(data)

# Z-puanı hesaplama
z_scores <- scale(data)

# Aykırı değerleri belirleme
outliers <- abs(z_scores) > 3

# Alt ve üst çeyreklikleri hesaplama
Q1 <- quantile(data, 0.25)
Q3 <- quantile(data, 0.75)

# Alt ve üst sınırı hesaplama
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Aykırı değerleri belirleme
outliers <- data < lower_bound | data > upper_bound

# Histogram oluşturma
hist(data)

# Kutu grafiği oluşturma
boxplot(data)

# Q-Q plot oluşturma
qqnorm(data)
qqline(data)

# Kantillerden yararlanma
summary(data)

## Çoklu doğrusal regresyon modelini oluşturma
model <- lm(data$bagimli ~ data$bagimsiz1 + data$bagimsiz2)

# Model özetini alma
summary(model)

# Bağımsız değişkenler arasındaki korelasyonu hesaplama
correlation_matrix <- cor(data.frame(data$bagimsiz1, data$bagimsiz2, data$bagimli))
print(correlation_matrix)

# corrplot paketi ile grafik çizme 
library(corrplot)

# Korelasyon matrisini görselleştirme
corrplot(correlation_matrix, method = "color")

# Değişkenlerin grafiğini çizme
par(mfrow=c(1,3)) 
plot(data$bagimsiz1, data$bagimli, main = "Bagimsiz1 vs. Bagimli", xlab = "x1", ylab = "y", col = "blue", pch = 16)
plot(data$bagimsiz2, data$bagimli, main = "Bagimsiz2 vs Bagimli", xlab = "x2", ylab = "y", col = "red", pch = 16)
plot(data$bagimsiz1, data$bagimsiz2, main = "Bagimsiz1 vs Bagimsiz2", xlab = "x3", ylab = "y", col = "green", pch = 16)

# Veriyi standartlaştırma
x1_standardized <- scale(data$bagimsiz1)
x2_standardized <- scale(data$bagimsiz2)
y_standardized <- scale(data$bagimli)

# Standartlaştırılmış veriyi kontrol etme
summary(x1_standardized)
summary(x2_standardized)
summary(y_standardized)

## Son değişiklik
# Veriyi test ve eğitim alt kümelerine böleme
library(caTools)

split <- sample.split(data$bagimli, SplitRatio = 0.8) # 70% eğitim, 20% test
train_data <- subset(data.frame(data$bagimsiz1, data$bagimsiz2, data$bagimli), split == TRUE)
test_data <- subset(data.frame(data$bagimsiz1, data$bagimsiz2, data$bagimli), split == FALSE)

# Verinin boyutlarını kontrol etme
dim(train_data)
dim(test_data)

#--------------------------------------------------
#--------------------------------------------------