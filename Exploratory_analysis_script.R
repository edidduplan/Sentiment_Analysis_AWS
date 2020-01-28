#============ Import libraries ===================
pacman::p_load(readr, tidyverse, plotly)
pacman::p_load(rstudioapi)
pacman::p_load(caret, caretEnsemble, corrplot, data.table)

#============ Reading csv =======================
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))
getwd()

iphone <- read.csv("Data/iphone_smallmatrix_labeled_8d.csv")
galaxy <- read.csv("Data/galaxy_smallmatrix_labeled_9d.csv")
galaxy8d <- read.csv("Data/galaxy_smallmatrix_labeled_8d.csv")

#============ Exploratory analysis ======================
head(iphone)
summary(iphone$iphonesentiment)

str(galaxy)

#---------------- Histograms ---------------------------
ggplot(iphone, aes(iphonesentiment)) +
  geom_histogram(stat = "count")

ggplot(iphone, aes(iphone)) + 
  geom_histogram() +
  ggtitle("iphone dataset - freq. iphone")

ggplot(iphone, aes(samsunggalaxy)) +
  geom_histogram() +
  ggtitle("iphone dataset - freq. samsung galaxy")

ggplot(galaxy, aes(galaxysentiment)) +
  geom_histogram()

ggplot(galaxy, aes(samsunggalaxy)) + 
  geom_histogram() +
  ggtitle("samsung galaxy dataset - freq. samsung galaxy")

ggplot(galaxy, aes(iphone)) +
  geom_histogram() +
  ggtitle("samsung galaxy dataset - freq. iphone")

ggplot(galaxy8d, aes(iphone)) +
  geom_histogram() +
  ggtitle("samsung galaxy8d dataset - freq. iphone")

ggplot(galaxy, aes(samsungdisneg)) +
  geom_histogram() +
  ggtitle("samsung galaxy dataset - freq. samsungdisneg")

#---------------- Boxplot ------------------------------
ggplot(galaxy, aes(x=0, y=samsungdisneg)) + 
  geom_boxplot() + 
  stat_summary(fun.y = median, color = "white", geom = "text",
               vjust = -0.7, aes(label = round(..y.., digits = 1)))

#============= Looking for rows with only zeros =====================
#------------- Sentiment > 4 ---------------------
galaxy45 <- filter(galaxy, galaxysentiment >= 4)

sum(galaxy45$iphone)
sum(galaxy45)
xxx = colSums(galaxy45)
min(xxx)

#------------- 
temp <- galaxy
temp$galaxysentiment <- NULL

#----------- Adding column "total numer of occurrences of words" -----

galaxy$total <- rowSums(select(galaxy, -galaxysentiment))

ggplot(galaxy, aes(total)) + geom_histogram()

ggplot(galaxy, aes(x = total, y = galaxysentiment)) +
  geom_point()

ggplot(galaxy, aes(x = samsungdispos, y = galaxysentiment)) +
  geom_point()

ggplot(galaxy, aes(x = samsungdisneg, y = galaxysentiment)) +
  geom_point()

ggplot(galaxy, aes(x = samsungdisneg, y = galaxysentiment)) +
  geom_point()

#---------- splitting daset "total" <3 ------------------
galaxy_lowtotal <- filter(galaxy, total < 3)
galaxy_hightotal <- filter(galaxy, total > 2)

ggplot(galaxy_hightotal, aes(x=0, y=total)) + 
  geom_boxplot() + 
  stat_summary(fun.y = median, color = "white", geom = "text",
               vjust = -0.7, aes(label = round(..y.., digits = 1))) +
  ggtitle("galaxy_hightotal")

ggplot(galaxy_lowtotal, aes(x=0, y=total)) + 
  geom_boxplot() + 
  stat_summary(fun.y = median, color = "white", geom = "text",
               vjust = -0.7, aes(label = round(..y.., digits = 1))) +
  ggtitle("galaxy_lowtotal")

#---------- splitting daset "total" <250 ------------------
galaxy_low250 <- filter(galaxy, total < 250)
galaxy_high250 <- filter(galaxy, total > 250)

#---------- Unit norm normalization per column ------------


#============= Correlation matrix ===================
corr_iphone <- cor(iphone, method = "pearson")

corrplot(corr_iphone, tl.cex = 0.6, method = "pie")

corrplot(cor_existingprod, method = "pie")

#============= Near Zero Variance ===================
nzvMetrics_iphone <- nearZeroVar(iphone, saveMetrics = TRUE)
nzvMetrics_galaxy <- nearZeroVar(galaxy, saveMetrics = TRUE)

nzvMetrics_galaxy_l <- nearZeroVar(galaxy_lowtotal, saveMetrics = TRUE)
nzvMetrics_galaxy_h <- nearZeroVar(galaxy_hightotal, saveMetrics = TRUE)

#============= iphone - feature selection =================

#------------ Recursive feature elimination ---------------------
# Sampling data
set.seed(45)
iphonesample <- iphone[sample(1:nrow(iphone), 1000, replace = F),]

# Set-up rfeControl
ctrl <- rfeControl(functions = rfFuncs, 
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

# Use rfe
rfeResults <- rfe(iphonesample[,1:58], 
                  iphonesample$iphonesentiment, 
                  sizes=(1:58), 
                  rfeControl=ctrl)

# Plot results
plot(rfeResults, type=c("g", "o"))

#------------- Feature selection -----------------------
iphonerfe <- iphone[, predictors(rfeResults)]
iphonerfe$iphonesentiment <- iphone$iphonesentiment

#----------- Adding column "total numer of occurrences of words" -----

iphone$total <- rowSums(select(iphone, -iphonesentiment))
ggplot(iphone, aes(x = total, y = iphonesentiment)) +
  geom_point()

#---------- splitting daset "total" <250 ------------------
iphone_low250 <- filter(iphone, total < 250)
iphone_high250 <- filter(iphone, total > 250)

#------ nearZeroVar() with saveMetrics = FALSE returns a vector 
nzv_iphone <- nearZeroVar(iphone, saveMetrics = FALSE) 
nzv_iphone

#------- create a new data set and remove near zero variance features
iphone_nzv <- iphone[,-nzv_iphone]

#========== Making the sentiment categorical =================
iphone_low250$iphonesentiment <- as.ordered(iphone_low250$iphonesentiment)
iphonerfe$iphonesentiment <- as.ordered(iphonerfe$iphonesentiment)
iphone_nzv$iphonesentiment <- as.ordered(iphone_nzv$iphonesentiment)
iphone$iphonesentiment <- as.ordered(iphone$iphonesentiment)

iphone$total <- NULL
iphone_low250$total <- NULL

#========== Reducing number of calsses to 3 ================
iphone_low250[(iphone_low250$iphonesentiment == 0) | 
                (iphone_low250$iphonesentiment == 1), 
              "iphonesentiment"] <- -1

iphone_low250[is.na(iphone_low250$iphonesentiment),  
              "iphonesentiment"] <- 0

iphone_low250[(iphone_low250$iphonesentiment == 2) | 
                (iphone_low250$iphonesentiment == 3), 
              "iphonesentiment"] <- 1

iphone_low250[(iphone_low250$iphonesentiment == 4) | 
                (iphone_low250$iphonesentiment == 5), 
              "iphonesentiment"] <- 2

iphone_low250 <- droplevels(iphone_low250)

ggplot(iphone_low250, aes(iphonesentiment)) +
  geom_histogram(stat = "count")

