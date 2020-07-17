#Installing the package
install.packages('dplyr')
#Loading the library
library(dplyr)
#Loading the data
data(heights)
#mutating to create new table in cm
heights2 <- mutate(heights,ht_cm = height*2.54)
str(heights2)
#Obtaining th cm column
heights2$ht_cm[18]
#Get the average
avg_cm=mean(heights2$ht_cm)
#Filtering to females only
females<- heights2$sex == "Female"
#Obtaining the number of females
sum(females)
#Average of females height
avg_f<- mean(females)
fem <- filter(heights2, heights2$sex == "Female")
avg_f <- mean(fem$ht_cm)


