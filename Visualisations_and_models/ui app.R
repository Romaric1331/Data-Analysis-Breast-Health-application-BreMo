# 1 is BreMo
# 2,3,4,5,6 are the other apps
mydata <- read_xls("C:/Users/romar/OneDrive/Desktop/BHA_Study - Overall_concordance_matrix.xlsx")
mydata
#total_score = c(3,2,4,3,4,3,2,4,2,4,2,4,4,4,3,3,4,2,1,4,2,4,4,3,1,3,2,3,3,1)
#mydata$total_score <- total_score
#mydata



mydata$User_ID <- as.factor(mydata$User_ID)
mydata$App_ID <- as.factor(mydata$App_ID)
library(ggplot2)
install.packages("ggpubr")a
library(ggpubr)
ggboxplot(newdata, x = 'App_ID', y = 'total_score')

#install.packages("lmerTest")   # For obtaining p-values

#library(lmerTest)
analysis <- lmer( mydata$f2 ~ mydata$App_ID + (1 | mydata$User_ID), newdata)
summary(analysis)
fixef(analysis)
#lmer(analysis)
