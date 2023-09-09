# Install and load the required packages
# install.packages("missMDA")
library(missMDA)
library(FactoMineR)
#initiation
repertoire <- "/home/thalabard/Documents/AIV/CRI_PhDProjects/Sweekrity/Romaric/"
file_name = paste(repertoire,"mca.csv",sep="",collapse=NULL)
data <- read.csv(file_name)
head(data)
df <- data[1:61, 4:18]
dataset <- data.frame(lapply(df, as.factor))
summary(dataset)
str(dataset)
#
# ==================================================================
# FUnction for transforming factor a variable expressed in characters into factor variable expressed 
call_function <- function(variable)
{
  variable = as.integer(dataset[,variable])
  variable = as.factor(variable)
  return(variable)
}
result = call_function(names(dataset)[1]) 
for(nom in names(dataset)[-1])
{
  z = call_function(nom)
  result = data.frame(result,z)
}
#
# result = data.frame(result)
names(result) = names(dataset)
head(result)
# ======================================================================
#
mca.res <- MCA(result)
# confirmation of mca datatype
str(mca.res$eig)

#visualisation
barplot(mca.res$eig[,2], names= paste("Dim",1:nrow(mca.res$eig)))
plot(mca.res, invisible=c("var"), habillage = 1) #, select = "contrib 10")
plot(mca.res, choix = "var")
plot(mca.res, invisible = "ind", axes=3:4)
# description of data
round(mca.res$eig[1:14,],2)
mca.res$var
mca.res$ind
#full discription of dataset
dimdesc(mca.res, axes = c(3,4))

#### NA variables imputed
# Some variables have to be discarded because their take only one value and NA's.
# They don't bring any information.
# Reducing the variables by discarding variables with an excess of NA's
result.ext = result[, -c(6:10,15)]
dim(result.ext)
summary(result.ext)
#
# Na's values are imputed using 

result.imp <- imputeMCA(result.ext, ncp = 4)

res.mca <- MCA(result.imp[[2]])
#
barplot(res.mca$eig[,2], names= paste("Dim",1:nrow(res.mca$eig)))
plot(res.mca, invisible=c("var"), habillage = 1, select = "contrib 10")
plot(res.mca, choix = "var")
plot(res.mca, invisible = "ind", axes=3:4)
# description of data
round(res.mca$eig ,2)
res.mca$var
res.mca$ind
#full discription of dataset
dimdesc(res.mca, axes = c(3,4))
#
# We increase the dataset by artificially increased the rows. 
# We generate N= 1000 "individuals" by randomly choosing with replacement lines in the previous data.frame
# Therefore we generate a data.frame which is very close to the vnf data.frame used as example by Husson & Josse
N = 1000
rows = sample(1:dim(result.ext)[1],N,replace = TRUE)
for(i in rows)
{
result.ext = rbind(result.ext,result.ext[i,])  
}
#
summary(result.ext)
#
# Beginning of the troubles!
# ======================================================
# 
# ncomp <- estim_ncpMCA(result.ext, ncp = 0)
# Perform MCA with the estimated number of components
# res.mca <- MCA(dataset)
result.imp <- imputeMCA(result.ext, ncp = 4)
res.mca <- MCA(result.imp[[2]])
#
barplot(res.mca$eig[,2], names= paste("Dim",1:nrow(res.mca$eig)))
plot(res.mca, invisible=c("var"), habillage = 1) #, select = "contrib 10")
plot(res.mca, choix = "var")
plot(res.mca, invisible = "ind", axes=3:4)
# description of data
round(res.mca$eig ,2)
res.mca$var
res.mca$ind
#full discription of dataset
dimdesc(res.mca, axes = c(3,4))

