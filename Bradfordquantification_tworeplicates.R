#Load the libraries
library("readxl")
library("ggplot2")
library("dplyr")
library("ggpubr")
library("matrixStats")
#Import the data interactively from xls or xlsx files
myexcel <- file.choose()
dataset1 <- read_excel(myexcel, sheet=1)
dataset2 <- read_excel(myexcel, sheet=2) 
#Report the number of replicates
num_replicates = 2
#Obtain the average absorbance
dataset1[,4] = (dataset1[,2]+dataset1[,3])/2
names(dataset1)[1] <-paste("Albumin")
names(dataset1)[4] <- paste("Average")
print(dataset1)
#Input the volume of the sample
#vol1 = readline("enter the volume of the sample in mL: ")
#vol1 = as.numeric(vol1)
vol1=0.1
#Input the volume of the Bradford solution
#vol2 = readline("enter the volume of the Bradford solution in mL: ")
#vol2 = as.numeric(vol2)
vol2=5
vol3=vol1+vol2
#Change the concentration to consider the dilution factor
dataset1[,1] = (dataset1[,1]*vol1)/vol3
dataset1 <- slice(dataset1,-c(1))
#Graph
ggplot(dataset1,aes(x = Albumin, y = Average)) + 
  geom_point() + labs(x = "Protein (mg/mL)", y = "Absorbance 595 nm") +
  geom_smooth(formula = y ~ x, method = "lm", se=FALSE) +
  stat_regline_equation(label.y = 0.45, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 0.4, aes(label = ..rr.label..))
#Get the intercept and slope values
x<-dataset1$Albumin
y<-dataset1$Average
model<-lm(y ~ x)
model_summary <- summary(model)
intercept_value <- model_summary$coefficients[1,1]
slope_value <- model_summary$coefficients[2,1]
#calculate protein content and SD
dataset2[,2] = ((dataset2[,2]-intercept_value)/slope_value)*vol3/vol1
dataset2[,3] = ((dataset2[,3]-intercept_value)/slope_value)*vol3/vol1
dataset2[,4] = (dataset2[,2]+dataset2[,3])/2
names(dataset2)[4] <- paste("Protein (mg/mL)")
dataset2[,5] = rowSds(as.matrix(dataset2[,c(2,3)]))
names(dataset2)[5] <- paste("SD (mg/L)")
dataset2 <- subset (dataset2, select = -c(2,3))
#convert mg_of_protein/mL to mg_of_protein/g_of_sample
convf = 0.5 #g_of_sample/mL
dataset2[,4] = dataset2[,2]*convf
names(dataset2)[4] <- paste("Protein (mg/g)")
dataset2[,5] = dataset2[,3]*convf
names(dataset2)[5] <- paste("SD (mg/g)")
dataset2