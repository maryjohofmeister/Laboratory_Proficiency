#This R file cleans and summarizes the results from the Proficiency Panel completed by PAHO/CDC in 2014 for Rabies Diagnosis
#The focus of this code is to print out to file the crosstabs and measures of agreement between the lab and the CDC Results
#- BY LAB
#The .csv is sourced from the original excel files from the project
#The original excel spreadsheet and a record of any modifications made prior to import 
#are outlined in the Word File "REDILAR_analysis plan" 
#This code must be run prior to running the bivariate_analysis.R code associated with the project
#Datacoding (1=Positive, 2=Negative, 3-Missing/Undetermined)

#Set options/libraries
#for new machines
install.packages ("rtf")
install.packages("caret")
install.packages("e1071")
install.packages("ROCR")
install.packages("gplots")
install.packages("bitops")

#Run 1st time though code
library(caret)
library (rtf)


getwd()

#Import Datasets

#for work
setwd("C:/Users/carvalhom/Documents/PAHO_drive/I EJERCICIO INTER-LABORATORIAL/Mary Synthesis/Results/Code and Datasets Used")#set data library


#for home
setwd("~/Documents/PAHO_Drive/I EJERCICIO INTER-LABORATORIAL/Mary Synthesis/")

# read text file
paneldata = read.csv("Panel_BinaryData_05292015.csv", header = TRUE)  #This file is 
cdcdata = read.table("CDC_Values.txt")  # read text file
head (paneldata) #print table
head (cdcdata)    #print table


#Create new table without the CDC Values for analysis by sample
panel_ncdc <-data.frame(t(paneldata))
panel_ncdc$LCDC = NULL
panel_ncdc <-data.frame(t(panel_ncdc))


#Transpose paneldata table
paneldata2<-paneldata[,1:21]
tpaneldata <- data.frame(t(paneldata2))
tpaneldata = setNames(data.frame(t(paneldata2[,-1])), paneldata2[,1])

head(tpaneldata)


#Print out crosstabs with undetermined/missing results (value=3), labs without results are hashed out 
#table(tpaneldata$L1, tpaneldata$LCDC)
#table(tpaneldata$L2, tpaneldata$LCDC)
table(tpaneldata$L3, tpaneldata$LCDC)
table(tpaneldata$L4, tpaneldata$LCDC)
table(tpaneldata$L5, tpaneldata$LCDC)
table(tpaneldata$L6, tpaneldata$LCDC)
table(tpaneldata$L7, tpaneldata$LCDC)
table(tpaneldata$L8, tpaneldata$LCDC)
table(tpaneldata$L9, tpaneldata$LCDC)
table(tpaneldata$L10, tpaneldata$LCDC)
table(tpaneldata$L11, tpaneldata$LCDC)
table(tpaneldata$L12, tpaneldata$LCDC)
#table(tpaneldata$L13, tpaneldata$LCDC)
table(tpaneldata$L14, tpaneldata$LCDC)
#table(tpaneldata$L15, tpaneldata$LCDC)
table(tpaneldata$L16, tpaneldata$LCDC)
#table(tpaneldata$L17, tpaneldata$LCDC)
#table(tpaneldata$L18, tpaneldata$LCDC)
table(tpaneldata$L19, tpaneldata$LCDC)
table(tpaneldata$L20, tpaneldata$LCDC)
table(tpaneldata$L21, tpaneldata$LCDC)
table(tpaneldata$L22, tpaneldata$LCDC)
table(tpaneldata$L23, tpaneldata$LCDC)
table(tpaneldata$L24, tpaneldata$LCDC)
#table(tpaneldata$L25, tpaneldata$LCDC)
#table(tpaneldata$L26, tpaneldata$LCDC)
table(tpaneldata$L27, tpaneldata$LCDC)
table(tpaneldata$L28, tpaneldata$LCDC)
table(tpaneldata$L29, tpaneldata$LCDC)
table(tpaneldata$L30, tpaneldata$LCDC)

#Calculate and output to file the Sens, Sp, PP, NP - Missing/Undetermined are set to NA for purposes of Analysis

#Set Undetermined to NA for purpose of analysis
x1<-tpaneldata
x1[x1 == 3] <- NA

#Create Matrix, 2x2 and Sp/Sens results for each Lab

#confusionMatrix(x1$L1,x1$LCDC, positive= NULL,dnn = c("Lab 1", "CDC")) 
#confusionMatrix(x1$L2,x1$LCDC, positive= NULL,dnn = c("Lab 2", "CDC")) 

capture.output(print("Laboratory 1: No results"), file = "Sens_Spec.doc", append = TRUE)
capture.output(print("Laboratory 2: No results"), file = "Sens_Spec.doc", append = TRUE)
capture.output(print("Laboratory 3"), file = "Sens_Spec.doc", append = TRUE)
capture.output(confusionMatrix(x1$L3,x1$LCDC, positive= NULL,dnn = c("Lab 3", "CDC")), file = "Sens_Spec.doc", append = TRUE)
capture.output(print("Laboratory 4"), file = "Sens_Spec.doc", append = TRUE)
capture.output(confusionMatrix(x1$L4,x1$LCDC, positive= NULL,dnn = c("Lab 4", "CDC")), file = "Sens_Spec.doc", append = TRUE)
capture.output(print("Laboratory 5"), file = "Sens_Spec.doc", append = TRUE)
capture.output(confusionMatrix(x1$L5,x1$LCDC, positive= NULL,dnn = c("Lab 5", "CDC")) , file = "Sens_Spec.doc", append = TRUE)
capture.output(print("Laboratory 6"), file = "Sens_Spec.doc", append = TRUE)
capture.output(confusionMatrix(x1$L6,x1$LCDC, positive= NULL,dnn = c("Lab 6", "CDC")) , file = "Sens_Spec.doc", append = TRUE)
capture.output(print("Laboratory 7"), file = "Sens_Spec.doc", append = TRUE)
capture.output(confusionMatrix(x1$L7,x1$LCDC, positive= NULL,dnn = c("Lab 7", "CDC")) , file = "Sens_Spec.doc", append = TRUE)
capture.output(print("Laboratory 8"), file = "Sens_Spec.doc", append = TRUE)
capture.output(confusionMatrix(x1$L8,x1$LCDC, positive= NULL,dnn = c("Lab 8", "CDC")) , file = "Sens_Spec.doc", append = TRUE)
capture.output(print("Laboratory 9"), file = "Sens_Spec.doc", append = TRUE)
capture.output(confusionMatrix(x1$L9,x1$LCDC, positive= NULL,dnn = c("Lab 9", "CDC")) , file = "Sens_Spec.doc", append = TRUE)
capture.output(print("Laboratory 10"), file = "Sens_Spec.doc", append = TRUE)
capture.output(confusionMatrix(x1$L10,x1$LCDC, positive= NULL,dnn = c("Lab 10", "CDC")) , file = "Sens_Spec.doc", append = TRUE)
capture.output(print("Laboratory 11"), file = "Sens_Spec.doc", append = TRUE)
capture.output(confusionMatrix(x1$L11,x1$LCDC, positive= NULL,dnn = c("Lab 11", "CDC")) , file = "Sens_Spec.doc", append = TRUE)
capture.output(print("Laboratory 12"), file = "Sens_Spec.doc", append = TRUE)
capture.output(confusionMatrix(x1$L12,x1$LCDC, positive= NULL,dnn = c("Lab 12", "CDC")) , file = "Sens_Spec.doc", append = TRUE)
capture.output(print("Laboratory 13: No Results"), file = "Sens_Spec.doc", append = TRUE)
#confusionMatrix(x1$L13,x1$LCDC, positive= NULL,dnn = c("Lab 13", "CDC")) , file = "Sens_Spec.doc", append = TRUE)
capture.output(print("Laboratory 14"), file = "Sens_Spec.doc", append = TRUE)
capture.output(confusionMatrix(x1$L14,x1$LCDC, positive= NULL,dnn = c("Lab 14", "CDC")) , file = "Sens_Spec.doc", append = TRUE)
capture.output(print("Laboratory 15: No Results"), file = "Sens_Spec.doc", append = TRUE)
#confusionMatrix(x1$L15,x1$LCDC, positive= NULL,dnn = c("Lab 15", "CDC")) 
capture.output(print("Laboratory 16"), file = "Sens_Spec.doc", append = TRUE)
capture.output(confusionMatrix(x1$L16,x1$LCDC, positive= NULL,dnn = c("Lab 16", "CDC")) , file = "Sens_Spec.doc", append = TRUE)
capture.output(print("Laboratory 17: No Results"), file = "Sens_Spec.doc", append = TRUE)
#confusionMatrix(x1$L17,x1$LCDC, positive= NULL,dnn = c("Lab 17", "CDC")) 
capture.output(print("Laboratory 18: No Results"), file = "Sens_Spec.doc", append = TRUE)
#confusionMatrix(x1$L18,x1$LCDC, positive= NULL,dnn = c("Lab 18", "CDC")) 
capture.output(print("Laboratory 19"), file = "Sens_Spec.doc", append = TRUE)
capture.output(confusionMatrix(x1$L19,x1$LCDC, positive= NULL,dnn = c("Lab 19", "CDC")) , file = "Sens_Spec.doc", append = TRUE)
capture.output(print("Laboratory 20"), file = "Sens_Spec.doc", append = TRUE)
capture.output(confusionMatrix(x1$L20,x1$LCDC, positive= NULL,dnn = c("Lab 20", "CDC")) , file = "Sens_Spec.doc", append = TRUE)
capture.output(print("Laboratory 21"), file = "Sens_Spec.doc", append = TRUE)
capture.output(confusionMatrix(x1$L21,x1$LCDC, positive= NULL,dnn = c("Lab 21", "CDC")) , file = "Sens_Spec.doc", append = TRUE)
capture.output(print("Laboratory 22"), file = "Sens_Spec.doc", append = TRUE)
capture.output(confusionMatrix(x1$L22,x1$LCDC, positive= NULL,dnn = c("Lab 22", "CDC")) , file = "Sens_Spec.doc", append = TRUE)
capture.output(print("Laboratory 23"), file = "Sens_Spec.doc", append = TRUE)
capture.output(confusionMatrix(x1$L23,x1$LCDC, positive= NULL,dnn = c("Lab 23", "CDC")) , file = "Sens_Spec.doc", append = TRUE)
capture.output(print("Laboratory 24"), file = "Sens_Spec.doc", append = TRUE)
capture.output(confusionMatrix(x1$L24,x1$LCDC, positive= NULL,dnn = c("Lab 24", "CDC")) , file = "Sens_Spec.doc", append = TRUE)
capture.output(print("Laboratory 25: No Results"), file = "Sens_Spec.doc", append = TRUE)
#confusionMatrix(x1$L25,x1$LCDC, positive= NULL,dnn = c("Lab 25", "CDC")) 
capture.output(print("Laboratory 26"), file = "Sens_Spec.doc", append = TRUE)
capture.output(confusionMatrix(x1$L26,x1$LCDC, positive= NULL,dnn = c("Lab 26", "CDC")) , file = "Sens_Spec.doc", append = TRUE)
capture.output(print("Laboratory 27"), file = "Sens_Spec.doc", append = TRUE)
capture.output(confusionMatrix(x1$L27,x1$LCDC, positive= NULL,dnn = c("Lab 27", "CDC")) , file = "Sens_Spec.doc", append = TRUE)
capture.output(print("Laboratory 28"), file = "Sens_Spec.doc", append = TRUE)
capture.output(confusionMatrix(x1$L28,x1$LCDC, positive= NULL,dnn = c("Lab 28", "CDC")) , file = "Sens_Spec.doc", append = TRUE)
capture.output(print("Laboratory 29"), file = "Sens_Spec.doc", append = TRUE)
capture.output(confusionMatrix(x1$L29,x1$LCDC, positive= NULL,dnn = c("Lab 29", "CDC")) , file = "Sens_Spec.doc", append = TRUE)
capture.output(print("Laboratory 30"), file = "Sens_Spec.doc", append = TRUE)
capture.output(confusionMatrix(x1$L30,x1$LCDC, positive= NULL,dnn = c("Lab 30", "CDC")) , file = "Sens_Spec.doc", append = TRUE)

#Print Tables of Sensitivity/Specificity and Agreement for further analysis
#Set Undetermined to NA for purpose of analysis
x2<-tpaneldata
x2[x2 == 3] <- NA 
#Flip Values, 0 = Negative, 1= positive
x2[x2 == 2] <- 0 

#Create a variable in panel data with the values of missing and undetermined  by lab
paneldata[1:24,23] <-colSums(tpaneldata == 3, na.rm = T)
paneldata[1:24,24] <-colSums(is.na(tpaneldata))



goldstand <- x2$LCDC
prev <- sum(goldstand)
cprev <- sum(!goldstand)
n <- prev + cprev

#Calculates a,b,c,d, N, sens, spec and agreement
#a
outputa<-lapply(x2[1:23], function(x){
  tab <- table(x, goldstand)
  if(nrow(tab) > 1 && ncol(tab) > 1) {
    out <-tab[2,2]
  }
})

#b
outputb<-lapply(x2[1:23], function(x){
  tab <- table(x, goldstand)
  if(nrow(tab) > 1 && ncol(tab) > 1) {
    out <-tab[2,1]
  }
})

#c
outputc<-lapply(x2[1:23], function(x){
  tab <- table(x, goldstand)
  if(nrow(tab) > 1 && ncol(tab) > 1) {
    out <-tab[1,2]
  }
})

#d
outputd<-lapply(x2[1:23], function(x){
  tab <- table(x, goldstand)
  if(nrow(tab) > 1 && ncol(tab) > 1) {
    out <-tab[1,1]
  }
})
#N
outputN<-lapply(x2[1:23], function(x){
  tab <- table(x, goldstand)
  if(nrow(tab) > 1 && ncol(tab) > 1) {
    out <-(tab[1,1]+tab[1,2]+tab[2,1]+tab[2,2])
  }
})
#specificity
output<-lapply(x2[1:23], function(x){
  tab <- table(x, goldstand)
  cS <- colSums(tab)
  if(nrow(tab) > 1 && ncol(tab) > 1) {
    out <-format(round(as.numeric( c(sp = tab[1,1]/ cS[1])),2), nsmall=2)
  }
})

#Sensitivity
output2<-lapply(x2[1:23], function(x){
  tab <- table(x, goldstand)
  cS <- colSums(tab)
  if(nrow(tab) > 1 && ncol(tab) > 1) {
    out <- format(round(as.numeric(c(sn = tab[2,2]/ cS[2])),2), nsmall =2)
  }
})
#Agreement/Accuracy
output3<-lapply(x2[1:23], function(x){
  tab <- table(x, goldstand)
  cS <- colSums(tab)
  n<-c(cS[1]+cS[2])
  if(nrow(tab) > 1 && ncol(tab) > 1) {
    out <- format(round(c((tab[1,1]+tab[2,2])/n),2), nsmall=2)
  }
})
#Kappa
output4<-lapply(x2[1:23], function(x){
  tab <- table(x, goldstand)
  cS <- colSums(tab)
  n<-c(cS[1]+cS[2])
  OA<-c((tab[1,1]+tab[2,2])/n)
  EA<-c(((tab[1,1]+tab[1,2])/n *((tab[1,1]+tab[2,1])/n)) +((tab[2,1]+tab[2,2])/n * ((tab[2,2]+tab[1,2])/n)))
  if(nrow(tab) > 1 && ncol(tab) > 1) {
    out <- format(round(c((OA-EA)/(1-EA)),2), nsmall=2)
  }
})

#output N, a,b,c,d, sens, spec and agreement in table format
Labname<-as.matrix(paneldata[1:23,22])
Missing <-as.matrix(paneldata[1:23,24])
Indetermined<-as.matrix(paneldata[1:23,23])
N<-as.matrix(outputN)
a<-as.matrix(outputa)
b<-as.matrix(outputb)
c<-as.matrix(outputc)
d<-as.matrix(outputd)
Specificity<-as.matrix(output)
Sensitivity<-as.matrix(output2)
Accuracy<-as.matrix(output3)
Kappa<-as.matrix(output4)


newtable<-data.frame(Labname,Missing,Indetermined, a,b,c,d, Sensitivity, Specificity, Accuracy, Kappa, row.names = NULL)

#sort newtable by new Lab ID
newtable <- newtable[order(Labname), ] 
row.names(newtable)<-NULL


#creat a .png file of the senstivity and specificity plot to import into RTF document.  
attach(newtable)
png(file = "Sens_Spec_plot.png", bg = "transparent")
plot(1-as.numeric(Specificity), Sensitivity, main="Sensitivity v. 1-Specificity", 
     xlab="1-Specificity", ylab="Sensitivity",  col= "blue", pch = 18, cex = 2, family="serif", lty = "solid", lwd = 2)
pos_vector <- rep(4, length(Labname))
pos_vector[Labname %in% c("L12","L14")] <- 3
name_vector<-Labname
name_vector[Labname %in% c("L30")]<-"L7, L11, L30"
name_vector[Labname %in% c("L7")]<-" "
name_vector[Labname %in% c("L11")]<-" "
name_vector[Labname %in% c("L29")]<-"L22, L29"
name_vector[Labname %in% c("L22")]<-" "
text(1-as.numeric(Specificity), Sensitivity, labels=name_vector, cex= .8,  pos=pos_vector)
dev.off()
detach(newtable)


output<-"Sens_SpecII.doc"

rtf<-RTF(output,width=8.5,height=11,font.size=10,omi=c(1,1,1,1,1,1))
addHeader(rtf,title="Panel Results by Laboratory: Agreement")
addParagraph(rtf,"\n")
addNewLine(rtf)
addTable(rtf,newtable,font.size=9,row.names=FALSE,NA.string="-",col.justify="C", header.col.justify="C", col.widths=c(.75,.4,.4,.4,.4,.4,.4,.75,.75,.75,.75) )
addNewLine(rtf)
addPng(rtf,"Sens_Spec_plot.png", width =7, height = 7)

done(rtf) # writes and closes the file







#Extra Code

#Create a Frequency Table of Positive, Negative and Undetermined Results by Sample
freq(ordered(paneldata2$values), plot = FALSE)   #Quick frequency of positive and negative responses, how to do by?
freq_sample <- with (paneldata2, table(ind, values)) # frequency table to panel results by sample
head (freq_sample)
prop.table (freq_sample,1)       #row proportions
prop.table (freq_sample,2)        #column proportions
margin.table(freq_sample,1)      #row sums
margin.table(freq_sample,2)       #column sums
