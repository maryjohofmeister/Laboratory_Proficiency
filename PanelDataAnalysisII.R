#This R file cleans and summarizes the results from the Proficiency Panel completed by PAHO/CDC in 2014  for Rabies Diagnosis
#The focus of this code is to print out to file the crosstabs and level of agreement between the lab and the CDC 
#- BY SAMPLE
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

library(caret)
library (rtf)
library(ROCR)

getwd()

#Import Datasets

#for work
setwd("C:/Users/carvalhom/Documents/PAHO_drive/I EJERCICIO INTER-LABORATORIAL/Mary Synthesis/Results/Code and Datasets Used")#set data library

#for home
setwd("~/Documents/PAHO_Drive/I EJERCICIO INTER-LABORATORIAL/Mary Synthesis/")

# read text file
paneldata = read.table("Panel_BinaryData_Final.txt", header = TRUE)  
cdcdata = read.table("CDC_Values.txt")  # read text file
head (paneldata) #print table
head (cdcdata)    #print table


#Take out CDC Values for analysis by sample

tpaneldata = setNames(data.frame(t(paneldata[,-1])), paneldata[,1])
panel_ncdc <-data.frame(tpaneldata)
panel_ncdc$LCDC = NULL
panel_ncdc <-data.frame(t(panel_ncdc))


#create an EMPTY matrix to hold the results by sample. This table is used further down in the analysis to capture and print results.
cname<-cdcdata$V1[2:21]
rname<-c("CDCValue_Text", "CDCValue_Num", "Positive", "Negative", "Indetermined", "Missing","PercAgree")
agree<-matrix(nrow=20, ncol =7, dimnames = list(cname,rname) )

agree[1:20,1]<-as.character(cdcdata[2:21,2])
agree[1:20,2]<-cdcdata[2:21,3]

#Print out CrossTabs by Sample

#Create an .rtf file of Sample Results
output<-"rtf_surveyresults.doc"
rtf<-RTF(output,width=8.5,height=11,font.size=10,omi=c(1,1,1,1))
addHeader(rtf,title="Analysis by Sample Number", subtitle="Samples 1 to 20 (18-20 are controls)")
addParagraph(rtf,"Tables of Positive (1), Negative (2) and Undetermined (3) and Missing (-) by Sample for all Labs\n")
addNewLine(rtf)

addHeader(rtf,title="Sample 1", subtitle="Texas Gray Fox")
tab<-as.data.frame(table(panel_ncdc$S01, exclude = NULL,  dnn = c("Result S01")))
tab <- transform(tab, cumFreq = cumsum(Freq), PercentTotal = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

ttab<-t(tab)
agree[1,3:6]<-ttab[2,1:4]


addHeader(rtf,title="Sample 2", subtitle="Mongoose/Dog Variant")
tab<-as.data.frame(table(panel_ncdc$S02, exclude = NULL,  dnn = c("Result S02")))
tab <- transform(tab, cumFreq = cumsum(Freq), PercentTotal = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

ttab<-t(tab)
agree[2,3:6]<-ttab[2,1:4]


addHeader(rtf,title="Sample 3", subtitle="Negative Cat Sample")
tab<-as.data.frame(table(panel_ncdc$S03, exclude = NULL,  dnn = c("Result S03")))
tab <- transform(tab, cumFreq = cumsum(Freq), PercentTotal = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

ttab<-t(tab)
agree[3,3:6]<-ttab[2,1:4]


addHeader(rtf,title="Sample 4", subtitle="AZ Gray Fox (AV7)")
tab<-as.data.frame(table(panel_ncdc$S04, exclude = NULL,  dnn = c("Result S04")))
tab <- transform(tab, cumFreq = cumsum(Freq), PercentTotal = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

ttab<-t(tab)
agree[4,3:6]<-ttab[2,1:4]

addHeader(rtf,title="Sample 5", subtitle="Negative Dog Sample")
tab<-as.data.frame(table(panel_ncdc$S05, exclude = NULL,  dnn = c("Result S05")))
tab <- transform(tab, cumFreq = cumsum(Freq), PercentTotal = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

ttab<-t(tab)
agree[5,3:6]<-ttab[2,1:4]

addHeader(rtf,title="Sample 6", subtitle="Lasionycteris noctivagans")
tab<-as.data.frame(table(panel_ncdc$S06, exclude = NULL,  dnn = c("Result S06")))
tab <- transform(tab, cumFreq = cumsum(Freq), PercentTotal = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

ttab<-t(tab)
agree[6,3:4]<-ttab[2,1:2]
agree[6,6]<-ttab[2,3]

addHeader(rtf,title="Sample 7", subtitle="Tadarida brasiliensis (AV4)")
tab<-as.data.frame(table(panel_ncdc$S07, exclude = NULL,  dnn = c("Result S07")))
tab <- transform(tab, cumFreq = cumsum(Freq), PercentTotal = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

ttab<-t(tab)
agree[7,3:4]<-ttab[2,1:2]
agree[7,6]<-ttab[2,3]

addHeader(rtf,title="Sample 8", subtitle="Negative Sample infected with bacteria (Listeriosis)")
tab<-as.data.frame(table(panel_ncdc$S08, exclude = NULL,  dnn = c("Result S08")))
tab <- transform(tab, cumFreq = cumsum(Freq), PercentTotal = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

ttab<-t(tab)
agree[8,3:6]<-ttab[2,1:4]


addHeader(rtf,title="Sample 9", subtitle="Hognose skunk")
tab<-as.data.frame(table(panel_ncdc$S09, exclude = NULL,  dnn = c("Result S09")))
tab <- transform(tab, cumFreq = cumsum(Freq), PercentTotal = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

ttab<-t(tab)
agree[9,3:4]<-ttab[2,1:2]
agree[9,6]<-ttab[2,3]

addHeader(rtf,title="Sample 10", subtitle="South Central Skunk US North Central MX")
tab<-as.data.frame(table(panel_ncdc$S10, exclude = NULL,  dnn = c("Result S10")))
tab <- transform(tab, cumFreq = cumsum(Freq), PercentTotal = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

ttab<-t(tab)
agree[10,3:6]<-ttab[2,1:4]


addHeader(rtf,title="Sample 11", subtitle="Negative Myotis vivesi")
tab<-as.data.frame(table(panel_ncdc$S11, exclude = NULL,  dnn = c("Result S11")))
tab <- transform(tab, cumFreq = cumsum(Freq), PercentTotal = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

ttab<-t(tab)
agree[11,3:6]<-ttab[2,1:4]

addHeader(rtf,title="Sample 12", subtitle="Western Eptesicus fuscus")
tab<-as.data.frame(table(panel_ncdc$S12, exclude = NULL,  dnn = c("Result S12")))
tab <- transform(tab, cumFreq = cumsum(Freq), PercentTotal = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

ttab<-t(tab)
agree[12,3:4]<-ttab[2,1:2]
agree[12,6]<-ttab[2,3]

addHeader(rtf,title="Sample 13", subtitle="Negative Fox")
tab<-as.data.frame(table(panel_ncdc$S13, exclude = NULL,  dnn = c("Result S13")))
tab <- transform(tab, cumFreq = cumsum(Freq), PercentTotal = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

ttab<-t(tab)
agree[13,3:6]<-ttab[2,1:4]

addHeader(rtf,title="Sample 14", subtitle="Negative Bos taurus")
tab<-as.data.frame(table(panel_ncdc$S14, exclude = NULL,  dnn = c("Result S14")))
tab <- transform(tab, cumFreq = cumsum(Freq), PercentTotal = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

ttab<-t(tab)
agree[14,3:4]<-ttab[2,1:2]
agree[14,6]<-ttab[2,3]

addHeader(rtf,title="Sample 15", subtitle="Negative Bos taurus")
tab<-as.data.frame(table(panel_ncdc$S15, exclude = NULL,  dnn = c("Result S15")))
tab <- transform(tab, cumFreq = cumsum(Freq), PercentTotal = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

ttab<-t(tab)
agree[15,3:4]<-ttab[2,1:2]
agree[15,6]<-ttab[2,3]

addHeader(rtf,title="Sample 16", subtitle="Mongoose (PuertoRico)")
tab<-as.data.frame(table(panel_ncdc$S16, exclude = NULL,  dnn = c("Result S16")))
tab <- transform(tab, cumFreq = cumsum(Freq), PercentTotal = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

ttab<-t(tab)
agree[16,3:6]<-ttab[2,1:4]

addHeader(rtf,title="Sample 17", subtitle="Negative Herpestes javanicus")
tab<-as.data.frame(table(panel_ncdc$S17, exclude = NULL,  dnn = c("Result S17")))
tab <- transform(tab, cumFreq = cumsum(Freq), PercentTotal = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

ttab<-t(tab)
agree[17,3:4]<-ttab[2,1:2]
agree[17,6]<-ttab[2,3]

addHeader(rtf,title="Sample 18", subtitle="Strong Positive Control Latin American Dog variant")
tab<-as.data.frame(table(panel_ncdc$S18, exclude = NULL,  dnn = c("Result S18")))
tab <- transform(tab, cumFreq = cumsum(Freq), PercentTotal = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

ttab<-t(tab)
agree[18,3:4]<-ttab[2,1:2]
agree[18,6]<-ttab[2,3]

addHeader(rtf,title="Sample 19", subtitle="Weak Positive Control (3) Vampire, Bos taurus")
tab<-as.data.frame(table(panel_ncdc$S19, exclude = NULL,  dnn = c("Result S19")))
tab <- transform(tab, cumFreq = cumsum(Freq), PercentTotal = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

ttab<-t(tab)
agree[19,3:6]<-ttab[2,1:4]

addHeader(rtf,title="Sample 20", subtitle="Negative Control")
tab<-as.data.frame(table(panel_ncdc$S20, exclude = NULL,  dnn = c("Result S20")))
tab <- transform(tab, cumFreq = cumsum(Freq), PercentTotal = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

ttab<-t(tab)
agree[20,4]<-ttab[2,1]
agree[20,6]<-ttab[2,2]

#NOTE! Exlcudes Missing Values from the Percent Agreement. Sets NA to O and creates a denominator for percentange calculation
agree[is.na(agree)] <- 0
denom<-(as.numeric(agree[1:20,3])+as.numeric(agree[1:20,4])+as.numeric(agree[1:20,5]))
agree[1:20,7]<-ifelse(agree[1:20,2]==1,round(as.numeric(agree[1:20,3])/denom[1:20]*100,2), round(as.numeric(agree[1:20,4])/denom[1:20]*100,2))

agree2<-data.frame(agree)
agree2$CDCValue_Num = NULL

addNewLine(rtf)
addNewLine(rtf)
addHeader(rtf,title="Summary Table", subtitle="Sample 1-20")
addTable(rtf,agree2,font.size=8,row.names=TRUE,NA.string="0")

done(rtf) # writes and closes the file


