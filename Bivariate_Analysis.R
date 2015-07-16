#This R file cleans and summarizes the results from the Laboratory Survey
#associated with the proficiency panel exercise completed by PAHO/CDC in 2014 for Rabies Diagnosis
#The focus of this code is to investigate the bivariate relationship between the survey variables 
# and the agreement level  of the proficiency panel
#The .csv is sourced from the original excel files from the project, downloaded from AdobeFormCenter
#The original excel spreadsheet and a record of any modifications made prior to import 
#are outlined in the Word File "REDILAR_analysis plan" 

#This code used the tables from the SURVEY_ANALYSIS.R file, countrycodes and survey. Run first!!!

#Datacoding ( For yes/no questions - 1=YES, 0=No, 3-Missing/Undetermined)



#Uses newtable and surevy datasets from survey_analysis and paneldataanalysis.
install.packages ("gmodels")
install.packages ("gplots")


library(gmodels)
library(gplots)
library(stats)


#Variables for Analysis from Survey

qlist <-c("Labname", "q02new", "q03", "q04", "q10", "q11", "q12", "q15", "q16", "q21", "q22", "q25", "q35", "q36","q37","q38","q39","q40","q41","q42","q43","q45", "q48","q50")


#Subset Survey

survey <-surveydata[qlist]


survey <- merge(survey, countrycodes, by = "Labname")

#Recode Variables into categories for analysis

#question 2 (three categories, 1-10, 11-30, >30)
survey$q02[survey$q02new=="00-05"]<-"0-10"
survey$q02[survey$q02new=="06-10"]<-"0-10"
survey$q02[survey$q02new=="11-20"]<-"11-30"
survey$q02[survey$q02new=="21-30"]<-"11-30"
survey$q02[survey$q02new=="31-50"]<-"30+"
survey$q02[survey$q02new=="50-100"]<-"30+"
survey$q02[survey$q02new=="Single Report of 120"]<-"30+"

#question 3 (three categories, 1-2, 3-4, >4)
survey$q03new[survey$q03=="1"|survey$q03=="2"]<-"1-2"
survey$q03new[survey$q03=="3"|survey$q03=="4"]<-"3-4"
survey$q03new[survey$q03=="5"|survey$q03=="7"]<-"4+"

#question 15
survey$q15new<- substr(survey$q15, 1,5)
survey$q15new[survey$q15new == "0-5 a"] <- "0-5 years"
survey$q15new[survey$q15new == "0-5 y"] <- "0-5 years"
survey$q15new[survey$q15new == "6-10 "] <- "6-15 years"
survey$q15new[survey$q15new == "11-15"] <- "6-15 years"
survey$q15new[survey$q15new == "15-20"] <- "Greater than 15 years"
survey$q15new[survey$q15new == "16-20"] <- "Greater than 15 years"
survey$q15new[survey$q15new == "> 20 "] <- "Greater than 15 years"
survey$q15new[survey$q15new == ">20 y"] <- "Greater than 15 years"
survey$q15new[survey$q15new == "No lo"] <- NA

#question 16
survey$q16new[survey$q16 == "LED (Primo other)"] <- "LED (Primo)"
survey$q16new[survey$q16 == "LED (Primo)"] <- "LED (Primo)"
survey$q16new[survey$q16 == "HBO 100W (Mercury)"] <- "HBO 100W (Mercury)"
survey$q16new[survey$q16 == "HBO 100W (Mercurio)"] <- "HBO 100W (Mercury)"
survey$q16new[survey$q16 == "HBO 50W (Mercury)"] <- "HBO 50W (Mercury)"
survey$q16new[survey$q16 == "HBO 50W (Mercurio)"] <- "HBO 50W (Mercury)"
survey$q16new[survey$q16 == "HALOGÊNICA 12V/100W"] <- "Halogenic 12V/100W"

#question 36

survey$q36new[survey$q36 == "Comercial"] <- "Comercial"
survey$q36new[survey$q36 == "Comercial; Producido en el laboratorio"] <- "Other"
survey$q36new[survey$q36 == "commercial or"] <- "Comercial"
survey$q36new[survey$q36 == "commercial or; in-house"] <- "Other"
survey$q36new[survey$q36 == "Del CDC"] <- "Other"
survey$q36new[survey$q36 == "Laboratório da rede do Ministério da Agricultura - LANAGRO Pedro Leopoldo - MG"] <- "In-house"
survey$q36new[survey$q36 == "Producido en el laboratorio"] <- "In-house"
survey$q36new[survey$q36 == "Produzido em um laboratório da rede do Ministério da Agricultura- MAPA"] <- "In-house"
survey$q36new[survey$q36 == "SENASA ARGENTINA"] <- "In-house"


#question 38
survey$q38new[survey$q38 == "Cerebro de raton"] <- "Yes"
survey$q38new[survey$q38 == "Do not use"] <- "No"
survey$q38new[survey$q38 == "EMD Millipore Light Diagnostics for Monoclonal Conjugate (Negative control) # 5102"] <- "Yes"
survey$q38new[survey$q38 == "Laboratório da rede do Ministério da Agricultura - LANAGRO Pedro Leopoldo - MG"] <- "Yes"
survey$q38new[survey$q38 == "Merck Millipore conjugado monoclonal (control negativo) #5102"] <- "Yes"
survey$q38new[survey$q38 == "No utiliza"] <- "No"
survey$q38new[survey$q38 == "Utilizamos como control negativo improntas de cerebros negativos"] <- "Yes"

#question 39
survey$q39new[survey$q39 == "15minutos"] <- "30m or less"
survey$q39new[survey$q39 == "20 min"] <- "30m or less"
survey$q39new[survey$q39 == "30 min"] <- "30m or less"
survey$q39new[survey$q39 == "40"] <- "Greater than 30m"
survey$q39new[survey$q39 == "45 min"] <- "Greater than 30m"
survey$q39new[survey$q39 == "60 min"] <- "Greater than 30m"

#question 41
survey$q41new[survey$q41 == "Agua; PBS"] <- "Water; PBS"
survey$q41new[survey$q41 == "PBS"] <- "PBS"
survey$q41new[survey$q41 == "PBS and water"] <- "Water; PBS"
survey$q41new[survey$q41 == "PBS y agua"] <- "Water; PBS"
survey$q41new[survey$q41 == "Solucion salina tamponada y agua"] <- "Saline Solution; Water"
survey$q41new[survey$q41 == "Water"] <- "Water"

#question 45 
survey$q45new[survey$q45=="1"]<-"1"
survey$q45new[survey$q45=="2"]<-">1"
survey$q45new[survey$q45=="3"]<-">1"

newtable_new<-newtable

library(plyr)
newtable_new<-rename(newtable, c("Labname"="Labname_new"))

merge_agree <- merge(survey, newtable_new, by = "Labname_new")

#Consider normality of Sensitivity Outcome and Transform if necessary

    # Distribution of outcome Sensitivity Kernel Density Plot
    d <- density(as.numeric(merge_agree$Sensitivity)) # returns the density data 
    plot(d, main="Distribution of Sensitivity")

    #Log transform merge_agree$Sensitivity
    
    logsn<-log(as.numeric(merge_agree$Sensitivity))
    l<- density(logsn)
    plot(l)
# No difference in log transformation.Reran results with log transform.

#Consider normality of Sensitivity Outcome and Transform if necessary

# Distribution of outcome Sensitivity Kernel Density Plot
d <- density(as.numeric(merge_agree$Accuracy)) # returns the density data 
plot(d, main="Distribution of Accuracy")

#Log transform merge_agree$Sensitivity

logsn<-log(as.numeric(merge_agree$Sensitivity))
l<- density(logsn)
plot(l)
#Plots of the mean sensitivity by category for all variables

factor <- factor(merge_agree$q02)
          aov.ex1 = aov(as.numeric(merge_agree$Sensitivity)~factor) #do the analysis of variance
print(summary(aov.ex1)) #show the summary table
print(model.tables(aov.ex1,"means"),digits=3) #report the means and the number of subjects/cell
          boxplot(as.numeric(merge_agree$Sensitivity)~factor)
        png(file = "q02.png", bg = "transparent")
          plotmeans(as.numeric(merge_agree$Sensitivity)~factor,xlab="# of rabies samples analyzed per week",
                    ylab= "Sensitivity", main="Sensitivity by # of rabies samples per week")
        dev.off()                  

factor <- factor(merge_agree$q03new)
          aov.ex1 = aov(as.numeric(merge_agree$Sensitivity)~factor) #do the analysis of variance
          summary(aov.ex1)                              #show the summary table
          print(model.tables(aov.ex1,"means"),digits=3) #report the means and the number of subjects/cell
          boxplot(as.numeric(merge_agree$Sensitivity)~factor)
          plotmeans(as.numeric(merge_agree$Sensitivity)~factor,xlab="# of People",
          ylab= "Sensitivity", main="Sensitivity by # Lab Workers Performing Rabies")

factor <- factor(merge_agree$q04)
          wilcox.test(as.numeric(merge_agree$Sensitivity)~factor) 
          ttest<-t.test(as.numeric(merge_agree$Sensitivity)~factor)
          print(ttest)
          boxplot(as.numeric(merge_agree$Sensitivity)~factor)
          plotmeans(as.numeric(merge_agree$Sensitivity)~factor,xlab="No (0)/Yes(1)",
          ylab= "Sensitivity", main="Sensitivity by Quality Control Program")


#only 1 lab reporting yes
          factor <- factor(merge_agree$q10)
          wilcox.test(as.numeric(merge_agree$Sensitivity)~factor) 
          ttest<-t.test(as.numeric(merge_agree$Sensitivity)~factor)
          print(ttest)
          boxplot(as.numeric(merge_agree$Sensitivity)~factor)
          plotmeans(as.numeric(merge_agree$Sensitivity)~factor,xlab="No (0)/Yes(1)",
                    ylab= "Sensitivity", main="Sensitivity by Biological Safety Cabinet") 
          

factor <- factor(merge_agree$q11)
          wilcox.test(as.numeric(merge_agree$Sensitivity)~factor) 
          ttest<-t.test(as.numeric(merge_agree$Sensitivity)~factor)
          print(ttest)
          boxplot(as.numeric(merge_agree$Sensitivity)~factor)
          plotmeans(as.numeric(merge_agree$Sensitivity)~factor,xlab="No (0)/Yes(1)",
                    ylab= "Sensitivity", main="Sensitivity by Biological Safety Cabinet II") 




factor <- factor(merge_agree$q12)
          wilcox.test(as.numeric(merge_agree$Sensitivity)~factor) 
          ttest<-t.test(as.numeric(merge_agree$Sensitivity)~factor)
          print(ttest)
          boxplot(as.numeric(merge_agree$Sensitivity)~factor)
          plotmeans(as.numeric(merge_agree$Sensitivity)~factor,xlab="No (0)/Yes(1)",
                    ylab= "Sensitivity", main="Sensitivity by Biological Safety Cabinet III") 


factor <- factor(merge_agree$q15new)
          aov.ex1 = aov(as.numeric(merge_agree$Sensitivity)~factor) #do the analysis of variance
          summary(aov.ex1)                              #show the summary table
          print(model.tables(aov.ex1,"means"),digits=3) #report the means and the number of subjects/cell
          boxplot(as.numeric(merge_agree$Sensitivity)~factor)
          plotmeans(as.numeric(merge_agree$Sensitivity)~factor,xlab="# of People",
                    ylab= "Sensitivity", main="Sensitivity by Age of Microscope")

factor <- factor(merge_agree$q16new)
          aov.ex1 = aov(as.numeric(merge_agree$Sensitivity)~factor) #do the analysis of variance
          summary(aov.ex1)                              #show the summary table
          print(model.tables(aov.ex1,"means"),digits=3) #report the means and the number of subjects/cell
          boxplot(as.numeric(merge_agree$Sensitivity)~factor)
          plotmeans(as.numeric(merge_agree$Sensitivity)~factor,xlab="# of People",
                    ylab= "Sensitivity", main="Sensitivity by Type of Light Source")


factor <- factor(merge_agree$q21)
          wilcox.test(as.numeric(merge_agree$Sensitivity)~factor) 
          ttest<-t.test(as.numeric(merge_agree$Sensitivity)~factor)
          print(ttest) #report the means and the number of subjects/cell
          boxplot(as.numeric(merge_agree$Sensitivity)~factor)
          plotmeans(as.numeric(merge_agree$Sensitivity)~factor,xlab="No (0)/Yes(1)",
                    ylab= "Sensitivity", main="Sensitivity by Existence of a Calibration Program") 

factor <- factor(merge_agree$q22)
          wilcox.test(as.numeric(merge_agree$Sensitivity)~factor) 
          ttest<-t.test(as.numeric(merge_agree$Sensitivity)~factor)
          print(ttest) #report the means and the number of subjects/cell
          boxplot(as.numeric(merge_agree$Sensitivity)~factor)
          plotmeans(as.numeric(merge_agree$Sensitivity)~factor,xlab="No (0)/Yes(1)",
                    ylab= "Sensitivity", main="Sensitivity by Existence of DFA Protocol") 
          
factor <- factor(merge_agree$q25)
          wilcox.test(as.numeric(merge_agree$Sensitivity)~factor) 
          ttest<-t.test(as.numeric(merge_agree$Sensitivity)~factor)
          print(ttest) #report the means and the number of subjects/cell
          boxplot(as.numeric(merge_agree$Sensitivity)~factor)
          plotmeans(as.numeric(merge_agree$Sensitivity)~factor,xlab="No (0)/Yes(1)",
                    ylab= "Sensitivity", main="Sensitivity by Existence of Written Approval/Rejection Criteria") 

factor <- factor(merge_agree$q35)
          wilcox.test(as.numeric(merge_agree$Sensitivity)~factor) 
          ttest<-t.test(as.numeric(merge_agree$Sensitivity)~factor)
          print(ttest) #report the means and the number of subjects/cell
          boxplot(as.numeric(merge_agree$Sensitivity)~factor)
          plotmeans(as.numeric(merge_agree$Sensitivity)~factor,xlab="No (0)/Yes(1)",
                    ylab= "Sensitivity", main="Sensitivity by Number of Conjugates Used for Each Test") 

factor <- factor(merge_agree$q36new)
          aov.ex1 = aov(as.numeric(merge_agree$Sensitivity)~factor) #do the analysis of variance
          summary(aov.ex1)                              #show the summary table
          print(model.tables(aov.ex1,"means"),digits=3) #report the means and the number of subjects/cell
          boxplot(as.numeric(merge_agree$Sensitivity)~factor)
          plotmeans(as.numeric(merge_agree$Sensitivity)~factor,xlab="# of People",
                    ylab= "Sensitivity", main="Sensitivity by Age of Light Source")

factor <- factor(merge_agree$q38new)
          wilcox.test(as.numeric(merge_agree$Sensitivity)~factor) 
          ttest<-t.test(as.numeric(merge_agree$Sensitivity)~factor)
          print(ttest) #report the means and the number of subjects/cell
          boxplot(as.numeric(merge_agree$Sensitivity)~factor)
          plotmeans(as.numeric(merge_agree$Sensitivity)~factor,xlab="Use",
                    ylab= "Sensitivity", main="Sensitivity by Use of Rabies Neg. Control FITC labeled conjugate (Y/N)") 

factor <- factor(merge_agree$q39new)
          wilcox.test(as.numeric(merge_agree$Sensitivity)~factor) 
          ttest<-t.test(as.numeric(merge_agree$Sensitivity)~factor)
          print(ttest) #report the means and the number of subjects/cell
          boxplot(as.numeric(merge_agree$Sensitivity)~factor)
          plotmeans(as.numeric(merge_agree$Sensitivity)~factor,xlab="Time",
                    ylab= "Sensitivity", main="Sensitivity by Incubation Time") 

factor <- factor(merge_agree$q41new)
          aov.ex1 = aov(as.numeric(merge_agree$Sensitivity)~factor) #do the analysis of variance
          summary(aov.ex1)                              #show the summary table
          print(model.tables(aov.ex1,"means"),digits=3) #report the means and the number of subjects/cell
          boxplot(as.numeric(merge_agree$Sensitivity)~factor)
          plotmeans(as.numeric(merge_agree$Sensitivity)~factor,xlab="Solution",
                    ylab= "Sensitivity", main="Sensitivity by Slide Rinse")

factor <- factor(merge_agree$q45new)
          wilcox.test(as.numeric(merge_agree$Sensitivity)~factor) 
          ttest<-t.test(as.numeric(merge_agree$Sensitivity)~factor)
          print(ttest) #report the means and the number of subjects/cell
          boxplot(as.numeric(merge_agree$Sensitivity)~factor)
          plotmeans(as.numeric(merge_agree$Sensitivity)~factor,xlab="No (0)/Yes(1)",
                    ylab= "Sensitivity", main="Sensitivity by Number of Conjugates Used for Each Test")

factor <- factor(merge_agree$q48)
          wilcox.test(as.numeric(merge_agree$Sensitivity)~factor) 
          ttest<-t.test(as.numeric(merge_agree$Sensitivity)~factor)
          print(ttest) #report the means and the number of subjects/cell
          boxplot(as.numeric(merge_agree$Sensitivity)~factor)
          plotmeans(as.numeric(merge_agree$Sensitivity)~factor,xlab="No (0)/Yes(1)",
                    ylab= "Sensitivity", main="Sensitivity by Participation in a Proficiency Program") 
          
          
factor <- factor(merge_agree$q50)
          wilcox.test(as.numeric(merge_agree$Sensitivity)~factor) 
          ttest<-t.test(as.numeric(merge_agree$Sensitivity)~factor)
          print(ttest) #report the means and the number of subjects/cell
          boxplot(as.numeric(merge_agree$Sensitivity)~factor)
          plotmeans(as.numeric(merge_agree$Sensitivity)~factor,xlab="No (0)/Yes(1)",
                    ylab= "Sensitivity", main="Sensitivity by VNA titer check") 
          

#Relationships between Survey Variables

tbl<-table (merge_agree$q02, merge_agree$q03new)
chisq.test(tbl) 

