#This R file cleans and summarizes the results from the Laboratory Survey
#associated with the proficiency panel exercise completed by PAHO/CDC in 2014 for Rabies Diagnosis
#The focus of this code is to print out to file the frequency tables for each question in the survey
#The .csv is sourced from the original excel files from the project, downloaded from AdobeFormCenter
#The original excel spreadsheet and a record of any modifications made prior to import 
#are outlined in the Word File "REDILAR_analysis plan" 

#Datacoding ( For yes/no questions - 1=YES, 0=No, 3-Missing/Undetermined)
#The labname and surveyid here are the originals. See the countrycode.csv for the translation of labname. 


#Call library
library (rtf)

#Set working drive - for work
setwd("C:/Users/carvalhom/Documents/PAHO_drive/I EJERCICIO INTER-LABORATORIAL/Mary Synthesis/Results/Code and Datasets Used")#set data library

#for home
setwd("~/Documents/PAHO_Drive/I EJERCICIO INTER-LABORATORIAL/Mary Synthesis/")

#Having issues on Mac, work around:
#surveyrecode <- read.csv("~/Documents/PAHO_Drive/I EJERCICIO INTER-LABORATORIAL/Mary Synthesis/surveyrecode.csv", header=FALSE)
#surveydata<-data.frame(surveyrecode[2:25,])
#colnames<-as.data.frame(surveyrecode[1,])
#colnames(surveydata)<-as.matrix(colnames[1,])


surveydata<-read.csv("surveyrecode.csv", header = TRUE, sep = ",")
head (surveydata)
questions<-read.csv("SurveyQuestionList.csv", header = TRUE, sep = ",")
countrycodes<-read.csv("CountryCodes_05292015.csv", header = TRUE, sep = ",")



#Remove L08/L3, L3 answered the survey twice. Am using the most recent and complete version of the answers from nov 2014.
surveydata<-subset(surveydata,  !(Surveycode =="L08"), rownames=NULL)
row.names (surveydata)<-NULL


#recode erroneous variables

#question 1a: (Blank, NA and Ninguno or Nothing were all coded as NA)
surveydata$q01anew[surveydata$q01a==0]<-NA
surveydata$q01anew[surveydata$q01a=="Inoculación en Ratón"]<-"Mouse Inoculation"
surveydata$q01anew[surveydata$q01a=="ninguno"]<-NA
surveydata$q01anew[surveydata$q01a=="Direct rapid immunohistochemistry test"]<-"Direct rapid immunohistochemistry test"

#question 2 (20-30 and 21-30 were coded as 21-30)
  surveydata$q02new[surveydata$q02=="0-5"]<-"00-05"
  surveydata$q02new[surveydata$q02=="6-10"]<-"06-10"
  surveydata$q02new[surveydata$q02=="11-20"]<-"11-20"
  surveydata$q02new[surveydata$q02=="20-30"]<-"21-30"
  surveydata$q02new[surveydata$q02=="21-30"]<-"21-30"
  surveydata$q02new[surveydata$q02=="31-50"]<-"31-50"
  surveydata$q02new[surveydata$q02=="51-100"]<-"50-100" #combine with 120 to make >50 category?
  surveydata$q02new[surveydata$q02=="120"]<-"Single Report of 120"
#question 3
  surveydata$q03[surveydata$q03 == "Tres"] <- 3
  surveydata$q03[surveydata$q03 == "Three"] <- 3
  surveydata$q03[surveydata$q03 == "02"] <- 2
  surveydata$q03[surveydata$q03 == "03"] <- 3

 #question 14
  surveydata$q14[surveydata$q14 == "Leika"] <- "Leica"

  #question 15
    surveydata$q15new<- substr(surveydata$q15, 1,5)
    surveydata$q15new[surveydata$q15new == "0-5 a"] <- "0-5 years"
    surveydata$q15new[surveydata$q15new == "0-5 y"] <- "0-5 years"
    surveydata$q15new[surveydata$q15new == "6-10 "] <- "6-10 years"
    surveydata$q15new[surveydata$q15new == "11-15"] <- "11-15 years"
    surveydata$q15new[surveydata$q15new == "15-20"] <- "16-20 years"
    surveydata$q15new[surveydata$q15new == "16-20"] <- "16-20 years"
    surveydata$q15new[surveydata$q15new == "> 20 "] <- "Greater Than 20"
    surveydata$q15new[surveydata$q15new == ">20 y"] <- "Greater Than 20"
    surveydata$q15new[surveydata$q15new == "No lo"] <- "Unknown"

  #question 16
        surveydata$q16new[surveydata$q16 == "LED (Primo other)"] <- "LED (Primo)"
        surveydata$q16new[surveydata$q16 == "LED (Primo)"] <- "LED (Primo)"
        surveydata$q16new[surveydata$q16 == "HBO 100W (Mercury)"] <- "HBO 100W (Mercury)"
        surveydata$q16new[surveydata$q16 == "HBO 100W (Mercurio)"] <- "HBO 100W (Mercury)"
        surveydata$q16new[surveydata$q16 == "HBO 50W (Mercury)"] <- "HBO 50W (Mercury)"
        surveydata$q16new[surveydata$q16 == "HBO 50W (Mercurio)"] <- "HBO 50W (Mercury)"
        surveydata$q16new[surveydata$q16 == "HALOGÊNICA 12V/100W"] <- "Halogenic 12V/100W"

        #question 18. Large variety of responses, check with A.C about if and how he wants to use this variable
            surveydata$q18new[surveydata$q18 == ">40x aceite"] <- ">40x oil"
            surveydata$q18new[surveydata$q18 == "> 40x oil"] <- ">40x oil"
            surveydata$q18new[surveydata$q18 == ">40x oil"] <- ">40x oil"
            surveydata$q18new[surveydata$q18 == "40x dry plan neofluorite"] <- "40x dry plan neofluorite"                            
            surveydata$q18new[surveydata$q18 == "20x dry plan achromat"] <- "20x dry plan achromat"                                 
            surveydata$q18new[surveydata$q18 == "Do not know"] <- "Do not know"                                        
            surveydata$q18new[surveydata$q18 == "40x dry achromat"] <- "40x dry achromat"                                     
            surveydata$q18new[surveydata$q18 == "40x apochromat seco"] <- "40x dry apochromat"                                  
            surveydata$q18new[surveydata$q18 == "40x achromat seco"] <- "40x dry achromat"                                    
            surveydata$q18new[surveydata$q18 == "40x de inmersión en aceite"] <- "40x oil immersion"                                                                    
            surveydata$q18new[surveydata$q18 == "20x apochromat seco; 40x apochromat seco"] <- "20x dry apochromat; 40x dry achromat"            
            surveydata$q18new[surveydata$q18 == "20x neufluorite seco; 40x neofluorite seco"] <- "20x dry neoflourite; 40x dry neoflourite"            
            surveydata$q18new[surveydata$q18 == "20x acromático seco; 40x achromat seco"] <- "20x dry achromat; 40x dry achromat"                        
            surveydata$q18new[surveydata$q18 == "40x neofluorite seco"] <- "40x dry neoflourite"                          
            surveydata$q18new[surveydata$q18 == "20x acromático seco; 40x achromat seco; >40x aceite"]<-"20x dry achromat; 40x dry achromat; >40x oil"
            table(surveydata$q18new)                                         

#question 26
        surveydata$q26new[surveydata$q26 == "4 areas medulla; cerebellum; hippocampus; pons.  Plus spinal cord if available."] <- "Medulla; cerebellum; hippocampus; pons; spinal cord (if available)"
        surveydata$q26new[surveydata$q26 == "Cerebellum"] <- "Cerebellum"
        surveydata$q26new[surveydata$q26 == "Cerebellum - hippocampus - cortex"] <- "Cerebellum; Hippocampus; Cortex"
        surveydata$q26new[surveydata$q26 == "Cerebelo"] <- "Cerebellum"
        surveydata$q26new[surveydata$q26 == "Cerebelo asta de Ammon y corteza"] <- "Cerebellum; Hippocampus; Cortex"
        surveydata$q26new[surveydata$q26 == "Cuando se remite únicamente un hemisferio cerebral se analizan de manera rutinaria tálamo; trígono olfatorio y corteza frontal. Cuando se remite medio cerebro en corte sagital se utilizan puente; cerebelo y trígono olfatorio."] <- "Olfatory Trigone; Cortex; Pons; Cerebellum"
        surveydata$q26new[surveydata$q26 == "Cuerno de Ammon; corteza y cerebelo."] <- "Cerebellum; Hippocampus; Cortex"
        surveydata$q26new[surveydata$q26 == "Hippocampus (cuerno de Ammon) derecho e izquierdo"] <- "Hippocampus"
        surveydata$q26new[surveydata$q26 == "las 3: Cerebelo; Hipocampo;Corteza cerebral"] <- "Cerebellum; Hippocampus; Cortex"
        surveydata$q26new[surveydata$q26 == "Medula oblonga (tronco cerebral)"] <- "Medulla oblongata (brain stem)"
        surveydata$q26new[surveydata$q26 == "Medula oblonga (tronco cerebral); Cerebelo e  Hippocampus (cuerno de Ammon) derecho e izquierdo"] <- "Medulla; Cerebellum; Hippocampus"
        surveydata$q26new[surveydata$q26 == "Medula oblonga; cerebelo; hippocampus; corteza cerebral"] <- "Medulla; Cerebellum; Hippocampus; Cortex"
        surveydata$q26new[surveydata$q26 == "Medula oblonga;Cerebelo; Hippocampus."] <- "Medulla; Cerebellum; Hippocampus"
        surveydata$q26new[surveydata$q26 == "médula; cerebelo; astas de Ammon; corteza cerebral"] <- "Medulla; Cerebellum; Hippocampus; Cortex"
        surveydata$q26new[surveydata$q26 == "Medulla oblongata (brain stem)"] <- "Medulla oblongata (brain stem)"
        surveydata$q26new[surveydata$q26 == "se utiliza tronco cerebral; cuerno de Ammon y cerebelo (estos dos últimos de ambos hemisferios)"] <- "Medulla; Cerebellum; Hippocampus"
        surveydata$q26new[surveydata$q26 == "Thalamus; cerebral cortex; hippocampus and cerebellum"] <- "Hippocampus; Cerebellum; Cortex; Thalamus"
        surveydata$q26new[surveydata$q26 == "todas las anteriores"] <- "Medulla; Cerebellum; Hippocampus; Cortex"
        surveydata$q26new[surveydata$q26 == "Tres areas:cerebelo; hipocampo; corteza"] <- "Cerebellum; Hippocampus; Cortex"
        surveydata$q26new[surveydata$q26 == "medula oblonga; cerebelo; hippocampus; corteza cerebral"] <- "Medulla; Cerebellum; Hippocampus; Cortex"

#question 27
         surveydata$q27new[surveydata$q27 == "Brain impressions"] <- "Brain impressions"
         surveydata$q27new[surveydata$q27 == "Brain smears"] <- "Brain smears"
         surveydata$q27new[surveydata$q27 == "Impresiones cerebrales"] <- "Brain impressions"
#question 28
          surveydata$q28new[surveydata$q28 == "1"] <- 1
          surveydata$q28new[surveydata$q28 == "2"] <- 2
          surveydata$q28new[surveydata$q28 == "3"] <- 3
          surveydata$q28new[surveydata$q28 == "4"] <- 4
          surveydata$q28new[surveydata$q28 == "2 por cada área de tejido (en total 10 por muestra)"] <- "Two impressions per area"
          surveydata$q28new[surveydata$q28 == "two impressions per area"] <- "Two impressions per area"
          surveydata$q28new[surveydata$q28 == "Suficiente para probar la sección transversal completa del tronco cerebral y el cerebelo"] <- "No number specified"


#question 30a

          surveydata$q30anew[surveydata$q30a == "Acetona"] <- "Acetone"
          surveydata$q30anew[surveydata$q30a == 0] <- NA
          surveydata$q30anew[surveydata$q30a == "Acetona en freezer (-20ºC)"] <- "Acetone in Freezer (-20C)"
          surveydata$q30anew[surveydata$q30a == "acetone"] <- "Acetone"
          surveydata$q30anew[surveydata$q30a == "Congelacion"] <- "Freezing"
          surveydata$q30anew[surveydata$q30a == "Fijo las laminillas solamente si es necesario."] <- "Fix only if necessary"
          surveydata$q30anew[surveydata$q30a == "UV irradiation for 2 minutes followed by acetone."] <- "UV irradiation for 2 minutes followed by acetone."


#question 31
    surveydata$q31new[surveydata$q31 == "0"] <- NA
    surveydata$q31new[surveydata$q31 == "1 - 4 hours"] <- "1 - 4 hours"
    surveydata$q31new[surveydata$q31 == "15 min"] <- "15-30 min"
    surveydata$q31new[surveydata$q31 == "20 min"] <- "15-30 min"
    surveydata$q31new[surveydata$q31 == "30 min"] <- "15-30 min"
    surveydata$q31new[surveydata$q31 == "45 min"] <- "31-60 min"
    surveydata$q31new[surveydata$q31 == "60 min"] <- "31-60 min"
    surveydata$q31new[surveydata$q31 == "2 a 4 horas"] <- "1 - 4 hours"
    surveydata$q31new[surveydata$q31 == "2 horas"] <- "1 - 4 hours"
    surveydata$q31new[surveydata$q31 == "Durante la noche"] <- "Overnight"
    surveydata$q31new[surveydata$q31 == "45 minutos"] <- "31-60 min"
    surveydata$q31new[surveydata$q31 == "4 horas"] <- "1 - 4 hours"

    #question 32
    surveydata$q32new[surveydata$q32 == "0"] <- NA
    surveydata$q32new[surveydata$q32 == "-20 °C"] <- "-20 °C"
    surveydata$q32new[surveydata$q32 == "4 °C"] <- "4 °C"
    surveydata$q32new[surveydata$q32 == "4ºC a 8ºC ou - 20ºC"] <- "4 -8°C; - 20°C"
    surveydata$q32new[surveydata$q32 == "-80 °C"] <- "-80 °C"
    surveydata$q32new[surveydata$q32 == "Temperatura ambiente"] <- "Ambient Temperature"



    #question 33
          surveydata$q33[surveydata$q33 == "Vidrio"] <- "Plain glass"
          surveydata$q33[surveydata$q33 == "Vidrio liso anillado"] <- "Ringed glass"
        #question 34
          surveydata$q34[surveydata$q34 == "3 por duplicado; o sea 6 impresiones de 3 tejidos diferentes"] <- "6"      


#question 36

        surveydata$q36new[surveydata$q36 == "Comercial"] <- "Comercial"
        surveydata$q36new[surveydata$q36 == "Comercial; Producido en el laboratorio"] <- "Comercial; In-house"
        surveydata$q36new[surveydata$q36 == "commercial or"] <- "Comercial"
        surveydata$q36new[surveydata$q36 == "commercial or; in-house"] <- "Comercial; In-house"
        surveydata$q36new[surveydata$q36 == "Del CDC"] <- "CDC"
        surveydata$q36new[surveydata$q36 == "Laboratório da rede do Ministério da Agricultura - LANAGRO Pedro Leopoldo - MG"] <- "LANAGRO-MG"
        surveydata$q36new[surveydata$q36 == "Producido en el laboratorio"] <- "In-house"
        surveydata$q36new[surveydata$q36 == "Produzido em um laboratório da rede do Ministério da Agricultura- MAPA"] <- "MAPA (laboratório da rede do Ministério da Agricultura)"
        surveydata$q36new[surveydata$q36 == "SENASA ARGENTINA"] <- "SENASA ARGENTINA"
#question 37
      surveydata$q37new[surveydata$q37 == "0"] <- NA
      surveydata$q37new[surveydata$q37 == "Biorad"] <- "Biorad"
      surveydata$q37new[surveydata$q37 == "EMD Millipore # 5100"] <- "EMD Millipore #5100"
      surveydata$q37new[surveydata$q37 == "EMD Millipore # 5100; EMD Millipore #5199"] <- "EMD Millipore #5100; EMD Millipore #5199"
      surveydata$q37new[surveydata$q37 == "EMD Millipore #5100"] <- "EMD Millipore #5100"
      surveydata$q37new[surveydata$q37 == "EMD Millipore #6500"] <- "EMD Millipore #6500"
      surveydata$q37new[surveydata$q37 == "FDI"] <- "FDI"
      surveydata$q37new[surveydata$q37 == "FDI; EMD Millipore #5100"] <- "FDI; EMD Millipore #5100"
      surveydata$q37new[surveydata$q37 == "Fujirebio"] <- "Fujirebio"
      surveydata$q37new[surveydata$q37 == "Fujirebio.Diagnostic. Inc. No. Catálogo: 800-092"] <- "Fujirebio"
      surveydata$q37new[surveydata$q37 == "Furijebio (Americano)"] <- "Fujirebio"
      surveydata$q37new[surveydata$q37 == "Instituto Pasteur"] <- "Instituto Pasteur"
      surveydata$q37new[surveydata$q37 == "Instituto Pasteur; Biorad"] <- "Instituto Pasteur; Biorad"
      surveydata$q37new[surveydata$q37 == "Instituto Pasteur; Centre of Expertise for Rabies (Canada)"] <- "Instituto Pasteur; Centre of Expertise for Rabies (Canada)"
      surveydata$q37new[surveydata$q37 == "Laboratório da rede do Ministério da Agricultura - LANAGRO Pedro Leopoldo - MG"] <- "LANAGRO"
      surveydata$q37new[surveydata$q37 == "Laboratório de Produção de Materiais de Referência - LANAGRO-Pedro Leopoldo"] <- "LANAGRO"
      surveydata$q37new[surveydata$q37 == "Lighat Diagnostics Cat # 5100"] <- "Lighat Diagnostics #5100"


#question 38
    surveydata$q38new[surveydata$q38 == "Cerebro de raton"] <- "Rat Cerebellum"
    surveydata$q38new[surveydata$q38 == "Do not use"] <- "Do not use"
    surveydata$q38new[surveydata$q38 == "EMD Millipore Light Diagnostics for Monoclonal Conjugate (Negative control) # 5102"] <- "EMD Millipore Light Diagnostics for Monoclonal Conjugate (Negative control) # 5102"
    surveydata$q38new[surveydata$q38 == "Laboratório da rede do Ministério da Agricultura - LANAGRO Pedro Leopoldo - MG"] <- "LANAGRO"
    surveydata$q38new[surveydata$q38 == "Merck Millipore conjugado monoclonal (control negativo) #5102"] <- "Merck Millipore conjugado monoclonal (control negativo) #5102"
    surveydata$q38new[surveydata$q38 == "No utiliza"] <- "Do not use"
    surveydata$q38new[surveydata$q38 == "Utilizamos como control negativo improntas de cerebros negativos"] <- "Negative Cerebellum Control Sample"

#question 39
    surveydata$q39new[surveydata$q39 == "15minutos"] <- "15 min"
    surveydata$q39new[surveydata$q39 == "20 min"] <- "20 min"
    surveydata$q39new[surveydata$q39 == "30 min"] <- "30 min"
    surveydata$q39new[surveydata$q39 == "40"] <- "40 min"
    surveydata$q39new[surveydata$q39 == "45 min"] <- "45 min"
    surveydata$q39new[surveydata$q39 == "60 min"] <- "60 min"


#question 41
surveydata$q41new[surveydata$q41 == "Agua; PBS"] <- "Water; PBS"
surveydata$q41new[surveydata$q41 == "PBS"] <- "PBS"
surveydata$q41new[surveydata$q41 == "PBS and water"] <- "Water; PBS"
surveydata$q41new[surveydata$q41 == "PBS y agua"] <- "Water; PBS"
surveydata$q41new[surveydata$q41 == "Solucion salina tamponada y agua"] <- "Saline Solution; Water"
surveydata$q41new[surveydata$q41 == "Water"] <- "Water"


#question 42

surveydata$q42new[surveydata$q42 == "03 enjuagues con PBS; seguido de 01 enjuague con agua"] <- "3 PBS rinses; 1 water rinse"
surveydata$q42new[surveydata$q42 == "1 enjuage con PBS y enjuage de agua"] <- "1 PBS rinse; 1 water rinse"
surveydata$q42new[surveydata$q42 == "1 enjuague de PBS y 1 enjuague de agua"] <- "1 PBS rinse; 1 water rinse"
surveydata$q42new[surveydata$q42 == "1 lavado"] <- "1 wash"
surveydata$q42new[surveydata$q42 == "1 lavado con buffer y 1 con agua destilada"] <- "1 Buffer wash; 1 distilled water wash"
surveydata$q42new[surveydata$q42 == "10 lavados en recipiente con PBS y 10 lavados con agua destilada"] <- "10 PBS washes; 10 distilled water washes"
surveydata$q42new[surveydata$q42 == "2 enjuagues de PBS"] <- "2 PBS rinses"
surveydata$q42new[surveydata$q42 == "2 enjuagues de PBS y 1 enjuague de agua"] <- "2 PBS rinses; 1 water rinse"
surveydata$q42new[surveydata$q42 == "2 enjuagues de PBS y 2 enjuagues de agua destilada"] <- "2 PBS rinses; 1 water rinse"
surveydata$q42new[surveydata$q42 == "2 PBS soaks and water soak"] <- "2 PBS soaks and water soak"
surveydata$q42new[surveydata$q42 == "2 soaks in water."] <- "2 soaks in water"
surveydata$q42new[surveydata$q42 == "agitate slide in container with PBS; soak for 2-3 minutes and then move to new container with PBS; agitate; soak for 2-3 minutes"] <- "agitate slide in container with PBS; soak for 2-3 minutes and then move to new container with PBS; agitate; soak for 2-3 minutes"
surveydata$q42new[surveydata$q42 == "Enjuague con una botella de lavado que contiene agua; seguido de 2 enjuagues en PBS"] <- "1 Rinse with Water Rinse Bottle; 2 PBS washes"
surveydata$q42new[surveydata$q42 == "Enjuague con una botella de lavado que contiene PBS seguido de 2 enjuagues en PBS"] <- "1 Rinse with PBS wash bottle;  2 PBS soaks"
surveydata$q42new[surveydata$q42 == "Enjuague con una botella de lavado que contiene PBS seguido de 2 enjuagues en PBS y 1 enjuague en agua"] <- "1 PBS wash; 2 PBS rinses; 1 water rinse"
surveydata$q42new[surveydata$q42 == "Rinse with wash bottle containing PBS followed by 1soak in PBS pH 7.4 and 1 rinse and 1 soak in water"] <- "1 Rinse with PBS wash bottle; 1 soak in PBS pH 7.4; 1 water rinse; 1 water soak"
surveydata$q42new[surveydata$q42 == "Rinse with wash bottle containing PBS followed by 2 soaks in PBS"] <- "1 Rinse with PBS wash bottle;  2 PBS soaks"
surveydata$q42new[surveydata$q42 == "se sumerge en PBS; seguido de 2 lavados en PBS de 5 minutos cada uno dentro de frasco Koplin; se gumerge en agua destilada"] <- "1 PBS soak; 2 PBS washes (5 min, Koplin); 1 water rinse"

#question 43
surveydata$q43new[surveydata$q43 == "03 enjuagues en PBS 10 minutos cada uno; seguido de lavabo con agua"] <- "3 10-min PBS rinses; 1 water wash"
surveydata$q43new[surveydata$q43 == "1 enjuague en PBS de 10 minutos y uno enjuage con agua"] <- "1 10-min PBS rinse; 1 water rinse"
surveydata$q43new[surveydata$q43 == "1 enjuague por 30 minutos en PBS y 1 minuto en agua"] <- "1 30-min PBS rinse; 1 water rinse"
surveydata$q43new[surveydata$q43 == "1 Rinse and 1 soaks in PBS 10 min y 1 rinse and 1 soak in Water 5 min"] <- "1 PBS rinse; 1 10-min PBS soak; 1 water rinse; 1 5-min PBS rinse"
surveydata$q43new[surveydata$q43 == "10 lavados PBS 2 minutos; 10 lavados con agua destilada 2 minutos"] <- "10 2-min PBS washes; 10 2-min water washes"
surveydata$q43new[surveydata$q43 == "10 minutos"] <- "10 min"
surveydata$q43new[surveydata$q43 == "2 enjuagues de PBS 10 minutos y 1 enjuague en agua destilada 5 minutos"] <- "2 10-min PBS rinses; 1 5-min water rinse"
surveydata$q43new[surveydata$q43 == "2 enjuagues en PBS 10 minutos cada uno"] <- "2 10-min PBS rinses"
surveydata$q43new[surveydata$q43 == "2 enjuagues en PBS 10 minutos cada uno; seguido de lavado con agua"] <- "2 10-min PBS rinses; 1 water wash"
surveydata$q43new[surveydata$q43 == "2 enjuagues en PBS 3-5 minutos cada uno; seguido de enjuague con agua"] <- "2 3 to 5-min PBS rinses; 1 water wash"
surveydata$q43new[surveydata$q43 == "2 enjuagues en PBS 3-5 minutos cada uno; seguido de enjuague con agua 3-5 minutos cada uno"] <- "2 3 to 5-min PBS rinses; 1 water wash"
surveydata$q43new[surveydata$q43 == "2 enjuagues en PBS 3-5min cada uno"] <- "2 3 to 5-min PBS rinses"
surveydata$q43new[surveydata$q43 == "2 soaks in PBS 10 min each followed by water rinse"] <- "2 10-min PBS soaks; 1 water rinse"
surveydata$q43new[surveydata$q43 == "2 soaks in PBS 3-5 each"] <- "2 3 to 5-min PBS rinses"
surveydata$q43new[surveydata$q43 == "2 soaks in water for 5 minutes."] <- "2 3 to 5-min water rinses"
surveydata$q43new[surveydata$q43 == "2 soaks of at least 2-3 minutes"] <- "2 3 to 5-min water rinses"
surveydata$q43new[surveydata$q43 == "dos enjuagues rapidos en PBS."] <- "2 quick PBS rinses"
surveydata$q43new[surveydata$q43 == "lavado rapido con SST e introducido en él por 10 minutos;y 1 lavado rápido con agua destilada"] <- "1 quick SST rinse; 1 10-min SST wash; 1 water rinse"
surveydata$q43new[surveydata$q43 == "se sumerge en PBS; seguido de 2 lavados en PBS de 5 minutos cada uno dentro de frasco Koplin; se gumerge en agua destilada"] <- "1 PBS soak; 2 5-min PBS wash; 1 water rinse"

#question 44
surveydata$q44new[surveydata$q44 == "20% Glycerol Tris-Buffered Saline pH 9.0"] <- "20% Glycerol; Tris-Buffered Saline pH 9.0"
surveydata$q44new[surveydata$q44 == "20%glycerol tampo Tris"] <- "20% Glycerol; Tris-Buffered Saline pH 9.0"
surveydata$q44new[surveydata$q44 == "50% glicerol - PBS 0;01M pH 7;5"] <- "50% Glycerol; PBS 7.4"
surveydata$q44new[surveydata$q44 == "50% Glycerol PBS 7.4"] <- "50% Glycerol; PBS 7.4"
surveydata$q44new[surveydata$q44 == "50% glycerol tris buffer pH 9.0"] <- "50% Glycerol; Tris-Buffered Saline pH 9.0"
surveydata$q44new[surveydata$q44 == "9 ml de glicerol mas 1 ml de PBS pH 7"] <- "90% Glycerol; PBS"
surveydata$q44new[surveydata$q44 == "90% de glicerol carbonato - bicarbonato pH 9;0"] <- "90% Glycerol; Carbonate-Bicarbonate Buffer pH 9.0"
surveydata$q44new[surveydata$q44 == "90% de glicerol solución salina tamponada con Tris pH 9;0"] <- "90% Glycerol Tris-Buffered Saline pH 9.0"
surveydata$q44new[surveydata$q44 == "90% Glycerol Carbonate-Bicarbonate pH 9.0"] <- "90% Glycerol Tris-Buffered Saline pH 9.0"
surveydata$q44new[surveydata$q44 == "Commercial Fluoro gel mounting medium"] <- "Commercial Fluoro gel mounting medium"
surveydata$q44new[surveydata$q44 == "Fluoprep de Biomerieux"] <- "Fluoprep from Biomerieux"
surveydata$q44new[surveydata$q44 == "Glicerol 50 PBS; pH 7;6"] <- "50% Glycerol PBS 7.4"
surveydata$q44new[surveydata$q44 == "Glicerol 90% con 10% de PBS"] <- "90% Glycerol; PBS"
surveydata$q44new[surveydata$q44 == "Glicerol 99 % - solucion salina tamponada"] <- "99% Glycerol PBS 7.4"
surveydata$q44new[surveydata$q44 == "glicerol al 50% con solución salina fosfatada"] <- "50% Glycerol PBS 7.4"
surveydata$q44new[surveydata$q44 == "Glicerol Sigma grade"] <- "Glycerol"
surveydata$q44new[surveydata$q44 == "Glicerol-Solução Salina Tamponada pH= 8;5"] <- "Glycerol with PBS"
surveydata$q44new[surveydata$q44 == "medio de montaje de fluorescencia marca Dako"] <- "Fluorescence Mounting Medium - Dako"
surveydata$q44new[surveydata$q44 == "MONTING FLUID LIGHT DIAGNOSTICS; CHEMICON; CAT. No. 5013"] <- "Monting Fluid Light Diagnostics; Chemicon; CAT. No. 5013"


      #question 47
  surveydata$q47new[surveydata$q47 == "Cultivo celular"] <- "Cell Culture"
  surveydata$q47new[surveydata$q47 == "Inoculación de ratón"] <- "Mouse inoculation"
  surveydata$q47new[surveydata$q47 == "Mouse inoculation"] <- "Mouse inoculation"
  surveydata$q47new[surveydata$q47 == "Repeat DFA with specificity control reagents and anti-bodies conjugates"] <- "Repeat DFA with specificity control reagents and anti-bodies conjugates"
  surveydata$q47new[surveydata$q47 == "Repetir IFD con reactivos de control de especificidad y conjugados antirrábicos"] <- "Repeat DFA with specificity control reagents and anti-bodies conjugates"
  surveydata$q47new[surveydata$q47 == "RT-PCR"] <- "RT-PCR"
#question 51
surveydata$q51new[surveydata$q51 == "0"] <- NA
surveydata$q51new[surveydata$q51 == "1;6 años"] <- "Every 1 to 6 years"
surveydata$q51new[surveydata$q51 == "Cada 6 meses"] <- "every 6 months"
surveydata$q51new[surveydata$q51 == "Cada dos años"] <- "every 2 years"
surveydata$q51new[surveydata$q51 == "every 2 years"] <- "every 2 years"
surveydata$q51new[surveydata$q51 == "every 6 months"] <- "every 6 months"
surveydata$q51new[surveydata$q51 == "every year"] <- "every year"
surveydata$q51new[surveydata$q51 == "Se realizò hace algunos años atraz"] <- "Some years ago"
surveydata$q51new[surveydata$q51 == "Todos los años"] <- "every year"

                  #question 52
surveydata$q52new[surveydata$q52 == "0"] <- NA
surveydata$q52new[surveydata$q52 == "ELISA (Does not test neutralizing antibody)"] <- "ELISA (Does not test neutralizing antibody)"
surveydata$q52new[surveydata$q52 == "ELISA (no detecta anticuerpos neutralizantes)"] <- "ELISA (Does not test neutralizing antibody)"
surveydata$q52new[surveydata$q52 == "Fluorescent antibody virus neutralization (FAVN) test"] <- "Fluorescent antibody virus neutralization (FAVN) test"
surveydata$q52new[surveydata$q52 == "Neutralización viral de anticuerpo fluorescente (FAVN)"] <- "Fluorescent antibody virus neutralization (FAVN) test"
surveydata$q52new[surveydata$q52 == "ninguna"] <- "Do not perform"
surveydata$q52new[surveydata$q52 == "Ninguno"] <- "Do not perform"
surveydata$q52new[surveydata$q52 == "ninguno"] <- "Do not perform"
surveydata$q52new[surveydata$q52 == "No se hace"] <- "Do not perform"
surveydata$q52new[surveydata$q52 == "no se realizan en el pais"] <- "Do not perform"
surveydata$q52new[surveydata$q52 == "Prueba de Suero Neutralización"] <- "Seroneutralization Test"
surveydata$q52new[surveydata$q52 == "Prueba rápida de inhibición de focos fluorescentes (RFFIT)"] <- "Rapid fluorescent focus inhibition test (RFFIT)"
surveydata$q52new[surveydata$q52 == "Rapid fluorescent focus inhibition test (RFFIT)"] <- "Rapid fluorescent focus inhibition test (RFFIT)"
surveydata$q52new[surveydata$q52 == "Se realiza en laboratorio de referencia (CDC EEUU)"] <- "Performed in CDC Reference Laboratory"
surveydata$q52new[surveydata$q52 == "Seroneutralizacion en ratones (S.N.)"] <- "Seroneutralization Test in Mice"
surveydata$q52new[surveydata$q52 == "SFIMT - Simplified Fluorescent Inhibition Microtest"] <- "SFIMT - Simplified Fluorescent Inhibition Microtest"

#question 53
surveydata$q53new[surveydata$q53 == "Cada 6 meses"] <- "Every 6  months"
surveydata$q53new[surveydata$q53 == "every 2 years"] <- "Every 2 years"
surveydata$q53new[surveydata$q53 == "every year"] <- "Every year"
surveydata$q53new[surveydata$q53 == "if an exposure occurs; if neutralizing antibody titer <0.5 IU/ml"] <- "If exposure occurs; if neutralizing antibody titer <0.5 IU/ml"
surveydata$q53new[surveydata$q53 == "if neutralizing antibody titer <0.5 IU/ml"] <- "If neutralizing antibody titer <0.5 IU/ml"
surveydata$q53new[surveydata$q53 == "primer refuerzo al año y luego cada 5 años"] <- "First reinforcement after one year; every 5 years after"
surveydata$q53new[surveydata$q53 == "Segun protocolo del Ministerio de Salud de Panamá"] <- "Following the Ministry of Health Protocol"
surveydata$q53new[surveydata$q53 == "Si el título de anticuerpos neutralizantes <0;5UI/ml"] <- "If neutralizing antibody titer <0.5 IU/ml"
surveydata$q53new[surveydata$q53 == "Si se produce una exposición"] <- "If exposure occurs"
surveydata$q53new[surveydata$q53 == "Si se produce una exposición; Si el título de anticuerpos neutralizantes <0;5UI/ml"] <- "If exposure occurs; if neutralizing antibody titer <0.5 IU/ml"
surveydata$q53new[surveydata$q53 == "Todos los años"] <- "Every year"

#question 54

surveydata$q54new[surveydata$q54 == "Buscar atención médica inmediata"] <- "seek medical consultation"
surveydata$q54new[surveydata$q54 == "do nothing if rabies titer > 1.5 or 0.5 IU/ml; notify a supervisor immediately; wash the wound with soap and water (15 minutes); seek medical consultation"] <- "do nothing if rabies titer > 1.5 or 0.5 IU/ml; notify a supervisor immediately; wash the wound with soap and water (15 minutes); seek medical consultation"
surveydata$q54new[surveydata$q54 == "Lavar la herida con agua y jabón (15 minutos); Si la muestra es positiva; vacuna pos-exposición"] <- "wash the wound with soap and water (15 minutes);  receive post-exposure vaccinations if the animal is positive"
surveydata$q54new[surveydata$q54 == "Notificar a un supervisor inmediato"] <- "notify a supervisor immediately"
surveydata$q54new[surveydata$q54 == "Notificar a un supervisor inmediato; Lavar la herida con agua y jabón (15 minutos)"] <- "notify a supervisor immediately; wash the wound with soap and water (15 minutes)"
surveydata$q54new[surveydata$q54 == "Notificar a un supervisor inmediato; Lavar la herida con agua y jabón (15 minutos); Buscar atención médica inmediata"] <- "notify a supervisor immediately; wash the wound with soap and water (15 minutes); seek medical consultation"
surveydata$q54new[surveydata$q54 == "Notificar a un supervisor inmediato; Lavar la herida con agua y jabón (15 minutos); Buscar atención médica inmediata; Implementa protocolo de vacunacion post exposición"] <- "notify a supervisor immediately; wash the wound with soap and water (15 minutes); seek medical consultation; Implement post-exposure vaccination protocol"
surveydata$q54new[surveydata$q54 == "Notificar a un supervisor inmediato; Lavar la herida con agua y jabón (15 minutos); Buscar atención médica inmediata; refuerzo con vacuna antirrábica"] <- "notify a supervisor immediately; wash the wound with soap and water (15 minutes); seek medical consultation; receive 1 booster"
surveydata$q54new[surveydata$q54 == "Notificar a un supervisor inmediato; Lavar la herida con agua y jabón (15 minutos); SE APLICA DOSIS DE REFUERZO DE LA VACUNA"] <- "notify a supervisor immediately; wash the wound with soap and water (15 minutes); seek medical consultation; receive 2 boosters"
surveydata$q54new[surveydata$q54 == "Notificar a un supervisor inmediato; Lavar la herida con agua y jabón (15 minutos); se aplica una dosis de refuerzo"] <- "notify a supervisor immediately; wash the wound with soap and water (15 minutes); seek medical consultation; receive 1 booster"
surveydata$q54new[surveydata$q54 == "notify a supervisor immediately; wash the wound with soap and water (15 minutes); seek medical consultation"] <- "notify a supervisor immediately; wash the wound with soap and water (15 minutes); seek medical consultation"
surveydata$q54new[surveydata$q54 == "notify a supervisor immediately; wash the wound with soap and water (15 minutes); seek medical consultation; receive 2 boosters (day 0 and day 3) if the animal is positive"] <- "notify a supervisor immediately; wash the wound with soap and water (15 minutes); seek medical consultation; receive 2 boosters (day 0 and day 3) if the animal is positive"
surveydata$q54new[surveydata$q54 == "notify a supervisor immediately; wash the wound with soap and water (15 minutes); seek medical consultation; receive 2 boosters (day 0 and day 3) if the animal is positive; notify Veterinary Public Health, Ministry of Health"] <- "notify a supervisor immediately; wash the wound with soap and water (15 minutes); seek medical consultation; receive 2 boosters (day 0 and day 3) if the animal is positive; notify Veterinary Public Health, Ministry of Health"

       
#Print tables to RTF for each question for data analysis

output<-"rtf_questresults.doc"
rtf<-RTF(output,width=8.5,height=11,font.size=10,omi=c(1,1,1,1))
addHeader(rtf,title="Questionnaire Results")
addParagraph(rtf,"\n")
addNewLine(rtf)

addHeader(rtf,title= questions[7,2])
tab<-data.frame(table(surveydata$q01, exclude = NULL,  dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE, NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[8,2])
addHeader(rtf,title="Only 3 labs answered with additional tests")
tab<-data.frame(table (surveydata$q01anew,  dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[9,2])
tab<-data.frame(table (surveydata$q02new,  dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[10,2])
tab<-subset(data.frame(table (surveydata$q03,  dnn = c("Response"))), !(Freq %in% c(0)))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[11,2])
tab<-data.frame(table (surveydata$q04,  dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[12,2])
tab<-data.frame(table (surveydata$q05,  dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[13,2])
tab<-data.frame(table (surveydata$q06,  dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[14,2])
tab<-data.frame(table (surveydata$q07,  dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[15,2])
tab<-data.frame(table (surveydata$q08,  dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[16,2])
tab<-data.frame(table (surveydata$q09,  dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title="Si la respuesta es si?, es el cuarto oscuro dedicado para la rabia?")
addHeader(rtf,title="SPANISH & PORTUGUESE VERSIONS ONLY")
tab<-data.frame(table (surveydata$q09a, exclude = NULL, dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[18,2])
tab<-data.frame(table (surveydata$q10, exclude = NULL, dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[19,2])
tab<-data.frame(table (surveydata$q11, exclude = NULL, dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[20,2])
tab<-data.frame(table (surveydata$q12, exclude = NULL, dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[21,2])
tab<-data.frame(table (surveydata$q13,  dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title="Si la respuesta es s?, es un microscopio de fluorescencia dedicado para la rabia?")
addHeader(rtf,title="SPANISH & PORTUGUESE VERSIONS ONLY")
tab<-data.frame(table (surveydata$q13a,exclude = NULL,  dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[23,2])
tab<-data.frame(table (surveydata$q14, exclude ="Leika",  dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[24,2])
tab<-data.frame(table (surveydata$q15new, exclude = NULL, dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[25,2])
tab<-data.frame(table (surveydata$q16new,  dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[26,2])
addHeader(rtf,title="ENGLISH VERSION ONLY")
tab<-data.frame(table (surveydata$q17,  dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[27,2])
tab<-data.frame(table (surveydata$q18new,  dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[28,2])
tab<-data.frame(table (surveydata$q19,  dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[29,2])
tab<-data.frame(table (surveydata$q20,  dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[30,2])
tab<-data.frame(table (surveydata$q21,  dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[31,2])
tab<-data.frame(table (surveydata$q22,  dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[32,2])
tab<-data.frame(table (surveydata$q23,  dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[33,2])
tab<-data.frame(table (surveydata$q24,  dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[34,2])
tab<-data.frame(table (surveydata$q25,  dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[35,2])
tab<-data.frame(table (surveydata$q26new,  dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)
addNewLine(rtf)

addHeader(rtf,title=questions[36,2])
tab<-data.frame(table (surveydata$q27new,  dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[37,2])
tab<-data.frame(table (surveydata$q28new,  dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[38,2])
tab<-data.frame(table (surveydata$q29,  dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[39,2])
tab<-data.frame(table (surveydata$q30,  dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[40,2])
tab<-subset(data.frame(table(surveydata$q30anew, exclude = NULL, dnn = c("Response"))), !(Response %in% c("0", "acetone")))  #30a-32 are subset of 19 labs, only those who answered yes to 30
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[41,2])
tab<-data.frame(table (surveydata$q31new, exclude = NULL, dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[42,2])
tab<-data.frame(table (surveydata$q32new, exclude = NULL, dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[43,2])
tab<-subset(data.frame(table (surveydata$q33,  dnn = c("Response"))), !(Freq %in% c(0)))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[44,2])
tab<-subset(data.frame(table (surveydata$q34, exclude = "3 por duplicado; o sea 6 impresiones de 3 tejidos diferentes",  dnn = c("Response"))), !(Freq %in% c(0)))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)


addHeader(rtf,title=questions[45,2])
tab<-data.frame(table (surveydata$q35, dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)


addHeader(rtf,title=questions[46,2])
tab<-data.frame(table (surveydata$q36new, dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[47,2])
tab<-data.frame(table (surveydata$q37new, exclude = NULL, dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[48,2])
tab<-data.frame(table (surveydata$q38new, dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[49,2])
tab<-data.frame(table (surveydata$q39new, dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[50,2])
tab<-data.frame(table (surveydata$q40, dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[51,2])
tab<-subset(data.frame(table (surveydata$q41new, dnn = c("Response"))), !(Freq %in% c(0)))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)


addHeader(rtf,title=questions[52,2])
tab<-data.frame(table (surveydata$q42new, dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)
addNewLine(rtf)


addHeader(rtf,title=questions[53,2])
tab<-data.frame(table (surveydata$q43new, dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)
addNewLine(rtf)


addHeader(rtf,title=questions[54,2])
tab<-data.frame(table (surveydata$q44new, exclude = NULL, dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)


addHeader(rtf,title=questions[55,2])
tab<-data.frame(table (surveydata$q45, dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[56,2])
tab<-data.frame(table (surveydata$q46, dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[57,2])
tab<-data.frame(table (surveydata$q47new, exclude = NULL, dnn = c("Response")) )
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[58,2])
tab<-data.frame(table (surveydata$q48, dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[59,2])
tab<-data.frame(table (surveydata$q49, dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[60,2])
tab<-data.frame(table (surveydata$q50, dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[61,2])
tab<-data.frame(table (surveydata$q51new, exclude = NULL, dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[62,2])
tab<-data.frame(table (surveydata$q52new, exclude = NULL, dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[63,2])
tab<-data.frame(table (surveydata$q53new, dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)

addHeader(rtf,title=questions[64,2])
tab<-data.frame(table (surveydata$q54new, dnn = c("Response")))
tab <- transform(tab, cumFreq = cumsum(Freq), Percent = round(prop.table(Freq)*100,2))
addTable(rtf,tab,font.size=9,row.names=FALSE,NA.string="-")
addNewLine(rtf)
done(rtf)

