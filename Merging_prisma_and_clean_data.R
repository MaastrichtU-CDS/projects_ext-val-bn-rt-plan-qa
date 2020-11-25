# read excel file
install.packages("readxl")
library("readxl")

#Read the 2 different databases cleaned and prisma data
Dataclean=read_excel('C:/Users/pkale/Desktop/QART/Data_clean.xlsx')
Dataprisma=read_excel('C:/Users/pkale/Desktop/QART/prisma.xlsx')

#check columnnames
colnames(Dataclean)
colnames(Dataprisma)

#Delete volgnummer from Dataprisma
Dataprisma$volgnummer <- NULL

#Merge the 2 databases according to the Patid
Datamerged <-merge(Dataclean, Dataprisma, by='Researchnumber')

#check column names of the merged file
colnames(Datamerged)

#write the merfed file into a new 
install.packages("openxlsx") 
library(openxlsx)
write.xlsx(Datamerged, "C:/Users/pkale/Desktop/QART/Data_merged.xlsx")
colnames(Datamerged)