source_folder <- "C:\\Users\\inigo.bermejo\\Documents\\Data\\BN in RT\\20201123"

data1 <- read.csv(paste(source_folder, "approach1finalCLEAN13112020.csv", sep = "\\"), sep = ';')
data2 <- read.csv(paste(source_folder, "approach2finalCLEAN23112020.csv", sep = "\\"), sep = ';')

data1_stripped <- data.frame(data1$Anatomic_tumor_loc, data1$T_Stage, data1$N_Stage, data1$M_Stage, data1$Treatment_Intent)
data2_stripped <- data.frame(data2$Anatomic_tumor_loc, data2$T_Stage, data2$N_Stage, data2$M_Stage, data2$Treatment_Intent)

write.csv(data1_stripped, paste(source_folder, "approach1finalCLEAN_stripped.dat", sep = "\\"), row.names = FALSE)
write.csv(data2_stripped, paste(source_folder, "approach2finalCLEAN_stripped.dat", sep = "\\"), row.names = FALSE)
