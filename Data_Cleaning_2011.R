#Load the data
Change_2011 <- read.csv(file="./Data/2011_1YR_3YR_change.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
Coact_2011 <- read.csv(file="./Data/2011_COACT.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
DataMap_2011 <- read.csv(file="./Data/2011_data_map.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
Enrlworking_2011 <- read.csv(file="./Data/2011_enrl_working.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
FinalGrade_2011 <- read.csv(file="./Data/2011_final_grade.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
FRL_2011 <- read.csv(file="./Data/2011_k_12_FRL.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
Remediation_2011 <- read.csv(file="./Data/2011_remediation_HS.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
Address_2011 <- read.csv(file="./Data/2011_school_address.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)


# Change_2011 : 1 & 3 year changes in academic acheivement, growth, overall grade.
# Coact_2011 : Ready for college or not based on ACT (Yes=1,No=2)
# DataMap_2011 : 
# Enrlworking_2011 : Percentage of each ethnicity/race characteristics
# FinalGrade_2011 : Grades given for each school
# FRL_2011 : Percentage of students getting free/reduced price lunch
# Remediation_2011 : Percentage of students requiring remediation
# Address_2011 : Physical address of the schools


#------------------Combine all the data for this year----------------------------#
#Order : FinalGrade - Change - Coact - Enrlworking - FRL- remediation

#1. Combine FinalGrade and Change -> total
Change_2011_new <- Change_2011[,-which(names(Change_2011) %in% 
                                          c("SCHOOL.NAME","DISTrictNUMBER","DISTRICT.NAME","EMH.Combined"))]
Total_2011 <- merge(FinalGrade_2011,Change_2011_new,
                    by.x=c("SCHOOLNUMBER","EMH"),by.y=c("SCHOOL.NUMBER","EMH"),
                    all.x = TRUE)

#2. Combine total and Coact
h = dim(Coact_2011)[1]
EMH = rep("H",h)
Coact_2011_new <- cbind(Coact_2011,EMH)
Coact_2011_new <- Coact_2011_new[,-which(names(Coact_2011_new) %in% 
                                            c("District.No","District.Name","School.Name"))]
Total_2011 <- merge(Total_2011,Coact_2011_new,
                    by.x=c("SCHOOLNUMBER","EMH"),by.y=c("School.No","EMH"),
                    all.x = TRUE)

#3. Combine total and EnrlWorking
Enrlworking_2011_new <- Enrlworking_2011[,-which(names(Enrlworking_2011) %in%
                                                    c("Org..Code","Organization.Name","School.Name"))]
Total_2011 <- merge(Total_2011,Enrlworking_2011_new,
                    by.x="SCHOOLNUMBER",by.y="School.Code",
                    all.x = TRUE)

#4. Combine total and FRL
FRL_2011_new = FRL_2011[,-which(names(FRL_2011) %in% 
                                   c("DISTRICT.CODE","DISTRICT.NAME","SCHOOL.NAME"))]
Total_2011 <- merge(Total_2011,FRL_2011_new,
                    by.x="SCHOOLNUMBER",by.y="SCHOOL.CODE",
                    all.x = TRUE)

#5. Combine Remediation
h = dim(Remediation_2011)[1]
EMH = rep("H",h)
Remediation_2011_new <- cbind(Remediation_2011,EMH)
Remediation_2011_new <- Remediation_2011_new[,-which(names(Remediation_2011_new) %in%
                                                        c("School_District","SchoolName"))]
Total_2011 <- merge(Total_2011,Remediation_2011_new,
                    by.x=c("SCHOOLNUMBER","EMH"),by.y=c("SchoolNumber","EMH"),
                    all.x = TRUE)

Total_2011_Names <- colnames(Total_2011)

write.table(Total_2011_Names,file="./CleanedData/Total_2011_Names.csv", row.names=FALSE,col.names=FALSE,sep=",")

#-------------Clean the data-------------------#
#Get rid of unnecessary data
Total_2011_Names[7:14]
Data_2011 <- Total_2011[,-(7:14)]
#To match format of Data_2010
Data_2011 <- as.data.frame(append(Data_2011, list(AlternativeSchool = 0), after = 6), stringsAsFactors = FALSE)
changeName <- read.csv(file="./Raw_Data_Names_2011.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)

#Clean up the column names
require(plyr)
colnames(Data_2011) <- mapvalues(colnames(Data_2011), from = changeName[,1],to=changeName[,2])

#Make the decimal values into percentage values
Data_2011[,28:34] <- Data_2011[,28:34]  * 100 
Data_2011[,35] <- as.numeric(sub("%", "", Data_2011[,35]),na.rm = TRUE)
Data_2011[,7] <- as.integer(Data_2011[,7])
Data_2011[,19] <- as.numeric(Data_2011[,19])

Data_2011_Names <- colnames(Data_2011)
Data_2011_Type = sapply(Data_2011, class) 

write.table(Data_2011_Names,file="./CleanedData/Data_2011_Names.csv", row.names=FALSE,col.names=FALSE,sep=",")
write.table(Data_2011,file="./CleanedData/Data_2011.csv", row.names=FALSE,col.names=FALSE,sep=",")
write.table(Data_2011_Type,file="./CleanedData/Data_2011_Type.csv", row.names=FALSE,col.names=FALSE,sep=",")



