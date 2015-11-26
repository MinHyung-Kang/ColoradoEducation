#Load the data
Change_2012 <- read.csv(file="./Data/2012_1YR_3YR_change.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
Coact_2012 <- read.csv(file="./Data/2012_COACT.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
DataMap_2012 <- read.csv(file="./Data/2012_data_map.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
Enrlworking_2012 <- read.csv(file="./Data/2012_enrl_working.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
FinalGrade_2012 <- read.csv(file="./Data/2012_final_grade.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
FRL_2012 <- read.csv(file="./Data/2012_k_12_FRL.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
Remediation_2012 <- read.csv(file="./Data/2012_remediation_HS.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
Address_2012 <- read.csv(file="./Data/2012_school_address.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)


# Change_2012 : 1 & 3 year changes in academic acheivement, growth, overall grade.
# Coact_2012 : Ready for college or not based on ACT (Yes=1,No=2)
# DataMap_2012 : 
# Enrlworking_2012 : Percentage of each ethnicity/race characteristics
# FinalGrade_2012 : Grades given for each school
# FRL_2012 : Percentage of students getting free/reduced price lunch
# Remediation_2012 : Percentage of students requiring remediation
# Address_2012 : Physical address of the schools


#------------------Combine all the data for this year----------------------------#
#Order : FinalGrade - Change - Coact - Enrlworking - FRL- remediation

#1. Combine FinalGrade and Change -> total
Change_2012_new <- Change_2012[,-which(names(Change_2012) %in% 
                        c("Record_no","SPF_DIST_NUMBER","SPF_DISTRICT_NAME","SPF_SCHOOL_NAME","SPF_INCLUDED_EMH_FOR_A"))]
Total_2012 <- merge(FinalGrade_2012,Change_2012_new,
                    by.x=c("School.Code","EMH"),by.y=c("SPF_SCHOOL_NUMBER","SPF_EMH_CODE"),
                    all.x = TRUE)

#2. Combine total and Coact
h = dim(Coact_2012)[1]
EMH = rep("H",h)
Coact_2012_new <- Coact_2012[,0:8]
Coact_2012_new <- cbind(Coact_2012_new,EMH)
Coact_2012_new <- Coact_2012_new[,-which(names(Coact_2012_new) %in% 
                                            c("District.No","District.Name","School.Name"))]
Total_2012 <- merge(Total_2012,Coact_2012_new,
                    by.x=c("School.Code","EMH"),by.y=c("School.No","EMH"),
                    all.x = TRUE)

#3. Combine total and EnrlWorking
Enrlworking_2012_new <- Enrlworking_2012[,0:12]
Enrlworking_2012_new <- Enrlworking_2012_new[,-which(names(Enrlworking_2012_new) %in%
                           c("Organization.Code","Organization.Name","School.Name"))]
Total_2012 <- merge(Total_2012,Enrlworking_2012_new,
                    by.x="School.Code",by.y="School.Code",
                    all.x = TRUE)

#4. Combine total and FRL
FRL_2012_new <- FRL_2012[,0:5]
FRL_2012_new = FRL_2012_new[,-which(names(FRL_2012_new) %in% 
                                   c("DISTRICT.CODE","DISTRICT.NAME","SCHOOL.NAME"))]
Total_2012 <- merge(Total_2012,FRL_2012_new,
                    by.x="School.Code",by.y="SCHOOL.CODE",
                    all.x = TRUE)

#5. Combine Remediation
Remediation_2012_new <- Remediation_2012[,0:5]
h = dim(Remediation_2012_new)[1]
EMH = rep("H",h)
Remediation_2012_new <- cbind(Remediation_2012_new,EMH)
Remediation_2012_new <- Remediation_2012_new[,-which(names(Remediation_2012_new) %in%
                                                        c("School_Districte","School_Name"))]
Total_2012 <- merge(Total_2012,Remediation_2012_new,
                    by.x=c("School.Code","EMH"),by.y=c("Schoolnumber","EMH"),
                    all.x = TRUE)

h = dim(Total_2012)[1]
Dummy <- matrix(-1,h,8)
DummyNames <- rep("Dummy",8)
colnames(Dummy) <- DummyNames
Total_2012 <- cbind(Total_2012,Dummy)
Total_2012_Names <- colnames(Total_2012)

#--------FIGURE OUT INDEX-----------#

Index_2012 <- read.csv(file="./CleanedData/Index_2012.csv", header=FALSE, sep=",", stringsAsFactors = FALSE)
Total_2012 <- Total_2012[,unlist(Index_2012)] 
Total_2012_Names <- colnames(Total_2012)
write.table(Total_2012_Names,file="./CleanedData/Total_2012_Names.csv", row.names=FALSE,col.names=FALSE,sep=",")



#-------------Clean the data-------------------#
#Get rid of unnecessary data

#Get rid of newly added variables
Data_2012 <- Total_2012[,-(44:48)]
#get rid of dummy variables
Data_2012 <- Data_2012[,-(7:14)]

#To match format of Data_2010

changeName <- read.csv(file="./CleanedData/Raw_Data_Names_2012.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)

#Clean up the column names
require(plyr)
colnames(Data_2012) <- mapvalues(colnames(Data_2012), from = changeName[,1],to=changeName[,2])

Data_2012 <- as.data.frame(append(Data_2012, list(AlternativeSchool = 0), after = 6), stringsAsFactors = FALSE)

#Make the decimal values into percentage values
Data_2012[,7] <- as.integer(Data_2012[,7])
Data_2012[,19] <- as.numeric(Data_2012[,19])
Data_2012[,20] <- as.integer(Data_2012[,20])
Data_2012[,21] <- as.integer(Data_2012[,21])
Data_2012[,22] <- as.integer(Data_2012[,22])
Data_2012[,33] <- as.numeric(Data_2012[,33])
Data_2012[,34] <- as.numeric(Data_2012[,34])
Data_2012[,28:34] <- Data_2012[,28:34]  * 100 
Data_2012[,35] <- as.numeric(sub("%", "", Data_2012[,35]),na.rm = TRUE)
Data_2012[,36] <- as.numeric(sub("%", "", Data_2012[,36]),na.rm = TRUE)
Data_2012[,36] <- Data_2012[,36]/100.0



Data_2012_Names <- colnames(Data_2012)
Data_2012_Type = sapply(Data_2012, class) 

write.table(Data_2012_Names,file="./CleanedData/Data_2012_Names.csv", row.names=FALSE,col.names=FALSE,sep=",")
write.table(Data_2012,file="./CleanedData/Data_2012.csv", row.names=FALSE,col.names=FALSE,sep=",")
write.table(Data_2012_Type,file="./CleanedData/Data_2012_Type.csv", row.names=FALSE,col.names=FALSE,sep=",")



