##########################################
### CAT BEHAVIOUR PROJECT              ###
### ActiGraph validation behaviour ID  ###
### RATER RELIABILITY                  ###
### MICHELLE SMIT                      ###
##########################################
# rm(list=ls())


### READ R LIBRARIES
library(plyr);library(dplyr);library(tidyverse);library(lubridate); library(rgdal);library(reshape2);library(readxl);

########################################################################################################### PREPARATION ANNOTATED DATA RATING 1
####################################################
### STEP 1 - PREPARATION ANNOTATED DATA RATING 1 ###
####################################################
# Required input: - Behaviour scoring data output for each cat - xlsx file
start_path <- "C:/Users/20017948/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/"
setwd(paste0(start_path,"annotated_data"))
anno <- list.files(getwd(),pattern="*.xlsx")

### CREATE A LIST OF DATAFRAMES AND RENAME THEM
lanno <- lapply(setNames(anno, make.names(gsub("ch0", "pen",gsub("*.xlsx$", "", anno)))), read_excel)
length(lanno);length(anno)

### COMBINE ALL THE FILES IN THE LIST INTO ONE DATAFRAME AND ASSSIGN TO AN OBJECT CALLED LONGANNO
longanno<-ldply(lanno, rbind)
head(longanno)
str(longanno)

### SET PROPER DATE TO TIMESTAMP
df <- colsplit(string = longanno$Time,pattern = " ",names = c("Date","Time"))
head(df)
longanno <- longanno %>% select(-Time)
longanno <- cbind(df, longanno)
longanno <- longanno %>% select(-Date)
head(longanno)

df <- longanno %>% mutate(Time = paste("2021/06/30",longanno$Time, sep=" "))
str(df)
longanno <- df

### RENAME THE '.ID' COLUMN TO 'CAT_ID' AND 'TIME' TO 'TIMESTAMP'
names(longanno)[names(longanno)==".id"]<-"Cat_id"
names(longanno)[names(longanno)=="Time"]<-"Timestamp"

### SEPERATE PEN FROM CAT_ID
longanno <- longanno %>% separate(Cat_id, c("Pen", "Cat_id"))
head(longanno)
str(longanno)

### REMOVE 'ABUNDANT TEXT FROM SOME VARIABLES
df <- longanno
df$Cat_id <- gsub(".20210630", "", df$Cat_id)
df$Pen <- gsub("pen", "", df$Pen)
str(df)
longanno <- df

### CHANGE VARIABLE TYPES
# Change Cat_id to a factor variable
longanno$Cat_id <- as.factor(longanno$Cat_id)
# Change Timestamp to a POSIXct factor
longanno$Timestamp<-as.POSIXct(longanno$Timestamp,format="%Y/%m/%d %H:%M:%S")
str(longanno)

### REMOVE NA VALUES
sum(is.na(longanno$Active_Climbing)); sum(is.na(longanno$Inactive_Lying.Crouch)); sum(is.na(longanno$Maintenance_Littering.Digging))
df <- longanno[!is.na(longanno$Other_ActigraphOff),]
str(df)
# Check
df1 <- na.omit(longanno) # OR
df2 <- longanno[rowSums(is.na(longanno))==0,]
# All dataframes arrive at the same number of obs, so we are confident all NA's are removed
Rater1 <- df

### CHANGE FROM WIDE FORMAT TO LONG FORMAT
Rater1 <- Rater1 %>% gather(key=Rater1, value=Status, Other_ActigraphOff:Active_Walking)

### CHANGE VARIABLE TYPES
# Change Cat_id to a factor variable
Rater1$Cat_id <- as.factor(Rater1$Cat_id)
# Change Behaviour to a factor variable
Rater1$Rater1 <- as.factor(Rater1$Rater1)

### REMOVE EVERYTHING WITH STATUS=0
df <- Rater1[grep(pattern="0", x=Rater1$Status, invert = TRUE),]
Rater1 <- df

### REMOVE ABUNDANT COLUMNS
Rater1 <- Rater1 %>% select(-Pen)
Rater1 <- Rater1 %>% select(-Status)

### SAVE ANNOTATED DATAFRAME
start_path <- "C:/Users/20017948/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/Rater_reliability/"
setwd(paste0(start_path,"Processed_data"))
save(Rater1, file="Rater1.RDATA")


########################################################################################################### PREPARATION ANNOTATED DATA RATING 2
####################################################
### STEP 2 - PREPARATION ANNOTATED DATA RATING 2 ###
####################################################
# Required input: - Behaviour scoring data output for each cat - xlsx file
start_path <- "C:/Users/20017948/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/Rater_reliability/"
setwd(paste0(start_path,"Annotated_data"))
anno <- list.files(getwd(),pattern="*.xlsx")

### CREATE A LIST OF DATAFRAMES AND RENAME THEM
lanno <- lapply(setNames(anno, make.names(gsub("ch0", "pen",gsub("*.xlsx$", "", anno)))), read_excel)
length(lanno);length(anno)

### COMBINE ALL THE FILES IN THE LIST INTO ONE DATAFRAME AND ASSSIGN TO AN OBJECT CALLED LONGANNO
longanno<-ldply(lanno, rbind)
head(longanno)
str(longanno)

### CONVERT LOGICAL COLUMNS TO NUMERIC
df <- sapply(longanno, is.logical)
longanno[,df] <- lapply(longanno[,df], as.numeric)

### SET PROPER DATE TO TIMESTAMP
df <- colsplit(string = longanno$Time,pattern = " ",names = c("Date","Time"))
head(df)
longanno <- longanno %>% select(-Time)
longanno <- cbind(df, longanno)
longanno <- longanno %>% select(-Date)
head(longanno)

df <- longanno %>% mutate(Time = paste("2021/06/30",longanno$Time, sep=" "))
str(df)
longanno <- df

### RENAME THE '.ID' COLUMN TO 'CAT_ID' AND 'TIME' TO 'TIMESTAMP'
names(longanno)[names(longanno)==".id"]<-"Cat_id"
names(longanno)[names(longanno)=="Time"]<-"Timestamp"

### SEPERATE PEN FROM CAT_ID
longanno <- longanno %>% separate(Cat_id, c("Pen", "Cat_id"))
head(longanno)
str(longanno)

### REMOVE 'ABUNDANT TEXT FROM SOME VARIABLES
df <- longanno
df$Pen <- gsub("pen", "", df$Pen)
str(df)
longanno <- df

### CHANGE VARIABLE TYPES
# Change Cat_id to a factor variable
longanno$Cat_id <- as.factor(longanno$Cat_id)
# Change Timestamp to a POSIXct factor
longanno$Timestamp<-as.POSIXct(longanno$Timestamp,format="%Y/%m/%d %H:%M:%S")
str(longanno)

### REMOVE NA VALUES
sum(is.na(longanno$Active_Climbing)); sum(is.na(longanno$Inactive_Lying.Crouch)); sum(is.na(longanno$Maintenance_Littering.Digging))
df <- longanno[!is.na(longanno$Other_ActigraphOff),]
str(df)
# Check
df1 <- na.omit(longanno) # OR
df2 <- longanno[rowSums(is.na(longanno))==0,]
# All dataframes arrive at the same number of obs, so we are confident all NA's are removed
Rater2 <- df

### CHANGE FROM WIDE FORMAT TO LONG FORMAT
Rater2 <- Rater2 %>% gather(key=Rater2, value=Status, Other_ActigraphOff:Active_Walking)

### CHANGE VARIABLE TYPES
# Change Cat_id to a factor variable
Rater2$Cat_id <- as.factor(Rater2$Cat_id)
# Change Behaviour to a factor variable
Rater2$Rater2 <- as.factor(Rater2$Rater2)

### REMOVE EVERYTHING WITH STATUS=0
df <- Rater2[grep(pattern="0", x=Rater2$Status, invert = TRUE),]
Rater2 <- df

### REMOVE ABUNDANT COLUMNS
Rater2 <- Rater2 %>% select(-Pen)
Rater2 <- Rater2 %>% select(-Status)

### SAVE ANNOTATED DATAFRAME
start_path <- "C:/Users/20017948/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/Rater_reliability/"
setwd(paste0(start_path,"Processed_data"))
save(Rater2, file="Rater2.RDATA")


###################################################################################################################### MERGE ANNOTATED DATASETS
#########################################################
### STEP 3 - MERGE ANNOTATED DATA RATER 1 AND RATER 2 ###
#########################################################
# Required input:   - Meta data
#                   - Annotated (scored) behaviour data

start_path <- "C:/Users/20017948/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/Rater_reliability/"
setwd(paste0(start_path,"Processed_data"))
load("Rater1.RDATA")
load("Rater2.RDATA")

### MERGE ANNOMETA AND ACCLMETA
Rater.Rel <- Rater1 %>% left_join(Rater2, by = c('Cat_id', 'Timestamp'))
str(Rater.Rel)

### REMOVE ROWS WITH NA'S
sum(is.na(Rater.Rel$Rater2))
df <- Rater.Rel[!is.na(Rater.Rel$Rater2),]
str(df)
Rater.Rel <- df

### SAVE COMPLETE DATAFRAME
save(Rater.Rel, file= "Rater_reliability.RDATA")

################################################################################################################# CHANGE BEHAVIOUR CATETEGORIES
############################################
### STEP 4 - CHANGE BEHAVIOUR CATEGORIES ###
############################################
levels(Rater.Rel$Rater1)
levels(Rater.Rel$Rater2)

levels(Rater.Rel$Rater1) <- c("Climbing","Jumping","Jumping","Playfight","Playfight","Rolling","Rubbing","Running","Trotting","Walking",
                              "Lying","Lying","Lying","Lying","Sitting","Sitting","Sitting","Standing","Standing","Grooming","Littering",
                              "Digging","Littering","Littering","Drinking","Eating","Scratching","Shake","Shake","ActigraphOff","Other",
                              "OutOfSight","Allogrooming","Human","Start")
levels(Rater.Rel$Rater2) <- c("Climbing","Jumping","Jumping","Playfight","Playfight","Rolling","Rubbing","Running","Trotting","Walking",
                              "Lying","Lying","Lying","Lying","Sitting","Sitting","Sitting","Standing","Standing","Grooming","Littering",
                              "Digging","Littering","Littering","Drinking","Eating","Scratching","Shake","Shake","ActigraphOff","Other",
                              "OutOfSight","Allogrooming","Human","Start")

levels(Rater.Rel$Rater1)
levels(Rater.Rel$Rater2)

### SAVE COMPLETE DATAFRAME
save(Rater.Rel, file= "Rater_reliability.RDATA")


############################################################################################################################# RATER RELIABILITY
##################################
### STEP 5 - RATER RELIABILITY ###
##################################
library(irr)

start_path <- "C:/Users/20017948/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/Rater_reliability/"
setwd(paste0(start_path,"Processed_data"))
load("Rater_reliability.RDATA")

kappa2(Rater.Rel[,c(3,4)])

