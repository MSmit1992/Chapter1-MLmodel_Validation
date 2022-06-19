##########################################
### CAT BEHAVIOUR PROJECT              ###
### ActiGraph validation behaviour ID  ###
### Merge data                         ###
### MICHELLE SMIT                      ###
##########################################

### READ R LIBRARIES
library(plyr);library(tidyverse);library(lubridate); library(rgdal);library(reshape2);library(readxl);

######################################################################################################### PREPARATION ANNOTATED DATA
###########################################
### STEP 1 - PREPARATION ANNOTATED DATA ###
###########################################
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
longanno <- df

### SAVE ANNOTATED DATAFRAME
start_path <- "C:/Users/20017948/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/"
setwd(paste0(start_path,"processed_data"))
save(longanno, file="anno_data.RDATA")


##################################################################################################### PREPARATION ACCELEROMETER DATA
###############################################
### STEP 2 - PREPARATION ACCELEROMETER DATA ###
###############################################
# Required input: - Acceleration data from collar and halter mounted accelerometers - csv file(s)

start_path <- "C:/Users/20017948/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/raw_data/"
setwd(paste0(start_path,"FE1secRaw"))
accl <- list.files(getwd(),pattern="*.csv")

### CREATE A LIST OF DATAFRAMES AND RENAME THEM
laccl <- lapply(setNames(accl, make.names(gsub(" .*","",gsub("(20210630)", "",gsub("(20210706)", "",gsub("*.csv$", "", accl)))))), read.csv)
length(accl);length(laccl)

### COMBINE ALL THE FILES IN THE LIST INTO ONE DATAFRAME AND ASSSIGN TO AN OBJECT CALLED LONGACCL
longaccl <- ldply(laccl, rbind)
head(longaccl)
str(longaccl)

df <- longaccl

### REMOVE ROWS CONTAINING NA
df <- df %>% drop_na()

### RENAME .ID TO CAT_ID
names(df)[names(df)==".id"] <- "Cat_id"

### REMOVE/CHANGE TEXT FROM SOME VARIABLES
df$Cat_id <- gsub("X..", "", df$Cat_id)

### CHANGE VARIABLE TYPES
# Change Cat_id to a factor variable
df$Cat_id <- as.factor(df$Cat_id)
# Change Timestamp to date/time variable
df$Timestamp <- lubridate:: ymd_hms(df$Timestamp)

### SEPERATE POSITION FROM CAT_ID
#remove position from cat_id, create new column for position
df <- df %>% separate(Cat_id, c("Cat_id", "Position"))
str(df)
head(df)
longaccl <- df

### SAVE ACCELERATION DATAFRAME
start_path <- "C:/Users/20017948/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/"
setwd(paste0(start_path,"processed_data"))
save(longaccl, file="accel_data.RDATA")


############################################################################################################## PREPARATION META DATA
#######################################
### STEP 3 -  PREPARATION META DATA ###
#######################################
# Required input:   - Meta data

start_path <- "/Volumes/One Touch/PhD_Massey_Study1/Study1_072021_DataAnalysisR/Data/behaviour_data/"
setwd(paste0(start_path,"meta_data"))
meta <- read.csv("Behaviour_Basicdata.csv")
head(meta)
str(meta)

df <- meta

### ADD DATE TO COLUMNS OB_START AND OB_END
df <- df %>%
  mutate(Ob_Start = paste("2021/06/30",df$Ob_Start, sep=" "),Ob_End = paste("2021/06/30",df$Ob_End, sep=" "))

### REMOVE PEN VARIABLE
df <- subset(df, select = -c(Pen))

### CHANGE VARIABLE TYPES
# Change Cat_id from chr to factor
df$Cat_id <- as.factor(df$Cat_id)
# Change Ob_Start and Ob_End from chr to POSIXct
df$Ob_Start<-as.POSIXct(df$Ob_Start,format="%Y/%m/%d %H:%M:%S")
df$Ob_End<-as.POSIXct(df$Ob_End,format="%Y/%m/%d %H:%M:%S")
str(df)

### SAVE META DATAFRAME
start_path <- "/Volumes/One Touch/PhD_Massey_Study1/Study1_072021_DataAnalysisR/Data/behaviour_data/"
setwd(paste0(start_path,"processed_data"))
meta <- df
save(meta, file="meta_data.RDATA")


##################################################################################################################### MERGE DATASETS
###################################################
### STEP 4 - MERGE META DATA AND ANNOTATED DATA ###
###################################################
# Required input:   - Meta data
#                   - Annotated (scored) behaviour data

start_path <- "C:/Users/20017948/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/"
setwd(paste0(start_path,"processed_data"))
load("meta_data.RDATA")
load("anno_data.RDATA")

### MERGE META DATA WITH ANNOTATED DATA
dfAnnoMeta <- longanno %>% left_join(meta, by = 'Cat_id')
head(dfAnnoMeta)
str(dfAnnoMeta)

### SELECT ANNOTATED DATA THAT FALL BETWEEN START AND END TIME
df <- dfAnnoMeta %>% group_by(Cat_id) %>%
  filter(Timestamp >= Ob_Start & Timestamp <= Ob_End)
head(df)
dim(df)
tail(df)
str(df)

### REMOVE ABUNDANT COLUMNS
names(df)
df <- df %>% select(-Ob_Start,-Ob_End)

### SAVE ANNOMETA DATAFRAME
dfAnnoMeta <- df
save(dfAnnoMeta, file= "MetaAnno_data.RDATA")


#######################################################
### STEP 5 - MERGE META DATA AND ACCELEROMETER DATA ###
#######################################################
# Required input:   - Meta data
#                   - Accelerometer data

start_path <- "C:/Users/20017948/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/"
setwd(paste0(start_path,"processed_data"))
load("meta_data.RDATA")
load("accel_data.RDATA")

### SET TIMEZONE TO UTC
tz(meta$Ob_Start)
tz(meta$Ob_End)
tz(longaccl$Timestamp)
tz(meta$Ob_Start) <- "UTC"
tz(meta$Ob_End) <- "UTC"

### MERGE META DATA WITH ACCELEROMETER DATA
dfAcclMeta <- longaccl %>% left_join(meta, by = 'Cat_id')
head(dfAcclMeta)
str(dfAcclMeta)

### SELECT ACCELERATION DATA THAT FALL BETWEEN START AND END TIME
df <- dfAcclMeta %>% group_by(Cat_id) %>%
  filter(Timestamp >= Ob_Start & Timestamp <= Ob_End)
head(df)
tail(df)
str(df)
dim(df)

### SAVE ACCLMETA DATAFRAME
dfAcclMeta <- df
save(dfAcclMeta, file= "MetaAccl_data.RDATA")


########################################################
### STEP 6 - MERGE META-ANNO DATA AND META-ACCL DATA ###
########################################################
# Required input:   - MetaAnno data
#                   - MetaAccl data

start_path <- "/Volumes/One Touch/PhD_Massey_Study1/Study1_072021_DataAnalysisR/Data/behaviour_data/"
setwd(paste0(start_path,"processed_data"))
load("MetaAnno_data.RDATA")
load("MetaAccl_data.RDATA")

### CHECK AND SET TIMEZONE TO UTC
tz(dfAcclMeta$Timestamp)
tz(dfAnnoMeta$Timestamp)
tz(dfAnnoMeta$Timestamp) <- "UTC"

### MERGE ANNOMETA AND ACCLMETA
dfCompl <- dfAcclMeta %>% left_join(dfAnnoMeta, by = c('Cat_id', 'Timestamp'))
str(dfCompl)

### SAVE COMPLETE DATAFRAME
save(dfCompl, file= "Compl_data.RDATA")

########################################################################################################## PREPARATION COMPLETE DATA
#######################################################
### STEP 7 - PREPARATION COMPLETE (MERGED) DATASET ###
#######################################################
# Required input: - Complete data

start_path <- "/Volumes/One Touch/PhD_Massey_Study1/Study1_072021_DataAnalysisR/Data/behaviour_data/"
setwd(paste0(start_path,"processed_data"))
load("Compl_data.RDATA")

# REMOVE ABUNDANT COLUMNS
dfCompl <- dfCompl %>% select(-Ob_Start,-Ob_End)

### CHANGE FROM WIDE FORMAT TO LONG FORMAT
dfCompl <- dfCompl %>% gather(key=Behaviour, value=Status, Other_ActigraphOff:Active_Walking)
str(dfCompl)
head(dfCompl)
table(dfCompl$Behaviour, dfCompl$Status)

### CHANGE VARIABLE TYPES
# Change Cat_id to a factor variable
dfCompl$Cat_id <- as.factor(dfCompl$Cat_id)
# Change Behaviour to a factor variable
dfCompl$Behaviour <- as.factor(dfCompl$Behaviour)
# Change Position to a factor variable
dfCompl$Position <- as.factor(dfCompl$Position)

### REMOVE EVERYTHING WITH STATUS=0
df <- dfCompl[grep(pattern="0", x=dfCompl$Status, invert = TRUE),]
dfCompl <- df

### SAVE COMPLETE DATAFRAME
save(dfCompl, file= "Compl_data.RDATA")

