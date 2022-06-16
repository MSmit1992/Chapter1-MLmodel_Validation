##########################################
### CAT BEHAVIOUR PROJECT              ###
### ActiGraph validation behaviour ID  ###
### Feature extraction data - 1 second ###
### MICHELLE SMIT                      ###
##########################################
#rm(list=ls())

### READ R LIBRARIES
library(plyr);library(tidyverse);library(lubridate); library(rgdal);library(reshape2);library(readxl);library(moments)
library(parallel);library(foreach);library(doParallel)
library(zoo); library(data.table)

numCores <- detectCores()
numCores
registerDoParallel(8)

#################################################################################################################### LOAD DATASETS IN
#########################
### LOAD DATASETS IN  ###
#########################

# Required input: - Acceleration data from collar and halter mounted accelerometers - csv file(s)
start_path <- "/Volumes/One Touch/PhD_Massey_Study1/Study1_072021_DataAnalysisR/Data/behaviour_data/raw_data/"
setwd(paste0(start_path,"Raw"))

### LOAD DATAFRAMES EXCLUDING FIRST 10 ROWS AND EXPORT AS CSV
Cho_collar <- read.csv("20210630-20210706-Cho-CollarRAW.csv", skip=10, header=TRUE)
write.csv(Cho_collar, "/Volumes/One Touch/PhD_Massey_Study1/Study1_072021_DataAnalysisR/Data/behaviour_data/raw_data/FE1secRaw/20210630-20210706-Cho-Collar.csv",row.names=FALSE)
Cho_harness <- read.csv("20210630-20210706-Cho-CollarRAW.csv", skip=10, header=TRUE)
write.csv(Cho_collar, "/Volumes/One Touch/PhD_Massey_Study1/Study1_072021_DataAnalysisR/Data/behaviour_data/raw_data/FE1secRaw/20210630-20210706-Cho-Harness.csv",row.names=FALSE)
rm(list=ls())

George_collar <- read.csv("20210630-20210706-George-CollarRAW.csv", skip=10, header=TRUE)
write.csv(George_collar, "/Volumes/One Touch/PhD_Massey_Study1/Study1_072021_DataAnalysisR/Data/behaviour_data/raw_data/FE1secRaw/20210630-20210706-George-Collar.csv",row.names=FALSE)
George_harness <- read.csv("20210630-20210706-George-CollarRAW.csv", skip=10, header=TRUE)
write.csv(George_collar, "/Volumes/One Touch/PhD_Massey_Study1/Study1_072021_DataAnalysisR/Data/behaviour_data/raw_data/FE1secRaw/20210630-20210706-George-Harness.csv",row.names=FALSE)
rm(list=ls())

Hagrid_collar <- read.csv("20210630-20210706-Hagrid-CollarRAW.csv", skip=10, header=TRUE)
write.csv(Hagrid_collar, "/Volumes/One Touch/PhD_Massey_Study1/Study1_072021_DataAnalysisR/Data/behaviour_data/raw_data/FE1secRaw/20210630-20210706-Hagrid-Collar.csv",row.names=FALSE)
Hagrid_harness <- read.csv("20210630-20210706-Hagrid-HarnessRAW.csv", skip=10, header=TRUE)
write.csv(Hagrid_harness, "/Volumes/One Touch/PhD_Massey_Study1/Study1_072021_DataAnalysisR/Data/behaviour_data/raw_data/FE1secRaw/20210630-20210706-Hagrid-Harness.csv",row.names=FALSE)
rm(list=ls())

Harry_collar <- read.csv("20210630-20210706-Harry-CollarRAW.csv", skip=10, header=TRUE)
write.csv(Harry_collar, "/Volumes/One Touch/PhD_Massey_Study1/Study1_072021_DataAnalysisR/Data/behaviour_data/raw_data/FE1secRaw/20210630-20210706-Harry-Collar.csv",row.names=FALSE)
Harry_harness <- read.csv("20210630-20210706-Harry-HarnessRAW.csv", skip=10, header=TRUE)
write.csv(Harry_harness, "/Volumes/One Touch/PhD_Massey_Study1/Study1_072021_DataAnalysisR/Data/behaviour_data/raw_data/FE1secRaw/20210630-20210706-Harry-Harness.csv",row.names=FALSE)
rm(list=ls())

Hermione_collar <- read.csv("20210630-20210706-Hermione-CollarRAW.csv", skip=10, header=TRUE)
write.csv(Hermione_collar, "/Volumes/One Touch/PhD_Massey_Study1/Study1_072021_DataAnalysisR/Data/behaviour_data/raw_data/FE1secRaw/20210630-20210706-Hermione-Collar.csv",row.names=FALSE)
Hermione_harness <- read.csv("20210630-20210706-Hermione-HarnessRAW.csv", skip=10, header=TRUE)
write.csv(Hermione_harness, "/Volumes/One Touch/PhD_Massey_Study1/Study1_072021_DataAnalysisR/Data/behaviour_data/raw_data/FE1secRaw/20210630-20210706-Hermione-Harness.csv",row.names=FALSE)
rm(list=ls())

JellyB_collar <- read.csv("20210630-20210706-JellyB-CollarRAW.csv", skip=10, header=TRUE)
write.csv(JellyB_collar, "/Volumes/One Touch/PhD_Massey_Study1/Study1_072021_DataAnalysisR/Data/behaviour_data/raw_data/FE1secRaw/20210630-20210706-JellyB-Collar.csv",row.names=FALSE)
JellyB_harness <- read.csv("20210630-20210706-JellyB-HarnessRAW.csv", skip=10, header=TRUE)
write.csv(JellyB_harness, "/Volumes/One Touch/PhD_Massey_Study1/Study1_072021_DataAnalysisR/Data/behaviour_data/raw_data/FE1secRaw/20210630-20210706-JellyB-Harness.csv",row.names=FALSE)
rm(list=ls())

Jube_collar <- read.csv("20210630-20210706-Jube-CollarRAW.csv", skip=10, header=TRUE)
write.csv(Jube_collar, "/Volumes/One Touch/PhD_Massey_Study1/Study1_072021_DataAnalysisR/Data/behaviour_data/raw_data/FE1secRaw/20210630-20210706-Jube-Collar.csv",row.names=FALSE)
Jube_harness <- read.csv("20210630-20210706-Jube-CollarRAW.csv", skip=10, header=TRUE)
write.csv(Jube_harness, "/Volumes/One Touch/PhD_Massey_Study1/Study1_072021_DataAnalysisR/Data/behaviour_data/raw_data/FE1secRaw/20210630-20210706-Jube-Harness.csv",row.names=FALSE)
rm(list=ls())

Merry_collar <- read.csv("20210630-20210706-Merry-CollarRAW.csv", skip=10, header=TRUE)
write.csv(Merry_collar, "/Volumes/One Touch/PhD_Massey_Study1/Study1_072021_DataAnalysisR/Data/behaviour_data/raw_data/FE1secRaw/20210630-20210706-Merry-Collar.csv",row.names=FALSE)
Merry_harness <- read.csv("20210630-20210706-Merry-HarnessRAW.csv", skip=10, header=TRUE)
write.csv(Merry_harness, "/Volumes/One Touch/PhD_Massey_Study1/Study1_072021_DataAnalysisR/Data/behaviour_data/raw_data/FE1secRaw/20210630-20210706-Merry-Harness.csv",row.names=FALSE)
rm(list=ls())

MrsNorris_collar <- read.csv("20210630-20210706-MrsNorris-CollarRAW.csv", skip=10, header=TRUE)
write.csv(MrsNorris_collar, "/Volumes/One Touch/PhD_Massey_Study1/Study1_072021_DataAnalysisR/Data/behaviour_data/raw_data/FE1secRaw/20210630-20210706-MrsNorris-Collar.csv",row.names=FALSE)
MrsNorris_harness <- read.csv("20210630-20210706-MrsNorris-HarnessRAW.csv", skip=10, header=TRUE)
write.csv(MrsNorris_harness, "/Volumes/One Touch/PhD_Massey_Study1/Study1_072021_DataAnalysisR/Data/behaviour_data/raw_data/FE1secRaw/20210630-20210706-MrsNorris-Harness.csv",row.names=FALSE)
rm(list=ls())

Neville_collar <- read.csv("20210630-20210706-Neville-CollarRAW.csv", skip=10, header=TRUE)
write.csv(Neville_collar, "/Volumes/One Touch/PhD_Massey_Study1/Study1_072021_DataAnalysisR/Data/behaviour_data/raw_data/FE1secRaw/20210630-20210706-Neville-Collar.csv",row.names=FALSE)
Neville_harness <- read.csv("20210630-20210706-Neville-HarnessRAW.csv", skip=10, header=TRUE)
write.csv(Neville_harness, "/Volumes/One Touch/PhD_Massey_Study1/Study1_072021_DataAnalysisR/Data/behaviour_data/raw_data/FE1secRaw/20210630-20210706-Neville-Harness.csv",row.names=FALSE)
rm(list=ls())

Nimbus_collar <- read.csv("20210630-20210706-Nimbus-CollarRAW.csv", skip=10, header=TRUE)
write.csv(Nimbus_collar, "/Volumes/One Touch/PhD_Massey_Study1/Study1_072021_DataAnalysisR/Data/behaviour_data/raw_data/FE1secRaw/20210630-20210706-Nimbus-Collar.csv",row.names=FALSE)
Nimbus_harness <- read.csv("20210630-20210706-Nimbus-HarnessRAW.csv", skip=10, header=TRUE)
write.csv(Nimbus_harness, "/Volumes/One Touch/PhD_Massey_Study1/Study1_072021_DataAnalysisR/Data/behaviour_data/raw_data/FE1secRaw/20210630-20210706-Nimbus-Harness.csv",row.names=FALSE)
rm(list=ls())

Scabbers_collar <- read.csv("20210630-20210706-Scabbers-CollarRAW.csv", skip=10, header=TRUE)
write.csv(Scabbers_collar, "/Volumes/One Touch/PhD_Massey_Study1/Study1_072021_DataAnalysisR/Data/behaviour_data/raw_data/FE1secRaw/20210630-20210706-Scabbers-Collar.csv",row.names=FALSE)
Scabbers_harness <- read.csv("20210630-20210706-Scabbers-HarnessRAW.csv", skip=10, header=TRUE)
write.csv(Scabbers_harness, "/Volumes/One Touch/PhD_Massey_Study1/Study1_072021_DataAnalysisR/Data/behaviour_data/raw_data/FE1secRaw/20210630-20210706-Scabbers-Harness.csv",row.names=FALSE)
rm(list=ls())

################################################################################################################ CALCULATE PREDICTORS
#############################
### CALCULATE PREDICTORS  ###
#############################
# Cho, George, Hagrid, Harry, Hermione, JellyB, Jube, Merry, MrsNorris, Neville, Nimbus, Scabbers
# Cho, George, Hagrid, Harry, Hermione, JellyB, Jube, Merry, MrsNorris, Neville, Nimbus, Scabbers
start_path <- "/Volumes/One Touch/PhD_Massey_Study1/Study1_072021_DataAnalysisR/Data/behaviour_data/raw_data/"
setwd(paste0(start_path,"Raw"))
# Collar
Cho_collar <- read.csv("20210630-20210706-Cho-Collar.csv", header=TRUE)
names(Cho_collar) <- c('Timestamp','X','Y','Z')
df1 <- Cho_collar
# Harness
Cho_harness <- read.csv("20210630-20210706-Cho-Harness.csv", header=TRUE)
names(Cho_harness) <- c('Timestamp','X','Y','Z')
df1 <- Cho_harness


### CALCULATE VECTOR MAGNITUDE (VM) = SQRT (X2+Y2+Z2)
df1$VM <- sqrt(df1$X^2+df1$Y^2+df1$Z^2)

### CALCULATE MEAN, MIN, MAX, SUM, SD, SKEWNESS, KURTOSIS FOR X,Y,Z,VM AND CORRELATION FOR XYZ
df2 <- df1 %>%
  group_by(sec = floor_date(dmy_hms(Timestamp), "second")) %>% 
  summarise(Timestamp = first(Timestamp),
            across(-Timestamp, 
                   list(Mean = mean, Min = min, Max = max, Sum=sum, sd=sd, Skew=skewness, Kurt=kurtosis),
                   .names = "{.col}_{.fn}"),Cor_XY=cor(X,Y), Cor_XZ=cor(X,Z), Cor_YZ=cor(Y,Z))

# Warning messages: NaN and NA, replace these with 0
is.nan.data.frame <- function(x)
do.call(cbind, lapply(x, is.nan))
df2[is.nan.data.frame(df2)] <-0
df2[is.na(df2)] <-0
# Remove Timestamp variable and rename sec to Timestamp
df2 <- select(df2, -"Timestamp") %>% rename("Timestamp"="sec")

### CALCULATE DYNAMIC BODY ACCELERATION (DBA), OVERALL DYNAMIC BODY ACCELERATION (ODBA),VECTOR MAGNITUDE (VM)
# DBA = sum axis - running mean axis (running mean = static acceleration)
# ODBA = SUM |X_DBA| + |Y_DBA| + |Z_DBA|

# Calculate running mean for each axis using sum value of each axis
# Calculate rolling average
df2 <- df2 %>% mutate(X_rm=rollmean(X_Sum, 2, na.pad=TRUE, align="right")) %>%
  mutate(Y_rm=rollmean(Y_Sum, 2, na.pad=TRUE, align="right")) %>%
  mutate(Z_rm=rollmean(Z_Sum, 2, na.pad=TRUE, align="right"))
# Calculate DBA and remove rolling average
df2 <- df2 %>% mutate(X_DBA=X_Sum-X_rm) %>%
  mutate(Y_DBA=Y_Sum-Y_rm) %>%
  mutate(Z_DBA=Z_Sum-Z_rm) %>%
  select(-"X_rm",-"Y_rm",-"Z_rm")
# Calculate ODBA
df2 <- df2 %>% mutate(ODBA=abs(X_DBA)+abs(Y_DBA)+abs(Z_DBA))
  
########################################################################################################################### SAVE FILE
#################
### SAVE FILE ###
#################
# Collar - change cat name!
fwrite(df2, "/Volumes/One Touch/PhD_Massey_Study1/Study1_072021_DataAnalysisR/Data/behaviour_data/raw_data/FE1secRaw/20210630-20210706-Scabbers-Cho.csv")
rm(df1,df2,Cho_collar)
# Harness - change cat name!
fwrite(df2, "/Volumes/One Touch/PhD_Massey_Study1/Study1_072021_DataAnalysisR/Data/behaviour_data/raw_data/FE1secRaw/20210630-20210706-Scabbers-Cho.csv")
rm(df1,df2,Cho_harness)
