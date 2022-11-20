##########################################
### CAT BEHAVIOUR PROJECT              ###
### ActiGraph validation behaviour ID  ###
### Feature engineering - 1 second     ###
##########################################
#rm(list=ls())

### READ R LIBRARIES
library(dplyr);library(tidyverse);library(lubridate);library(rgdal);library(reshape2);library(readxl);library(moments)
library(parallel);library(foreach);library(doParallel)
library(zoo); library(data.table)

numCores <- detectCores()
numCores
registerDoParallel(16)

##########################################
### FEATURE ENGINEERING                ###
##########################################
# Required input: - Acceleration data from collar and harness mounted accelerometers - csv file(s)
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/raw_data/"
setwd(paste0(start_path,"Raw"))

### COLLAR
filenames <- (Sys.glob("*Collar.csv"))

# Load Excel file(s)
for(ii in 1:length(filenames)){
  df1 <- read.csv(filenames[ii], skip=10, header=TRUE) # The raw accelerometer files contain rows that are not of interest. These first 10 rows need to be deleted
  
  # Change names
  names(df1) <- c('Timestamp','X','Y','Z')
  
  # Calculate rolling average (0.5, 1, 1.5, 2, 3 seconds = 15, 30, 45, 60, 90 datapoints with sampling frequency at 30 Hz)
  df1 <- df1 %>% 
    mutate(X_rm30=rollmean(X,30,na.pad=TRUE, align="center"))%>% mutate(Y_rm30=rollmean(Y,30,na.pad=TRUE, align="center"))%>% mutate(Z_rm30=rollmean(Z,30,na.pad=TRUE, align="center"))
  
  # Calculate DBA
  df1 <- df1 %>% mutate(X_DBA=X-X_rm) %>% mutate(Y_DBA=Y-Y_rm) %>% mutate(Z_DBA=Z-Z_rm)
  
  # Calculate ODBA
  df1 <- df1 %>% mutate(ODBA=abs(X_DBA)+abs(Y_DBA)+abs(Z_DBA))
  
  # Remove abundant columns
  df2 <- df1 %>% select(Timestamp,X,Y,Z,ODBA)
  
  # Remove df1 to clear out space
  rm(df1)
  
  # Calculate Vector Magnitude (VM)
  df2$VM <- sqrt(df2$X^2+df2$Y^2+df2$Z^2)
  
  #Calculate mean, min, max, sd, skewness, kurtosis for X,Y,Z,VM, and correlation for XYZ
  df2 <- df2 %>% group_by(sec= floor_date(dmy_hms(Timestamp), 'second')) %>%
    summarise(Timestamp = first(Timestamp),
              dplyr::across(-Timestamp,
                            list(Mean=mean, Min=min, Max=max, Sum=sum, sd=sd, Skew=skewness, Kurt=kurtosis),
                            .names="{.col}_{.fn}"), Cor_XY=cor(X,Y), Cor_XZ=cor(X,Z), Cor_YZ=cor(Y,Z))
  
  df2 <- select(df2, -"Timestamp") %>% rename("Timestamp"="sec")
  df2 <- df2 %>% select(Timestamp, X_Mean,X_Min,X_Max,X_Sum,X_sd,X_Skew,X_Kurt,Y_Mean, Y_Min,Y_Max,Y_Sum,Y_sd,Y_Skew,Y_Kurt,
                        Z_Mean,Z_Min,Z_Max,Z_Sum,Z_sd,Z_Skew,Z_Kurt, VM_Mean,VM_Min,VM_Max,VM_Sum,VM_sd,VM_Skew,VM_Kurt,
                        ODBA_Mean,Cor_XY,Cor_XZ,Cor_YZ)
  
  # Safe new excel files
  setwd(paste0(start_path,"FE")) # safe to another folder, so set working directory for this
  fwrite(df2,paste0(filenames[ii],'.csv'))
  rm(df2)
  setwd(paste0(start_path,"Raw")) # reset working directory to where original excel files are
  
}

rm(list=ls())

### HARNESS
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/raw_data/"
setwd(paste0(start_path,"Raw"))
filenames <- (Sys.glob("*Harness.csv"))

# Load Excel file(s)
for(ii in 1:length(filenames)){
  df1 <- read.csv(filenames[ii], skip=10, header=TRUE) # The raw accelerometer files contain rows that are not of interest. These first 10 rows need to be deleted
  
  # Change names
  names(df1) <- c('Timestamp','X','Y','Z')
  
  # Calculate rolling average (0.5, 1, 1.5, 2, 3 seconds = 15, 30, 45, 60, 90 datapoints with sampling frequency at 30 Hz)
  df1 <- df1 %>%
    mutate(X_rm30=rollmean(X,30,na.pad=TRUE, align="center"))%>% mutate(Y_rm30=rollmean(Y,30,na.pad=TRUE, align="center"))%>% mutate(Z_rm30=rollmean(Z,30,na.pad=TRUE, align="center"))
  
  # Calculate DBA
  df1 <- df1 %>% mutate(X_DBA30=X-X_rm30) %>% mutate(Y_DBA30=Y-Y_rm30) %>% mutate(Z_DBA30=Z-Z_rm30)
  
  # Calculate ODBA
  df1 <- df1 %>% mutate(ODBA30=abs(X_DBA30)+abs(Y_DBA30)+abs(Z_DBA30))
  
  # Remove abundant columns
  df2 <- df1 %>% select(Timestamp,X,Y,Z,ODBA)
  
  # Remove df1 to clear out space
  rm(df1)
  
  # Calculate Vector Magnitude (VM)
  df2$VM <- sqrt(df2$X^2+df2$Y^2+df2$Z^2)
  
  #Calculate mean, min, max, sd, skewness, kurtosis for X,Y,Z,VM, and correlation for XYZ
  df2 <- df2 %>% group_by(sec= floor_date(dmy_hms(Timestamp), 'second')) %>%
    summarise(Timestamp = first(Timestamp),
              dplyr::across(-Timestamp,
                            list(Mean=mean, Min=min, Max=max, Sum=sum, sd=sd, Skew=skewness, Kurt=kurtosis),
                            .names="{.col}_{.fn}"), Cor_XY=cor(X,Y), Cor_XZ=cor(X,Z), Cor_YZ=cor(Y,Z))
  
  df2 <- select(df2, -"Timestamp") %>% rename("Timestamp"="sec")
  df2 <- df2 %>% select(Timestamp, X_Mean,X_Min,X_Max,X_Sum,X_sd,X_Skew,X_Kurt,Y_Mean, Y_Min,Y_Max,Y_Sum,Y_sd,Y_Skew,Y_Kurt,
                        Z_Mean,Z_Min,Z_Max,Z_Sum,Z_sd,Z_Skew,Z_Kurt, VM_Mean,VM_Min,VM_Max,VM_Sum,VM_sd,VM_Skew,VM_Kurt,
                        ODBA_Mean,Cor_XY,Cor_XZ,Cor_YZ)

  # Safe new excel files
  setwd(paste0(start_path,"FE")) # safe to another folder, so set working directory for this
  fwrite(df2,paste0(filenames[ii],'.csv'))
  rm(df2)
  setwd(paste0(start_path,"Raw")) # reset working directory to where original excel files are
  
}  

