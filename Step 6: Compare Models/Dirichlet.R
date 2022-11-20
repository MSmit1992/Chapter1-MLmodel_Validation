##########################################
### CAT BEHAVIOUR PROJECT              ###
### ActiGraph validation behaviour ID  ###
### Dirichlet test proportions         ###
##########################################
library(plyr); library(tidyverse); library(splines); library(data.table); library(lubridate); library(DirichletReg)

#################################################################################################################### HOURLY PROPORTIONS
######################################################
### STEP 1 - HOURLY PROPORTIONS & ACTIVITY BUDGETS ###
######################################################
####################################
### STEP 1.1 - PREPARE DATAFRAME ###
####################################
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Predictions"))
load("long.pred.RDATA")

### SUMMARISE DATA BY HOUR
long.pred$hour <- as.POSIXlt(long.pred$Timestamp)$hour
range(long.pred$hour)

### SAFE DATAFRAME
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/Proportions/"
setwd(paste0(start_path,"Hourly"))
save(long.pred, file="long.pred.RDATA")

###################################
### STEP 1.2 - ACTIVITY BUDGETS ###
###################################
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/Proportions/"
setwd(paste0(start_path,"Hourly"))
load("long.pred.RDATA")
###################
### BEHAVIOUR 1 ###
###################
### COLLAR RANDOM FOREST
df1 <- long.pred %>% select(Cat_id, Timestamp, hour, C.RF1)
df1 <- rename(df1, Behaviour=C.RF1)

setDT(df1)
Model <- df1[ , .(n=.N), by = .(Cat_id,hour,Behaviour)
][, dcast(.SD,Cat_id+hour ~ Behaviour,fill=0)]

#Remove Climbing
df2<- Model %>%
  mutate(Total=Jumping+Rubbing+Trotting+Walking+Lying+Sitting+Standing+
           Grooming+Littering+Digging+Eating+Scratching+Shaking+Allogroom,
         propJumping=Jumping/Total, propRubbing=Rubbing/Total,
         propTrotting=Trotting/Total,propWalking=Walking/Total,propLying=Lying/Total, 
         propSitting=Sitting/Total,propStanding=Standing/Total,propGrooming=Grooming/Total, 
         propLittering=Littering/Total,propDigging=Digging/Total,propEating=Eating/Total, 
         propScratching=Scratching/Total,propShaking=Shaking/Total,propAllogroom=Allogroom/Total) %>% 
  drop_na() %>% gather(key=Behaviour, value=prop, propJumping:propAllogroom) %>% ungroup()

Model<-ggplot(df2, aes(x=hour,y=prop))+aes(color=Behaviour)+geom_point(position="jitter", size=0.2,alpha=0.2)+
  geom_smooth(method = "gam",formula = y ~ splines::ns(x, 12),se=T)+
  labs(x = 'Hour of Day', y = 'Average daily activity budget (%)')+theme_bw(base_size=12) +
  theme(text = element_text(size=13),
        axis.title=element_text(size=14),
        axis.text.x=element_text(size=13),
        axis.text.y=element_text(size=13))+
  theme_minimal() + theme(legend.position="bottom")
Model

### HARNESS RANDOM FOREST
df1 <- long.pred %>% select(Cat_id, Timestamp, hour, H.RF1)
df1 <- rename(df1, Behaviour=H.RF1)

setDT(df1)
Model <- df1[ , .(n=.N), by = .(Cat_id,hour,Behaviour)
][, dcast(.SD,Cat_id+hour ~ Behaviour,fill=0)]

df2<- Model %>%
  mutate(Total=Climbing+Jumping+Rubbing+Trotting+Walking+Lying+Sitting+Standing+
           Grooming+Littering+Digging+Eating+Scratching+Shaking+Allogroom,
         propClimbing= Climbing/Total,propJumping=Jumping/Total,propRubbing=Rubbing/Total,
         propTrotting=Trotting/Total,propWalking=Walking/Total,propLying=Lying/Total, 
         propSitting=Sitting/Total,propStanding=Standing/Total,propGrooming=Grooming/Total, 
         propLittering=Littering/Total,propDigging=Digging/Total,propEating=Eating/Total, 
         propScratching=Scratching/Total, propShaking=Shaking/Total, propAllogroom=Allogroom/Total) %>%
  drop_na() %>% gather(key=Behaviour, value=prop, propClimbing:propAllogroom) %>% ungroup()

Model<-ggplot(df2, aes(x=hour,y=prop))+aes(color=Behaviour)+geom_point(position="jitter", size=0.2,alpha=0.2)+
  geom_smooth(method = "gam",formula = y ~ splines::ns(x, 12),se=T)+
  labs(x = 'Hour of Day', y = 'Average daily activity budget (%)')+theme_bw(base_size=12) +
  theme(text = element_text(size=13),
        axis.title=element_text(size=14),
        axis.text.x=element_text(size=13),
        axis.text.y=element_text(size=13))+
  theme_minimal() + theme(legend.position="bottom")
Model

### COLLAR SOMS
df1 <- long.pred %>% select(Cat_id, Timestamp, hour, C.SOM1)
df1 <- rename(df1, Behaviour=C.SOM1)

setDT(df1)
Model <- df1[ , .(n=.N), by = .(Cat_id,hour,Behaviour)
][, dcast(.SD,Cat_id+hour ~ Behaviour,fill=0)]

#Remove climbing, jumping, trotting, littering, digging, shaking, allogroom
df2<- Model %>%
  mutate(Total=Rubbing+Walking+Lying+Sitting+Standing+Grooming+Eating+Scratching,
         propRubbing=Rubbing/Total,propWalking=Walking/Total,propLying=Lying/Total, 
         propSitting=Sitting/Total,propStanding=Standing/Total,propGrooming=Grooming/Total, 
         propEating=Eating/Total,propScratching=Scratching/Total) %>% 
  drop_na() %>% gather(key=Behaviour, value=prop, propRubbing:propScratching) %>% ungroup()

Model<-ggplot(df2, aes(x=hour,y=prop))+aes(color=Behaviour)+geom_point(position="jitter", size=0.2,alpha=0.2)+
  geom_smooth(method = "gam",formula = y ~ splines::ns(x, 12),se=T)+
  labs(x = 'Hour of Day', y = 'Average daily activity budget (%)')+theme_bw(base_size=12) +
  theme(text = element_text(size=13),
        axis.title=element_text(size=14),
        axis.text.x=element_text(size=13),
        axis.text.y=element_text(size=13))+
  theme_minimal() + theme(legend.position="bottom")
Model

### HARNESS SOMS
df1 <- long.pred %>% select(Cat_id, Timestamp, hour, H.SOM1)
df1 <- rename(df1, Behaviour=H.SOM1)

setDT(df1)
Model <- df1[ , .(n=.N), by = .(Cat_id,hour,Behaviour)
][, dcast(.SD,Cat_id+hour ~ Behaviour,fill=0)]

#Remove climbing, jumping, trotting, littering, digging, scratching, shaking, allogroom
df2<- Model %>%
  mutate(Total=Rubbing+Walking+Lying+Sitting+Standing+Grooming+Eating,
         propRubbing=Rubbing/Total,propWalking=Walking/Total,propLying=Lying/Total, 
         propSitting=Sitting/Total,propStanding=Standing/Total,propGrooming=Grooming/Total, 
         propEating=Eating/Total) %>% drop_na() %>% gather(key=Behaviour, value=prop, propRubbing:propEating) %>% ungroup()

Model<-ggplot(df2, aes(x=hour,y=prop))+aes(color=Behaviour)+geom_point(position="jitter", size=0.2,alpha=0.2)+
  geom_smooth(method = "gam",formula = y ~ splines::ns(x, 12),se=T)+
  labs(x = 'Hour of Day', y = 'Average daily activity budget (%)')+theme_bw(base_size=12) +
  theme(text = element_text(size=13),
        axis.title=element_text(size=14),
        axis.text.x=element_text(size=13),
        axis.text.y=element_text(size=13))+
  theme_minimal() + theme(legend.position="bottom")
Model

###################
### BEHAVIOUR 2 ###
###################
### COLLAR RANDOM FOREST
df1 <- long.pred %>% select(Cat_id, Timestamp, hour, C.RF2)
df1 <- rename(df1, Behaviour=C.RF2)

setDT(df1)
Model <- df1[ , .(n=.N), by = .(Cat_id,hour,Behaviour)
][, dcast(.SD,Cat_id+hour ~ Behaviour,fill=0)]

df2<- Model %>%
  mutate(Total=Active+Lying+Sitting+Standing+Grooming+Littering+Eating+Scratching, 
         propActive=Active/Total,propLying=Lying/Total,propSitting=Sitting/Total, 
         propStanding=Standing/Total, propGrooming=Grooming/Total,propLittering=Littering/Total, 
         propEating=Eating/Total,propScratching=Scratching/Total) %>% drop_na() %>% 
  gather(key=Behaviour, value=prop, propActive:propScratching) %>% ungroup()

Model<-ggplot(df2, aes(x=hour,y=prop))+aes(color=Behaviour)+geom_point(position="jitter", size=0.2,alpha=0.2)+
  geom_smooth(method = "gam",formula = y ~ splines::ns(x, 12),se=T)+
  labs(x = 'Hour of Day', y = 'Average daily activity budget (%)')+theme_bw(base_size=12) +
  theme(text = element_text(size=13),
        axis.title=element_text(size=14),
        axis.text.x=element_text(size=13),
        axis.text.y=element_text(size=13))+
  theme_minimal() + theme(legend.position="bottom")
Model

### HARNESS RANDOM FOREST
df1 <- long.pred %>% select(Cat_id, Timestamp, hour, H.RF2)
df1 <- rename(df1, Behaviour=H.RF2)

setDT(df1)
Model <- df1[ , .(n=.N), by = .(Cat_id,hour,Behaviour)
][, dcast(.SD,Cat_id+hour ~ Behaviour,fill=0)]

df2<- Model %>%
  mutate(Total=Active+Lying+Sitting+Standing+Grooming+Littering+Eating+Scratching, 
         propActive=Active/Total,propLying=Lying/Total,propSitting=Sitting/Total, 
         propStanding=Standing/Total, propGrooming=Grooming/Total,propLittering=Littering/Total, 
         propEating=Eating/Total,propScratching=Scratching/Total) %>% drop_na() %>% 
  gather(key=Behaviour, value=prop, propActive:propScratching) %>% ungroup()

Model<-ggplot(df2, aes(x=hour,y=prop))+aes(color=Behaviour)+geom_point(position="jitter", size=0.2,alpha=0.2)+
  geom_smooth(method = "gam",formula = y ~ splines::ns(x, 12),se=T)+
  labs(x = 'Hour of Day', y = 'Average daily activity budget (%)')+theme_bw(base_size=12) +
  theme(text = element_text(size=13),
        axis.title=element_text(size=14),
        axis.text.x=element_text(size=13),
        axis.text.y=element_text(size=13))+
  theme_minimal() + theme(legend.position="bottom")
Model

### COLLAR SOMS
df1 <- long.pred %>% select(Cat_id, Timestamp, hour, C.SOM2)
df1 <- rename(df1, Behaviour=C.SOM2)

setDT(df1)
Model <- df1[ , .(n=.N), by = .(Cat_id,hour,Behaviour)
][, dcast(.SD,Cat_id+hour ~ Behaviour,fill=0)]

#Remove Littering
df2<- Model %>%
  mutate(Total=Active+Lying+Sitting+Standing+Grooming+Eating+Scratching, 
         propActive=Active/Total,propLying=Lying/Total,propSitting=Sitting/Total, 
         propStanding=Standing/Total,propGrooming=Grooming/Total, 
         propEating=Eating/Total,propScratching=Scratching/Total) %>% drop_na() %>% 
  gather(key=Behaviour, value=prop, propActive:propScratching) %>% ungroup()

Model<-ggplot(df2, aes(x=hour,y=prop))+aes(color=Behaviour)+geom_point(position="jitter", size=0.2,alpha=0.2)+
  geom_smooth(method = "gam",formula = y ~ splines::ns(x, 12),se=T)+
  labs(x = 'Hour of Day', y = 'Average daily activity budget (%)')+theme_bw(base_size=12) +
  theme(text = element_text(size=13),
        axis.title=element_text(size=14),
        axis.text.x=element_text(size=13),
        axis.text.y=element_text(size=13))+
  theme_minimal() + theme(legend.position="bottom")
Model

### HARNESS SOMS
df1 <- long.pred %>% select(Cat_id, Timestamp, hour, H.SOM2)
df1 <- rename(df1, Behaviour=H.SOM2)

setDT(df1)
Model <- df1[ , .(n=.N), by = .(Cat_id,hour,Behaviour)
][, dcast(.SD,Cat_id+hour ~ Behaviour,fill=0)]

#Remove Littering and scratching
df2<- Model %>%
  mutate(Total=Active+Lying+Sitting+Standing+Grooming+Eating, 
         propActive=Active/Total,propLying=Lying/Total,propSitting=Sitting/Total, 
         propStanding=Standing/Total,propGrooming=Grooming/Total, propEating=Eating/Total) %>% 
  drop_na() %>% gather(key=Behaviour, value=prop, propActive:propEating) %>% ungroup()

Model<-ggplot(df2, aes(x=hour,y=prop))+aes(color=Behaviour)+geom_point(position="jitter", size=0.2,alpha=0.2)+
  geom_smooth(method = "gam",formula = y ~ splines::ns(x, 12),se=T)+
  labs(x = 'Hour of Day', y = 'Average daily activity budget (%)')+theme_bw(base_size=12) +
  theme(text = element_text(size=13),
        axis.title=element_text(size=14),
        axis.text.x=element_text(size=13),
        axis.text.y=element_text(size=13))+
  theme_minimal() + theme(legend.position="bottom")
Model

###################
### BEHAVIOUR 3 ###
###################
### COLLAR RANDOM FOREST
df1 <- long.pred %>% select(Cat_id, Timestamp, hour, C.RF3)
df1 <- rename(df1, Behaviour=C.RF3)

setDT(df1)
Model <- df1[ , .(n=.N), by = .(Cat_id,hour,Behaviour)
][, dcast(.SD,Cat_id+hour ~ Behaviour,fill=0)]

df2<- Model %>%
  mutate(Total=Active+Lying+Sitting+Standing+Grooming+Eating,
         propActive=Active/Total,propLying=Lying/Total,propSitting=Sitting/Total, 
         propStanding=Standing/Total,propGrooming=Grooming/Total,propEating=Eating/Total) %>% 
  drop_na() %>% gather(key=Behaviour, value=prop, propActive:propEating) %>% ungroup()

Model<-ggplot(df2, aes(x=hour,y=prop))+aes(color=Behaviour)+geom_point(position="jitter", size=0.2,alpha=0.2)+
  geom_smooth(method = "gam",formula = y ~ splines::ns(x, 12),se=T)+
  labs(x = 'Hour of Day', y = 'Average daily activity budget (%)')+theme_bw(base_size=12) +
  theme(text = element_text(size=13),
        axis.title=element_text(size=14),
        axis.text.x=element_text(size=13),
        axis.text.y=element_text(size=13))+
  theme_minimal() + theme(legend.position="bottom")
Model

### HARNESS RANDOM FOREST
df1 <- long.pred %>% select(Cat_id, Timestamp, hour, H.RF3)
df1 <- rename(df1, Behaviour=H.RF3)

setDT(df1)
Model <- df1[ , .(n=.N), by = .(Cat_id,hour,Behaviour)
][, dcast(.SD,Cat_id+hour ~ Behaviour,fill=0)]

df2<- Model %>%
  mutate(Total=Active+Lying+Sitting+Standing+Grooming+Eating,
         propActive=Active/Total, propLying=Lying/Total,propSitting=Sitting/Total, 
         propStanding=Standing/Total, propGrooming=Grooming/Total, propEating=Eating/Total) %>% 
  drop_na() %>% gather(key=Behaviour, value=prop, propActive:propEating) %>% ungroup()

Model<-ggplot(df2, aes(x=hour,y=prop))+aes(color=Behaviour)+geom_point(position="jitter", size=0.2,alpha=0.2)+
  geom_smooth(method = "gam",formula = y ~ splines::ns(x, 12),se=T)+
  labs(x = 'Hour of Day', y = 'Average daily activity budget (%)')+theme_bw(base_size=12) +
  theme(text = element_text(size=13),
        axis.title=element_text(size=14),
        axis.text.x=element_text(size=13),
        axis.text.y=element_text(size=13))+
  theme_minimal() + theme(legend.position="bottom")
Model

### COLLAR SOMS
df1 <- long.pred %>% select(Cat_id, Timestamp, hour, C.SOM3)
df1 <- rename(df1, Behaviour=C.SOM3)

setDT(df1)
Model <- df1[ , .(n=.N), by = .(Cat_id,hour,Behaviour)
][, dcast(.SD,Cat_id+hour ~ Behaviour,fill=0)]

df2<- Model %>%
  mutate(Total=Active+Lying+Sitting+Standing+Grooming+Eating,
         propActive=Active/Total, propLying=Lying/Total,propSitting=Sitting/Total, 
         propStanding=Standing/Total, propGrooming=Grooming/Total, propEating=Eating/Total) %>% 
  drop_na() %>% gather(key=Behaviour, value=prop, propActive:propEating) %>% ungroup()

Model<-ggplot(df2, aes(x=hour,y=prop))+aes(color=Behaviour)+geom_point(position="jitter", size=0.2,alpha=0.2)+
  geom_smooth(method = "gam",formula = y ~ splines::ns(x, 12),se=T)+
  labs(x = 'Hour of Day', y = 'Average daily activity budget (%)')+theme_bw(base_size=12) +
  theme(text = element_text(size=13),
        axis.title=element_text(size=14),
        axis.text.x=element_text(size=13),
        axis.text.y=element_text(size=13))+
  theme_minimal() + theme(legend.position="bottom")
Model

### HARNESS SOMS
df1 <- long.pred %>% select(Cat_id, Timestamp, hour, H.SOM3)
df1 <- rename(df1, Behaviour=H.SOM3)

setDT(df1)
Model <- df1[ , .(n=.N), by = .(Cat_id,hour,Behaviour)
][, dcast(.SD,Cat_id+hour ~ Behaviour,fill=0)]

df2<- Model %>%
  mutate(Total=Active+Lying+Sitting+Standing+Grooming+Eating,
         propActive=Active/Total, propLying=Lying/Total,propSitting=Sitting/Total, 
         propStanding=Standing/Total, propGrooming=Grooming/Total, propEating=Eating/Total) %>% 
  drop_na() %>% gather(key=Behaviour, value=prop, propActive:propEating) %>% ungroup()

Model<-ggplot(df2, aes(x=hour,y=prop))+aes(color=Behaviour)+geom_point(position="jitter", size=0.2,alpha=0.2)+
  geom_smooth(method = "gam",formula = y ~ splines::ns(x, 12),se=T)+
  labs(x = 'Hour of Day', y = 'Average daily activity budget (%)')+theme_bw(base_size=12) +
  theme(text = element_text(size=13),
        axis.title=element_text(size=14),
        axis.text.x=element_text(size=13),
        axis.text.y=element_text(size=13))+
  theme_minimal() + theme(legend.position="bottom")
Model

###################
### BEHAVIOUR 4 ###
###################
### COLLAR RANDOM FOREST
df1 <- long.pred %>% select(Cat_id, Timestamp, hour, C.RF4)
df1 <- rename(df1, Behaviour=C.RF4)

setDT(df1)
Model <- df1[ , .(n=.N), by = .(Cat_id,hour,Behaviour)
][, dcast(.SD,Cat_id+hour ~ Behaviour,fill=0)]

df2<- Model %>%
  mutate(Total=Active+Inactive+Maintenance,
         propActive=Active/Total, propInactive=Inactive/Total,propMaintenance=Maintenance/Total) %>% 
  drop_na() %>% gather(key=Behaviour, value=prop, propActive:propMaintenance) %>% ungroup()

Model<-ggplot(df2, aes(x=hour,y=prop))+aes(color=Behaviour)+geom_point(position="jitter", size=0.2,alpha=0.2)+
  geom_smooth(method = "gam",formula = y ~ splines::ns(x, 12),se=T)+
  labs(x = 'Hour of Day', y = 'Average daily activity budget (%)')+theme_bw(base_size=12) +
  theme(text = element_text(size=13),
        axis.title=element_text(size=14),
        axis.text.x=element_text(size=13),
        axis.text.y=element_text(size=13))+
  theme_minimal() + theme(legend.position="bottom")
Model

### HARNESS RANDOM FOREST
df1 <- long.pred %>% select(Cat_id, Timestamp, hour, H.RF4)
df1 <- rename(df1, Behaviour=H.RF4)

setDT(df1)
Model <- df1[ , .(n=.N), by = .(Cat_id,hour,Behaviour)
][, dcast(.SD,Cat_id+hour ~ Behaviour,fill=0)]

df2<- Model %>%
  mutate(Total=Active+Inactive+Maintenance,
         propActive=Active/Total, propInactive=Inactive/Total,propMaintenance=Maintenance/Total) %>% 
  drop_na() %>% gather(key=Behaviour, value=prop, propActive:propMaintenance) %>% ungroup()

Model<-ggplot(df2, aes(x=hour,y=prop))+aes(color=Behaviour)+geom_point(position="jitter", size=0.2,alpha=0.2)+
  geom_smooth(method = "gam",formula = y ~ splines::ns(x, 12),se=T)+
  labs(x = 'Hour of Day', y = 'Average daily activity budget (%)')+theme_bw(base_size=12) +
  theme(text = element_text(size=13),
        axis.title=element_text(size=14),
        axis.text.x=element_text(size=13),
        axis.text.y=element_text(size=13))+
  theme_minimal() + theme(legend.position="bottom")
Model

### COLLAR SOMS
df1 <- long.pred %>% select(Cat_id, Timestamp, hour, C.SOM4)
df1 <- rename(df1, Behaviour=C.SOM4)

setDT(df1)
Model <- df1[ , .(n=.N), by = .(Cat_id,hour,Behaviour)
][, dcast(.SD,Cat_id+hour ~ Behaviour,fill=0)]

# Remove Active
df2<- Model %>%
  mutate(Total=Inactive+Maintenance,
         propInactive=Inactive/Total,propMaintenance=Maintenance/Total) %>% 
  drop_na() %>% gather(key=Behaviour, value=prop, propInactive:propMaintenance) %>% ungroup()

Model<-ggplot(df2, aes(x=hour,y=prop))+aes(color=Behaviour)+geom_point(position="jitter", size=0.2,alpha=0.2)+
  geom_smooth(method = "gam",formula = y ~ splines::ns(x, 12),se=T)+
  labs(x = 'Hour of Day', y = 'Average daily activity budget (%)')+theme_bw(base_size=12) +
  theme(text = element_text(size=13),
        axis.title=element_text(size=14),
        axis.text.x=element_text(size=13),
        axis.text.y=element_text(size=13))+
  theme_minimal() + theme(legend.position="bottom")
Model

### HARNESS SOMS
df1 <- long.pred %>% select(Cat_id, Timestamp, hour, H.SOM4)
df1 <- rename(df1, Behaviour=H.SOM4)

setDT(df1)
Model <- df1[ , .(n=.N), by = .(Cat_id,hour,Behaviour)
][, dcast(.SD,Cat_id+hour ~ Behaviour,fill=0)]

df2<- Model %>%
  mutate(Total=Active+Inactive+Maintenance,
         propActive=Active/Total, propInactive=Inactive/Total,propMaintenance=Maintenance/Total) %>% 
  drop_na() %>% gather(key=Behaviour, value=prop, propActive:propMaintenance) %>% ungroup()

Model<-ggplot(df2, aes(x=hour,y=prop))+aes(color=Behaviour)+geom_point(position="jitter", size=0.2,alpha=0.2)+
  geom_smooth(method = "gam",formula = y ~ splines::ns(x, 12),se=T)+
  labs(x = 'Hour of Day', y = 'Average daily activity budget (%)')+theme_bw(base_size=12) +
  theme(text = element_text(size=13),
        axis.title=element_text(size=14),
        axis.text.x=element_text(size=13),
        axis.text.y=element_text(size=13))+
  theme_minimal() + theme(legend.position="bottom")
Model

###################
### BEHAVIOUR 5 ###
###################
### COLLAR RANDOM FOREST
df1 <- long.pred %>% select(Cat_id, Timestamp, hour, C.RF5)
df1 <- rename(df1, Behaviour=C.RF5)

setDT(df1)
Model <- df1[ , .(n=.N), by = .(Cat_id,hour,Behaviour)
][, dcast(.SD,Cat_id+hour ~ Behaviour,fill=0)]

df2<- Model %>%
  mutate(Total=Active+Inactive+Maintenance,
         propActive=Active/Total, propInactive=Inactive/Total,propMaintenance=Maintenance/Total) %>% 
  drop_na() %>% gather(key=Behaviour, value=prop, propActive:propMaintenance) %>% ungroup()

Model<-ggplot(df2, aes(x=hour,y=prop))+aes(color=Behaviour)+geom_point(position="jitter", size=0.2,alpha=0.2)+
  geom_smooth(method = "gam",formula = y ~ splines::ns(x, 12),se=T)+
  labs(x = 'Hour of Day', y = 'Average daily activity budget (%)')+theme_bw(base_size=12) +
  theme(text = element_text(size=13),
        axis.title=element_text(size=14),
        axis.text.x=element_text(size=13),
        axis.text.y=element_text(size=13))+
  theme_minimal() + theme(legend.position="bottom")
Model

### HARNESS RANDOM FOREST
df1 <- long.pred %>% select(Cat_id, Timestamp, hour, H.RF5)
df1 <- rename(df1, Behaviour=H.RF5)

setDT(df1)
Model <- df1[ , .(n=.N), by = .(Cat_id,hour,Behaviour)
][, dcast(.SD,Cat_id+hour ~ Behaviour,fill=0)]

df2<- Model %>%
  mutate(Total=Active+Inactive+Maintenance,
         propActive=Active/Total, propInactive=Inactive/Total,propMaintenance=Maintenance/Total) %>% 
  drop_na() %>% gather(key=Behaviour, value=prop, propActive:propMaintenance) %>% ungroup()

Model<-ggplot(df2, aes(x=hour,y=prop))+aes(color=Behaviour)+geom_point(position="jitter", size=0.2,alpha=0.2)+
  geom_smooth(method = "gam",formula = y ~ splines::ns(x, 12),se=T)+
  labs(x = 'Hour of Day', y = 'Average daily activity budget (%)')+theme_bw(base_size=12) +
  theme(text = element_text(size=13),
        axis.title=element_text(size=14),
        axis.text.x=element_text(size=13),
        axis.text.y=element_text(size=13))+
  theme_minimal() + theme(legend.position="bottom")
Model

### COLLAR SOMS
df1 <- long.pred %>% select(Cat_id, Timestamp, hour, C.SOM5)
df1 <- rename(df1, Behaviour=C.SOM5)

setDT(df1)
Model <- df1[ , .(n=.N), by = .(Cat_id,hour,Behaviour)
][, dcast(.SD,Cat_id+hour ~ Behaviour,fill=0)]

df2<- Model %>%
  mutate(Total=Active+Inactive+Maintenance,
         propActive=Active/Total, propInactive=Inactive/Total,propMaintenance=Maintenance/Total) %>% 
  drop_na() %>% gather(key=Behaviour, value=prop, propActive:propMaintenance) %>% ungroup()

Model<-ggplot(df2, aes(x=hour,y=prop))+aes(color=Behaviour)+geom_point(position="jitter", size=0.2,alpha=0.2)+
  geom_smooth(method = "gam",formula = y ~ splines::ns(x, 12),se=T)+
  labs(x = 'Hour of Day', y = 'Average daily activity budget (%)')+theme_bw(base_size=12) +
  theme(text = element_text(size=13),
        axis.title=element_text(size=14),
        axis.text.x=element_text(size=13),
        axis.text.y=element_text(size=13))+
  theme_minimal() + theme(legend.position="bottom")
Model

### HARNESS SOMS
df1 <- long.pred %>% select(Cat_id, Timestamp, hour, H.SOM5)
df1 <- rename(df1, Behaviour=H.SOM5)

setDT(df1)
Model <- df1[ , .(n=.N), by = .(Cat_id,hour,Behaviour)
][, dcast(.SD,Cat_id+hour ~ Behaviour,fill=0)]

df2<- Model %>%
  mutate(Total=Active+Inactive+Maintenance,
         propActive=Active/Total, propInactive=Inactive/Total,propMaintenance=Maintenance/Total) %>% 
  drop_na() %>% gather(key=Behaviour, value=prop, propActive:propMaintenance) %>% ungroup()

Model<-ggplot(df2, aes(x=hour,y=prop))+aes(color=Behaviour)+geom_point(position="jitter", size=0.2,alpha=0.2)+
  geom_smooth(method = "gam",formula = y ~ splines::ns(x, 12),se=T)+
  labs(x = 'Hour of Day', y = 'Average daily activity budget (%)')+theme_bw(base_size=12) +
  theme(text = element_text(size=13),
        axis.title=element_text(size=14),
        axis.text.x=element_text(size=13),
        axis.text.y=element_text(size=13))+
  theme_minimal() + theme(legend.position="bottom")
Model


#################################################################################################################### DAILY PROPORTIONS
##################################################################################################################### DAILY PROPORTIONS
###################################
### STEP 2 -  DAILY PROPORTIONS ###
###################################
#########################################
### STEP 2.1 -  PREPARATION META DATA ###
#########################################
# Required input:   - Meta data
### PREPARE META DATASET
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/"
setwd(paste0(start_path,"meta_data"))
meta <- read.csv("Budget_Meta2.csv")
head(meta)
str(meta)

# Change variable types
df <- meta
df$Start<-as.POSIXct(df$Start,format="%d/%m/%Y %H:%M")
df$End<-as.POSIXct(df$End,format="%d/%m/%Y %H:%M")
df$Cat_id <- as.factor(df$Cat_id)

# Set timezone to UTC
tz(df$Start)
tz(df$End)
tz(df$Start) <- "UTC"
tz(df$End) <- "UTC"

# Safe meta dataframe
meta <- df
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path, "Proportions"))
save(meta, file="meta.RDATA")

################################################################################################## MERGE META DATA AND PREDICTION DATA

#######################################################
### STEP 2.2 -  MERGE META DATA AND PREDICTION DATA ###
#######################################################
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path, "Proportions"))
load("meta.RDATA")

start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Predictions"))
load("long.pred.RDATA")

### MERGE META DATA AND PREDICTION DATA
df <- long.pred %>% left_join(meta, by = 'Cat_id')
head(df)
str(df)

### SELECT DATA THAT FALL BETWEEN START AND END TIME
df2 <- df %>% group_by(Cat_id) %>%
  filter(Timestamp >= Start & Timestamp <= End)
head(df2)
tail(df2)
str(df2)
dim(df2)

### CALCULATE 24 HOUR BLOCKS
df2$days_from_start<-difftime(df2$Timestamp,df2$Start,units="days")
df2$Day<-ceiling(df2$days_from_start) 
df2$ID_Day<-paste(df2$Cat_id,df2$Day,sep="_")

### CLEAN UP DATAFRAME
df2 <- df2 %>% select(-Start, -End)

# Remove day = 0
df2 <- df2[!grepl("0",df2$Day),]
df2 <- droplevels(df2)

### CHANGE VARIABLE TYPE
df2$Cat_id <- as.factor(df2$Cat_id)

### SAFE DATAFRAME
Act_bud <- df2
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path, "Proportions"))
save(Act_bud, file="Activity_Budget.RDATA")


################################################################################################## CREATE SUB DATAFRAMES PER BEHAVIOUR

################################################################################################################### HOURLY PROPORTIONS
#########################################
### STEP 2.3 -  CREATE SUB DATAFRAMES ###
#########################################
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path, "Proportions"))
load("Activity_Budget.RDATA")

### BEHAVIOUR 1
B1_ab <- Act_bud %>% select (Cat_id, ID_Day, Day, C.RF1, H.RF1, C.SOM1, H.SOM1)
df <- B1_ab %>% gather(key=Model, value=Behaviour, C.RF1:H.SOM1)
# Change variable types
df$ID_Day <- as.factor(df$ID_Day)
df$Model <- as.factor(df$Model)
df$Behaviour <- as.factor(df$Behaviour)
# Safe dataframe
B1_ab <- df
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/Proportions/"
setwd(paste0(start_path, "Daily"))
save(B1_ab, file="B1_activitybudget.RDATA")

### BEHAVIOUR 2
B2_ab <- Act_bud %>% select (Cat_id, ID_Day, Day, C.RF2, H.RF2, C.SOM2, H.SOM2)
df <- B2_ab %>% gather(key=Model, value=Behaviour, C.RF2:H.SOM2)
# Change variable types
df$ID_Day <- as.factor(df$ID_Day)
df$Model <- as.factor(df$Model)
df$Behaviour <- as.factor(df$Behaviour)
# Safe dataframe
B2_ab <- df
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/Proportions/"
setwd(paste0(start_path, "Daily"))
save(B2_ab, file="B2_activitybudget.RDATA")

### BEHAVIOUR 3
B3_ab <- Act_bud %>% select (Cat_id, ID_Day, Day, C.RF3, H.RF3, C.SOM3, H.SOM3)
df <- B3_ab %>% gather(key=Model, value=Behaviour, C.RF3:H.SOM3)
# Change variable types
df$ID_Day <- as.factor(df$ID_Day)
df$Model <- as.factor(df$Model)
df$Behaviour <- as.factor(df$Behaviour)
# Safe dataframe
B3_ab <- df
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/Proportions/"
setwd(paste0(start_path, "Daily"))
save(B3_ab, file="B3_activitybudget.RDATA")

### BEHAVIOUR 4
B4_ab <- Act_bud %>% select (Cat_id, ID_Day, Day, C.RF4, H.RF4, C.SOM4, H.SOM4)
df <- B4_ab %>% gather(key=Model, value=Behaviour, C.RF4:H.SOM4)
# Change variable types
df$ID_Day <- as.factor(df$ID_Day)
df$Model <- as.factor(df$Model)
df$Behaviour <- as.factor(df$Behaviour)
# Safe dataframe
B4_ab <- df
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/Proportions/"
setwd(paste0(start_path, "Daily"))
save(B4_ab, file="B4_activitybudget.RDATA")

### BEHAVIOUR 5
B5_ab <- Act_bud %>% select (Cat_id, ID_Day, Day, C.RF5, H.RF5, C.SOM5, H.SOM5)
df <- B5_ab %>% gather(key=Model, value=Behaviour, C.RF5:H.SOM5)
# Change variable types
df$ID_Day <- as.factor(df$ID_Day)
df$Model <- as.factor(df$Model)
df$Behaviour <- as.factor(df$Behaviour)
# Safe dataframe
B5_ab <- df
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/Proportions/"
setwd(paste0(start_path, "Daily"))
save(B5_ab, file="B5_activitybudget.RDATA")

############################################################################################# DAILY PROPORTIONS & DIRICHLET REGRESSION
#######################################
### STEP 2.4 - DIRICHLET REGRESSION ###
#######################################
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/Proportions/"
setwd(paste0(start_path, "Daily"))

###################
### BEHAVIOUR 1 ###
###################
load("B1_activitybudget.RDATA")

### PROPORTIONS
df1 <- B1_ab

setDT(df1)
Model <- df1[ , .(n=.N), by = .(ID_Day,Day,Model,Behaviour)
][, dcast(.SD, Model + ID_Day+Day ~ Behaviour,fill=0)]

B1_prop <- Model %>%
  mutate(Total=Climbing+Jumping+Rubbing+Trotting+Walking+Lying+Sitting+Standing+
           Grooming+Littering+Digging+Eating+Scratching+Shaking+Allogroom,
         propClimbing= Climbing/Total, propJumping=Jumping/Total, propRubbing=Rubbing/Total,
         propTrotting=Trotting/Total, propWalking=Walking/Total,propLying=Lying/Total, 
         propSitting=Sitting/Total, propStanding=Standing/Total, propGrooming=Grooming/Total, 
         propLittering=Littering/Total, propDigging=Digging/Total, propEating=Eating/Total, 
         propScratching=Scratching/Total, propShaking=Shaking/Total, propAllogroom=Allogroom/Total)%>%drop_na() 

save(B1_prop, file="B1_proportion.RDATA")

### DIRICHLET
dirig <- DR_data(B1_prop[, c("propClimbing","propJumping","propRubbing",
                             "propTrotting","propWalking","propLying","propSitting",
                             "propStanding","propGrooming","propLittering","propDigging",
                             "propEating","propScratching","propShaking","propAllogroom")], base=1)
str(dirig)
dirig

m1 <- DirichReg(dirig ~ Model + Day, data = B1_prop, model = "common")
summary(m1)
m1

predicted_m1 <- predict.DirichletRegModel(m1, B1_prop)

plot(DR_data(predicted_m1))

###################
### BEHAVIOUR 2 ###
###################
load("B2_activitybudget.RDATA")

### PROPORTIONS
df1 <- B2_ab

setDT(df1)
Model <- df1[ , .(n=.N), by = .(ID_Day,Day,Model,Behaviour)
][, dcast(.SD, Model + ID_Day+Day ~ Behaviour,fill=0)]

head(Model)
tail(Model)

B2_prop <- Model %>%
  mutate(Total=Active+Lying+Sitting+Standing+Grooming+Littering+Eating+Scratching,
         propActive=Active/Total, propLying=Lying/Total, propSitting=Sitting/Total, 
         propStanding=Standing/Total,propGrooming=Grooming/Total, propLittering=Littering/Total,
         propEating=Eating/Total, propScratching=Scratching/Total) %>% drop_na()

save(B2_prop, file="B2_proportion.RDATA")

### DIRICHLET
dirig <- DR_data(B2_prop[, c("propActive","propLying","propSitting","propStanding",
                             "propGrooming","propLittering","propEating","propScratching")], base=1)
str(dirig)
dirig

m1 <- DirichReg(dirig ~ Model + Day, data = B2_prop, model = "common")
summary(m1)
m1

predicted_m1 <- predict.DirichletRegModel(m1, B2_prop)

plot(DR_data(predicted_m1))

###################
### BEHAVIOUR 3 ###
###################
load("B3_activitybudget.RDATA")

### PROPORTIONS
df1 <- B3_ab

setDT(df1)
Model <- df1[ , .(n=.N), by = .(ID_Day,Day,Model,Behaviour)
][, dcast(.SD, Model + ID_Day+Day ~ Behaviour,fill=0)]

head(Model)
tail(Model)

B3_prop <- Model %>%
  mutate(Total=Active+Lying+Sitting+Standing+Grooming+Eating,
         propActive=Active/Total, propLying=Lying/Total, propSitting=Sitting/Total, 
         propStanding=Standing/Total,propGrooming=Grooming/Total, propEating=Eating/Total) %>% drop_na()

save(B3_prop, file="B3_proportion.RDATA")

### DIRICHLET
dirig <- DR_data(B3_prop[, c("propActive","propLying","propSitting","propStanding",
                             "propGrooming","propEating")], base=1)
str(dirig)
dirig

m1 <- DirichReg(dirig ~ Model + Day, data = B3_prop, model = "common")
summary(m1)
m1

predicted_m1 <- predict.DirichletRegModel(m1, B3_prop)

plot(DR_data(predicted_m1))

###################
### BEHAVIOUR 4 ###
###################
load("B4_activitybudget.RDATA")

df1 <- B4_ab

setDT(df1)
Model <- df1[ , .(n=.N), by = .(ID_Day,Day,Model,Behaviour)
][, dcast(.SD, Model + ID_Day+Day ~ Behaviour,fill=0)]

head(Model)
tail(Model)

B4_prop <- Model %>%
  mutate(Total=Active+Inactive+Maintenance, propActive=Active/Total,
         propInactive=Inactive/Total, propMaintenance=Maintenance/Total) %>% drop_na()

save(B4_prop, file="B4_proportion.RDATA")

### DIRICHLET
dirig <- DR_data(B4_prop[, c("propActive","propInactive","propMaintenance")], base=1)
str(dirig)
dirig

m1 <- DirichReg(dirig ~ Model + Day, data = B4_prop, model = "common")
summary(m1)
m1

predicted_m1 <- predict.DirichletRegModel(m1, B4_prop)

plot(DR_data(predicted_m1))

###################
### BEHAVIOUR 5 ###
###################
load("B5_activitybudget.RDATA")

### PROPORTIONS
df1 <- B5_ab

setDT(df1)
Model <- df1[ , .(n=.N), by = .(ID_Day,Day,Model,Behaviour)
][, dcast(.SD, Model + ID_Day+Day ~ Behaviour,fill=0)]

head(Model)
tail(Model)

B5_prop <- Model %>%
  mutate(Total=Active+Inactive+Maintenance, propActive=Active/Total,
         propInactive=Inactive/Total, propMaintenance=Maintenance/Total) %>% drop_na()

save(B5_prop, file="B5_proportion.RDATA")

### DIRICHLET
dirig <- DR_data(B5_prop[, c("propActive","propInactive","propMaintenance")], base=1)
str(dirig)
dirig

m1 <- DirichReg(dirig ~ Model + Day, data = B5_prop, model = "common")
summary(m1)
m1

predicted_m1 <- predict.DirichletRegModel(m1, B5_prop)

plot(DR_data(predicted_m1))