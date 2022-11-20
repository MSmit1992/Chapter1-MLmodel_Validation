##########################################
### CAT BEHAVIOUR PROJECT              ###
### ActiGraph validation behaviour ID  ###
### Self Organizing Maps (SOM)         ###
##########################################
# rm(list=ls())

### BASIC INFORMATION
# SOM is a type of Artificial Neural Network (ANN)
# Can be trained supervised and unsupervised (with already labelled data = supervised)

# The number of hexagonals is determined by the sqr root of (the number behaviours x 4)


### READ R LIBRARIES
library(tidyverse);library(kohonen);library(data.table);library(caret);library(inlmisc)

####################################################################################################################### MODEL BUILDING
#################################
### STEP 5.1 - MODEL BUILDING ###
#################################
# Required input: - Collar dataset: Collar_B2 & Collar_B3 & Collar_B4
#                 - Harness dataset: Harness_B2 & Harness_B3 & Harness_B4
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Preparation"))

################################################################################ RF BEHAVIOURAL SELECTION
###################
### BEHAVIOUR 1 ###
###################
# 15 x 4 = 60 (8x8)

### COLLAR
load("Collar_B1.RDATA")

# Divide data in training & test
trsamp <- function(x, n=1:nrow(x)) {
  d <- x[n, 2:33]
  act <- as.factor(x$Behaviour[n])
  out <- list(measurements = as.matrix(d),Activity=act)
  return(out)
}
ind <- createDataPartition(df.collar$Behaviour, p = 0.70, list = FALSE)
df.train <- trsamp(df.collar, n=ind)
df.test <- trsamp(df.collar, n=-ind)

# Build model
ssom <- supersom(df.train,grid=somgrid(8,8,"hexagonal"))

# Confusion Matrix
ssom.pred <- predict(ssom, df.test)
ptab <- table(predictions = ssom.pred$predictions$Activity, Behaviour = df.test$Activity)
ptab

# Safe model
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Models"))
save(ssom, file="Model_SOM_collar1.rds")
save(ssom, file="Model_SOM_collar1.RDATA")


### HARNESS
load("Harness_B1.RDATA")

# Divide data in training & test
trsamp <- function(x, n=1:nrow(x)) {
  d <- x[n, 2:33]
  act <- as.factor(x$Behaviour[n])
  out <- list(measurements = as.matrix(d),Activity=act)
  return(out)
}
ind <- createDataPartition(df.harness$Behaviour, p = 0.70, list = FALSE)
df.train <- trsamp(df.harness, n=ind)
df.test <- trsamp(df.harness, n=-ind)

# Build model
ssom <- supersom(df.train,grid=somgrid(8,8,"hexagonal"))

# Confusion Matrix
ssom.pred <- predict(ssom, df.test)
ptab <- table(predictions = ssom.pred$predictions$Activity, Behaviour = df.test$Activity)
ptab

# Safe Model
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Models"))
save(ssom, file="Model_SOM_harness1.rds")
save(ssom, file="Model_SOM_harness1.RDATA")



######################################################################################################################### VIZUALIZING
###################
### BEHAVIOUR 2 ###
###################
# 8 x 4 = 32 (6x6)

### COLLAR
load("Collar_B2.RDATA")

# Divide data in training & test
trsamp <- function(x, n=1:nrow(x)) {
  d <- x[n, 2:33]
  act <- as.factor(x$Behaviour[n])
  out <- list(measurements = as.matrix(d),Activity=act)
  return(out)
}
ind <- createDataPartition(df.collar$Behaviour, p = 0.70, list = FALSE)
df.train <- trsamp(df.collar, n=ind)
df.test <- trsamp(df.collar, n=-ind)

# Build model
ssom <- supersom(df.train,grid=somgrid(6,6,"hexagonal"))

# Confusion Matrix
ssom.pred <- predict(ssom, df.test)
ptab <- table(predictions = ssom.pred$predictions$Activity, Behaviour = df.test$Activity)
ptab

# Safe model
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Models"))
save(ssom, file="Model_SOM_collar2.rds")
save(ssom, file="Model_SOM_collar2.RDATA")


### HARNESS
load("Harness_B2.RDATA")

# Divide data in training & test
trsamp <- function(x, n=1:nrow(x)) {
  d <- x[n, 2:33]
  act <- as.factor(x$Behaviour[n])
  out <- list(measurements = as.matrix(d),Activity=act)
  return(out)
}
ind <- createDataPartition(df.harness$Behaviour, p = 0.70, list = FALSE)
df.train <- trsamp(df.harness, n=ind)
df.test <- trsamp(df.harness, n=-ind)

# Build model
ssom <- supersom(df.train,grid=somgrid(6,6,"hexagonal"))

# Confusion Matrix
ssom.pred <- predict(ssom, df.test)
ptab <- table(predictions = ssom.pred$predictions$Activity, Behaviour = df.test$Activity)
ptab

# Safe Model
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Models"))
save(ssom, file="Model_SOM_harness2.rds")
save(ssom, file="Model_SOM_harness2.RDATA")


################################################################################ SOMS BEHAVIOURAL SELECTION
###################
### BEHAVIOUR 3 ###
###################
# 6 x 4 = 24 (5x5)

### COLLAR
load("Collar_B3.RDATA")

# Divide data in training & test
trsamp <- function(x, n=1:nrow(x)) {
  d <- x[n, 2:33]
  act <- as.factor(x$Behaviour[n])
  out <- list(measurements = as.matrix(d),Activity=act)
  return(out)
}
ind <- createDataPartition(df.collar$Behaviour, p = 0.70, list = FALSE)
df.train <- trsamp(df.collar, n=ind)
df.test <- trsamp(df.collar, n=-ind)

# Build model
ssom <- supersom(df.train,grid=somgrid(5,5,"hexagonal"))

# Confusion Matrix
ssom.pred <- predict(ssom, df.test)
ptab <- table(predictions = ssom.pred$predictions$Activity, Behaviour = df.test$Activity)
ptab

# Safe model
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Models"))
save(ssom, file="Model_SOM_collar3.rds")
save(ssom, file="Model_SOM_collar3.RDATA")


### HARNESS
load("Harness_B3.RDATA")

# Divide data in training & test
trsamp <- function(x, n=1:nrow(x)) {
  d <- x[n, 2:33]
  act <- as.factor(x$Behaviour[n])
  out <- list(measurements = as.matrix(d),Activity=act)
  return(out)
}
ind <- createDataPartition(df.harness$Behaviour, p = 0.70, list = FALSE)
df.train <- trsamp(df.harness, n=ind)
df.test <- trsamp(df.harness, n=-ind)

# Build model
ssom <- supersom(df.train,grid=somgrid(5,5,"hexagonal"))

# Confusion Matrix
ssom.pred <- predict(ssom, df.test)
ptab <- table(predictions = ssom.pred$predictions$Activity, Behaviour = df.test$Activity)
ptab

# Safe Model
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Models"))
save(ssom, file="Model_SOM_harness3.rds")
save(ssom, file="Model_SOM_harness3.RDATA")

###################
### BEHAVIOUR 4 ###
###################
# 3 x 4 = 12 (3x4)

### COLLAR
load("Collar_B4.RDATA")

# Divide data in training & test
trsamp <- function(x, n=1:nrow(x)) {
  d <- x[n, 2:33]
  act <- as.factor(x$Behaviour[n])
  out <- list(measurements = as.matrix(d),Activity=act)
  return(out)
}
ind <- createDataPartition(df.collar$Behaviour, p = 0.70, list = FALSE)
df.train <- trsamp(df.collar, n=ind)
df.test <- trsamp(df.collar, n=-ind)

# Build model
ssom <- supersom(df.train,grid=somgrid(3,4,"hexagonal"))

# Confusion Matrix
ssom.pred <- predict(ssom, df.test)
ptab <- table(predictions = ssom.pred$predictions$Activity, Behaviour = df.test$Activity)
ptab

# Safe model
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Models"))
save(ssom, file="Model_SOM_collar4.rds")
save(ssom, file="Model_SOM_collar4.RDATA")


### HARNESS
load("Harness_B4.RDATA")

# Divide data in training & test
trsamp <- function(x, n=1:nrow(x)) {
  d <- x[n, 2:33]
  act <- as.factor(x$Behaviour[n])
  out <- list(measurements = as.matrix(d),Activity=act)
  return(out)
}
ind <- createDataPartition(df.harness$Behaviour, p = 0.70, list = FALSE)
df.train <- trsamp(df.harness, n=ind)
df.test <- trsamp(df.harness, n=-ind)

# Build model
ssom <- supersom(df.train,grid=somgrid(3,4,"hexagonal"))

# Confusion Matrix
ssom.pred <- predict(ssom, df.test)
ptab <- table(predictions = ssom.pred$predictions$Activity, Behaviour = df.test$Activity)
ptab

# Safe Model
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Models"))
save(ssom, file="Model_SOM_harness4.rds")
save(ssom, file="Model_SOM_harness4.RDATA")

################################################################################ SOM BEHAVIOURAL SELECTION
###################
### BEHAVIOUR 5 ###
###################
# 3 x 4 = 12 (3x4)

### COLLAR
load("Collar_B5.RDATA")

# Divide data in training & test
trsamp <- function(x, n=1:nrow(x)) {
  d <- x[n, 2:33]
  act <- as.factor(x$Behaviour[n])
  out <- list(measurements = as.matrix(d),Activity=act)
  return(out)
}
ind <- createDataPartition(df.collar$Behaviour, p = 0.70, list = FALSE)
df.train <- trsamp(df.collar, n=ind)
df.test <- trsamp(df.collar, n=-ind)

# Build model
ssom <- supersom(df.train,grid=somgrid(3,4,"hexagonal"))

# Confusion Matrix
ssom.pred <- predict(ssom, df.test)
ptab <- table(predictions = ssom.pred$predictions$Activity, Behaviour = df.test$Activity)
ptab

# Safe model
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Models"))
save(ssom, file="Model_SOM_collar5.rds")
save(ssom, file="Model_SOM_collar5.RDATA")


### HARNESS
load("Harness_B5.RDATA")

# Divide data in training & test
trsamp <- function(x, n=1:nrow(x)) {
  d <- x[n, 2:33]
  act <- as.factor(x$Behaviour[n])
  out <- list(measurements = as.matrix(d),Activity=act)
  return(out)
}
ind <- createDataPartition(df.harness$Behaviour, p = 0.70, list = FALSE)
df.train <- trsamp(df.harness, n=ind)
df.test <- trsamp(df.harness, n=-ind)

# Build model
ssom <- supersom(df.train,grid=somgrid(3,4,"hexagonal"))

# Confusion Matrix
ssom.pred <- predict(ssom, df.test)
ptab <- table(predictions = ssom.pred$predictions$Activity, Behaviour = df.test$Activity)
ptab

# Safe Model
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Models"))
save(ssom, file="Model_SOM_harness5.rds")
save(ssom, file="Model_SOM_harness5.RDATA")

################################################################################ SOM BEHAVIOURAL SELECTION

###################
### VISUALIZING ###
###################
# Figure (codes): further apart, the more different, the closer the together, the more similar the behaviours are
# Figure (codes): Size of triangle/arrow indicates how often a particular behaviour is represented by that element
# Figure (codes): Full triangle of one colour: determined with an overall 100% accuracy, large sample size behaviours
# Figure (codes): Clustered shapes of different colours: False positive or false negative, small sample size behaviours
plot(ssom, type="codes", codeRendering = "segments", shape="straight")
som.hc <- cutree(hclust(object.distances(ssom,"codes")),15)
add.cluster.boundaries(ssom, som.hc) 

plot(ssom, type="dist.neighbours", shape="straight")
plot(ssom, type="mapping", shape="straight") #The lower the number of empty cells, the better the SOM
plot(ssom, type="count", shape="straight") #Number of observations assigned to node, grey = no observations

# Heat maps, shows us the values of the specific predictor for the specific node. Blue is lower values of predictor, red is higher values of predictors
colnames(getCodes(ssom,1))
cols_out <- inlmisc::GetColors(32, scheme="sunset")
cols_out[1:32]
yb<-colorRampPalette(cols_out[1:32])

# Number between [] is the number of the specific classifier (gotten from colnames)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,1], main=colnames(getCodes(ssom,1))[1], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,2], main=colnames(getCodes(ssom,1))[2], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,3], main=colnames(getCodes(ssom,1))[3], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,4], main=colnames(getCodes(ssom,1))[4], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,5], main=colnames(getCodes(ssom,1))[5], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,6], main=colnames(getCodes(ssom,1))[6], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,7], main=colnames(getCodes(ssom,1))[7], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,8], main=colnames(getCodes(ssom,1))[8], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,9], main=colnames(getCodes(ssom,1))[9], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,10], main=colnames(getCodes(ssom,1))[10], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,11], main=colnames(getCodes(ssom,1))[11], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,12], main=colnames(getCodes(ssom,1))[12], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,13], main=colnames(getCodes(ssom,1))[13], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,14], main=colnames(getCodes(ssom,1))[14], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,15], main=colnames(getCodes(ssom,1))[15], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,16], main=colnames(getCodes(ssom,1))[16], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,17], main=colnames(getCodes(ssom,1))[17], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,18], main=colnames(getCodes(ssom,1))[18], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,19], main=colnames(getCodes(ssom,1))[19], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,20], main=colnames(getCodes(ssom,1))[20], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,21], main=colnames(getCodes(ssom,1))[21], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,22], main=colnames(getCodes(ssom,1))[22], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,23], main=colnames(getCodes(ssom,1))[23], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,24], main=colnames(getCodes(ssom,1))[24], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,25], main=colnames(getCodes(ssom,1))[25], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,26], main=colnames(getCodes(ssom,1))[26], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,27], main=colnames(getCodes(ssom,1))[27], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,28], main=colnames(getCodes(ssom,1))[28], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,29], main=colnames(getCodes(ssom,1))[29], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,30], main=colnames(getCodes(ssom,1))[30], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,31], main=colnames(getCodes(ssom,1))[31], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,32], main=colnames(getCodes(ssom,1))[32], palette.name=yb)


rm(list=ls())


########################
### BEHAVIOUR 2 -SOM ###
########################
# 10 x 4 = 40 (6x7)

### COLLAR
load("Collar_B2_SOM.RDATA")

# Divide data in training & test
trsamp <- function(x, n=1:nrow(x)) {
  d <- x[n, 2:33]
  act <- as.factor(x$Behaviour[n])
  out <- list(measurements = as.matrix(d),Activity=act)
  return(out)
}
ind <- createDataPartition(df.collar$Behaviour, p = 0.70, list = FALSE)
df.train <- trsamp(df.collar, n=ind)
df.test <- trsamp(df.collar, n=-ind)

# Build model
ssom <- supersom(df.train,grid=somgrid(6,7,"hexagonal"))

# Confusion Matrix
ssom.pred <- predict(ssom, df.test)
ptab <- table(predictions = ssom.pred$predictions$Activity, Behaviour = df.test$Activity)
ptab

# Safe model
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Models"))
save(ssom, file="Model_SOM_collar5.rds")
save(ssom, file="Model_SOM_collar5.RDATA")


### HARNESS
load("Harness_B2_SOM.RDATA")

# Divide data in training & test
trsamp <- function(x, n=1:nrow(x)) {
  d <- x[n, 2:33]
  act <- as.factor(x$Behaviour[n])
  out <- list(measurements = as.matrix(d),Activity=act)
  return(out)
}
ind <- createDataPartition(df.harness$Behaviour, p = 0.70, list = FALSE)
df.train <- trsamp(df.harness, n=ind)
df.test <- trsamp(df.harness, n=-ind)

# Build model
ssom <- supersom(df.train,grid=somgrid(6,7,"hexagonal"))

# Confusion Matrix
ssom.pred <- predict(ssom, df.test)
ptab <- table(predictions = ssom.pred$predictions$Activity, Behaviour = df.test$Activity)
ptab

# Safe Model
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Models"))
save(ssom, file="Model_SOM_harness5.rds")
save(ssom, file="Model_SOM_harness5.RDATA")


