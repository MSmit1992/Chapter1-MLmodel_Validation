This folder contains:
- R code (Dirichtlet.R).

# Comparing Models

# Steps
## (1) Hourly proportions and activity budgets
### 1. Prepare dataframe
The dataframe containting all of the behavioural predictions using the models, was formatted into a long dataframe
### 2. Activity budgets
For each model, the hourly proportion of each behaviour was determined and plotted.
## (2) Daily proportions
### 1. Preparation meta data
A meta file was created to be able to distinguish the different days and to select data within a certain timeframe.
### 2. Merge meta data and predicted behaviour data
The meta data and data containing the behavioural predictions, were merged. Only data within the timeframe specified in the meta data was kept and 24 hour blocks were calculated and renamed as a specific day (1-6)
### 3. Create sub dataframes
A seperate dataframe was created for each behavioural round that was done.
### 4. Dirichlet Regression
For each behavioural round, a Dirichlet Regression was done to determine differences between the models in the proportions of the predicted behaviours.
