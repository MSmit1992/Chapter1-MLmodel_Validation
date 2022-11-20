This folder contains:
- R Code for behavioural prediction (predict.behaviour.R)

*The RDATA files containing the predictor variables and the actual models from this study are to big to be uploaded to GitHub*

# (1) Preperation meta data
The meta data contains the start time and end time for which you want to make behavioural predictions. I selected times that were not included in the building and training of the model. If you have more than one day of data and the day is of importance to your study, you will need to include the date.
# (2) Merge meta data and accelerometer data
The meta dataframe is merged with the dataframe that contains all the prediction variables, selecting only the dates and times included in the meta dataframe.
It is important to make sure the timezone of both the meta datrame and dataframe containing the prediction variables are the same!
# (3) Data splitting
Two seperate dataframes are created, one for the collar attachment and one for the harness attachment, as there is a different model for each.
# (4) Predict behaviour
Using the models, you can now predict the behaviour using the predictor variables that were not used to build and train the models
