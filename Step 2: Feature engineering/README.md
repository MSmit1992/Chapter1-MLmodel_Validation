This folder contains:
- R code (FeatureEngineering.R).

# Feature engineering
Acceleration data was sampled at a frequency of 30 Hz (raw data). A total of 32 predictor variabes were derived from the raw x,y,z accelerometer data and summarized into 1 second epochs
# Steps
## (1) Prepare datasets
The files containing the raw acceleration data, contain extra rows, which need to be removed.
## (2) Calculate predictors
For this study, a total of 32 predictors were included. These are calculated from the raw accelerometer data and summarized into 1 second time intervals (i.e. epochs)

