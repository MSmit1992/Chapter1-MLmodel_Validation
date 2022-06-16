
This folder contains:
- R Code (Merge.datasets.R)

# Merge datasets
Following step 2, a file exists for each containing the 35 predictor variables. For each cat, there is also a file available which contains the behaviours that were scored from the videorecordings. These files containing the scored behaviours are referred to as annotated data. In step 3, the files containing the predictor variables will be merged with the annotated data. One big dataset will be created including both the predictor variables and annotated data of each cat.

# General steps
## (1) Preparation annotated data
For each cat, a file exists containing the behaviours that were scored from the videorecordings. All individual datasets are merged into a big dataset containing the annotated data for all cats.
## (2) Preparation accelerometer data
For each cat, a file exists containing all 35 predictor variables. All individual datasets are merged into a big dataset containing all predictor variables for all cats.
## (3) Preparation meta data
The accelerometer dataset (containing the predictor variables) includes 7 days worth of data for each cat. However, only 4 hours worth of videorecordings were scored for behaviour for every cat. A file was created that included the start and end date and time of the scored videorecordings. This file was used to select the accelerometer data within this timeframe.
## (4) Merge meta data and annotated data
The file containing the annotated data for all cats was trimmed using the times defined in the meta data file.
## (5) Merge meta data and accerelometer data
The file containing the accelerometer data for all cats was trimmed using the times defined in the meta data file.
