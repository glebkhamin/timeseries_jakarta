# timeseries_jakarta
Forecasting for Ambulance Services in Jakarta Project

- You can access the R code in the main branch; the file is named 'Jakarta_timeseries_code.R'
- The data used in the project is contained in 'TimeSeriesData.xlsx'
- For detailed analysis, refer to 'Poster_with_commentary.pdf'.

Ambulance service providers in Jakarta are seeking a planning tool for scheduling and rostering their crews. For efficient medical treatment and transportation to patients, ambulance services require an accurate forecast of demand. The aim of this project is to analyse the provided data on the daily number of patients (01/04/2015 - 31/05/2019) to see if there is any pattern to it, and to forecast the number of patients for the first week of June 2019. The following steps will be taken:

(i) Analysing and pre-processing the provided data
(ii) Converting the data into daily time series format and splitting it into training (70%) and testing (30%) sets to facilitate model training and validation
(iii) Training the 10 selected models on the train set, and subsequently testing their accuracy on the test set
(iv) Evaluating the performance of these models using statistical measures, such as MAE, RMSE, and MAPE
(v) Selecting the model with the best performance, and using it to forecast the number of patients for the first week in June 2019.
