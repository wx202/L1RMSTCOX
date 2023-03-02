# L1RMSTCOX
Quantifying and interpreting the prediction accuracy of models for the survival time using mean absolute differences (MAD) 

(1) Fit the observed survival time with Cox PH model.

(2) Cauculate restricted mean survival time (RMST) based on the fitted model in (1).

(3) Calculate MAD using the “inverse probability of censoring weighting” (IPCW) technique.

There are two version of codes. One version uses the same data to build Cox model, estimate RMST, and caculate MAD.
The other cross-validated version splits the dataset into two halves of equal sample size, uses one subset to build Cox model, and uses the other subset to estimate RMST and calculate MAD. 

Reference

Quantifying and Interpreting the Prediction Accuracy of Models for the Time of a Cardiovascular Event-Moving Beyond C Statistic: A Review. DOI: 10.1001/jamacardio.2022.5279 
