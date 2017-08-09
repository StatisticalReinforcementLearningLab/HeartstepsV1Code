Daily POMPD : Data Aggregation and Estimation
==============================

This folder contains code for (1) constructing the daily Heartsteps
dataset and (2) analyzing the dataset using Bayesian causal inference
methods.

1.  dailydata.R :  Constructs the complete dataset
  * Inputs : 
    * daily.jbstes.csv : From M+Box in location "Heartsteps/April's
    Stuff/daily jawbone/". This includes the following variables:
    	* m_active_time: Amount of total active time (in seconds)
    	* m_inactive_time: Total inactive time (in seconds)
		* m_lcat: Longest consecutive active time (in seconds)
		* m_lcit: Longest consecutive inactive time (in seconds)
		* m_steps: Total number of steps taken (daily)
		* m_missing: Indicator 
    * weather.RData : 
  * Output : Each line is a person-day observation with columns. Output saved to "dailydata.csv".
