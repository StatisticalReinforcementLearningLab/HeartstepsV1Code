
* Outcome

- Step count - 30 min to 1 hour after decision point

* Context

- Weather
- Home or work
- Time of day, week
- Use of the phone (at notification, for non-essential tasks)
- Fraction of past suggestions that timed out with user interaction
- Recent EMA answers (hecticness, stress, energy) averaged over a rolling window - attempt to predict today's - no response category for 617
- Past thumbs up/down
- Past step count - past rolling average, variance

* Imputation, missing data

- JMLR paper

* Todos

- Exclude users: non-English locale, international travel, technical issues
- Dataset for imputation of step count 1-2 hours following suggestions

- Proportion of users disconnected by time point - of those available by standard definition, omit suspect users

- first pass - omit non-English locale
- proximal effect with factor(time), weekday-weekend / time of day / week of study / - => factor(day of study)

- second pass - refine definition of variables, add in contextual variables

- New measure of availability - see activity usage

- See what is left

- Modeling availability - reasons for availability

- catch net - no handshake with server for receipt
