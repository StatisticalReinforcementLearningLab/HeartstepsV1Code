## Variable list for data analysis

### Outcome

* Step count in 30 minutes after intervention delivery
* Transformation to normalize residuals: TBD - depends on choice of main effects

### Treatment

* (1) Any intervention vs. no intervention
* (2) No intervention vs. sedentary intervention vs. active intervention

### Main effects for variance reduction

(*) Functional form TBD

From baseline surveys:
* Gender
* Age
* How often turn on screen? (Code as midpoint)
* Comfort with mobile phone? (Likert scale)
* Used physical activity tracker? (Binary)
* Conscientiousness scale score (see scoring instructions)
* Self-efficacy scale score (see scoring instructions)
* Baseline IPAQ scores*
* Baseline barriers*
* Baseline facilitators*
* Study phone user? (Binary)

From EMA data:
* Average hectic rating* (sliding average of past X days)
* Average stress rating* (sliding average of past X days)
* Average energetic rating* (sliding average of past X days)
* Number of other activities (strength, cardio, etc) checked over past week

From context/randomization data:
* Weather (good, bad)
* Weekday/weekend
* Time of day (from 5 time points)
* Sedentary or active intervention (Binary)

From activity tracker:
* Step count in prior 30 minutes
* Average step count over past 7 days
* Step count of prior time on same day of week in prior week* (TBD for first week)

From past message suggestions:
* % messages responded to (thumbs up/thumbs down/snooze) over past two days
* % messages that received thumbs up over past two days

### Moderators (for secondary analysis)

* Weather
* Home or work
* Time of day, week
* Use of the phone (at notification; group apps into non-essential tasks, social interaction/synchronous communication, games, videos)
* Proportion of past suggestions that timed out with(?) user interaction, that were rated up/down
* Recent EMA answers averaged over a rolling window
* Past step count (average or variance)
