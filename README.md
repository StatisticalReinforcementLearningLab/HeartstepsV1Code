### HeartSteps pilot data analysis

#### Overview

File | Description
---- | ----
[ema.options.R](ema.options.R) | Response options for each EMA question. Loaded by [init.R](init.R).
[functions.R](functions.R) | Helper functions, mainly for data formatting. Loaded by [init.R](init.R).
[init.R](init.R) | Initialize common variables. You should run this at the beginning of every R session.
[read.data.R](read.data.R) | Read and tidy up CSV-formatted data. Called by [workspace.csv.R](workspace.csv.R).
[summary.R](summary.R) | Compiles [summary.Rnw](summary.Rnw) into summary.pdf
[summary.Rnw](summary.Rnw) | [knitr](http://yihui.name/knitr/) document for the data summary. Loads csv.RData and analysis.RData, the R workspace files
[workspace.analysis.R](workspace.analysis.R) | Create a workspace file containing data frames for summary and analysis. Loads csv.RData and analysis.RData, the R workspace files created respectively by [workspace.csv.R](workspace.csv.R) and [workspace.analysis.R](workspace.analysis.R).
[workspace.csv.R](workspace.csv.R) | Create a R workspace file containing data frames for the source data files
[xzoo.R](xzoo.R) | Extensions for the time series R package zoo. Loaded by [init.R](init.R).

#### Getting started

Refer to the wiki pages for detail directions to access the data. Unless you maintain the data, only steps 1 and 5 are required.

1. [Mounting M+Box](https://github.com/nseewald1/heartstepsdata/wiki/1-Mounting-Box)
2. [Exporting data](https://github.com/nseewald1/heartstepsdata/wiki/2-Exporting-data)
3. [Preparing data for analysis](https://github.com/nseewald1/heartstepsdata/wiki/3-Preparing-data-for-analysis)
4. [Running the data summary](https://github.com/nseewald1/heartstepsdata/wiki/4-Running-the-data-summary)
5. [Running data analysis](https://github.com/nseewald1/heartstepsdata/wiki/5-Running-data-analysis)
