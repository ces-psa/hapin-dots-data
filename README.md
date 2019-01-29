## HAPIN DOTs data

This repository documents code for managing and analizing raw temperature data
collected using Geocene DOTs stove use monitors, and data related to the setup
of these DOTs within study households.


### Use of the code

#### Data sources

This code requires two different data sources:

- DOTs raw data

These should be downloaded from the Geocene data server, and unzipped in the
`data/geocene-exports` folder (all files should be directly in that folder, e.g.
`data/geocene-exports/missions.csv`, `data/geocene-exports/events.csv`, etc.)

- DOTs setup data from RedCap

Raw exports from RedCap for each IRC should be placed in the folder
`data/redcap-exports/`, folowing the `IRC_H40_YYYYMMDD.csv` naming convention
(e.g. `data/redcap-exports/GUATEMALA_H40_20190129.csv`).
These exports should have the variable HHID for each household's study id,
and should contain _at least_ all variables from the H40 crf, including
both copies of the crf (i.e. corresponding to h40_date and h40_date_v2), but
can contain all variables from the main study.


#### R packages

This codes depends on the following R packages:

- `tidyverse`: data managing and tidying, and operations on rectangular tables
- `trelliscopejs`: dashboard showing plots for all DOTs in each household
