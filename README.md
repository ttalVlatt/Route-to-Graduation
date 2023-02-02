# Route to Graduation: Reproduction Files

:oncoming_bus: :busstop: :mortar_board:

_Public repo housing scripts to reproduce the analysis from Capaldi's (2023) "Route to Graduation" paper from JCSR._

In this repo you will find 3 scripts needed to reproduce my analysis from the paper:

Capaldi, M.J. (2023). The route to graduation: An exploration of the association between transit stop proximity and Pell completion rates at US commuter colleges. _Journal of College Student Retention: Research, Theory, and Practice_.

Scripts should be kept together in the top-level directory. They are set up to call each other if necessary (i.e., you can start by running 03-data-analysis.R and it will source scripts 01 and 02), but they can also be run in order.

## Data required

All data should be placed in the data folder subfolder. You will need to download data from two publically available sources.

1) Transit stop proximity data is avaialble from the Seldin | Harring-Smith Foundation's Transit Map XL Project here: https://public.tableau.com/app/profile/shsf/viz/TransitAccessibilityatCollegesUniversities/TransitDashboard
    * Select the **_download icon_** in the bottom right, then **_data_**, then **_full data_**, then **_download_**
    * Open and rename this file "raw-transit-data.csv", don't change anything
      + At least on my Mac, this was necessary to re-format the file to read into R smoothly without warnings

2) Institutional data is available from IPEDS for the survey year 2019 here:
https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?year=2019
    * Download the basic data file (.csv) for; HD2019, IC2019_AY, EFFY2019, EF2019A_DIST, S2019_SIS, F1819_F1A, F1819_F2, SFA1819, OM2019
    * Use _rv revised files for all data except HD2019 and IC2019_AY (where there is no revised file)
    * Store those .csv files directly in the data folder (out of the folders the _rv versions come in)

Once this is set up, all the scripts should work, providing you **set working directory** to the top level of this folder where the scripts are located. 

I attempted to annotate what everything does, but if you have any questions feel free to contact me.
