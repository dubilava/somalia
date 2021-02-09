# Replication Material

This repository hosts the data and R codes for the paper "Price Transmission in Conflict--Affected States: Evidence from Cereal Markets of Somalia," joint work with Justin Hastings, Sarah Phillips, and Andrey Vasnev.

The codes should run with no issue and replicate the results of the study as long as all the supplied material (i.e., the data file as well as the R codes) are stored in the same folder.

## Data

dataset.RData contains all the data needed to replicate the results of the study. These data were obtained from multiple sources, and in most instances, have been manipulated to their present form. The data sources are as follows:

- Conflict: Armed Conflict Location & Event Data (ACLED) Project, available at https://acleddata.com
- Prices: FAO Food Prices Monitoring and Analysis database of Global Information and Early Warning System (GIEWS), available at http://www.fao.org/giews/data-tools
- Roads: OpenStreetMap project via Geofabrik, available at: http://download.geofabrik.de/africa/somalia.html 
- Distances and Travel Time: Google maps, accessed on 4 October 2020.

## R codes

- conflict_map.r generates Figure 1
- descriptive.r generates Tables 1 and 2, Figure 2, and Appendix Figure 1
- regressions_pstr.r generates the main results of the study, Tables 3 and 4, and Appendix Figure 2
- regressions_pstr_omit.r generates Figure 3
- mysript.r is an auxiliary file that contains a function to estimate the PSTR model, used in regressions_pstr.r and regressions_pstr_omit.r

Shield: [![CC BY 4.0][cc-by-shield]][cc-by]

This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png
[cc-by-shield]: https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg
