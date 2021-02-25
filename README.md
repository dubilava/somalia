# Replication Material

This repository hosts the data and R codes for the paper "[Price Transmission in Conflict--Affected States: Evidence from Cereal Markets of Somalia](https://ideas.repec.org/p/syd/wpaper/2020-16.html)," joint work with Justin Hastings, Sarah Phillips, and Andrey Vasnev.

The codes should run with no issue and replicate the results of the study as long as all the supplied material (i.e., the data file as well as the R codes) are stored in the same folder.

## Data

dataset.RData contains all the data needed to replicate the results of the study. These data were obtained from multiple sources, and in most instances, have been manipulated to their present form. The data sources are as follows:

- Conflict: Armed Conflict Location & Event Data (ACLED) Project of Raleigh, et al. (2010). Introducing ACLED: An Armed Conflict Location and Event Dataset: Special Data Feature. Journal of Peace Research 47(5): 651–660. Data available at https://acleddata.com
- Prices: Food and Agriculture Organization of the United Nation's (FAO) Food Price Monitoring and Analysis (FPMA) Tool database of Global Information and Early Warning System on Food and Agriculture (GIEWS). Data available at http://www.fao.org/giews/data-tools
- Roads: OpenStreetMap project via Geofabrik, available at: http://download.geofabrik.de/africa/somalia.html 
- Distances and Travel Time: Google maps. Accessed on 4 October 2020.

## R codes

- conflict_map.r generates Figure 1
- descriptive.r generates Tables 1 and 2, Figure 2, and Appendix Figure 1
- regressions_pstr.r generates the main results of the study, Tables 3 and 4, and Appendix Figure 2
- regressions_pstr_omit.r generates Figure 3
- mysript.r is an auxiliary file that contains a function to estimate the PSTR model, used in regressions_pstr.r and regressions_pstr_omit.r

## License

<a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.
