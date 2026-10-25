# Analysis of cumulative impacts of mining and wind power development on reindeer on Malå Sámi reindeer herding district in Sweden

This repo presents the code used to prepare the data and perform the analyses for the paper: Niebuhr, B.B.; Cronvall, E.; Duris, L.; Alam, M.; Panzacchi, M.; van Moorter, B.; Sandström, P.; Skarin, A. **Reindeer habitat selection and movement change with cumulative impacts from mining and wind power development**. Environmental Impact Assessment Review. _In press_.

## Study overview

Reindeer husbandry in northern Sweden is increasingly shaped by the combined footprint of multiple land-use pressures. In this study, we quantify the cumulative effects of mining and wind power infrastructure on reindeer space use in Malå Sámi reindeer herding district by integrating GPS movement data with environmental, weather, forage (NDVI), and infrastructure-related variables, including distance- and zone of influence (ZOI) metrics. Using habitat selection and movement-oriented modelling across seasonal contexts, we evaluate how reindeer responses vary with ecological conditions and overlapping disturbance sources, and we synthesize results to identify where and when cumulative impacts are strongest. The work provides an evidence base for cumulative impact assessment and for planning processes that aim to balance renewable energy and mineral development with the long-term viability of Sámi reindeer husbandry.

## Repository structure

The scripts are present in the folder `code`, spread in the following sub-folders:
- `01_env_database`: scripts to build the spatial database, using python and GRASS GIS;
- `02_NDVI`: scripts for computing NDVI indices;
- `03_weather`: scripts for computing weather variables;
- `04_ZoI_vars`: scripts to compute the zone of influence (ZOI) variables;
- `05_prepare_gps`: scripts to clean GPS fixes and prepare the GPS data for habitat selection and movement analyses;
- `06_model_fit`: scripts for modeling fitting;
- `07_synthesis_results`: scripts for synthesizing the results;
- `08_calving_site_selection`: scripts for calving site selection analyses.

The other folders are empty, and kept to maintain the structure of the data used for analysis.

## Data

The data used for this is study is proprietary to the Sámi reindeer herding district that collaborated with us. As such, the data is not made openly available.

## Citation

Niebuhr, B.B.; Cronvall, E.; Duris, L.; Alam, M.; Panzacchi, M.; van Moorter, B.; Sandström, P.; Skarin, A. **Reindeer habitat selection and movement change with cumulative impacts from mining and wind power development**. Environmental Impact Assessment Review. _In press_.

## Contact

Bernardo Brandão Niebuhr <bernardo.brandao@nina.no>, <bernardo_brandaum@yahoo.com.br>
Anna Skarin <anna.skarin@slu.se>
