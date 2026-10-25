# Analysis of cumulative impacts of mining and wind power development on reindeer on Malå Sámi reindeer herding district in Sweden

This repo presents the code used to prepare the data and perform the analyses for the paper: Niebuhr, B.B.; Cronvall, E.; Duris, L.; Alam, M.; Panzacchi, M.; van Moorter, B.; Sandström, P.; Skarin, A. **Reindeer habitat selection and movement change with cumulative impacts from mining and wind power development**. Environmental Impact Assessment Review. _In press_.

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
