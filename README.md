# simpson
This repository contains the raw data, R scripts, and image outputs used in our study on Simpson's Paradox in data analysis of mortality and PM10 in NYC, presented in a talk on 25/07/2023 at the 67th RBras and 20th SEAGRO in Londrina-PR. In this study, we utilized daily air quality measurements from the City of New York for the period between 1996 and 2010, with respect to PM10 (µg/m³), available from the United States Environmental Protection Agency (http://www.epa.gov), as well as weekly mortality data for the same city and period, available from the Centers for Disease Control and Prevention (http://www.cdc.gov). The data was compiled using the ISO 8601 criterion for correspondence between the dates of PM10 measurements, weeks of the year, and mortality, respectively. We adopted a general linear regression methodology and, subsequently, in different subgroups, compared the mortality variable with PM10 measurements. All data processing, model development, and results were implemented in R. The raw and transformed data, as well as the scripts for verification and reproducibility, are available on the first author's GitHub.


| Abbreviation | Variable                   | Meaning                                                         |
|--------------|----------------------------|-----------------------------------------------------------------|
| ID           | ID                         | Identification number of each observation                       |
| Date         | Date                       | Date of the observation in YYYY-MM-DD format                     |
| PM10         | PM10                       | Particulate Matter with diameter ≤ 10 μm (µg/m³)                |
| total        | All causes of death, all ages** | Total number of deaths for all causes and all ages               |
| I65          | All causes of death, >=65 years | Total number of deaths for all causes in individuals aged 65 years or older |
| I45.64       | All causes of death, 45-64 years | Total number of deaths for all causes in individuals aged 45-64 years |
| I25.44       | All causes of death, 25-44 years | Total number of deaths for all causes in individuals aged 25-44 years |
| I1.24        | All causes of death, 1-24 years  | Total number of deaths for all causes in individuals aged 1-24 years |
| I0.1         | All causes of death, less than 1 year | Total number of deaths for all causes in individuals aged less than 1 year |
| IN           | Pneumonia and influenza     | Total number of deaths due to pneumonia and influenza             |


Click [here](https://msrcos3s.shinyapps.io/pm10) to access my Shiny app.
