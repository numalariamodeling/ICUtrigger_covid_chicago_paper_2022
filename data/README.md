## Data files used for fitting in the format as shown below:

Raw data can be requested using the [Office of IDPH Patient Safety and Quality Discharge Data Request Form](https://dph.illinois.gov/content/dam/soi/en/web/idph/files/forms/formsoppsdischarge-data-request-form.pdf) 


### EMresource data,Illinois Department of Public Health (IDPH)

| emresource_by_region   | label                                    | format  | levels           |
|------------------------|------------------------------------------|---------|------------------|
| date_of_extract        | Date of data extract                     | date    |                  |   
| covid_region           | Operational covid region                 | string  | covidregion_1-11 |  
| n_hospitals            | number of hospitals submitted report     | integer |                  |   
| confirmed_covid_icu    | test confirmed COVID-19 patients in ICUs | integer |                  |  
| covid_non_icu          | non-COVID-19 patients in ICUs            | integer |                  |   


### Illinois National Electronic Disease Surveillance System (I-NEDSS) 

| 210426_aggregated_covidregion            | label                           | format  | levels           |
|------------------------------------------|---------------------------------|---------|------------------|
| date                                     | Date of data extract            | date    |                  |
| covid_region                             | Operational covid region        | string  | covidregion_1-11 |
| cases                                    | Cumulative number of cases      | integer |                  |
| deaths                                   | Cumulative number of deaths     | integer |                  |
| admissions                               | Cumulative number of admissions | integer |                  |



### Hospital occupancy data, Illinois Department of Public Health (IDPH)
#### I
| capacity_by_covid_region | label                                                                        | format  | levels           |
|--------------------------|------------------------------------------------------------------------------|---------|------------------|
| date                     | Date                                                                         | date    |                  |
| geography_level          | Selected geography (i.e. Operational covid region)                           | string  | covid region     |
| geography_name           | Operational covid region                                                     | string  | covidregion_1-11 |
| medsurg_total            | Number of medical/Surgery (non ICU) beds occupied                            | integer |                  |
| icu_total                | Number of ICU beds occupied                                                  | integer |                  |
| vent_total               | Number of ventilators in use                                                 | integer |                  |
| medsurg_noncovid         | Number of medical/Surgery (non ICU) beds occupied by non-COVID-19   patients | integer |                  |

#### II
| capacity_weekday_average_20200915 | label                                             | format  | levels                              |
|-----------------------------------|---------------------------------------------------|---------|-------------------------------------|
| date_capacity_run                 | Date of capacity number extract                   | date    |                                     |
| geography_modeled                 | Operational covid region                          | string  | covidregion_1-11                    |
| resource_type                     | Type of hospital beds (med/surg, or ICU)          | string  | hb_availforcovid, icu_availforcovid |
| avg_resource_available_prev2weeks | Number of neds occupied in past 2 weeks           | integer |                                     |
| overflow_threshold_percent        | Occupancy threshold for calculating probabilities | float   | 0.75,1                              |
| date_window_upper_bound           | Date for timewindow                               | date    |                                     |




