
## COVID-19 occupancy data in Intensive Care Units (ICUs) in Chicago for 2020

COVID-19_ICU_Chicago_2020

| column name                              | label                           | format  |
|------------------------------------------|---------------------------------|---------|
| date                                     | Date of reported ICU occupancies           | date    |
| confirmed_covid_icu                      | Number of ICU beds occupied by confirmed COVID-19 patients        | integer |
| suspected_covid                          | Number of ICU beds occupied by suspected COVID-19 patients      | integer |
| suspected_and_confirmed_covid_icu        | Number of ICU beds occupied by confirmed+suspected COVID-19 patients     | integer |
| icu_total                                | Number of total ICU beds | integer |
| availbility_icu                          | Number of total ICU beds available (icu_availforcovid - suspected_and_confirmed_covid_icu) | integer |
| icu_noncovid                             | Number of ICU beds occupied by non-COVID-19 patients  | integer |
| icu_availforcovid                        | 'ICU capacity': Number of total ICU beds available for COVID-19 patients (icu_total - icu_noncovid) | integer |
| icu_availforcovid_7avrg                  |7-day moving average for 'ICU capacity' | integer |

Data shared with permission from the Illinois Department of Health.
Raw data can be requested using the [Office of IDPH Patient Safety and Quality Discharge Data Request Form](https://dph.illinois.gov/content/dam/soi/en/web/idph/files/forms/formsoppsdischarge-data-request-form.pdf) 
