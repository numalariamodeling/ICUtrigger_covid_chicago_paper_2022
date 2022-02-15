 [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5018779.svg)](https://doi.org/10.5281/zenodo.5018779)
 # Modeling robust COVID-19 intensive care unit occupancy thresholds for imposing mitigation to prevent exceeding capacities

Simulating COVID-19 transmission and hospital burden to assess at which intensive care unit (ICU) occupancies mitigation, that reduces transmission, needs to be triggered to avoid
exceeding ICU capacity limits, using the city of Chicago, Illinois as an example.

Simulation data files are stored on Zenodo (DOI: 10.5281/zenodo.5018779)

Raw data can be requested using the [Office of IDPH Patient Safety and Quality Discharge Data Request Form](https://dph.illinois.gov/content/dam/soi/en/web/idph/files/forms/formsoppsdischarge-data-request-form.pdf) 

Manuscript is under review for scientific publication, a [see preprint](https://www.medrxiv.org/content/10.1101/2021.06.27.21259530v1.
) posted on medRxiv. 

The [Compartmental Modeling Software (CMS)](https://idmod.org/docs/cms/index.html) is used to simulate the COVID-19 
transmission and disease progression and a custom python simulation and postprocessing framework and R scipts for plotting and result descrption.

This repository contains stationary files to reproduce the related publication and customized folder structure for publication scripts.
The COVID-19 transmission model and python modeling framework are available from https://github.com/numalariamodeling/covid-chicago.
