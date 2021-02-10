# PlantPlan

A Shiny app for updating/creating spatial layouts for trials stored in BreedBase. This app can also be used to output a planting plan and other metadata needed
for printing barcodes/envelopes.

# Notes
* This app connects to Breedbase using [BRAPI](https://www.brapi.org/) version 2
* Authentication information is stored as an R environment variable in .Renviron. This file needs to be transferred to the Shiny server, but it is not tracked on GitHub for security reasons.

# TODO

* Implement BRAPI function for updating layouts directly in the database
* Get BrapiV2 working for layouts
