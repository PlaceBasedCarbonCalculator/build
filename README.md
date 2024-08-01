# Carbon & Place: Main Build Pipeline

This repo contains the main build pipeline for the [Carbon & Place website](https://www.carbon.place).

The pipeline in written in R and uses [targets](https://books.ropensci.org/targets/)

The pipeline can be run by:

```{r}
library(targets)
tar_make()
```

Note that the pipeline looks for data folders defined in `parameters.json` some inputs are in a secure data folder and are not public due to licencing restrictions.

This pipeline requires significant computational resources and has been designed to work well on computers >36 cores and >256GB of RAM. The whole pipeline takes several days to run. On completion the build produces more than 70GB of outputs.

Open input data will be available at [this repo](https://github.com/PlaceBasedCarbonCalculator/inputdata)
