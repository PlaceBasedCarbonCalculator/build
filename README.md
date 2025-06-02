# Carbon & Place: Main analysis pipeline

This repo contains the main analysis pipeline for the [Carbon & Place website](https://www.carbon.place).

The pipeline in written in R and uses [targets](https://books.ropensci.org/targets/)

## Licence and Use

This repo is under the GNU Affero General Public License v3.0 which mean it can be used and reused as defined in the [LICENSE](https://github.com/PlaceBasedCarbonCalculator/build/blob/main/LICENSE) file. 

## Input Data

Note that the pipeline looks for data folders defined in [`parameters.json`](https://github.com/PlaceBasedCarbonCalculator/build/blob/main/parameters.json) which requires two input data locations:

`path_data` should point to a local copy of [this repo](https://github.com/PlaceBasedCarbonCalculator/inputdata) which holds all open source inputs

`path_secure_data` should point to a local copy of [this repo](https://github.com/PlaceBasedCarbonCalculator/inputdatasecure)

Note that because this secure data can not be published on GitHub the repo is empty and contains place holders that describe the data and how it can be obtained. More information on data sources is provided in each repo.

## Running the pipeline.

**This pipeline requires significant computational resources and has been designed to work well on computers >36 cores and >256GB of RAM. The whole pipeline takes several days to run. On completion the build produces more than 70GB of outputs.**

### Pre-requsites

To run the analysis you will need to clone:

1. This repo
1. The [inputdata](https://github.com/PlaceBasedCarbonCalculator/inputdata) repo
1. The [inputdatasecure](https://github.com/PlaceBasedCarbonCalculator/inputdatasecure) repo 

You will also need to follow the instructions in those repos about acquiring additional datasets that are too large or have licencing restrictions that prevent publication on GitHub.

**Note** it is possible to run the analysis pipline without all the input datasets (see below), however parts that require those datasets will be unable to run.

You will also require:

1. R Version 4.3.3 or later
2. RStudio
3. [tippecanoe](https://github.com/felt/tippecanoe) installed in Linux/Mac or on Windows installed via [Windows Subsystem for Linux](https://learn.microsoft.com/en-us/windows/wsl/install)
4. Install the `targets` package with `install.packages("targets")`
5. Install the required packages with

```
packages = c("tibble","sf","readODS","readxl","dplyr","tidyr","smoothr",
               "osmextract","nngeo","pbapply","stplanr","rmapshaper",
               "igraph","plyr","terra","furrr","future","humanleague",
               "jsonlite","readr","lubridate","purrr","yyjsonr")
install.packages(packages)
```

6. Adjust the `parameters.json` file to point to your `inputdata` and `inputdatasecure` repos.

### Using targets

The analysis pipeline uses [targets](https://books.ropensci.org/targets/), which is a tool to assist computationally demanding analysis projects. The analysis is broken into many different tasks (also called targets), and the targets package manages dependencies between tasks.

The whole analysis pipeline is defined in the [`_targets.R` file](https://github.com/PlaceBasedCarbonCalculator/build/blob/main/_targets.R). Each "target" can be thought of a single unit of the analysis (e.g. loading and cleaning a dataset). The `_targets.R` file is like a long to-do list of tasks that also contains interdependencies of tasks. The to-do list does not need to be in order.

To get started with the pipeline.

1. In RStudio open the `build.Rproj` project file.
1. Run `library(targets)` to load the `targets` package

A simple interactive flow chart of the analysis can be produced using `tar_visnetwork(T)` however this is not particularly easy to understand so a more advanced method is to run [`mermaid.R`](https://github.com/PlaceBasedCarbonCalculator/build/blob/main/RScripts/mermaid.R) which will produce the file [`mermaid.txt`](https://github.com/PlaceBasedCarbonCalculator/build/blob/main/mermaid.txt) the contents of this file can be pasted into [https://mermaid.live/](https://mermaid.live/) to view the flowchart.

The whole pipeline can be run with `tar_make()` or a specific target (and any precedents) with `tar_make(TARGET_NAME_HERE)`.

Most of the targets in [`_targets.R` file](https://github.com/PlaceBasedCarbonCalculator/build/blob/main/_targets.R) look something like this:

```
tar_target(bounds_lsoa_GB_generalised,{
  combine_lsoa_bounds(bounds_lsoa21_generalised, bounds_dz11, keep = 0.2)
}),
```
In this case `bounds_lsoa_GB_generalised` is the name of the target. `bounds_lsoa21_generalised` and `bounds_dz11` are the names of pre-requisite targets used as inputs. `keep = 0.2` Is a fixed variable used and an input and `combine_lsoa_bounds()` is a function that takes the inputs and does the analysis. All functions can be found in the [`R` folder](https://github.com/PlaceBasedCarbonCalculator/build/tree/main/R).

After running `tar_make()` targets will report progress in the console about which targets have been skipped, run, and have experienced errors.

If you wish to load all the functions used by the pipeline you can run `tar_source()` to load all functions in the `R` folder into the global environment. 

The [`RScripts` folder](https://github.com/PlaceBasedCarbonCalculator/build/tree/main/RScripts) is a place for self contained scripts, these may be for testing or validation and are not part of the analysis pipeline so can be safely ignored.


### Running without the secure data

Within the [`_targets.R` file](https://github.com/PlaceBasedCarbonCalculator/build/blob/main/_targets.R) the `tar_option_set` setting `error = "continue"` can be defined. In this case any target that fails (e.g. because the input data is missing) will be skipped. This means that as much as possible of the analysis will be completed without the secure data.

### Inspecting itermidate targets

After a target has run successfully its results can be viewed by `tar_load(TARGET_NAME_HERE)`. This allows any indeterminate stage of the analysis to be loaded.

**Note** that some stages of the analysis include random number generation. Therefore running your own analysis may produce slightly different results from published outputs. 

## Ouputs

Final outputs are saved in the `ouputdata` folder within the repo. 

Outputs are mostly in four formats

1. GeoJSON files of map data (used below)
1. PMTiles files produced from one of more the GeoJSON files. These are the main format used for maps on the Carbon & Place website.
1. Zipped folders of JSON files. These JSON files typically provide data form the popup reports in the Carbon & Place website.
1. Zipped folders of bulk data. These provide data in common formats such as CSV and are intended for publication on the [Data page](https://www.carbon.place/data/) of the Carbon & Place website.






