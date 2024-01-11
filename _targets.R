# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tibble") # packages that your targets need to run
  # format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # For distributed computing in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller with 2 workers which will run as local R processes:
  #
  #   controller = crew::crew_controller_local(workers = 2)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package. The following
  # example is a controller for Sun Grid Engine (SGE).
  #
  #   controller = crew.cluster::crew_controller_sge(
  #     workers = 50,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.0".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# tar_make_clustermq() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
options(clustermq.scheduler = "multiprocess")

# tar_make_future() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(


# Set Up ------------------------------------------------------------------

# Detect when parameter file has changed:
tar_target(name = param_file, command = "parameters.json", format = "file"),
# Check Renviron exists, create output directory, load params:
tar_target(parameters, {
  p = jsonlite::read_json(param_file, simplifyVector = TRUE)
  p
}),

# Download Input Datasets -------------------------------------------------
tar_target(population,{
  dowload_lsoa_population(path = file.path(parameters$path_data,"population"))
  pop = build_lsoa_population()
  pop
}),

# Gas and Electricity
tar_target(dl_gas_electric,{
  dowload_gas_electric(path = file.path(parameters$path_data,"gas_electric"))
}),
tar_target(domestic_gas,{
  load_lsoa_gas(dl_gas_electric)
}),
tar_target(domestic_electricity,{
  load_lsoa_electric(dl_gas_electric)
}),
tar_target(nondomestic_gas,{
  load_msoa_gas_nondom(dl_gas_electric)
}),
tar_target(nondomestic_electricity,{
  load_msoa_electric_nondom(dl_gas_electric)
}),

# Boundaries
tar_target(dl_boundaries,{
  download_boundaries(path = file.path(parameters$path_data,"boundaries"))
}),
tar_target(bounds_la,{
  read_bounds_la(dl_boundaries)
}),
tar_target(bounds_wards,{
  read_bounds_wards(dl_boundaries)
}),
tar_target(bounds_parish,{
  read_bounds_parish(dl_boundaries)
}),
tar_target(bounds_westminster,{
  read_bounds_westminster(dl_boundaries)
}),
tar_target(bounds_lsoa21_full,{
  read_bounds_lsoa_full(dl_boundaries)
}),
tar_target(bounds_lsoa21_generalised,{
  read_bounds_lsoa_generalised(dl_boundaries)
}),
tar_target(bounds_lsoa21_super_generalised,{
  read_bounds_lsoa_super_generalised(dl_boundaries)
}),
tar_target(centroids_lsoa11,{
  read_centroids(dl_boundaries)
}),

# Contextual Data
tar_target(dl_area_classifications,{
  download_area_classifications(path = file.path(parameters$path_data,"area_classifications"))
}),
tar_target(area_classifications,{
  load_area_classifications(dl_area_classifications)
}),

# Income



# Car Stats
tar_target(dl_vehicle_registrations,{
  download_dft_vehicle_registrations(path = file.path(parameters$path_data,"vehicle_registrations"))
}),
tar_target(vehicle_registrations,{
  # Long running target ~4 hours
  #load_dft_vehicle_registrations(dl_vehicle_registrations)
}),

# EPCs

# Housing

# Travel to Work
tar_target(travel2work,{
  load_travel2work(path = file.path(parameters$path_secure_data,"LSOA Flow Data/Public/WM12EW[CT0489]_lsoa.zip"))
})


# Travel to School

# Other Travel

# Consumption

# Flights

# Isochrones

# Transit Stops

# Emissions Footprints

# Scenarios

# LA Summaries

# Build GeoJSON

# Build PMTiles

# Build JSON


)
