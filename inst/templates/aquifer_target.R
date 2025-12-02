# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  packages = c("dataRetrieval", "HASP"))

# list of targets to build
list(
  # Define site list
  tar_target(name = aquifer,
             command = "{{{ national_aquifer_cd }}}"),
  tar_target(name = parameter_cd,
             command = "{{{ parameter_cd }}}"),
  tar_target(name = statistic_cd,
             command = "{{{ statistic_cd }}}"),
  tar_target(name = start_date,
             command = "{{{ start_date }}}"),
  tar_target(name = end_date,
             command = "{{{ end_date }}}"),
  tar_target(name = chunk_up_field,
             command = 100),
  tar_target(name = chunk_up_daily,
             command = 1),
  tar_target(name = site_file,
             command = dataRetrieval::read_waterdata_monitoring_location(
               national_aquifer_code = aquifer
             )),
  tar_target(name = states,
             command = unique(site_file$state_name)),
  tar_target(name = sites_with_data,
             command = dataRetrieval::read_waterdata_ts_meta(state_name = states,
                                                             parameter_code = parameter_cd,
                                                             statistic_id = statistic_cd,
                                                             computation_period_identifier = "Daily",
                                                             skipGeometry = TRUE),
             pattern = map(states),
             iteration = "list"),
  tar_target(name = sites_with_data_df,
             command = dplyr::bind_rows(sites_with_data)),
  tar_target(name = aquifer_sites,
             command = sites_with_data_df$monitoring_location_id[sites_with_data_df$monitoring_location_id %in% site_file$monitoring_location_id]),
  tar_target(name = site_chunks, #seems like a lot of sites go missing if we just do the sites with daily data
             command = split(site_file$monitoring_location_id, ceiling(seq_along(site_file$monitoring_location_id)/chunk_up_field))
  ),
  tar_target(name = site_dv_chunks,
             command = split(aquifer_sites, ceiling(seq_along(aquifer_sites)/chunk_up_daily))
  ),
  tar_target(name = field_measurements,
             command = dataRetrieval::read_waterdata_field_measurements(monitoring_location_id = site_chunks[[1]],
                                                                        time = c(start_date, end_date),
                                                                        parameter_code = parameter_cd,
                                                                        # approval_status = "Approved",
                                                                        skipGeometry = TRUE,
                                                                        properties = c("monitoring_location_id",
                                                                                       "parameter_code",
                                                                                       "time",
                                                                                       "value")),
             pattern = site_chunks,
             iteration = "list"
  ),
  tar_target(name = gwl_levels,
             command = dplyr::bind_rows(field_measurements)
             
  ),
  tar_target(name = dv_measurements,
             command = dataRetrieval::read_waterdata_daily(monitoring_location_id = site_dv_chunks[[1]],
                                                           time = c(start_date, end_date),
                                                           parameter_code = parameter_cd,
                                                           # approval_status = "Approved",
                                                           statistic_id = statistic_cd,
                                                           skipGeometry = TRUE,
                                                           properties = c("time",
                                                                          "value",
                                                                          "monitoring_location_id",
                                                                          "parameter_code",
                                                                          "statistic_id")),
             pattern = site_dv_chunks,
             iteration = "list"
  ),
  tar_target(name = dv_levels,
             command = dplyr::bind_rows(dv_measurements)
             
  ),
  tar_target(name = aquifer_data,
             command = dplyr::bind_rows(dv_levels, gwl_levels) |>
               dplyr::mutate(year = as.numeric(format(time, "%Y")),
                             water_year = water_year(time))
  ),
  tar_target(name = site_info,
             command = dplyr::filter(site_file, 
                                     monitoring_location_id %in% unique(aquifer_data$monitoring_location_id)))
)