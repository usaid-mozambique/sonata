## code to prepare `lookup_table` dataset goes here

data_observation_lookup <- pull_lookup_table(conn_ddb, table = "observation_lookup", store = FALSE)
usethis::use_data(data_observation_lookup, overwrite = TRUE)

data_location_lookup <- pull_lookup_table(conn_ddb, table = "location_lookup_clean", store = FALSE)
usethis::use_data(data_location_lookup, overwrite = TRUE)

data_type_id_lookup <- pull_lookup_table(conn_ddb, table = "type_id_lookup", store = FALSE)
usethis::use_data(data_type_id_lookup, overwrite = TRUE)
