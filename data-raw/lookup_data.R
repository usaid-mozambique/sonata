


data_location_lookup <- pull_lookup_table(conn_ddb, table = "observation_lookup", store = FALSE)
data_location_lookup <- pull_lookup_table(conn_ddb, table = "location_lookup_clean", store = FALSE)
data_type_id_lookup <- pull_lookup_table(conn_ddb, table = "type_id_lookup", store = FALSE)


usethis::use_data(data_location_lookup,
                  data_observation_lookup,
                  data_type_id_lookup,
                  internal = TRUE,
                  overwrite = TRUE)

