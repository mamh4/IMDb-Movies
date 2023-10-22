library(lubridate)


dob_table_directors_cleaned <- dob_table_directors |> mutate(dob_directors = ymd(dob_directors))
dob_table_stars_cleaned <- dob_table_stars |> mutate(dob_stars = ymd(dob_stars))


dob_table_directors_cleaned$dob_directors_orig <- dob_table_directors$dob_directors
dob_table_stars_cleaned$dob_stars_orig <- dob_table_stars$dob_stars


