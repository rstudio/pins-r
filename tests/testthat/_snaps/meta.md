# standard metadata is useful

    List of 7
     $ file       : chr "df.rds"
     $ file_size  : int 200
     $ pin_hash   : chr "db696042be80dbb4"
     $ type       : chr "arrow"
     $ description: chr "A pinned 10 x 1 data frame"
     $ created    : chr "<TODAY>"
     $ api_version: num 1

# newer version triggers error

    Metadata requires pins 2.0.0 or greater
    i Do you need to upgrade the pins package?

# produces reasonable default descriptions

    Code
      default_description(NULL, c("data.csv"))
    Output
      [1] "A pinned .csv file"
    Code
      default_description(NULL, c("data.csv", "foo.csv"))
    Output
      [1] "A pinned 2 files"
    Code
      default_description(mtcars, "data.csv")
    Output
      [1] "A pinned 32 x 11 data frame"
    Code
      default_description(1:10, "data.csv")
    Output
      [1] "A pinned integer vector"

