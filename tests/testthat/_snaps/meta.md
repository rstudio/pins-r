# standard metadata is useful

    List of 9
     $ file       : chr "df.rds"
     $ file_size  : int 200
     $ pin_hash   : chr "db696042be80dbb4"
     $ type       : chr "arrow"
     $ title      : chr "title"
     $ description: NULL
     $ tags       : NULL
     $ created    : chr "<TODAY>"
     $ api_version: int 1

# newer version triggers error

    Metadata requires pins 2.0.0 or greater
    i Do you need to upgrade the pins package?

# produces reasonable default title

    Code
      default_title("name", path = c("data.csv"))
    Output
      [1] "name: a pinned .csv file"
    Code
      default_title("name", path = c("data.csv", "foo.csv"))
    Output
      [1] "name: 2 pinned files"
    Code
      default_title("name", data = mtcars)
    Output
      [1] "name: a pinned 32 x 11 data frame"
    Code
      default_title("name", data = 1:10)
    Output
      [1] "name: a pinned integer vector"

