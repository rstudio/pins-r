# standard metadata is useful

    List of 7
     $ file       : chr "df.rds"
     $ file_size  : int 159
     $ pin_hash   : chr "d0e746d5ea7ef897"
     $ type       : chr "arrow"
     $ description: chr "A pin containing a data frame with 10 rows and 1 columns"
     $ created    : chr "<TODAY>"
     $ api_version: num 1

# newer version triggers error

    Metadata requires pins 2.0.0 or greater
    i Do you need to upgrade the pins package?

# produces reasonable default descriptions

    Code
      default_description(NULL, c("data.csv"))
    Output
      [1] "A pin containing a .csv file"
    Code
      default_description(NULL, c("data.csv", "foo.csv"))
    Output
      [1] "A pin containing 2 files"
    Code
      default_description(mtcars, "data.csv")
    Output
      [1] "A pin containing a data frame with 32 rows and 11 columns"
    Code
      default_description(1:10, "data.csv")
    Output
      [1] "A pin containing an integer vector"

