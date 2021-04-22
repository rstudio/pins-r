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

