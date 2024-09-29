# Deparse works

    Code
      board_databricks("THIS-IS-A-TEST", host = "NOT-A-HOST")
    Output
      Pin board <pins_board_databricks>
      Cache size: 0

# can find board required pkgs

    Code
      required_pkgs(board)
    Output
      [1] "httr2"

