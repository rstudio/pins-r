# can find board required pkgs

    Code
      required_pkgs(board)
    Output
      [1] "googleCloudStorageR"

---

    Code
      required_pkgs(board)
    Output
      [1] "googleCloudStorageR"

# metadata checking functions give correct errors

    `tags` must be a character vector.

---

    `metadata` must be a list.

# can deparse

    Code
      board_deparse(board)
    Output
      board_gcs("pins-dev", prefix = NULL)

