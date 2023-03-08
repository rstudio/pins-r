# can find board required pkgs

    Code
      required_pkgs(board)
    Output
      [1] "paws.storage"

---

    Code
      required_pkgs(board)
    Output
      [1] "paws.storage"

# metadata checking functions give correct errors

    `tags` must be a character vector or `NULL`, not a list.

---

    `metadata` must be a list or `NULL`, not a character vector.

# can deparse

    Code
      board_deparse(board)
    Output
      board_s3(bucket = "pins-test-hadley", region = "us-east-2")

