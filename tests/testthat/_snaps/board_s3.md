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

# can deparse

    Code
      board_deparse(board)
    Output
      board_s3(bucket = "pins-test-hadley", region = "us-east-2")

