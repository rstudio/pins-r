# can find board required pkgs

    Code
      required_pkgs(board)
    Output
      [1] "AzureStor"

---

    Code
      required_pkgs(board)
    Output
      [1] "AzureStor"

# can deparse

    Code
      board_deparse(board)
    Output
      board_azure(AzureStor::storage_container("https://cipins2.blob.core.windows.net/pins-rstats-testing-ci"), 
          path = "test/path")

