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

# metadata checking functions give correct errors

    `tags` must be a character vector.

---

    `metadata` must be a list.

---

    `tags` must be a character vector.

---

    `metadata` must be a list.

# can deparse

    Code
      board_deparse(board)
    Output
      board_azure(AzureStor::storage_container("https://cipins2.dfs.core.windows.net/pins-rstats-testing-ci"), 
          path = "test/path")

