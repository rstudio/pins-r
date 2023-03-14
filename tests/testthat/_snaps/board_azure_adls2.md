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

    `tags` must be a character vector or `NULL`, not a list.

---

    `metadata` must be a list or `NULL`, not a character vector.

---

    `tags` must be a character vector or `NULL`, not a list.

---

    `metadata` must be a list or `NULL`, not a character vector.

# can deparse

    Code
      board_deparse(board)
    Output
      board_azure(AzureStor::storage_container("https://cipins2.dfs.core.windows.net/pins-rstats-testing-ci"), 
          path = "test/path")

