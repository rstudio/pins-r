# Prepare a data dictionary pin bundle

This example prepares a standard pin bundle for Posit Connect.
The script stores dictionary YAML in `user.data_dict` and includes the dictionary page in the pin preview.
The default `CONNECT_SERVER` value is `https://connect.example.com`, so bundle creation works offline.
Set `CONNECT_SERVER` to your server URL when preparing a deployment bundle.
The script uses sample data.

Run from the repository root with the development dependencies installed:

```{.sh filename="Terminal"}
Rscript examples/data-dict/create-bundle.R /tmp/pins-r-data-dict-bundle examples/data-dict/data-dict.yaml
Rscript examples/data-dict/create-bundle.R /tmp/pins-r-standard-pin-bundle --no-dictionary
```

The output directory must be empty.
Replace the YAML path with your authored dictionary.
Omit the YAML path to draft a dictionary, or pass `--no-dictionary` to prepare a standard pin bundle.
Authored dictionaries must omit table `source` fields.
When a bundle contains one Parquet file and one dictionary table, the renderer adds the bundle-relative path to a temporary copy and embeds column profiles in the dictionary page.
You can also pass YAML text through `metadata[["data_dict"]]` when calling `pin_write()`.
Dictionary-only changes require `force_identical_write = TRUE` because pins compares data hashes when deciding whether to write.
Review draft `todo` notes before relying on them.

Drafting and rendering require the optional datadict package and its CLI.
Install them with:

```{.r filename="Terminal"}
pak::pak("tidyverse/data-dict/r")
datadict::dd_install(version = "0.0.3")
```

For direct writes to a Connect board, set `CONNECT_SERVER` to your server URL and configure an `rsconnect` account for that server:

```{.r filename="Terminal"}
server <- Sys.getenv("CONNECT_SERVER", unset = "https://connect.example.com")
board <- pins::board_connect(server = server, auth = "rsconnect")
data <- data.frame(id = 1:3, value = c(10, 20, 30))
pins::pin_write(board, data, name = "dictionary-example", type = "parquet", data_dict = TRUE)
pins::pin_write(board, data, name = "dictionary-example", type = "parquet", data_dict = "data-dict.yaml", force_identical_write = TRUE)
```

To deploy the prepared manifest, set `CONNECT_ACCOUNT` to a saved rsconnect-python account name for your server:

```{.sh filename="Terminal"}
uv tool run --from rsconnect-python rsconnect list
env -u CONNECT_SERVER uv tool run --from rsconnect-python rsconnect deploy manifest /tmp/pins-r-data-dict-bundle/manifest.json --name "$CONNECT_ACCOUNT" --new --title 'Pins R data dictionary' --no-metadata
```

The bundle keeps the pin metadata and Parquet file alongside the rendered dictionary page.
The manifest sets `index.html` as the primary page.
