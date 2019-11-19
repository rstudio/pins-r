library(shiny)
library(pins)

rsApiUpdateDialog <- function(code) {
  if (exists(".rs.api.updateDialog")) {
    updateDialog <- get(".rs.api.updateDialog")
    updateDialog(code = code)
  }
}

rsConnectServers <- function() {
  auth_servers <- rsconnect::accounts()$server
  all_servers <- rsconnect::servers()
  all_servers <- all_servers[!grepl("^shinyapps.io", all_servers$name),]
  valid_urls <- all_servers[all_servers$name %in% auth_servers, ]

  valid_servers <- gsub("/__api__.*", "", valid_urls$url)
  as.character(valid_servers)
}

#' @import rstudioapi
pins_connection_ui <- function() {
  elementSpacing <- if (.Platform$OS.type == "windows") 2 else 7

  tags$div(
    tags$head(
      tags$style(
        HTML(paste("
          body {
            background: none;

            font-family : \"Lucida Sans\", \"DejaVu Sans\", \"Lucida Grande\", \"Segoe UI\", Verdana, Helvetica, sans-serif;
            font-size : 12px;
            -ms-user-select : none;
            -moz-user-select : none;
            -webkit-user-select : none;
            user-select : none;

            margin: 0;
            margin-top: 7px;
          }

          select {
            background: #FFF;
          }

          .shiny-input-container {
            min-width: 100%;
            margin-bottom: ", elementSpacing, "px;
            display: table;
          }

          .shiny-input-container > .control-label,
          .shiny-input-container > label {
            display: table-cell;
            min-width: 195px;
          }

          .shiny-input-container > div {
            display: table-cell;
            width: 300px;
          }

          .shiny-input-container > .input-group {
            width: 100%;
          }

          .shiny-input-container > .input-group > input {
            border: solid 1px #A2A2A2;
            border-radius: 3px;
            padding: 2px;
          }

          .shiny-input-container > .input-group > .input-group-btn {
            margin-right: 6px;
            border: solid 1px #A2A2A2;
            border-radius: 3px;
            padding: 1px 6px 1px 6px;
          }

          .shiny-input-container > .input-group > input[type=\"text\"] {
            width: 218px;
          }

          .shiny-input-container .progress-bar { display: none; }

          #shiny-disconnected-overlay {
            display: none;
          }

          .token-label {
            text-align: right;
            margin-right: 14px;
          }

          .shiny-input-container > label {
            display: table-cell;
          }

          .shiny-input-container > input[type=\"text\"] {
            border: 1px solid rgba(0,0,0,0.3);
            border-radius: 4px;
            margin-left: 4px;
            width: 290px;
            padding: 2px 2px 2px 6px;
          }
        ", sep = ""))
      )
    ),
    tags$div(
      selectInput(
        "board",
        "Board:",
        choices = c(
          list(
            local = "local",
            azure = "azure",
            gcloud = "gcloud",
            github = "github",
            kaggle = "kaggle",
            rsconnect = "rsconnect",
            s3 = "s3",
            website = "datatxt"
          )
        ),
        selectize = FALSE,
        selected = "local"
      ),
      conditionalPanel(
        condition = "input.board == 'rsconnect'",
        selectInput(
          "server",
          "Server:",
          list(),
          selectize = FALSE
        )
      ),
      conditionalPanel(
        condition = "input.board == 'kaggle'",
        fileInput(
          "token",
          "Token:",
          placeholder = "Kaggle token file",
          accept = ".json"
        ),
        tags$div(
          "Dowload token file from",
          tags$a(
            "kaggle.com/me/account",
            href = "https://www.kaggle.com/me/account"
          ),
          class = "token-label"
        )
      ),
      conditionalPanel(
        condition = "input.board == 'github'",
        textInput(
          "repo",
          "Repo:",
          value = "owner/repo"
        ),
        textInput(
          "branch_path",
          "Branch/Path:",
          value = "master"
        ),
        conditionalPanel(
          condition = "output.showOptional",
          tags$div(
            "Retrieve token from",
            tags$a(
              "github.com/settings/tokens",
              href = "https://github.com/settings/tokens"
            ),
            class = "token-label"
          )
        )
      ),
      conditionalPanel(
        condition = "input.board == 'datatxt'",
        textInput(
          "datatxt_url",
          "Url:",
          value = "https://datatxt.org"
        ),
        tags$div(
          "Using the ",
          tags$a(
            "datatxt.org",
            href = "https://datatxt.org"
          ),
          "specification",
          class = "token-label"
        )
      ),
      conditionalPanel(
        condition = "input.board == 'gcloud'",
        textInput(
          "gcloud_bucket",
          "Bucket:",
          value = ""
        ),
        tags$div(
          "Requires Google Cloud SDK and bucket",
          tags$br(),
          "from ",
          tags$a(
            "console.cloud.google.com",
            href = "https://console.cloud.google.com/"
          ),
          class = "token-label"
        )
      )
    ),
    conditionalPanel(
      condition = "input.board == 'azure'",
      textInput(
        "azure_container",
        "Container:",
        value = ""
      ),
      conditionalPanel(
        condition = "output.showOptional",
        textInput(
          "azure_account",
          "Account:",
          value = ""
        )
      ),
      tags$div(
        "Requires an Azure container from",
        tags$a(
          "portal.azure.com",
          href = "https://portal.azure.com/"
        ),
        class = "token-label"
      )
    ),
    conditionalPanel(
      condition = "input.board == 's3'",
      textInput(
        "s3_bucket",
        "Bucket:",
        value = ""
      ),
      conditionalPanel(
        condition = "output.showOptional",
        textInput(
          "s3_key",
          "Key:",
          value = ""
        )
      ),
      tags$div(
        "Requires an S3 bucket from",
        tags$a(
          "amazonaws.com",
          href = "http://amazonaws.com/"
        ),
        class = "token-label"
      )
    ),
    tags$div(
      style = paste("display: table-row; height: 10px")
    )
  )
}

pins_connection_server <- function(input, output, session) {
  output$showOptional <- reactive({
    if (identical(input$board, "github")) {
      nchar(Sys.getenv("GITHUB_PAT")) == 0
    }
    else if (identical(input$board, "azure")) {
      nchar(Sys.getenv("AZURE_STORAGE_ACCOUNT")) == 0
    }
    else if (identical(input$board, "s3")) {
      nchar(Sys.getenv("AWS_ACCESS_KEY_ID")) == 0
    }
    else {
      TRUE
    }
  })
  outputOptions(output, 'showOptional', suspendWhenHidden = FALSE)

  observe({
    if (identical(input$board, "rsconnect")) {
      updateSelectizeInput(
        session,
        "server",
        choices = rsConnectServers()
      )
    }
  })

  generateCode <- function(board) {
    parameters <- ""
    initializer <- paste0("pins::board_register(\"", board, "\"", parameters, ")")

    if (identical(board, "rsconnect") && !is.null(input$server)) {
      initializer <- paste(
        "pins::board_register(\"rsconnect\", ",
        "server = \"", input$server, "\")\n", sep = "")
    }
    else if (identical(board, "kaggle") && !is.null(input$token)) {
      initializer <- tryCatch({
        contents <- jsonlite::read_json(input$token$datapath)

        if (!dir.exists("~/.kaggle")) dir.create("~/.kaggle")

        paste(
          "jsonlite::write_json(list(",
          paste(names(contents), " = \"", contents, "\"", collapse = ", ", sep = ""),
          "), \"~/.kaggle/kaggle.json\", auto_unbox = TRUE)\n",
          "\n",
          "pins::board_register(\"kaggle\")\n",
          sep = "")
      }, error = function(e) {
        rstudioapi::showDialog("Invalid Token", paste("Failed to parse the Kaggle token file:", e$message))
        ""
      })
    }
    else if (identical(board, "github") && !identical(input$repo, "owner/repo")) {
      path_parts <- strsplit(input$branch_path, "/")[[1]]
      branch <- path_parts[1]
      path <- if (length(path_parts) > 1) paste(path_parts[2:length(path_parts)], sep = "/") else ""

      repo_parts <- strsplit(input$repo, "/")[[1]]
      repo_name <- repo_parts[2]

      retrieve_token <- ""
      if (nchar(Sys.getenv("GITHUB_PAT")) == 0) {
        retrieve_token <- ", token = rstudioapi::askForSecret(\"github_pat\", \"Your GitHub Personal Access Token\", \"GitHub PAT\")"
      }

      initializer <- paste0(
        "pins::board_register(\"github\", ",
        "name = \"", repo_name, "\", ",
        "repo = \"", input$repo, "\"",
        ifelse(nchar(path) == 0, "", paste0(", path = \"", path, "\"")),
        ifelse(identical(branch, "master"), "", paste0(", branch = \"", branch, "\"")),
        retrieve_token,
        ")\n")
    }
    else if (identical(board, "datatxt")) {
      initializer <- paste0(
        "pins::board_register(",
        "\"", input$datatxt_url, "\"",
        ")\n")
    }
    else if (identical(board, "gcloud") && nchar(input$gcloud_bucket) > 0) {
      initializer <- paste(
        "pins::board_register(\"gcloud\", ",
        "name = \"", input$gcloud_bucket, "\", ",
        "bucket = \"", input$gcloud_bucket, "\")\n", sep = "")
    }
    else if (identical(board, "azure") && nchar(input$azure_container) > 0) {
      azure_params <- ""
      if (nchar(Sys.getenv("AZURE_STORAGE_ACCOUNT")) == 0) {
        azure_params <- paste0(", account = \"", input$azure_account, "\"")
      }

      if (nchar(Sys.getenv("AZURE_STORAGE_KEY")) == 0) {
        azure_params <- paste0(
          azure_params,
          ", token = rstudioapi::askForSecret(\"azure_secret\", \"Your Azure Storage Secret\", \"Azure Secret\")"
        )
      }

      initializer <- paste(
        "pins::board_register(\"azure\", ",
        "name = \"", input$azure_container, "\", ",
        "bucket = \"", input$azure_container, "\"", azure_params, ")\n", sep = "")
    }
    else if (identical(board, "s3") && nchar(input$s3_bucket) > 0) {
      s3_params <- ""
      if (nchar(Sys.getenv("AWS_ACCESS_KEY_ID")) == 0) {
        s3_params <- paste0(", key = \"", input$s3_key, "\"")
      }

      if (nchar(Sys.getenv("AWS_SECRET_ACCESS_KEY")) == 0) {
        s3_params <- paste0(
          s3_params,
          ", secret = rstudioapi::askForSecret(\"s3_token\", \"Your S3 Storage Token\", \"S3 Token\")"
        )
      }

      initializer <- paste(
        "pins::board_register(\"s3\", ",
        "name = \"", input$s3_bucket, "\", ",
        "bucket = \"", input$s3_bucket, "\"", s3_params, ")\n", sep = "")
    }

    initializer
  }

  codeReactive <- reactive({
    generateCode(input$board)
  })

  observe({
    rsApiUpdateDialog(codeReactive())
  })
}

shinyApp(pins_connection_ui, pins_connection_server)
