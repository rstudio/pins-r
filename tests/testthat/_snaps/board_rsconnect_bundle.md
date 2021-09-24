# generates index files

    Code
      board <- list(account = "TEST", server_name = "example.com")
      class(board) <- c("pins_board_rsconnect", "pins_board")
      df <- data.frame(x = 1:2, y = 2:1)
      metadata <- list(file = "test.csv")
      cat(rsc_bundle_preview_index(board, "test", df, metadata))
    Output
      <!doctype html>
      <html>
        <head>
          <link rel="stylesheet" href="pagedtable-1.1/pagedtable.css">
          <script language="javascript" src="pagedtable-1.1/pagedtable.js"></script>
          <link rel="stylesheet" href="highlight.js-9.15.9/qtcreator_light.css">
          <script src="highlight.js-9.15.9/highlight.js"></script>
          <style>
            body {
              font-size: 16px;
              font-family: 'Lato', sans-serif;
              color: #333;
            }
            section {
              border-left: solid 6px #dddddd;
              padding: 0.5em 0 0.5em 10px;
              margin-bottom: 1em;
            }
            pre { margin: 0 }
            h3 {
              font-weight: normal;
              color: #888;
              margin: 0 0 0.5em 0;
            }
          </style>
        </head>
        <body>
          <section>
          <h3>Code</h3>
            <pre id="pin-r" class="pin-code"><code class="r">library(pins)
      board <- board_rsconnect(server = "example.com")
      pin_read(board, "TEST/test")</code></pre>
          <script type="text/javascript">
            hljs.registerLanguage("r", highlight_r);
            hljs.initHighlightingOnLoad();
          </script>
          </section>
      
          <section>
            <h3>Raw data</h3>
            <div class="pin-download">
              <a href="test.csv">test.csv</a>
            </div>
          </section>
      
          <section>
            <h3>Metadata</h3>
            <pre>{
        "file": "test.csv"
      }</pre>
          </section>
      
          <section style="">
            <h3>Preview <small>(up to 100 rows)</small></h3>
            <div data-pagedtable style="height: 25em;">
              <script data-pagedtable-source type="application/json">
                {"data":[{"x":1,"y":2},{"x":2,"y":1}],"columns":[{"name":"x","label":"x","align":"right","type":""},{"name":"y","label":"y","align":"right","type":""}],"options":{"columns":{"max":10},"rows":{"min":1,"total":2}}}
              </script>
            </div>
          </section>
        </body>
      </html>

# generates preview data

    Code
      df <- data.frame(x = 1:2, y = c("a", "b"), stringsAsFactors = FALSE)
      str(rsc_bundle_preview_data(df))
    Output
      List of 3
       $ data   :'data.frame':	2 obs. of  2 variables:
        ..$ x: int [1:2] 1 2
        ..$ y: chr [1:2] "a" "b"
       $ columns:List of 2
        ..$ :List of 4
        .. ..$ name : chr "x"
        .. ..$ label: chr "x"
        .. ..$ align: chr "right"
        .. ..$ type : chr ""
        ..$ :List of 4
        .. ..$ name : chr "y"
        .. ..$ label: chr "y"
        .. ..$ align: chr "left"
        .. ..$ type : chr ""
       $ options:List of 2
        ..$ columns:List of 1
        .. ..$ max: num 10
        ..$ rows   :List of 2
        .. ..$ min  : num 1
        .. ..$ total: int 2

---

    Code
      str(rsc_bundle_preview_data(NULL))
    Output
      List of 2
       $ data   : list()
       $ columns: list()

