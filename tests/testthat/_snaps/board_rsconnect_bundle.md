# generates index files

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
          pre {
            margin: 0;
            padding: 1em;
          }
          h3 {
            font-weight: normal;
            color: #888;
            margin: 0 0 0.5em 0;
          }
        </style>
      </head>
      <body>
    
        <section>
           <h3>TEST/test</h3>
           <p>
             <b>Last updated:</b> 2021-11-11 11:39:00 &bull;
             <b>Format:</b> rds &bull;
             <b>API:</b> v1.0
           </p>
           <p></p>
           <p>Download data: <a href="test.csv">test.csv</a></p>
           <details>
             <summary>Raw metadata</summary>
             <pre>file: test.csv
    file_size: 2908.0
    pin_hash: 77fee172a9275a62
    type: rds
    title: 'test: a pinned 2 x 2 data frame'
    desctiption: Some simple data to test with
    created: 20211111T113956Z
    api_version: '1.0'
    user:
      my_meta: User defined metadata
    </pre>
           </details>
         </section>
    
        <section>
        <h3>Code</h3>
          <pre id="pin-r" class="pin-code"><code class="r">library(pins)
    board <- board_rsconnect("envvar", server = "http://example.com")
    pin_read(board, "TEST/test")</code></pre>
        <script type="text/javascript">
          hljs.registerLanguage("r", highlight_r);
          hljs.initHighlightingOnLoad();
        </script>
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

