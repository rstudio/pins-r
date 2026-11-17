# generates index files

    <!doctype html>
    <html lang="en">
      <head>
        <meta charset="utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <script src="theme.js"></script>
        <title>TEST/test · Data pin</title>
        <link rel="stylesheet" href="pagedtable-1.1/pagedtable.css">
        <script src="pagedtable-1.1/pagedtable.js" defer></script>
        <link rel="stylesheet" href="highlight.js-9.15.9/qtcreator_light.css">
        <script src="highlight.js-9.15.9/highlight.js" defer></script>
        <link rel="stylesheet" href="pin.css">
        <script src="pin.js" defer></script>
      </head>
      <body>
        <a class="skip-link" href="#main">Skip to content</a>
        <header class="topbar">
          <div class="topbar-inner">
            <div class="brand">
              <svg class="brand-mark" viewBox="0 0 32 32" fill="none" aria-hidden="true"><path d="M6 8L16 3l10 5v16l-10 5-10-5V8zM6 8l10 5 10-5M16 13v16M6 16l10 5 10-5" stroke="currentColor" stroke-width="1.8" stroke-linejoin="round"/></svg>
              Posit Connect <small>Shared data</small>
            </div>
            <span class="badge">Data pin</span>
          </div>
        </header>
        <main id="main">
          <header class="hero">
            <p class="eyebrow">Published from R</p>
            <h1>TEST/test</h1>
            <p class="description">Some simple data to test with</p>
            <dl class="facts">
              <div><dt>Format</dt><dd>rds</dd></div>
              <div><dt>Last updated</dt><dd>2021-11-11 11:39:56 UTC</dd></div>
              <div><dt>Pin API</dt><dd>v1.0</dd></div>
              <div><dt>Content hash</dt><dd>77fee172a9275a62</dd></div>
            </dl>
          </header>
          <nav class="views" aria-label="Pin views">
            <a href="#overview" data-pin-view="overview" aria-current="page">Overview</a>
            <a href="#metadata" data-pin-view="metadata">Metadata</a>
          </nav>
          <section id="overview" data-pin-panel aria-label="Pin overview">
            <div class="overview-grid">
              <div class="stack">
                <section class="card">
                  <div class="card-head"><div><h2>Data preview</h2><p>Up to 100 rows from this version</p></div></div>
                  <div class="table-wrap">
                    <div data-pagedtable>
                      <script data-pagedtable-source type="application/json">{"data":[{"x":1,"y":2},{"x":2,"y":1}],"columns":[{"name":"x","label":"x","align":"right","type":""},{"name":"y","label":"y","align":"right","type":""}],"options":{"columns":{"max":10},"rows":{"min":2,"total":2,"max":10}}}</script>
                    </div>
                  </div>
                </section>
                <section class="card" aria-labelledby="use-data">
                  <div class="card-head"><div><h2 id="use-data">Use this data</h2><p>Connect with your account, then read the pin.</p></div></div>
                  <div class="code-block">
                    <div class="code-label"><h3>R</h3><button class="copy" type="button" data-copy-code="pin-r" aria-label="Copy R code" hidden>Copy</button></div>
                    <pre class="pin-code"><code id="pin-r" class="r">library(pins)
    
    board &lt;- board_connect(auth = &quot;envvar&quot;)
    pin_read(board, &quot;TEST/test&quot;)</code></pre>
                  </div>
                </section>
              </div>
              <aside class="stack sidebar" aria-label="Files">
                <section class="card">
                  <div class="card-head"><h2>Download files</h2></div>
                  <div class="card-body files">
                    <a class="file-link" href="test.csv" download><span>test.csv</span><svg width="16" height="16" viewBox="0 0 20 20" fill="none" aria-hidden="true"><path d="M10 3v9m-4-4 4 4 4-4M4 13v4h12v-4" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/></svg></a>
                    <p class="helper">Files from this published version.</p>
                  </div>
                </section>
              </aside>
            </div>
          </section>
          <section id="metadata" class="stack" data-pin-panel aria-label="Pin metadata">
            <section class="card">
              <div class="card-head"><h2>Pin metadata</h2></div>
              <div class="card-body"><p class="helper">test: a pinned 2 x 2 data frame</p></div>
              <div class="metadata"><pre>file: test.csv
    file_size: 2908.0
    pin_hash: 77fee172a9275a62
    type: rds
    title: 'test: a pinned 2 x 2 data frame'
    description: Some simple data to test with
    urls:
    - https://posit.co/
    - https://www.tidyverse.org/
    created: 20211111T113956Z
    api_version: '1.0'
    user:
      my_meta: User defined metadata
    </pre><p class="metadata-download"><a href="data.txt" download>Download metadata</a></p></div>
            </section>
            <section class="card">
              <div class="card-head"><h2>Related resources</h2></div>
              <div class="card-body related-links">
                <a href="https://posit.co/" rel="noopener">https://posit.co/</a><a href="https://www.tidyverse.org/" rel="noopener">https://www.tidyverse.org/</a>
              </div>
            </section>
          </section>
          <p class="footer">Published with pins for R · Data and metadata stay together in this pin.</p>
        </main>
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
    Code
      str(rsc_bundle_preview_data(df, preview = FALSE))
    Output
      List of 2
       $ data   : list()
       $ columns: list()

---

    Code
      str(rsc_bundle_preview_data(NULL))
    Output
      List of 2
       $ data   : list()
       $ columns: list()

