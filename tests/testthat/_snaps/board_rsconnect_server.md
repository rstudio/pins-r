# auth='auto' picks appropriate method and error if none found

    Code
      check_auth()
    Error <rlang_error>
      auth = `auto` has failed to find a way to authenticate:
      * `server` and `key` not provided for `auth = 'manual'`
      * Can't find CONNECT_SERVER and CONNECT_API_KEY envvars for `auth = 'envvar'`
      * Can't find any rsconnect::accounts() for `auth = 'rsconnect'`

# delivers useful messages if can't find RSC account

    Code
      rsc_server_rsconnect()
    Error <rlang_error>
      No RStudio Connect servers have been registered

---

    Code
      rsc_server_rsconnect()
    Error <rlang_error>
      Found multiple matching RStudio Connect servers
      i Please disambiguate with `server` and/or `account`

# auth is hidden

    Code
      server <- rsc_server_manual("http://example.com", "SECRET")
      server$auth
    Output
      <hidden>
    Code
      str(list(1, server$auth, 2))
    Output
      List of 3
       $ : num 1
       $ : <hidden>
       $ : num 2

# clearly errors if env vars missing

    Code
      rsc_server("envvar")
    Error <rlang_error>
      Can't find CONNECT_SERVER env var

---

    Code
      rsc_server("envvar")
    Error <rlang_error>
      Can't find CONNECT_API_KEY env var

---

    Code
      rsc_server("envvar", server = "", key = "")
    Error <rlang_error>
      Can't find CONNECT_API_KEY env var

