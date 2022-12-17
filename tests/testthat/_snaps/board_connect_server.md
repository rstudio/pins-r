# auth='auto' picks appropriate method and error if none found

    Code
      check_auth()
    Condition
      Error in `check_auth()`:
      ! auth = `auto` has failed to find a way to authenticate:
      * `server` and `key` not provided for `auth = 'manual'`
      * Can't find CONNECT_SERVER and CONNECT_API_KEY envvars for `auth = 'envvar'`
      * Can't find any rsconnect::accounts() for `auth = 'rsconnect'`

# delivers useful messages if can't find RSC account

    Code
      rsc_server_rsconnect()
    Condition
      Error in `rsc_server_rsconnect()`:
      ! No Posit Connect servers have been registered

---

    Code
      rsc_server_rsconnect()
    Condition
      Error in `rsc_server_rsconnect()`:
      ! Found multiple matching Posit Connect servers
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
    Condition
      Error in `rsc_server()`:
      ! Can't find CONNECT_SERVER env var

---

    Code
      rsc_server("envvar")
    Condition
      Error in `rsc_server()`:
      ! Can't find CONNECT_API_KEY env var

---

    Code
      rsc_server("envvar", server = "", key = "")
    Condition
      Error in `rsc_server()`:
      ! Can't find CONNECT_API_KEY env var

