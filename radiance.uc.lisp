{EQL
:STANDALONE T
:INTERFACES {EQL
  :DATABASE "-i-sqlite",
  :DATA-MODEL "-r-simple-model",
  :USER "-r-simple-users",
  :AUTH "-r-remote-auth",
  :SESSION "-r-simple-sessions",
  :LOGGER "-i-verbose",
  :SERVER "-i-hunchentoot"},
:SERVER {EQL
:DOMAINS ("-localhost"),
  :INSTANCES ({EQL :PORT 2015})},
:SQLITE {EQL
  :DEFAULT :AUTOBUILD,
  :CONNECTIONS {EQL
    :AUTOBUILD "d/autobuild.sqlite.db"}},
:REMOTE-AUTH {EQL
  :URLS {EQL
    :REQUEST-TOKEN "-http://auth.tymoon.eu/api/oauth/request-token",
    :AUTHENTICATE "-http://auth.tymoon.eu/api/oauth/authenticate",
    :ACCESS-TOKEN "-http://auth.tymoon.eu/api/oauth/access-token",
    :USERNAME "-http://auth.tymoon.eu/api/oauth/username"},
  :API-KEY "-",
  :API-SECRET "-",
  :IF-USER-DOES-NOT-EXIST :CREATE }}
