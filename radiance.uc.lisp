{EQL
:STANDALONE T
:INTERFACES {EQL
  :DATABASE "-i-sqlite",
  :DATA-MODEL "-r-simple-model",
  :USER "-r-simple-users",
  :AUTH "-r-simple-auth",
  :SESSION "-r-simple-sessions",
  :LOGGER "-i-verbose",
  :SERVER "-i-hunchentoot"},
:SERVER {EQL
:DOMAINS ("-localhost"),
  :INSTANCES ({EQL :PORT 2015})},
:SQLITE {EQL
  :DEFAULT :AUTOBUILD,
  :CONNECTIONS {EQL
    :AUTOBUILD "d/autobuild.sqlite.db"}}}
