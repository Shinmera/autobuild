{EQL
:INTERFACES {EQL
  :CACHE "-r-simple-cache",
  :LOGGER "-i-verbose",
  :SERVER "-i-hunchentoot"},
:SERVER {EQL
  :DOMAINS ("-localhost" "-linuz.com" "-radiance.test"),
  :INSTANCES ({EQL :PORT 2015})}}
