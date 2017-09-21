## This is Unfinished
DON'T LOOK AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA

## Components
Autobuild is divided up into multiple components to separate concerns and make things a bit cleaner to manage.

### Build
Provides the low-level build and recipe constructs and operations. This can be used to manually assemble recipes and builds.

### Client
Allows the running of recipes standalone or as a build machine tied to a managing server.

### Manager
Manages builds and recipes and ties them into an event system to allow integration with extensions.

### Repository
A thin interface library to access different kinds of repositories.

### Server
Provides a web interface to manage multiple remote autobuild clients and recipes.
