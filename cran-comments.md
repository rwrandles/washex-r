## Test environments
* local OS X install, R 4.0.2
* ubuntu 16.04.6 (on travis-ci), R 4.0.2
* solaris 10 (on Rhub), patched
* debian (on Rhub), patched
* local Windows 10, patched
* win_builder, devel

## R CMD check results
There were no ERRORs or WARNINGs

There are 2 NOTEs:

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Rohnin Randles <rrandles@princeton.edu>' 
  New submission
  
  This is the initial submission for this package.

* checking examples ... NOTE
  Examples with CPU (user + system) or elapsed time > 5s
  
  The package is designed to retrieve data from a web database. 
  Some functions are designed to retrieve a year's worth (or more) of legislative data.
  This also means that build time is dependent on the machine's network connection.

## Downstream dependencies
There are currently no downstream dependencies for this package
