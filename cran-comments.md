## Resubmission
This is a resubmission. In this version I have:

* Updated host prefixes to align with WSL's adoption of https
* Corrected an error in getBillSponsors that created illegal URLs
* Eliminated notes and bugs related to recent updates from curl

## Test environments
* local Windows 10, R 4.1.0
* win_builder, r-devel
* debian (on Rhub), R 4.1.2
* fedora (on Rhub), r-devel
* linux (on Rhub), R 4.1.2
* solaris 10 (on Rhub), 4.1.1
* local OS X install, R 4.0.2
* ubuntu 16.04.6 (on travis-ci), R 4.0.2

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs

## Downstream dependencies
There are currently no downstream dependencies for this package
