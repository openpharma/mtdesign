## Third resubmission
In DESCRIPTION, I have:
* Amended the date record to `Date: 2022-10-04`.
* Amended DOI references from `<https://doi.org/10...` to `<doi:10...>.

## Second resubmission
I have

*  replaced the original `Author:` field in DESCRIPTION with `Authors@R: person(...)`
*  deleted the `Maintainer:` field in DESCRIPTION as it is now covered by roles="cre" `Authors@R:`
*  added `doi` links to the references in DESCRIPTION
*  added a check to README.Rmd to limit the number of cores used to two if `!identical(Sys.getenv("NOT_CRAN"), "true")`
*  checked example code to ensure that all calls to `augmentGrid` have either `parallel=FALSE` or `cores=2`.  Similarly, calls to `obtainDesign` are either in a pipe containing a call to `augmentGrid` with such parameters or have `parallel=FALSE` themselves.

## Resubmission
In this resubmission I have 

* Corrected the URL that links to the original Mander & Thompson paper.  My apologies for the original error.
* Added a "first release" section to NEWS.md"

I have *not* corrected the flagged potential misspelling of "Mander" in the README document.  As indicated above,  "Mander" is the correct spelling.  It is the surname of prof Adrian Mander, the first author of the paper whose methods the package implements.  Therefore, no correction is required.

These were the only two issues raised during the automated validation of my first submission.

## R CMD check results

Duration: 5m 41.1s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## check_rhub_release() results
Ran `devtools::check_rhub()`.  Three builds were successful.  The build for Debian Linux failed with a PREPERROR.

### Debian Linux
I have found two types of error in the build log

* (First occurrence on line 176) Bioconductor does not yet build and check packages for R version 4.3
  
  This is outside my control.
  
* Various C++ compile time errors, such as 
  * /home/docker/R/BH/include/boost/math/tools/config.hpp:275:32: error: expected initializer before ‘noexcept’
      3862#> 275 | #define BOOST_MATH_NOEXCEPT(T) noexcept(std::is_floating_point::value)
  * /home/docker/R/BH/include/boost/math/tools/config.hpp:400:11: error: ‘constexpr’ does not name a type
      3880#> 400 | static constexpr bool value = std::is_integral::value || (std::numeric_limits::is_specialized && std::numeric_limits::is_integer);
  * /home/docker/R/BH/include/boost/math/tools/promotion.hpp:62:53: error: expected nested-name-specifier before ‘type’
      3891#> 62 | template <> struct promote_arg{ using type = double; };
      
  My understanding is that these errors are caused by incompatibilities between an out-of-date g++ compiler and the header files provided by the `BH` package.

[https://builder.r-hub.io/status/mtdesign_0.1.0.tar.gz-8bd9afe9fa324b7e81e49aad1184f785]


The NOTES reported by the other builds were:

### All operating systems

* This is a new submission

* Possibly misspelled words in DESCRIPTION:
  Mander (3:8, 7:25)
  
  "Mander" is the correct spelling

* checking for future file timestamps ... NOTE unable to verify current time

  Outside my control: Please see [https://stackoverflow.com/questions/63613301/r-cmd-check-note-unable-to-verify-current-time]
  
### Windows Server 2022

* checking for detritus in the temp directory ... NOTE Found the following files/directories: 'lastMiKTeXException'
 
  Please see [https://github.com/r-hub/rhub/issues/503] 

[https://builder.r-hub.io/status/mtdesign_0.1.0.tar.gz-3e29716188ba4734bbfa3aa3456d4bb7]
  
### Ubuntu Linux 20.04.1
No other notes.

[https://builder.r-hub.io/status/mtdesign_0.1.0.tar.gz-3888146ebfd343d58ac65dc18738eb09]

### Fedora Linux 
* checking HTML version of manual ... NOTE Skipping checking HTML validation: no command 'tidy' found

  I cannot add Tidy to the PATH, nor update Tidy on the external Fedora Linux server.
  
[https://builder.r-hub.io/status/mtdesign_0.1.0.tar.gz-a65ee4600cd74a23b54d390171868ea9]

## Downstream dependencies
There are currently no downstream dependencies for this package: it is a new package.
  
## Other

* This is a first submission
