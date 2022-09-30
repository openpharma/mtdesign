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
