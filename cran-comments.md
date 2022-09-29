## R CMD check results

Duration: 5m 35.9s

❯ checking top-level files ... NOTE
  Non-standard files/directories found at top level:
    ‘codecov.yml’ ‘cran-comments.md’ ‘LICENSE.md’ ‘mtdesign.bib’
    ‘README.Rmd’

0 errors ✔ | 0 warnings ✔ | 1 note ✖

## check_rhub_release() results
Ran `devtools::check_rhub()`.  Three builds were successful.  The build for Debian Linux failed with a PREPERROR.  The build log was empty.

The NOTES reported by the other builds were:

### All operating systems

* Possibly misspelled words in DESCRIPTION:
  Mander (3:8, 7:25)
  
  "Mander" is the correct spelling

* checking for future file timestamps ... NOTE unable to verify current time

  Outside my control: Please see [https://stackoverflow.com/questions/63613301/r-cmd-check-note-unable-to-verify-current-time]
  
### Windows Server 2022

* checking for detritus in the temp directory ... NOTE Found the following files/directories: 'lastMiKTeXException'
 
  Please see [https://github.com/r-hub/rhub/issues/503] 
  
### Ubuntu Linux 20.04.1
No other notes.

### Fedora Linux 
* checking HTML version of manual ... NOTE Skipping checking HTML validation: no command 'tidy' found
  
## Other

* This is a first submission
* This is a new release.
