IC Temporal Pattern Discovery
=============================


Introduction
============

This R package is an implementation of the IC Temporal Pattern Discovery method to estimate risk by combining a self-controlled and cohort design. It is designed to run against observational databases in the OMOP Common Data Model.

Features
========
- Todo

Examples
========
Todo

Technology
============
IcTemporalPatternDiscovery is an R package.

System Requirements
============
Requires R (version 3.1.0 or higher). Libraries used in this package require Java.

Dependencies
============
 * DatabaseConnector
 * SqlRender

Getting Started
===============
1. The DatabaseConnector and SqlRender packages require Java. Java can be downloaded from
<a href="http://www.java.com" target="_blank">http://www.java.com</a>.
2. In R, use the following commands to download and install IcTemporalPatternDiscovery:

  ```r
  install.packages("devtools")
  library(devtools)
  install_github("ohdsi/SqlRender") 
  install_github("ohdsi/DatabaseConnector") 
  install_github("ohdsi/IcTemporalPatternDiscovery") 
  ```

Getting Involved
=============
* Package manual: [IcTemporalPatternDiscovery.pdf](https://raw.githubusercontent.com/OHDSI/IcTemporalPatternDiscovery/master/man/IcTemporalPatternDiscovery.pdf) 
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="../../issues">GitHub issue tracker</a> for all bugs/issues/enhancements
 
License
=======
IcTemporalPatternDiscovery is licensed under Apache License 2.0

Development
===========
IcTemporalPatternDiscovery is being developed in R Studio.

###Development status

Alpha


# Acknowledgements
This package was developed by Tomas Bergvall
