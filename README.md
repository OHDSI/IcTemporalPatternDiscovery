IC Temporal Pattern Discovery
=============================

[![Build Status](https://github.com/OHDSI/IcTemporalPatternDiscovery/workflows/R-CMD-check/badge.svg)](https://github.com/OHDSI/IcTemporalPatternDiscovery/actions?query=workflow%3AR-CMD-check)
[![codecov.io](https://codecov.io/github/OHDSI/IcTemporalPatternDiscovery/coverage.svg?branch=main)](https://codecov.io/github/OHDSI/IcTemporalPatternDiscovery?branch=main)

Introduction
============

This R package is an implementation of the IC Temporal Pattern Discovery method to estimate risk by combining a self-controlled and cohort design. It is designed to run against observational databases in the OMOP Common Data Model.

Features
========
- Extracts the necessary data from a database in OMOP Common Data Model format.
- Supports various risk and control window definitions.
- Create Chronographs to explore time-to-event relationships.

Example
=======
```r
library(IcTemporalPatternDiscovery)

connectionDetails <- createConnectionDetails(dbms = "postgresql",
                                             user = "joe",
                                             password = "secret",
                                             server = "myserver")
                                             
exposureOutcomePairs = data.frame(outcomeId = c(196794, 196794, 312648), 
                                  exposurId = c(1501700, 1545958, 1551803))
                                   
ictpdData <- getDbIctpdData(connectionDetails, 
                            cdmDatabaseSchema = "cdm_schema.dbo", 
                            exposureOutcomePairs = exposureOutcomePairs)
                             
ictpdResults <- calculateStatisticsIC(ictpdData)
 
ictpdResults                                             
```

Technology
============
IcTemporalPatternDiscovery is an R package.

System Requirements
============
Requires R (version 4.0.0 or higher). Libraries used in this package require Java.

Installation
============

1. See the instructions [here](https://ohdsi.github.io/Hades/rSetup.html) for configuring your R environment, including Java.

2. In R, use the following commands to download and install IcTemporalPatternDiscovery:

  ```r
  install.packages("remotes")
  remotes::install_github("ohdsi/IcTemporalPatternDiscovery")
  ```

User Documentation
==================
Documentation can be found on the [package website](https://ohdsi.github.io/DatabaseConnector/).

PDF versions of the documentation are also available:

* Package manual: [IcTemporalPatternDiscovery.pdf](https://raw.githubusercontent.com/OHDSI/IcTemporalPatternDiscovery/master/extras/IcTemporalPatternDiscovery.pdf) 

Support
=======
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="https://github.com/OHDSI/IcTemporalPatternDiscovery/issues">GitHub issue tracker</a> for all bugs/issues/enhancements

License
=======
IcTemporalPatternDiscovery is licensed under Apache License 2.0

Development
===========
IcTemporalPatternDiscovery is being developed in R Studio.

### Development status

Beta

# Acknowledgements
This package was developed by Tomas Bergvall, adapted by Martijn Schuemie.
