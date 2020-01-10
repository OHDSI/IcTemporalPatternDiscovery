IC Temporal Pattern Discovery
=============================

[![Build Status](https://travis-ci.org/OHDSI/IcTemporalPatternDiscovery.svg?branch=master)](https://travis-ci.org/OHDSI/IcTemporalPatternDiscovery)

Introduction
============

# Please note that the development branch is under development, and is currently not recommended for use.

This R package is an implementation of the IC Temporal Pattern Discovery method to estimate risk by combining a self-controlled and cohort design. IC stands for Information component and is log2 of a shrunken observed-to-expected ratio. The package is designed to run against observational databases in the OMOP Common Data Model.

Features
========
- Extracts the necessary data from a database in OMOP Common Data Model format.
- Supports various risk and control window definitions.
- Create Chronographs (a two-panel longitudinal plot of IC and observed/expected counts) to explore time-to-event relationships.

Example
=======
```r
library(SelfControlledCohort)

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
Requires R (version 3.1.0 or higher). Libraries used in this package require Java.

Installation
============
1. The DatabaseConnector and SqlRender packages require Java. Java can be downloaded from
<a href="http://www.java.com" target="_blank">http://www.java.com</a>.
2. In R, use the following commands to download and install IcTemporalPatternDiscovery:

  ```r
  install.packages("drat")
  drat::addRepo("OHDSI")
  install.packages("IcTemporalPatternDiscovery")
  ```

User Documentation
==================
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
This package was developed by Tomas Bergvall, adapted by Martijn Schuemie and modified by Ola Caster and Oskar Gauffin
