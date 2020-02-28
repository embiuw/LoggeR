# LoggeR
R package for reading and pre-processing whale and seabird sightings data collected using the IFAW Logger software.
Logger stores data in a Microsoft Access database, and the LoggeR package contains functions for reading data directly from such a database.
The package also contains function for initial pre-processing, such as correcting on- and off effort periods, and filtering out erroneous GPS data points, and then fill in missing GPS data in all tables.
Finally, the package also contains a function for plotting sightings on an interactive map, using the `R leaflet` package. 
The user can zoom and pan in the map, and sightings (represented by dots) can be clicked to obtain a tooltip containing information about the sighting.

NOTE 1. The Access functions will only work on Windows, while the PostgreSQL functions should be platform-independent.

NOTE 2. For the package to recognise the Access databases, they will have to be set up as 32-bit ODBC data sources. See [here](https://support.office.com/en-us/article/administer-odbc-data-sources-b19f856b-5b9b-48c9-8b93-07484bfab5a7) for instructions.

NOTE 3. R (RStudio) must to run in 32-bit mode, otherwise the Access functions will not run.

NOTE 4. In order to display the map background tiles, the computer must be online. Offline use requires map tiles to have been previously downloaded (using e.g. the RGoogleMaps package, see example [here](http://rgooglemaps.r-forge.r-project.org/OfflineMaps-RgoogleMaps-leaflets.html)).


# Installation
On PC's running Windows, ensure you have installed [Rtools](https://cran.r-project.org/bin/windows/Rtools/). Install `devtools` and its dependencies, and finally install the `LoggeR` package:

```R
install('devtools')
devtools::install_github("embiuw/LoggeR")
```
