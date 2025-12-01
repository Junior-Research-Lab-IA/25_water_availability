
<!-- README.md is generated from README.Rmd. Please edit that file -->

# airGRccia

<!-- badges: start -->
<!-- badges: end -->

airGRccia is an R package providing tools to facilitate Climate Change
Impact and Adaptation (CCIA) studies with the integrated hydrological
model airGRiwrm.

The vignettes included in the package contain an example of the modeling
of the middle valley of the Herault River, France.

- Source: <https://forgemia.inra.fr/umr-g-eau/airgrccia>
- Documentation: <https://umr-g-eau.pages.mia.inra.fr/airgrccia>

## Installation

### Requirements

A recent version of R (\>4.3.1) must be installed. The use of the
RStudio IDE is recommended.

MacOS and Linux users have to first install the GDAL library. Details of
such installation are available in the documentation of the [sf
package](https://r-spatial.github.io/sf/).

### Get source code

First, clone the source with GIT, or copy them as a ZIP file:

- GIT: <https://forgemia.inra.fr/umr-g-eau/airgrccia.git>
- ZIP:
  <https://forgemia.inra.fr/umr-g-eau/airgrccia/-/archive/main/airgrccia-main.zip>

And then, open the file `airgrccia.Rproj` with RStudio.

### Install dependencies

Type the following instruction in the R console

``` r
install.packages("remotes") # If not already installed
remotes::install_deps(dep = TRUE)
```

## Get started

For using the functions contained in the package you can:

- Load the functions in the working environment with
  `pkgload::load_all()`
- Install the package on your computer with `devtools::install()`

The vignettes in this package contain a complete example of an
integrated semi-distributed hydrological model of the middle valley of
the River Hérault:

- [V01 - Watershed delineation](articles/V01-Watershed_delineation.html)
- [V02 - Hydroclimatic data
  preprocessing](articles/V02-Hydroclimatic_preprocessing.html)
- [V03 - Usages data
  preprocessing](articles/V03-Usages_preprocessing.html)
- [V04 - Dam releases
  preprocessing](articles/V04-Dam_releases_preprocessing.html)
- [V05 - Hydrologic model
  calibration](articles/V05-Hydrologic_model_calibration.html)

The vignettes are using data stored on a cloud at the following URL:
<https://nextcloud.inrae.fr/s/QJS9kxkTPT5fdF5>

For each data file used in the vignettes, there is a call to the
function `getDataPath`. This function automatically download and copy
the file in a temporary folder.

If the data seems to be corrupted, one can reset the temporary folder by
calling `cleanDataCache()`.
