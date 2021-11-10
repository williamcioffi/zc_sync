# zc_sync
[![DOI](https://zenodo.org/badge/334234071.svg)](https://zenodo.org/badge/latestdoi/334234071)

This repository contains the data and code used to produce analyses and figures for:

> Cioffi WR, Quick NJ, Foley HJ, Waples DM, Swaim ZT, Shearer JM, Webster DL, Friedlaender AS, Southall BL, Baird RW, Nowacek DP, Read AJ. Adult male Cuvier's beaked whales (*Ziphius cavirostris*) engage in prolonged bouts of synchronous diving. *Marine Mammal Science*. 2021;37:1085–1100. https://doi.org/10.1111/mms.12799.

If you have questions about anything in this repository or are interested in using this dataset please contact wrc14 [at] duke [dot] edu.

# details

directories:
- `00_data` contains input data as well as several intermediate data products.
- `02_figures` contains code to produce all figures included in the MS and ESM. There is a small amount additional data in these directories including GIS files for map creation.
- `01_helper_functions` contains several helper functions used in the analysis. These are loaded by the various analysis scripts and should not need to be manually loaded by the user. Versions of many of these helper functions are available as a part of an R package [sattagutils](https://github.com/williamcioffi/sattagutils).

input data files:
- `00_data/zcsync_behavior.csv` this is the input data for the bulk of the analysis of diving behavior. This is largely in the format of a 'behavior' data stream from a SPLASH10 Wildlife Computers satellite-linked tag. More information can be found at the Wildlife Computers [downloads page](https://wildlifecomputers.com/support/downloads/). The SPLASH10 [manual](https://static.wildlifecomputers.com/SPLASH10-TDR10-User-Guide-3.pdf) and [file descriptions](https://static.wildlifecomputers.com/Spreadsheet-File-Descriptions-3.pdf) are a good place to start.
- `00_data/zcsync_foiegras.csv` These are positions estimated in the script `03_execfoiegras_NOTRUN.r` using [Argos](https://www.argos-system.org/manual/) positions and the [foieGras](https://github.com/ianjonsen/foieGras) package version 0.2.2. `00_data/deploy_locations.csv` are the approximate attachment locations for each tagged animal.
- `rngseed.rdata` This is a random seed for the large simulation (which is set up to run  in parallel). You can use this seed to produce the exact results presented in the manuscript or re-run the simulation with a seed of your choosing.
- `00_data/*.rdata` these are intermediate data products saved and loaded by the scripts.

code files:
- `03_*.r` to `08_*.r` these scripts will produce intermediate data files and analyses and are meant to be run in numbered sequence. All code has been pre-run in this repository and any intermediate and final outputs should be present including figures, but can also be re-run if you desire. See below for software versions used.

Note: `03_execfoiegras_NOTRUN.r` and `04_prepdata_NOTRUN.r` will not run because they depend on the raw unfiltered data not present in this repository, but are included to present the methods used to generate positional estimates and to filter the data for future analysis. The raw data are not included because they contain experimental treatments that are truncated out of the present analysis, but all truncated data are included here. See below for contact information if you have any questions about this or the data.

# software versions

This code was run on `R 3.6.2`.

Additional attached package versions are as follows:
```
beanplot_1.2      marmap_1.0.3      raster_2.9-23     rgeos_0.4-3      
rgdal_1.4-4       coin_1.3-1        survival_3.1-8    doRNG_1.7.1      
rngtools_1.4      pkgmaker_0.27     registry_0.5-1    doParallel_1.0.15
iterators_1.0.10  foreach_1.4.4     sp_1.3-1          sf_0.8-0         
foieGras_0.2.2   
```
# citation

Please cite this dataset as:

> Cioffi WR, Quick NJ, Foley HJ, Waples DM, Swaim ZT, Shearer JM, Webster DL, Friedlaender AS, Southall BL, Baird RW, Nowacek DP, Read AJ. 2021. Dataset and code: Adult male Cuvier's beaked whales (*Ziphius cavirostris*) engage in prolonged bouts of synchronous diving. https://github.com/williamcioffi/zc_sync doi: 10.5281/zenodo.4542749.

Please cite the manuscript as:

> Cioffi WR, Quick NJ, Foley HJ, Waples DM, Swaim ZT, Shearer JM, Webster DL, Friedlaender AS, Southall BL, Baird RW, Nowacek DP, Read AJ. Adult male Cuvier's beaked whales (*Ziphius cavirostris*) engage in prolonged bouts of synchronous diving. *Marine Mammal Science*. 2021;37:1085–1100. https://doi.org/10.1111/mms.12799.

# contact information

If you have questions about anything in this repository or are interested in using this dataset please contact wrc14 [at] duke [dot] edu.
