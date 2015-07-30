# 10th Conference of Parties (Nagoya, Japan) #

This work was performed in view of the 10th Conference of Parties, to be held in Nagoya (Japan). Most of the scripting was done for the R statistical package (R version 2.11.1), and an additional part was done under ArcInfo Workstation (ESRI).

The purpose of the work was to assess the impact of climate change on Andean plant and bird diversity, using current and future spatially explicit climatic datasets, a database of occurrences, and a niche modeling technique named Maxent. We gathered data from GBIF, CONDESAN and the CDC-UNALM (Peru), filtered such data and performed a thorough modeling for a numerous set of species and then assessed the vulnerability of such species under different future climatic scenarios.


## Data structure ##

All the modeling inputs and outputs are stored into the network path \\172.22.33.79/COP\_CONDESAN. Data contained in folders within this directory is as follows:

  * **background-files:** Contains background files used in the modeling. Naming is done using the structure `bg-speciesID.csv`
  * **backgrounds:** Contains 10,000 background samples for each of the Andean countries. These domains were used for pseudo-absences selection, based on the occurrence rule (if a species has an occurrence within a country then 10,000 pseudo absences from that country are used).
  * **climateData:** Contains all current and future climate data used in the modeling. **andes** contains everything used for projection of maxent models, **andesADM0** contains 30s data used for maxent model training, and **global** contains global data used for uncertainties mapping and original GCM data.
  * **correlations:** Contains the result of the correlation analysis done with the script _`IoBio-correl.R`_
  * **documents:** Contains relevant documents that have been shared
  * **maskData:** Contains shapefiles, ESRI grids and ASCII grids used for masking climate data and projections of maxent models
  * **maxent332:** Contains the maxent Java application
  * **mxe\_outputs:** Contains all species models and projections. Each species is stored under a folder with its respective ID. Folders are named as `sp-speciesID`
  * **occurrences:** Contains the species-specific occurrence files used for the modeling (folder `splitted-occurrence-files`). Important file within this folder is `speciesListToModel.csv`
  * **papers:** Relevant literature used in the research
  * **raw-occurrence-data:** Original databases (GBIF, CONDESAN, CDC-UNALM)
  * **summaries:** This folder contains the impact, accuracy and thresholding metrics files concatenated in a single file, and the richness folders.

These folders contain all the information used in the modeling, and also contain the whole set of outputs analyzed in the publication.


## Processing ##

### Occurrence files preparation ###
The process started with the creation of the individual species occurrences files. You need to define a variable that points to the folder where everything is stored:

```
base.folder <- "\\172.22.33.79/COP_CONDESAN"
```

Now you need to set up additional folders for running the first script (`createOccurrenceFiles.R`):

```
occ <- paste(idir, "/occurrences/modeling-data/andean-species-data-sampleArea.csv", sep="")
spL <- paste(idir, "/occurrences/modeling-data/speciesListToModel.csv", sep="")
clDir <- paste(idir, "/climateData/andesADM0/baseline/20C3M/WorldClim-30s-bioclim/1950_2000", sep="")
outDir <- paste(idir, "/occurrences/splitted-occurrence-files", sep="")
```

Now simply source the script and run it appropriately:

```
output <- createOccFiles(occ, spL, clDir, outDir, Initial-species, Final-species)
```

Where initial species are numbers between 1 and the maximum number of taxa (11,012).

```

===Modeling individual species===

Now you need to perform the initial modeling. To do so, type the following into the R console:

source("modelingApproach.R")
setOptions(overwrite=T)

idir <- "\\172.22.33.79/COP_CONDESAN"
ddir <- "\\172.22.33.79/COP_CONDESAN"
outp <- NagoyaProcess(idir, ddir, Initial-Species, Final-Species, OSys="LINUX")
```

`idir` and `ddir` can be the same or different. For efficiency in the model runs, `idir` should be local, but `ddir` should be always the network path. If `idir` is to be local, input data should be placed there.


### Calculating genus-level species richness and turnover ###

Now you'll need to calculate turnover and richness at the genus level:

```
source("speciesRichness.R")
source("turnover.R")
setOptions(overwrite=T)

idir <- "\\172.22.33.79\COP_CONDESAN"

outp <- richnessProcess(idir, type='plants', Initial-Genus, Final-Genus, OSys="LINUX")
outp <- turnoverProcess(idir, type='plants', Initial-Genus, Final-Genus, OSys="LINUX")

outp <- richnessProcess(idir, type='aves', Initial-Genus, Final-Genus, OSys="LINUX")
outp <- turnoverProcess(idir, type='aves', Initial-Genus, Final-Genus, OSys="LINUX")
```


### Calculate total species richness and turnover ###

Now you need to perform the entire calculation (richness and turnover) for the whole set of genera under study. To do this you'll need to perform the following commands:

```
source("totalTurnover.R")
source("totalSpeciesRichness.R")
idir <- "\\172.22.33.79/COP_CONDESAN"

outp <- speciesRichness(idir, type='plants' , OSys='LINUX')
outp <- speciesTurnover(idir, type='plants' , OSys='LINUX')

outp <- speciesTurnover(idir, type='aves' , OSys='LINUX')
outp <- speciesRichness(idir, type='aves' , OSys='LINUX')

outp <- richnessChange(idir)
outp <- turnoverCalc(idir, OSys='LINUX')
```


### Calculate impact metrics for individual species ###

Now you can calculate the individual species impact metrics. These can be calculated using the following commands:

```
source("impactMetrics.R")
idir <- "\\172.22.33.79/COP_CONDESAN"
setOptions(overwrite=T)
options(warn=2)

outp <- impactProcess(idir, Initial-Species, Final-Species)
```


### Summarize data ###

Now you can summarize the whole set of impact, accuracy and thresholding metrics. You just need to source the appropriate scripts as follows:

```
source("summarizeThresholds.R")
source("summarizeImpactMetrics.R")

idir <- "\\172.22.33.79/COP_CONDESAN"

outp <- summarizeThresholds(idir, ini, fin)
outp <- summarizeMetrics(idir, ini, fin)

source("summarizeMetrics.R")
outp <- summarizeMetrics(idir, ini, fin)
```

`ini` and `fin` correspond to numbers of taxa to be processed: say, if you want to process the first 100 taxa, you use `ini=1` and `fin=100`. But since this calculation is quite fast, you can do it to the whole set of taxa. This will produce a big comma-separated-values for each of the type of metrics. Each with a different name.

### New!: Cut to a new domain ###

Since the initial modeling was performed using a wrong mask (Andean mask), a script named `cutNewDomain.R` was created to do so. This script was already run for the whole set of species, but some errors seem to have occurred. These errors have not been detected (i.e. the species that have such errors). This script should be run as follows (in the case of further corrections to certain taxa).

```
source("cutNewDomain.R")
bdir <- "\\172.22.33.79/COP_CONDESAN"
ldir <- bdir

setOptions(overwrite=T)
options(warn=2)

outp <- cutDomain(bdir, ldir, ini, fin, OSys='linux', overwrite=T)
```

Despite `ldir` can be exactly the same as `bdir`, it is suggested that you use a different folder. Use a local with the same file structure as the remote one, with the appropriate masks placed within it.

### Re-run impact metrics ###

After this you can just run again the impact metrics function and you'll be able to get updated results. If this script fails, it means you need to reprocess something. Check what species it failed and re-do the process it failed on.

The files `runs.xls` and `run.txt` contain a brief of the processes that have been performed.