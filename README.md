# Code for paper: 

Repo supporting Not Just Heat: place-based vulnerability to temperature extremes overestimated in Europe without including measures of adaptive capacity

Results from the paper are in `Figures/Main` and `Figures/SI`. Raw data is publicly available online.

## How to replicate results

### File structure
- Home directory contains scripts that load, clean, analyse data and generate figures
`[x] list of all scripts in this directory`
- `Main.R` umbrella script that sources scripts for analysis and figures
- `HAC/` directory with scripts and data for generating HAC index
- `Figures/` directory for saving figure outputs of `Main`
- `Data/` directory for storing raw data 


### Data sources
- Temperature data file "X" can be downloaded from: 
- HDI raster data from Kummu et al. (2018) available at [Dryad Data](https://datadryad.org/stash/dataset/doi:10.5061/dryad.dk1j0)


### Replicating results
1. Dowload raw data and save into `Data` folder
2. Install packages
3. Generate HAC
   - Navigate to `HAC` folder and run scripts to generate HAC index as a .csv 
   - 
4. In Home folder, run `Main.R`


## Computational environment


R packages versions are specified below.
