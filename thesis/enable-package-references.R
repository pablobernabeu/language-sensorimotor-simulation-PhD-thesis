
# Enable the citation of some of the R packages used in this project. Specifically, references 
# for all packages loaded below will be stored in the file 'r-references.bib' (further 
# information at http://frederikaust.com/papaja_man/writing.html).

library(rmarkdown)
library(papaja)

library(tidyverse)
library(LSAfun)
library(lme4)
library(lmerTest)
library(sjPlot)
library(GGally)
library(robustlmm)
library(simr)

r_refs(file = "thesis/package-references.bib")
