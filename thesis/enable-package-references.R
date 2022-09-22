
# Enable the citation of some of the R packages used in this project. Specifically, references 
# for all packages currently loaded will be stored in the file 'r-references.bib'
# (further information at http://frederikaust.com/papaja_man/writing.html).

library(papaja)

library(lme4)
library(lmerTest)
library(robustlmm)
library(simr)

r_refs(file = "manuscript/package-references.bib")
