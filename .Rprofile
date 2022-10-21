

# This .Rprofile script is run automatically when the project is opened.

# Activate 'renv' package to access packages in the same versions that were
# originally used for the current study, to avoid dependency conflicts.
source('renv/activate.R')

# Create function to load all custom functions
load_functions =
  function(){
    setwd('R_functions')
    sapply(list.files(), source, echo = FALSE)
    setwd('../')
  }

# Print note in the console
cat('\n ###############################################################\n\n',
    ' Project information available in README.pdf (root directory). \n\n',
    ' [Note from .Rprofile] \n\n',
    '###############################################################\n\n')

