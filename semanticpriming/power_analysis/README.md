

# The 'smaller random-effects model'

The scripts stored in 'individual_scripts/scripts_using_reduced_randomeffects_model' 

contain power curves that are based on a now-deprecated model. That model contained

an incomplete random-effects grouping factor, namely, target words. As we later 

realised, this grouping factor was incomplete because the priming-related variables 

(i.e., language-based similarity and visual-strength difference) were not simply 

nested within target words, but were actually nested within prime-target pairs 

(factor `primeword_targetword`). Since every target word was preceded by four 

prime words over different trials (Hutchison et al., 2013, https://doi.org/10.3758/s13428-012-0304-z), 

the target-word grouping factor was four times smaller than the prime-target 

grouping factor. Nonetheless, the results of both models regarding the effects 

of interest did not substantially differ. For this reason, and because the power 

curves take several weeks to run, the older power curves were not updated using 

the newer model.


In the set of scripts using the deprecated model, the model is loaded from 

'semanticpriming/power_analysis/reduced_randomeffects_model/results/semanticpriming_lmerTest.rds'.

In contrast, in the set of scripts using the updated model, the model is loaded

from 'semanticpriming/frequentist_analysis/results/semanticpriming_lmerTest.rds'.



# Numbered power curves

The number assigned to each power curve serves refers to the effect that was 

examined in each case. Using these numbers allows for shorter file names.

