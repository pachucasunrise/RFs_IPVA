# RFs_IPVA

R code pertaining to the paper 'Risk factors for intimate partner violence and abuse (IPVA) victimisation and perpetration among young men and women: findings from a UK population-based cohort', Authors: Annie Herbert, Jon Heron, Christine Barter, Eszter Szilassy, Maria Barnes, Laura D Howe, Gene Feder, Abigail Fraser

Short abstract: In Avon Longitudinal Study of Parents and Children (ALSPAC) data, we reported the prevalence of interpersonal violence and abuse (IPVA) up to 21 years old, by a range of demographic, characteristics, extreme parental monitoring, mental health conditions, externalising behaviours (e.g. regular smoking), no-low education/employment, adverse childhood experiences. 

The paper is currently at submission stage with Wellcome Open Research: https://wellcomeopenresearch.org/articles/5-176

analyses.R is main analysis file to re-produce all tables and values cited in the text.  
This file calls the following functions:
1) rrTable.R: to produce tables of relative risks of IPVA for all proposed risk factors in mids objects (imputed datasets)
2) rrTable_c.R: to produce tables of relative risks of IPVA for all proposed risk factors in data.frame objects
3) prevTable.R: to produce tables of prevalence of IPVA for all proposed risk factors in mids objects (imputed datasets)
