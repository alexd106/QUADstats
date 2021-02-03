## ----Q2, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------
loyn <- read.table("./data/loyn.txt", header = TRUE,
                   stringsAsFactors = TRUE)
str(loyn)
loyn$FGRAZE <- factor(loyn$GRAZE)
loyn$LOGAREA <- log10(loyn$AREA)
loyn$LOGDIST <- log10(loyn$DIST)
loyn$LOGLDIST <- log10(loyn$LDIST)


## ----Q3, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------
# Example:

# Rank	|	Predictor	|	Biological effect
# 1	    |	LOGAREA	  |	Large patches containing proportionally
#                     more core habitat: enable persistence of
#                     species with larger home ranges. 
# 3	    |	LOGDIST	  |	?
# ?	    |	LOGLDIST	|	?
# 2	    |	YR.ISOL	  |	?
# ?	    |	ALT		    |	?
# ?	    |	FGRAZE	  |	?

# expect to come up with different predictions!
# The main limitation is our lack of insider and expert knowledge of the
# study system and area, of course.


## ----Q4, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------
VOI<- c("ABUND", "LOGAREA", "LOGDIST", "LOGLDIST", "YR.ISOL", "ALT", "FGRAZE")
pairs(loyn[, VOI])

# There is variable degrees of imbalance (correlation) between predictors
# such as:
# LOGAREA and FGRAZE, 
# LOGDIST and LOGLDIST (quite expected), 
# YR.ISOL and other variables like LOGAREA or FGRAZE, 
# LOGAREA and ALT, 
# but overall a decent spread of observations across these pairs of predictors.

# The relationship between the response variable ABUND and all the predictors
# is visible in the top row:
#  Some potential correlations present like with LOGAREA (positive), 
# YR.ISOl (positive), maybe ALT (positive) and FGRAZE (negative).


## ----Q5, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------
M1 <- lm(ABUND ~ LOGAREA + LOGDIST + LOGLDIST +
                 YR.ISOL + ALT + FGRAZE,
         data = loyn)


## ----Q6, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------
anova(M1)
summary(M1)


## ----Q7, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------
library(car)
vif(M1)

# potential problem with FGRAZE (as seen from data exploration) with LOGAREA
# or YR.ISOL
# ignore for the moment


## ----Q8, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------
anova(M1)
summary(M1)

# ANOVA allows testing overall significance for categorical predictors,
# which is more handy. 
# But the results of this ANOVA depend on the order of the variables,
# which is arbitrary here.
# We could change the order but there are too many possible permutations
# Summary p-values don't suffer that problem but test different hypotheses
# It would be useful to use an ANOVA that doesn't depend on the order
# of inclusion of the variables


## ----Q9, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------
drop1(M1, test = "F")

# LOGDIST is the least significant, hence makes the least 
# contribution to the variability explained by the model, 
# with respect to the number of degrees of freedom it uses (1)


## ----Q10, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------------------------------------------
M2 <- lm(ABUND ~ LOGAREA + LOGLDIST + # removing LOGDIST here
                 YR.ISOL + ALT + FGRAZE,
         data = loyn)

# or use the shortcut:

M2<- update(M1, formula = . ~ . - LOGDIST) # "." means all previous variables
drop1(M2, test = "F")

# YR.ISOL is now the least significant, hence makes the least 
# contribution to the variability explained by the model, 
# with respect to the number of degrees of freedom it uses (1)

M3 <- update(M2, formula = . ~ . - YR.ISOL)
drop1(M3, test = "F")

# LOGLDIST and ALT now the least significant. Choosing on the basis of
# p-values this similar is really quite arbitrary, so would be best guided
# by expert knowledge if we have it.
# in the absence of strong a-priori expertise, we'll go for LOGLDIST

M4 <- update(M3, formula = . ~ . - LOGLDIST)
drop1(M4, test = "F")

# and finally drop ALT from the model
M5 <- lm(ABUND ~ LOGAREA + FGRAZE, data = loyn) # writing model in full for clarity
drop1(M5, test = "F")
# All significant: no more terms to drop. Here we are, back to a
# familiar version of the model!


## ----Q11, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------------------------------------------
par(mfrow = c(2,2))
plot(M5)

# Seen that already!
# For info:
# To test the normality of residuals assumption we use the Normal Q-Q plot. 
# The central residuals are not too far from the Q-Q line but the extremes
# are too extreme (the tails of the distribution are too long).

# Some observations, both high and low, are poorly explained by the model.
# The plot of the residuals against the fitted values suggests
# these extreme residuals happen for intermediate fitted values.

# Looking at the homogeneity of variance assumption
# (Residuals vs Fitted and Scale-Location plot),
# the graphs are mostly messy, with no clear pattern emerging.
# There is a hint of smaller variance with the lowest fitted values,
# which is not ideal.
# This could mean that the homogeneity of variance assumption is not met
# (i.e. the variances are not the same). 
# The observations with the highest leverage don't appear to be overly
# influential, based on Cook's distances in the Residuals vs Leverage plot.  

# ABUND being bounded by zero, it wouldn't be too surprising that the
# variance increases with the mean abundance.


## ----Q12, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------------------------------------------
# No we don't, in this simple case:
# when an interaction is significant, the main effects should be left
# in irrespective of significance, because the interaction cannot be
# interpreted correctly without its main effect.

# Likewise, when an interaction is non-significant it must go first,
# and only then the evidence for the main effects can be assessed.

# Because R always includes interactions *after* their main effects
# in the models, the anova of the model returns the same result as drop1, 
# in our simple model which has no interactions with other terms

# Demo:
M6<- lm(ABUND ~ LOGAREA * FGRAZE, data = loyn) # writing the model in full for clarity
anova(M6)
drop1(M6, test= "F") # drop1 is clever enough that it doesn't let you
# see the p-values for the main effect, in the presence of their interaction.


## ----Q13, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------------------------------------------
# Biologically: what we already found out in the previous LM exercises:
# There is a significant effect of grazing levels, especially the highest
# level with a negative effect on bird abundance

# There is a significant positive effect of patch area, too.

# The relative importance of patch area and grazing is not clear, as these
# are correlated, with smaller patches also having higher grazing intensity
# on average, and larger patches lower grazing intensity.

# Some observations are poorly predicted (fitted) using the set of
# available predictors.

# Methodologically:
# Doing model selection is difficult without an intrinsic / expert knowledge
# of the system, to guide what variables to include.
# Even with this dataset, many more models could have been formulated.
# For example, for me, theory would have suggested to test an interaction 
# between YR.ISOL and LOGDIST (or LOGLDIST?), 
# because LOGDIST will affect dispersal, 
# and the time since isolation of the patch may affect how important
# dispersal has been to maintain or rescue populations 
# (for recently isolated patches, dispersal, and hence distance to nearest
# patches may have a less important effect)

