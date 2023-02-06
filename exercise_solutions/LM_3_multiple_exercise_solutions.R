## ----Q2, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------
loyn <- read.table("data/loyn.txt", header = TRUE, stringsAsFactors = TRUE)
str(loyn)


## ----Q3, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------
loyn$LOGAREA <- log10(loyn$AREA)
# create factor GRAZE as it was originally coded as an integer
loyn$FGRAZE <- factor(loyn$GRAZE)






## ----Q6, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------
birds.add.1 <- lm(ABUND ~ LOGAREA + FGRAZE, data = loyn)


## ----Q7, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------
anova(birds.add.1)

# null hypothesis 1: There is no effect of LOGAREA on ABUND
# (the coefficient for LOGAREA is zero)
# null hypothesis 2: There is no effect of FGRAZE on ABUND
# (no difference between grazing levels *after* the effect of LOGAREA)

# the p values are all very small therefore reject both null hypotheses.


## ----Q8, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------
summary(birds.add.1)

# Here the intercept (baseline) is *NOT* the mean abundance of birds for
# FGRAZE level 1. It is the predicted `ABUND` for LOGAREA = 0 & FGRAZE level 1.
# the null hypothesis for the intercept is that the intercept = 0.
# As the p value (p < 2e-16) is very small we reject this null hypothesis
# and conclude that the intercept is significantly different from 0.
# Not a biologically relevant hypothesis test, in this context
# (in fact totally arbitrary, as the location of the zero is determined
# by the transformation we chose)

# For LOGAREA, the null hypothesis is that the slope of the relationship
# between LOGAREA and ABUND = 0 (no relationship)

# The remaining estimates are differences (contrasts) between each level
# and the reference level, FGRAZE1. 
# For example the FGRAZE2 estimate is 0.38, so there are 0.38 more birds on
# average in graze level 2 compared to graze level 1, *for a given patch area*.
# This difference is however not significantly different from zero (p = 0.89). 

# The difference between graze level 5 (FGRAZE5) and the reference FGRAZE1 is 
# -11.89 (11.89 fewer birds in graze 5 compared to graze 1),
# for *an identical patch area*.
# This difference is significantly different from 0 (p = 0.00017) and therefore
# the mean abundance of birds in graze level 5 is significantly lower than in
# graze level 1, for the same patch area.

# The Multiple R-square value is as we calculated from the anova table





## ----Q10a, eval=TRUE, echo=TRUE, collapse=FALSE------------------------------------------------------------
par(mfrow= c(1, 1))
plot(loyn$ABUND ~ loyn$LOGAREA, col = loyn$GRAZE, pch = 16)
# Note: # colour 1 means black in R
# colour 2 means red in R
# colour 3 means green in R
# colour 4 means blue in R
# colour 5 means cyan in R

# FGRAZE1
# create a sequence of increasing Biomass within the observed range
LOGAREA.seq <- seq(from = min(loyn$LOGAREA[loyn$FGRAZE == 1]),
					to = max(loyn$LOGAREA[loyn$FGRAZE == 1]),
					length = 20)
# create data frame for prediction
dat4pred <- data.frame(FGRAZE= "1", LOGAREA = LOGAREA.seq)
# predict for new data
dat4pred$predicted <- predict(birds.add.1, newdata = dat4pred)
# add the predictions to the plot of the data
lines(predicted ~ LOGAREA, data = dat4pred, col = 1, lwd = 2)

# FGRAZE2
LOGAREA.seq <- seq(from = min(loyn$LOGAREA[loyn$FGRAZE == 2]),
					to = max(loyn$LOGAREA[loyn$FGRAZE == 2]),
					length = 20)
dat4pred <- data.frame(FGRAZE = "2", LOGAREA = LOGAREA.seq)
dat4pred$predicted <- predict(birds.add.1, newdata= dat4pred)
lines(predicted ~ LOGAREA, data = dat4pred, col = 2, lwd = 2)

# FGRAZE3
LOGAREA.seq <- seq(from = min(loyn$LOGAREA[loyn$FGRAZE == 3]),
					to = max(loyn$LOGAREA[loyn$FGRAZE == 3]),
					length = 20)
dat4pred <- data.frame(FGRAZE = "3", LOGAREA = LOGAREA.seq)
dat4pred$predicted <- predict(birds.add.1, newdata = dat4pred)
lines(predicted ~ LOGAREA, data = dat4pred, col = 3, lwd = 2)

# FGRAZE4
LOGAREA.seq <- seq(from = min(loyn$LOGAREA[loyn$FGRAZE == 4]),
					to = max(loyn$LOGAREA[loyn$FGRAZE == 4]),
					length = 20)
dat4pred <- data.frame(FGRAZE = "4", LOGAREA = LOGAREA.seq)
dat4pred$predicted <- predict(birds.add.1, newdata= dat4pred)
lines(predicted ~ LOGAREA, data = dat4pred, col = 4, lwd = 2)

# FGRAZE5
LOGAREA.seq <- seq(from = min(loyn$LOGAREA[loyn$FGRAZE == 5]),
					to = max(loyn$LOGAREA[loyn$FGRAZE == 5]),
					length = 20)
dat4pred <- data.frame(FGRAZE = "5", LOGAREA = LOGAREA.seq)
dat4pred$predicted <- predict(birds.add.1, newdata = dat4pred)
lines(predicted ~ LOGAREA, data = dat4pred, col = 5, lwd = 2)

legend("topleft", 
 legend= paste("Graze = ", 5:1), 
 col = c(5:1), bty = "n",
 lty = c(1, 1, 1), 
 lwd = c(1, 1, 1))


## ----Q10b, eval=TRUE, echo=TRUE, collapse=FALSE------------------------------------------------------------
# Okay, that was a long-winded way of doing this.
# If, like me, you prefer more compact code and less risks of errors,
# you can use a loop, to save repeating the sequence 5 times:
par(mfrow = c(1, 1))
plot(loyn$ABUND ~ loyn$LOGAREA, col = loyn$GRAZE, pch = 16)

for(g in levels(loyn$FGRAZE)){# `g` will take the values "1", "2",..., "5" in turn
	LOGAREA.seq <- seq(from = min(loyn$LOGAREA[loyn$FGRAZE == g]),
										to = max(loyn$LOGAREA[loyn$FGRAZE == g]),
														length = 20)
	dat4pred <- data.frame(FGRAZE= g, LOGAREA= LOGAREA.seq)
	dat4pred$predicted <- predict(birds.add.1, newdata= dat4pred)
	lines(predicted ~ LOGAREA, data = dat4pred, col = as.numeric(g), lwd = 2)
}
legend("topleft", 
 legend = paste("Graze = ", 5:1), 
 col = c(5:1), bty = "n",
 lty = c(1, 1, 1), 
 lwd = c(1, 1, 1))




## ----Q12, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------------------------------------
birds.inter.1 <- lm(ABUND ~ FGRAZE * LOGAREA , data = loyn)


## ----Q13, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------------------------------------
anova(birds.inter.1)

# null hypothesis 1: There is no effect of LOGAREA on ABUND
# (the coefficient for LOGAREA is zero)

# null hypothesis 2: There is no effect of FGRAZE on ABUND
# (no difference between grazing levels *after* the effect of LOGAREA)

# null hypothesis 3: There is no effect of an FGRAZE by ABUND interaction
# *after* the effects of LOGAREA and FGRAZE combined).
# A couple of equivalent ways to say this: the effect of LOGAREA doesn't differ
# among FGRAZE levels or: the difference in bird abundance between grazing
# levels is the same for all patch areas.

# As long as there is an interaction in the model, the null hypotheses 1 and 2
# ("main effects") are not relevant to us
# the p value for the interaction is large, therefore we fail to reject the
# null hypothesis: there is no evidence supporting this interaction.


## ----Q14, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------------------------------------
summary(birds.inter.1)

# Here the intercept (baseline) is the predicted `ABUND` for LOGAREA = 0,
# for FGRAZE level 1.
# the null hypothesis for the intercept is that the intercept = 0
# (not biologically relevant).

# LOGAREA represents the slope for LOGAREA, specific to level FGRAZE = 1.
# The null hypothesis is that the slope of the relationship
# between LOGAREA and ABUND = 0, for level FGRAZE = 1 only. 

# FGRAZE2...5 estimate differences (contrasts) between the *intercept* of
# each level and the *intercept* of the reference level, FGRAZE = 1. 

# FGRAZE2...5:LOGAREA estimate differences (contrasts) between the *slope*
# of LOGAREA for each level and the *slope* of LOGAREA for the reference
# level, FGRAZE = 1. 


# The Multiple R-square value is 0.76, slightly up from the purely additive
# model (but not much, given that we have added a whole 4 parameters to the
# model, i.e. nearly doubled its complexity)


