## ----Q2, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------------
loyn <- read.table("./data/loyn.txt", header = TRUE, 
                   stringsAsFactors = TRUE)
str(loyn)


## ----Q3, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------------
loyn$LOGAREA <- log10(loyn$AREA)
# create factor GRAZE as it was originally coded as an integer
loyn$FGRAZE <- factor(loyn$GRAZE)


## ----Q4, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE-----------------------------------------
## plot(LOGAREA ~ GRAZE, xlab = "Grazing level", ylab = "Patch area", data = loyn)
## 
## # There is a good spread of patch areas within each grazing level overall,
## # although there is a trend for more grazing the smaller the patch is.
## # the lowest level of grazing intensity happens to be predominantly in
## # larger patches (including the two monster patches)
## 
## # How would we expect adding grazing level to the LOGAREA model to affect
## # the predictions of the model? Think particularly of the largest two patches
## # which were previously overestimated by the model (negative residuals -
## # see the linear model 1 exercise)?
## # Since the lowest grazing levels appear to be associated with the highest
## # bird abundances, we could expect a model combining area and grazing level
## # to predict an even higher abundance for these patches
## # this would not improve the situation for these patches, at least.
## # But let's find out if that's the case!


## ----Q5, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE-----------------------------------------
## coplot(ABUND ~ LOGAREA | FGRAZE, data = loyn)
## 
## # There is a lot of variation in there, but:
## # The mean abundance seems to decrease as grazing levels increase.
## # Most noticeable in the highest grazing level.
## # Within a grazing level, abundance seems to increase with the log-patch area.
## # It is unclear from this if the slope of the log-area effect is
## # different between grazing levels


## ----Q6, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------------
birds.add.1 <- lm(ABUND ~ LOGAREA + FGRAZE, data = loyn)


## ----Q7, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------------
anova(birds.add.1)

# null hypothesis 1: There is no effect of LOGAREA on ABUND
# (the coefficient for LOGAREA is zero)
# null hypothesis 2: There is no effect of FGRAZE on ABUND
# (no difference between grazing levels *after* the effect of LOGAREA)
# the p values are all very small therefore reject both null hypotheses.

# Proportions of variation can all be calculated by hand by copy-paste
# of the Sum Sq values if you prefer, but
# we can extract the Sums of squares like this:
str(anova(birds.add.1)) # examine the structure of the returned object
birds.add.1.SS<- anova(birds.add.1)$'Sum Sq' # store values of interest
birds.add.1.SS # checking that we have extracted the Sums of squares as intended

birds.add.1.SST<- sum(anova(birds.add.1)$'Sum Sq') # compute SST

# proportion of variation in the data explained by the model:
(birds.add.1.SST - birds.add.1.SS[3]) / birds.add.1.SST # 0.7269773

# proportion of variation in the data explained by predictors:
(birds.add.1.SS[1:2]) / birds.add.1.SST # 0.5476530 0.1793243

# So that's 55% for LOGAREA and 18% for FGRAZE after LOGAREA



## ----Q8, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------------
birds.add.2 <- lm(ABUND ~ FGRAZE + LOGAREA, data = loyn)
anova(birds.add.2)

# null hypothesis 1: There is no effect of FGRAZE on ABUND
# (no difference between grazing levels)
# null hypothesis 2: There is no effect of LOGAREA on ABUND
# (the coefficient for LOGAREA is zero *after* the effect of FGRAZE)
# the p values are all very small therefore reject both null hypotheses.

# we can also extract the Sums of squares like this:
birds.add.2.SS<- anova(birds.add.2)$'Sum Sq' # store values of interest
birds.add.2.SST<- sum(anova(birds.add.2)$'Sum Sq') # compute SST

# proportion of variation in the data explained by the model:
(birds.add.2.SST - birds.add.2.SS[3]) / birds.add.2.SST
# the same as the previous model

# proportion of variation in the data explained by predictors:
(birds.add.2.SS[1:2]) / birds.add.2.SST # 0.5449233 0.1820540

# So that's 54% for LOGAREA and 18% for FGRAZE after LOGAREA,
# essentially reversing the contributions found in the first model.
# Conclusion: here the order matters a lot. This is not too surprising since
# the design is unbalanced and FGRAZE and LOGAREA covary (they are correlated)



## ----Q9, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------------
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



## ----Q10, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------
# ABUND = 15.72*(Intercept) + 7.25*LOGAREA + 0.38*FGRAZE2 - 0.19*FGRAZE3
# - 1.59*FGRAZE4 - 11.89*FGRAZE5
# Note that (Intercept) = 1 always

# expected abundance for (A): replace LOGAREA by -0.5, and all FGRAZE2...5 by 0
15.72*1 + 7.25*(-0.5) + 0.38*0 - 0.19*0 - 1.59*0 - 11.89*0 # 12.095

# could also extract the model coefficients like this:
birds.add.1.coef<- coef(birds.add.1)
birds.add.1.coef
# and multiply by the predictor values to obtain the equivalent prediction
# (difference due to my own roundings of estimates above)
sum(birds.add.1.coef * c(1, -0.5, 0, 0, 0, 0)) # 12.09278 

# expected abundance for (B): input 1 for the GRAZE3 variable
15.72*1 + 7.25*(-0.5) + 0.38*0 - 0.19*1 - 1.59*0 - 11.89*0 # 11.905
# or
sum(birds.add.1.coef * c(1, -0.5, 0, 1, 0, 0)) # 11.90349

# the difference (B) 11.90349 - (A) 12.09278 should correspond to
# the estimate for the GRAZE3 coefficient

# expected abundance for (C)
sum(birds.add.1.coef * c(1, 0.5, 0, 1, 0, 0)) # 19.15072

# the difference (C) 19.15072 - (B) 11.90349 should coincide with the
# estimate of the slope for the LOGAREA effect, since there is just one
# LOGAREA unit of difference and the slope is the change in expected
# abundance for a 1-unit increase in the predictor.



## ----Q11, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------
## # first split the plotting device into 2 rows and 2 columns
## par(mfrow = c(2,2))
## 
## # now create the residuals plots
## plot(birds.add.1)
## 
## # To test the normality of residuals assumption we use the Normal Q-Q plot.
## # The central residuals are not too far from the Q-Q line but the extremes
## # are too extreme (the tails of the distribution are too long). Some
## # observations, both high and low, are poorly explained by the model.
## 
## # The plot of the residuals against the fitted values suggests these
## # extreme residuals happen for intermediate fitted values.
## 
## # Looking at the homogeneity of variance assumption (Residuals vs
## # Fitted and Scale-Location plot),
## # the graphs are mostly messy, with no clear pattern emerging. There is
## # a hint of smaller variance with the lowest fitted values, which is not ideal.
## # This could mean that the homogeneity of variance assumption is not met
## # (i.e. the variances are not the same).
## 
## # The observations with the highest leverage don't appear to be overly
## # influential, according to the Cook's distances in the Residuals vs
## # Leverage plot.
## 
## # ABUND being bounded by zero, it wouldn't be too surprising that the variance increases with the mean abundance.
## # This is often improved by log-transforming the response
## loyn$logABUND<- log(loyn$ABUND + 1) # here the natural log
## birds.add.3 <- lm(logABUND ~ LOGAREA + FGRAZE, data = loyn)
## par(mfrow = c(2,2))
## plot(birds.add.3)
## 
## # Not this time! Lots of extreme negative residuals generated.
## 
## # Back to `birds.add.1` the other issue was the extreme residuals.
## # This could be due to missing important predictors from the model, either
## # new predictors altogether, or interactions: is it okay to assume
## # the effect of LOGAREA to be the same for all grazing levels?
## 


## ----Q12a, eval=TRUE, echo=TRUE, collapse=FALSE-------------------------------------------------------------------
par(mfrow= c(1, 1))
plot(ABUND ~ LOGAREA, data= loyn, col= GRAZE, pch= 16)
# Note: # color 1 means black in R
# color 2 means red in R
# color 3 means green in R
# color 4 means blue in R
# color 5 means cyan in R

# FGRAZE1
# create a sequence of increasing Biomass within the observed range
LOGAREA.seq<- seq(from= min(loyn$LOGAREA[loyn$FGRAZE == 1]),
					to= max(loyn$LOGAREA[loyn$FGRAZE == 1]),
					length= 20)
# create data frame for prediction
dat4pred<- data.frame(FGRAZE= "1", LOGAREA= LOGAREA.seq)
# predict for new data
dat4pred$predicted<- predict(birds.add.1, newdata= dat4pred)
# add the predictions to the plot of the data
lines(predicted ~ LOGAREA, data= dat4pred, col= 1, lwd= 2)

# FGRAZE2
LOGAREA.seq<- seq(from= min(loyn$LOGAREA[loyn$FGRAZE == 2]),
					to= max(loyn$LOGAREA[loyn$FGRAZE == 2]),
					length= 20)
dat4pred<- data.frame(FGRAZE= "2", LOGAREA= LOGAREA.seq)
dat4pred$predicted<- predict(birds.add.1, newdata= dat4pred)
lines(predicted ~ LOGAREA, data= dat4pred, col= 2, lwd= 2)

# FGRAZE3
LOGAREA.seq<- seq(from= min(loyn$LOGAREA[loyn$FGRAZE == 3]),
					to= max(loyn$LOGAREA[loyn$FGRAZE == 3]),
					length= 20)
dat4pred<- data.frame(FGRAZE= "3", LOGAREA= LOGAREA.seq)
dat4pred$predicted<- predict(birds.add.1, newdata= dat4pred)
lines(predicted ~ LOGAREA, data= dat4pred, col= 3, lwd= 2)

# FGRAZE4
LOGAREA.seq<- seq(from= min(loyn$LOGAREA[loyn$FGRAZE == 4]),
					to= max(loyn$LOGAREA[loyn$FGRAZE == 4]),
					length= 20)
dat4pred<- data.frame(FGRAZE= "4", LOGAREA= LOGAREA.seq)
dat4pred$predicted<- predict(birds.add.1, newdata= dat4pred)
lines(predicted ~ LOGAREA, data= dat4pred, col= 4, lwd= 2)

# FGRAZE5
LOGAREA.seq<- seq(from= min(loyn$LOGAREA[loyn$FGRAZE == 5]),
					to= max(loyn$LOGAREA[loyn$FGRAZE == 5]),
					length= 20)
dat4pred<- data.frame(FGRAZE= "5", LOGAREA= LOGAREA.seq)
dat4pred$predicted<- predict(birds.add.1, newdata= dat4pred)
lines(predicted ~ LOGAREA, data= dat4pred, col= 5, lwd= 2)

legend("topleft", 
 legend= paste("Graze = ", 5:1), 
 col= c(5:1), bty= "n",
 lty= c(1, 1, 1), 
 lwd= c(1, 1, 1))


## ----Q12b, eval=TRUE, echo=TRUE, collapse=FALSE-------------------------------------------------------------------
# Okay, that was a long-winded way of doing this.
# If, like me, you prefer more compact code and less risks of errors,
# you can use a loop, to save repeating the sequence 5 times:
par(mfrow= c(1, 1))
plot(ABUND ~ LOGAREA, data= loyn, col= GRAZE, pch= 16)

for(g in levels(loyn$FGRAZE)){# `g` will take the values "1", "2",..., "5" in turn
	LOGAREA.seq<- seq(from= min(loyn$LOGAREA[loyn$FGRAZE == g]),
										to= max(loyn$LOGAREA[loyn$FGRAZE == g]),
														length= 20)
	dat4pred<- data.frame(FGRAZE= g, LOGAREA= LOGAREA.seq)
	dat4pred$predicted<- predict(birds.add.1, newdata= dat4pred)
	lines(predicted ~ LOGAREA, data= dat4pred, col= as.numeric(g), lwd= 2)
}
legend("topleft", 
 legend= paste("Graze = ", 5:1), 
 col= c(5:1), bty= "n",
 lty= c(1, 1, 1), 
 lwd= c(1, 1, 1))


## ----Q13, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------
## # There is a significant effect of grazing levels, especially the highest
## # level with a negative effect on bird abundance
## 
## # There is a significant positive effect of patch area, too.
## 
## # The relative importance of patch area and grazing is not clear, as these
## # are correlated, with smaller patches also having higher grazing intensity
## # on average, and larger patches lower grazing intensity.
## 
## # Some observations are poorly predicted (fitted) using the current set
## # of predictors.


## ----Q14, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------
birds.inter.1 <- lm(ABUND ~ FGRAZE * LOGAREA , data = loyn)


## ----Q15, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------
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


## ----Q16, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------
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



## ----Q17, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------
# ABUND = 21.243*(Intercept) - 6.165*FGRAZE2 - 7.215*FGRAZE3 - 17.910*FGRAZE4
# - 17.043*FGRAZE5 + 4.144*LOGAREA + 4.368*FGRAZE2:LOGAREA
# + 4.989*FGRAZE3:LOGAREA + 15.235*FGRAZE4:LOGAREA + 1.996*FGRAZE5:LOGAREA
# Note that (Intercept) = 1 always

# expected abundance for (A): replace LOGAREA by 2.5, and all terms involving
# FGRAZE2...5 by 0
21.243*1 - 6.165*0 - 7.215*0 - 17.910*0 - 17.043*0 + 4.144*2.5 + 4.368*0 + 4.989*0 + 15.235*0 + 1.996*0 # 31.603

# much lower risk of error by extracting the model coefficients like this:
birds.inter.1.coef<- coef(birds.inter.1)
birds.inter.1.coef # check coefficients and their order
# and multiply by the predictor values to obtain the equivalent prediction
sum(birds.inter.1.coef * c(1, 0, 0, 0, 0, 2.5, 0, 0, 0, 0)) # 31.60296 

# expected abundance for (B): input 1 for the GRAZE5 variable and -0.5 for
# variables LOGAREA and FGRAZE5:LOGAREA
21.243*1 - 6.165*0 - 7.215*0 - 17.910*0 - 17.043*1 + 4.144*(-0.5) + 4.368*0 + 4.989*0 + 15.235*0 + 1.996*(-0.5) # 1.13
# or
sum(birds.inter.1.coef * c(1, 0, 0, 0, 1, -0.5, 0, 0, 0, -0.5)) # 1.130203 

# Well done if you got there!


## ----Q18, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------
## # first split the plotting device into 2 rows and 2 columns
## par(mfrow = c(2,2))
## 
## # now create the residuals plots
## plot(birds.inter.1)
## 
## # Not a great deal of an improvement! Just marginally better in every respect,
## # thanks to increasing the fit slightly (by throwing lots of new model
## # parameters at the data).


## ----Q19, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=FALSE---------------------------------------
## # NOTE: I'm using the loop version of the plot, here.
## # If you don't like it, refer to the long-hand code version at Question 12
## 
## par(mfrow= c(1, 1))
## plot(ABUND ~ LOGAREA, data= loyn, col= GRAZE, pch= 16)
## 
## for(g in levels(loyn$FGRAZE)){# `g` will take the values "1", "2",..., "5" in turn
## 	LOGAREA.seq<- seq(from= min(loyn$LOGAREA[loyn$FGRAZE == g]),
## 										to= max(loyn$LOGAREA[loyn$FGRAZE == g]),
## 														length= 20)
## 	dat4pred<- data.frame(FGRAZE= g, LOGAREA= LOGAREA.seq)
## 	dat4pred$predicted<- predict(birds.inter.1, newdata= dat4pred)
## 	lines(predicted ~ LOGAREA, data= dat4pred, col= as.numeric(g), lwd= 2)
## }
## legend("topleft",
##  legend= paste("Graze = ", 5:1),
##  col= c(5:1), bty= "n",
##  lty= c(1, 1, 1),
##  lwd= c(1, 1, 1))


## ----Q20, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE-----------------------------------------------------------
## # The slopes of the LOGAREA effect across grazing levels are all over the
## # place, without any coherent pattern (for instance, they could have been
## # increasing or decreasing gradually from low to high grazing intensity)
## 
## # The interaction is non-significant, so isn't supported statistically either.
## 
## # Time to revert to the simpler, or a different model? More on this in the next session!
## 

