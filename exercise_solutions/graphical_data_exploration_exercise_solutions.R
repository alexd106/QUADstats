## ----Q4, eval=TRUE, echo=TRUE, collapse=TRUE----------------------------------------------------------------------
loyn <- read.table("./data/loyn.txt", header = TRUE, 
                   stringsAsFactors = TRUE)
str(loyn)

# 56 observations and 8 variables (from str())

summary(loyn)

# GRAZE is coded as numeric (i.e. 1,2,3,5)

# create a new factor variable variable FGRAZE which is a factor of GRAZE
loyn$FGRAZE <- factor(loyn$GRAZE)


## ----Q5, eval=TRUE, echo=TRUE, collapse=TRUE----------------------------------------------------------------------
table(loyn$FGRAZE)

# or use xtabs function
xtabs(~ FGRAZE, data = loyn)


## ----Q6, eval=TRUE, echo=TRUE, collapse=TRUE----------------------------------------------------------------------
# mean abundance of birds for each level of FGRAZE
tapply(loyn$ABUND, loyn$FGRAZE, mean, na.rm = TRUE)

# variance in the abundance of birds for each level of FGRAZE
tapply(loyn$ABUND, loyn$FGRAZE, var, na.rm = TRUE)

# minimum abundance of birds for each level of FGRAZE
tapply(loyn$ABUND, loyn$FGRAZE, min, na.rm = TRUE)

# maximum abundance of birds for each level of FGRAZE
tapply(loyn$ABUND, loyn$FGRAZE, max, na.rm = TRUE)

# OR use the summary function
tapply(loyn$ABUND, loyn$FGRAZE, summary, na.rm = TRUE)


## ----Q7a, eval=TRUE, echo=TRUE, collapse=TRUE---------------------------------------------------------------------
# first split the plotting device into 2 rows
# and 3 columns
par(mfrow = c(2,3))

# now produce the plots
dotchart(loyn$AREA, main = "Area")
dotchart(loyn$DIST, main = "Distance")
dotchart(loyn$LDIST, main = "Distance to larger patch")
dotchart(loyn$YR.ISOL, main = "Year of isolation")
dotchart(loyn$ALT, main = "Altitude")
dotchart(loyn$GRAZE, main = "Grazing levels")


## ----Q7b, eval=TRUE, echo=TRUE, collapse=TRUE---------------------------------------------------------------------
# A fancier version of a dotplot - just for fun!
Z <- cbind(loyn$ABUND, loyn$AREA, loyn$DIST,
           loyn$LDIST,loyn$YR.ISOL,loyn$ALT,
           loyn$GRAZE)

colnames(Z) <- c("Abundance", "Area","Distance",
                 "larger dist","year of isolation",
                 "Altitude", "Grazing")
                 
library(lattice)
dotplot(as.matrix(Z),
      groups=FALSE,
      strip = strip.custom(bg = 'white',
            par.strip.text = list(cex = 0.8)),
        scales = list(x = list(relation = "free"),
                      y = list(relation = "free"),
                      draw = FALSE),
        col=1, cex  =0.5, pch = 16,
        xlab = "Value of the variable",
        ylab = "Order of the data from text file")


## ----Q8, eval=TRUE, echo=TRUE, collapse=TRUE----------------------------------------------------------------------
# There appears to be two unusually large forest patches compared to the rest
# Also one potentially large distance in DIST
# One option would be to log10 transform AREA, DIST 
# log base 10 transform variables 

loyn$LOGAREA <- log10(loyn$AREA)
loyn$LOGDIST <- log10(loyn$DIST)

# check the dataframe
str(loyn)


## ----Q9a, eval=TRUE, echo=TRUE, collapse=TRUE---------------------------------------------------------------------
# Vanilla pairs plot

pairs(loyn[,c("LOGAREA","LOGDIST","DIST",
               "YR.ISOL","ALT","GRAZE")])

# or first create a new dataframe and then use this 
# data frame with the pairs function

explan_vars <- loyn[,c("LOGAREA","LOGDIST","DIST",
               "YR.ISOL","ALT","GRAZE")]
pairs(explan_vars)


## ----Q9b, eval=TRUE, echo=TRUE, collapse=TRUE---------------------------------------------------------------------
# And with correlations in the upper panel

# first need to define the panel.cor function
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}

# then use the panel.cor function when we use pairs
pairs(loyn[,c("LOGAREA","LOGDIST","DIST",
               "YR.ISOL","ALT","GRAZE")],
      upper.panel = panel.cor)


## ----Q10a, eval=TRUE, echo=TRUE, collapse=TRUE--------------------------------------------------------------------
pairs(loyn[,c("ABUND","LOGAREA","LOGDIST","DIST",
    	"YR.ISOL","ALT","GRAZE")],
      	lower.panel = panel.cor)


## ----Q10b, eval=TRUE, echo=TRUE, collapse=TRUE--------------------------------------------------------------------
plot(loyn$LOGAREA, loyn$ABUND, xlab = "log area", ylab = "bird abundance")


## ----Q11a, eval=TRUE, echo=TRUE, collapse=TRUE--------------------------------------------------------------------
# Interaction between LOGAREA and FGRAZE? 
# Do the slopes look similar or different? 
coplot(ABUND ~ LOGAREA | FGRAZE, data = loyn)


## ----Q11b, eval=TRUE, echo=TRUE, collapse=TRUE--------------------------------------------------------------------
# Fancier version of the above plot 
# with a line of best fit included just for fun
coplot(ABUND ~ LOGAREA | FGRAZE,
      data = loyn,
        panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

