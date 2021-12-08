
 
#-------------------------------------------------------------------------------
# Mediation and Moderation Models
#
# using R-package lavaan
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# PREPARATION
#-------------------------------------------------------------------------------

# Clear workspace
rm(list=ls(all=TRUE)) 

# (Install and) Load packages to use
library(haven)      # Read and write Stata, SPSS, or SAS data file 
library(lavaan)     # LAtent VAriable ANalysis 
library(semPlot)    # Diagrams of models estimated with lavaan
library(rstudioapi) # Easy changing working directories

# Make current directory the working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  

# Send output to text file with "any.name" and to screen (split = TRUE)
sink("MediationModeration.txt", split = TRUE)

# Start plotting the models into"file" name
pdf("MediationModerationPlots.pdf")

cat("
#-------------------------------------------------------------------------------
# Mediation and Moderation Models using R-package lavaan
#-------------------------------------------------------------------------------\n") 

# Print start date and time in output
cat("# Start at", date(), "")  

cat("\n# Code uses 1000 bootstrapped samples. Increase if more precision needed.\n")


# Read simulated data (in SPSS format) and name it "data"
data <- read_sav('data.sav')

cat("
#-------------------------------------------------------------------------------
# DATA EXPLORATION
#-------------------------------------------------------------------------------\n") 

cat("# Check data: sample size and number of variables")
dim(data)

cat("\n# Correlations in data, rounded to 2 digits \n")
round(cor(data), 2)

cat("
#-------------------------------------------------------------------------------
# DATA ANALYSIS
#-------------------------------------------------------------------------------\n")

cat("
#-------------------------------------------------------------------------------
# Model 1: Simple Mediation
#-------------------------------------------------------------------------------\n") 

model.1 <- "           
m1 ~ a*x               
y  ~ b*m1 + cp*x        
indirect := a*b       
direct   := cp         
total    := a*b + cp   
"                      
mediation.1 <- sem(model.1, data = data, se = "bootstrap", bootstrap=1000)  
                     
summary(mediation.1, ci=T, standardized=T, rsquare=T, fit.measures=F) 
cat("
# Numbers in column 'Estimate' are unstandardized regression weights
# Numbers in column 'Std.all' are standardized regression weights
# ci.lower and ci.upper give the 95% Confidence interval
\n") 

cat("
#-------------------------------------------------------------------------------
# Model 2: Parallel Mediation
#-------------------------------------------------------------------------------\n") 

model.2 <- "              
m1 ~ a1*x                 
m2 ~ d1*x                 
y  ~ b1*m1 + b2*m2+ cp*x  
m1 ~~ m2                       # Errors of m1 and m2 are correlated
a1b1      := a1*b1             # Mediation path 1
d1b2      := d1*b2             # Mediation path 2
diff      := a1b1 - d1b2       # Difference between the two mediation paths
indirect  := a1b1 + d1b2       # Total indirect (mediated) effect of x on y
direct    := cp                # Conditional direct effect of x on y
total     := indirect + direct # Total effect of x on y
"                              # <- End quotes of specification

mediation.2 <- sem(model.2, data=data, se="bootstrap", bootstrap = 1000)
summary(mediation.2, ci=T, standardized=T, rsquare=T,fit.measures=F) 

cat("
#-------------------------------------------------------------------------------
# Model 3: Serial Mediation
#-------------------------------------------------------------------------------\n")

model.3 <- "              
m1 ~ a1*x                 
m2 ~ d1*m1 + d2*x                  # eq. 2; d1, d1 are our labels of regression coefficient
y  ~ b1*m2 + b2*m1 + cp*x          # eq. 3; b1, b2, cp are our labels of regress coeffs
a1b2      := a1*b2                 # Mediation path 1
d2b1      := d2*b1                 # Mediation path 2
a1d1b1    := a1*d1*b1              # Hypothesized indirect effect of x on y
indirect  := a1b2 + d2b1 + a1d1b1  # Total indirect (mediated) effect of x on y
direct    := cp                    # Conditional direct effect of x on y
total     := indirect + direct     # Total effect of x on y
"
mediation.3 <- sem(model.3, data=data, se="bootstrap", bootstrap = 1000)  
summary(mediation.3, ci=T, standardized=T, rsquare=T, fit.measures=F) 

cat("
#-------------------------------------------------------------------------------
# Model 4: First-Stage Moderation
#-------------------------------------------------------------------------------\n")

model.4 <- "                                   
m1 ~ a1*x + a2*w + a3*xw  
y  ~ b1*m1 + b2*w + b3*xw + cp*x   
a1b1           := a1*b1               # Indirect effect of x on y via m
a3b1           := a3*b1               # Conditional indirect effect of xw on y 
cprime         := cp                  # Conditional direct effect of x on y
total          := a1*b1 + a3*b1 + cp  # Total effect of x on y
"
moderation.1 <- sem(model.4, data=data, se="bootstrap", bootstrap = 1000)  
summary(moderation.1, ci=T, standardized=T, rsquare=T, fit.measures=F) 

cat("
#-------------------------------------------------------------------------------
# Model 5: Second Stage Moderation
#-------------------------------------------------------------------------------\n")

cat("# WOHO WAIT. First some data management \n

# For this model, the interaction between mediator (here m1) and moderator 
# (here w) is needed.
#
# 1. mean-center the moderator (= w) and the mediator (= m1) 
# 2. multiply the moderator with the mediator 
#-------------------------------------------------------------------------------\n")

data$m1.c <- data$m1 - mean(data$m1) # Mean center m1, and give new name
data$w.c  <- data$w - mean(data$w)   # Mean center w, and give new name
data$m1w  <- data$m1.c * data$w.c    # Make interaction variable by multiplying: m1w


# Ready for Model 5: Second-Stage Moderation
model.5 <- "               
m1 ~ a1*x  
y  ~ b1*m1 + b2*w + b3*m1w + cp*x   
a1b1          := a1*b1               # Indirect effect of x on y
a1b3          := a1*b3               # Conditional indirect effect of xw on y (2nd stage moderated mediation)
cprime        := cp                  # Conditional direct effect of x on y
total         := a1*b1 + a1*b3 + cp  # Total effect of x on y
"
moderation.2 <- sem(model.5, data=data, se="bootstrap", bootstrap = 1000) 
summary(moderation.2, ci=T, standardized=T, rsquare=T, fit.measures=F) 

cat("
#-------------------------------------------------------------------------------
# Making Diagrams of Mediation and Moderation Models, using R-package semplot
#
# Useful to visually inspect correct model specification
#-------------------------------------------------------------------------------\n")

# Note on the diagrams:
# dotted arrow is a default, not estimated.
# (curved) bidirectional arrow on a single box is a variance
# (curved) bidirectional arrow between two boxes is a covariance (correlation)
# unidirectional arrow is a path, hypothesized to be directional/causal

semPaths(mediation.1,              # Diagram of mediation.1
         whatLabels = "name",      # "name" of the path. Other options "est" (estimates) or "stand" (standardized est)
         rotation = 2,             # Rotation = 2 locates X on the left of the diagram
         asize = 5,                # Arrow-head size
         sizeMan=10,               # Size of boxes
         edge.label.position=.65,  # Relative position of label on arrow. Default = .5
         edge.label.cex=1.5,       # Size of labels on the paths (= edges,
         residuals = FALSE)        # Hide residuals to reduce visual clutter

# And now just in two lines, and with the estimates ("est") rather than path names
semPaths(mediation.1,whatLabels="est",rotation=2,asize=5,sizeMan=10,
         edge.label.position=.65, edge.label.cex=1.5, residuals = FALSE) 

semPaths(mediation.2,whatLabels="est",rotation=2,asize=5,sizeMan=10,
         edge.label.position=.65, edge.label.cex=1.5, residuals = FALSE) 

semPaths(mediation.3,whatLabels="est",rotation=2,asize=5,sizeMan=10,
         edge.label.position=.65, edge.label.cex=1.5, residuals = FALSE) 

semPaths(moderation.1,whatLabels="est",rotation=2,asize=5,sizeMan=10,
         edge.label.position=.65, edge.label.cex=1.5, residuals = FALSE) 

semPaths(moderation.2,whatLabels="est",rotation=2,asize=5,sizeMan=10,
         edge.label.position=.65, edge.label.cex=1.5, residuals = FALSE) 

cat("
#-------------------------------------------------------------------------------
# FINALIZING
#-------------------------------------------------------------------------------\n")

# Print end date and time in output
cat("# End at", date(), "")

# Stop sinking
sink()

# End plotting
dev.off()

# Show results in console
file.show("MediationModeration.txt")
file.show("MediationModerationPlots.pdf")

## End of Code ##
