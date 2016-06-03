## ------------------------------------------------------------------------
packageDev <- TRUE

## ----eval=packageDev-----------------------------------------------------
library(stanard)

set.seed(1234)
myLosses <- OneSimulation(AY = 1:5)

myOtherLosses <- OneSimulation(freqFn = rpois, freqArgs = list(lambda = 50))

