library(shiny)
library(ggplot2)
library(dplyr)
library(xlsx)
library(genoRUtils)  # provides ModelObject(), ModelObjectFit()
library(pangr)  # provides smooth.adaptive.loess() for expandix()

## @DEPRECATED: no longer a dependency
#library(genoRSmallScale)  # provides getExpGrowthPhase()

source('lib/BioscreenExperimentRefClass.R')
source('lib/DataIndexExpanders.R')
source('lib/expandix.R')
source('lib/models.R')