## library() calls go here
library(conflicted)
library(dotenv)
library(drake)
library(tidyverse)
library(nloptr)
library(survival)
library(splines)
library(numDeriv)
library(rms)
library(statmod)
library(simsurv)
library(survMisc)
library(testthat)

conflicted::conflict_prefer("filter",    "dplyr")
conflicted::conflict_prefer("slice",     "dplyr")
conflicted::conflict_prefer("select",    "dplyr")
conflicted::conflict_prefer("summarise", "dplyr")
conflicted::conflict_prefer("summarize", "dplyr")
conflicted::conflict_prefer("gather",    "tidyr")
conflicted::conflict_prefer("set_names", "purrr")
conflicted::conflict_prefer("plan",      "drake")