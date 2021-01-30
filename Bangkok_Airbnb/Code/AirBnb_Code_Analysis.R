library(stargazer)
library(Hmisc)
library(readr)
library(rattle)
library(tidyverse)
library(caret)
library(ranger)
library(Hmisc)
library(knitr)
library(kableExtra)
library(xtable)
library(data.table)
library(stringr)
library(dplyr) 

rm(list=ls())

BangkokClean <- read.csv("https://raw.githubusercontent.com/BrunoHelmeczy/Prediction_Projects-CEU_DA3/main/Bangkok_Airbnb/Data/Clean/airbnb_bangkok_cleaned.csv",
                         stringsAsFactors = T)
df <- BangkokClean

