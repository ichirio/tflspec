# The company's code (programs/company_setup.R): copied from the company
# standards (Example Pharma TFL standards 3.2 of 2026-10-01) on 2026-11-20.  The study's own now: tflplanner
# never writes it again.  Every program runs it first (study_setup.R).

# Example Pharma: the company setup of XMP-204
library(cards)
library(dplyr)
library(rtfreporter)
suppressPackageStartupMessages(library(tidyr))
options(warn = 1, width = 120)
days_per_week <- 7
days_per_month <- 30
days_per_year <- 365
