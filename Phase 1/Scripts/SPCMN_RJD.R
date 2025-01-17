SPCMN_RJD=read.csv(file.choose(), sep=",", header=T)

library('dplyr')
library('ggplot2')
library('tidyverse')

str(SPCMN_RJD)
colnames(SPCMN_RJD)

SPCMN_RJD$Lab.Id..Lab. = as.factor(SPCMN_RJD$Lab.Id..Lab.)
SPCMN_RJD$Status = as.factor(SPCMN_RJD$Status)
levels(SPCMN_RJD$Lab.Id..Lab.)
levels(SPCMN_RJD$Status)

lab_name_for_id_11 = SPCMN_RJD$LAB_NAME..LAB.[SPCMN_RJD$Lab.Id..Lab. == 11]

Lab.Id.Lab = SPCMN_RJD$Lab.Id..Lab.
LAB_ID.SPECIMEN_REJECTED1 = SPCMN_RJD$LAB_ID..SPECIMEN_REJECTED.

# Check for discrepancies
discrepancies = Lab.Id.Lab != LAB_ID.SPECIMEN_REJECTED1

# which rows have discrepancies
which(discrepancies)

# Total number of discrepancies
sum(discrepancies)

