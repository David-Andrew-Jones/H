Explicit construction of cfdna shedding and non-shedding populations for differential analysis.
populations are indicated using cfdna_detectable variable.
How individuals are found is the found_using variable.

Updated parallel construction of interception model stage shift to handle multiple scenarios
efficiently all at once.  Code first computes the "MIS" scenario, then does slippage.

Script to regenerate all data and figures can be run as:
Rscript scripts/00_coordination_script.R
from the top level directory with this file.

Multiple packages from CRAN are required:
tidyverse (generally used throughout), 
Iso (used for generating sensitivity)
readxl (spreadsheets)
openxlsx (spreadsheets)
zoo (ordered observations)
gt (table generation)

