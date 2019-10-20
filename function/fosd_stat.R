# 1. Inputs
# Two vectors specifying the subject-level values. One group of subjects are from control and the other from treatment
# space specifies the spacing of the support of the CDF

# 2. Process
# Find the range of the support of the CDF
# Find to empirical CDFs of the two vectors
# Calculate the ks-stat, adjusted for FOSD

# 3. Output: ks-stat for FOSD

fosd_stat <- function(control,treatment,space){
  # support of the CDF
  rMin =  min(control, treatment)
  rMax =	max(control, treatment)
  range = seq(from=rMin, to=rMax, by=space)	
  
  # fosd_stat
  fControl = ecdf(control)
  fTreatment = ecdf(treatment)
  
  nControl = length(control)
  nTreatment = length(treatment)
  stat = ((nControl*nTreatment)/(nControl+nTreatment))^(1/2)*max(fControl(range) - fTreatment(range))
  return(stat)
}