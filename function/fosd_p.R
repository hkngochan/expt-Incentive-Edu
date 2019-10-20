# 1. Inputs
# control: vectors containing values from control group
# treatment: vectors containing values from treatment group
# space: spacing of the support of the CDF
# nboots: how many times the bootstraping should be done

# 2. Process
# Find the ks-stat for whether treatment FOSD control / control FOSD treatment
# Re-sample from both groups under the null hypothesis that they are the same, with replacement
# Find the statistic under resampling
# Repeat and find the p-value under the simulated distribution

# 3. Output: (ks-stat(treatment FOSD control),p-value(treatment FOSD control),ks-stat(control FOSD treatment),p-value(control FOSD treatment))


fosd_p <- function(control,treatment,space,nboots){ 
  #find ks-stat
  ks1 = fosd_stat(control,treatment,space)
  ks2 = fosd_stat(treatment,control,space)
  
  ## Simulate ks-stat under the null hypothesis
  # initiate the vectors of simulated ks-stat
  bootStat <- rep(NA,nboots)
  population <- c(control, treatment)
  
  for (i in 1:nboots) {
    # sampling under the null hypothesis
    sControl <- population[sample(1:length(population), length(control), replace=TRUE)]
    sTreatment <- population[sample(1:length(population), length(treatment), replace=TRUE)]
    
    # ks-stat under null hypothesis
    bootStat[i] <- fosd_stat(sControl, sTreatment,space) 
  }
  pVal1 = length(bootStat[which(bootStat > ks1)])/nboots
  pVal2 = length(bootStat[which(bootStat > ks2)])/nboots
  print("Treat FOSD Control")
  print(paste("ks-stat: ", ks1, "    p-value: ", pVal1))
  print("Control FOSD Treat")
  print(paste("ks-stat: ", ks2, "    p-value: ", pVal2))
}