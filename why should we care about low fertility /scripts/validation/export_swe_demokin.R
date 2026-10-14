# Validation only (not part of the pipeline): run DemoKin on its Swedish example data
library(DemoKin)
dir <- commandArgs(TRUE)[1]
write.csv(swe_px, file.path(dir, "swe_px.csv"))
write.csv(swe_asfr, file.path(dir, "swe_asfr.csv"))
write.csv(swe_pop, file.path(dir, "swe_pop.csv"))
k <- kin(p = swe_px, f = swe_asfr, n = swe_pop, time_invariant = FALSE, output_cohort = c(1930, 1950))
write.csv(k$kin_full[, c("kin","age_kin","age_focal","cohort","year","living")], file.path(dir, "demokin_swe_cohorts.csv"), row.names = FALSE)
k2 <- kin(p = swe_px[, "2000"], f = swe_asfr[, "2000"], time_invariant = TRUE)
write.csv(k2$kin_full[, c("kin","age_kin","age_focal","living")], file.path(dir, "demokin_swe_2000_invariant.csv"), row.names = FALSE)
cat("done\n")
