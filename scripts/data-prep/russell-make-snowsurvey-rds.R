# Combine snow survey obs into one file 

ss_0506 <- CRHMr::readObsFile(
  'data/russell-creek/2005-2006 Upper Stephanie-SWE.obs',
  timezone = 'Etc/GMT+8'
) |> select(datetime, OG2 = USteph_OG2_SWE.1, CC2 = USteph_CC2_SWE.1) 

ss_0607 <- CRHMr::readObsFile(
  'data/russell-creek/2006-2007 Upper Stephanie-SWE.obs',
  timezone = 'Etc/GMT+8'
) |> select(datetime, OG2 = SWE.8, CC2 = SWE.1) |> 
  mutate(
    OG2 = ifelse(OG2 == 0, NA, OG2),
    CC2 = ifelse(CC2 == 0, NA, CC2)
  )

ss_0708 <- CRHMr::readObsFile(
  'data/russell-creek/2007-2008 Upper Stephanie-SWE.obs',
  timezone = 'Etc/GMT+8'
) |> select(datetime, OG2 = USteph_OG2_SWE.1, CC2 = USteph_CC2_SWE.1) 

ss_out <- #rbind(ss_0506, ss_0607) |> 
  rbind(ss_0607, ss_0708)

saveRDS(ss_out, 'data/russell-creek/russell_upper_stephanie_all_swe_2006_2008.rds')
