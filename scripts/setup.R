read_crhm_obs <- function(path, prj, runtag, tz) {
  
  fullpath <- list.files(
    paste0(
      path,
      prj
    ),
    pattern = runtag,
    full.names = T
  )
  
  stopifnot(length(path) == 1)
  
  crhm_output_new <- CRHMr::readOutputFile(
    fullpath,
    timezone = tz) 
  
}
