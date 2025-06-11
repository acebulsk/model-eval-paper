library(CRHMr)
#HRU_OBS () - array[5][dim] of values indexing observations to HRUs. The order is 1) t, rh and ea; 2) p and ppt; 3) u; 4) Q; and 5) for special use.
# marmot prj first initiated from fortress cansnobal
prj_path_old <- 'crhm/prj/marmot_upper_forest_clearing_snowsurveytransect_baseline.prj' # from logan
prj_path_new <- 'crhm/prj/marmot_upper_forest_clearing_snowsurveytransect_cansnobal.prj'


setPrjBasinName(inputPrjFile = prj_path, basinName = 'Marmot')

    
hru_names <- readPrjHRUnames(prj_path_old)
setPrjHRUnames(inputPrjFile = prj_path_new, HRUnames = hru_names)

obs_path <- 'crhm/obs/Marmot_Hourly_ArrayMetData_withT_g_1Oct05-30Sept24_update_3Jan2025.obs'
setPrjObs(prj_path_new, obsFiles = obs_path)

start_date <- '2005-10-01'
end_date <- '2024-10-01'
setPrjDates(prj_path_new, startDate = start_date, endDate = end_date)

# select params we need to update for new site
# commented out ones have multiple matches so need to be manually changed
params <- c(
  'hru_ASL',
  'hru_elev',
  'hru_GSL',
  'hru_lat',
  # 'Ht' 15 1.5 
  'Zwind',
  'Gap_diameter',
  # 'LAI', 1.44 0.3 
  'Surrounding_Ht',
  'Zref',
  # 'Vt' 0.0 0.0
  'HRU_OBS',
  'obs_elev',
  'z_T',
  'z_u'
)

vals <- readPrjParameters(prj_path_old, paramName = 'HRU_OBS')
setPrjParameters(prj_path_new, paramName = 'HRU_OBS', paramVals = vals)

# loop through above to set params based on baseline values
lapply(params, function(param) {
  vals <- readPrjParameters(prj_path_old, paramName = param)
  setPrjParameters(prj_path_new, paramName = param, paramVals = vals)
})

CRHMr::setPrjParameters()