# renv::activate()
# renv::snapshot()
# renv::restore()
# renv::load()

CWP_regular_grid_areas <- read.csv("https://github.com/fdiwg/fdi-codelists/raw/refs/heads/main/global/cwp/cl_areal_grid.csv") %>% 
  dplyr::mutate(locationID = as.character(code)) %>% 
  dplyr::select(locationID,GRIDTYPE,geom_wkt) %>% 
  dplyr::rename('gridtype' = GRIDTYPE, geom=geom_wkt)  %>% 
  # dplyr::mutate('gridtype' = case_when(gridtype == '1deg_x_1deg' ~ '1deg_x_1deg',
  #                                      gridtype == '5deg_x_5deg' ~ '5deg_x_5deg',
  #                                      TRUE ~ 'others')) %>%
  sf::st_as_sf(wkt="geom", crs=4326)


cl_asfis_species <- read.csv("https://raw.githubusercontent.com/fdiwg/fdi-codelists/refs/heads/main/global/cwp/cl_asfis_species.csv")
colnames(cl_asfis_species)

DATA_DIR= "./data"

list_files <- paste0(DATA_DIR,"/SizeClass_files.csv")
DOIs <- readr::read_csv(list_files) 
# Use the function with lapply for each DOI
df_dois <-lapply(1:nrow(DOIs), function(i) {
  this_doi <- DOIs[i,]
  this_df <- NULL
  # record_id <- gsub(".*\\.", "",this_doi$url_zip)
  download.file(url = this_doi$url_zip,
                destfile = paste(DATA_DIR,this_doi$zip_file_name,sep="/"))
  # unzip
  unzip(zipfile = paste(DATA_DIR,this_doi$zip_file_name,sep="/"), exdir = DATA_DIR, overwrite = TRUE)
  # this_doi$filename <- gsub("\\..*", "",this_doi$url)
  if(gsub(".*\\.", "",this_doi$csv_file_name)=="csv"){
    this_df <- read.csv(file = paste(DATA_DIR,this_doi$csv_file_name,sep="/"), stringsAsFactors = FALSE)
  }
  # this_doi$file_mime <-  gsub(".*\\.", "",this_doi$url)
  this_list <-list("metadata"=this_doi,"data"=this_df)
})
loaded_data <- do.call(rbind, lapply(df_dois, function(l) l[[2]]))
colnames(loaded_data)

dwc_melted <- loaded_data %>% rename(code=SPECIES_CODE,
                                           year=YEAR,month=MONTH_START,
                                           locationID=FISHING_GROUND_CODE,
                                           individualCount=FISH_COUNT
                                     ) %>%    
  dplyr::filter(REPORTING_QUALITY==4)  %>% 
  mutate(eventDate=paste(eventDate=paste(year,month,"01T00:00:00",sep="-")),
         recordedBy = paste0("Individuals caught by ",GEAR_CODE),
         depth ="UNK",
         # sizeClass = paste0("C",CLASS_LOW,"-",CLASS_HIGH,"=",individualCount)
         sizeClass = paste0("C",CLASS_LOW,"-",CLASS_HIGH,"=",individualCount)
  ) %>% 
  select(recordedBy,depth,code,sizeClass,individualCount,eventDate,year,locationID) %>%
  dplyr::group_by(recordedBy,depth,code,eventDate,year,locationID) %>% 
  dplyr::summarise(sizeClasses=paste0(sizeClass,collapse = ","),count = n_distinct(individualCount))  %>% 
  dplyr::left_join((CWP_regular_grid_areas %>% as_tibble()), by=c('locationID'))  %>%
  dplyr::left_join((cl_asfis_species %>% as_tibble()), by=c('code')) %>%
  dplyr::select(-c("uri","label","definition","name_en","name_fr","name_es","name_ar","name_cn","name_ru","isscaap_group_code","taxon_code","taxon_author"))  %>% 
  rename(cl_asfis_species_code=code,order=taxon_order,family=taxon_family,scientificName=taxon_scientific_name) %>% 
  mutate(decimalLatitude=st_coordinates(st_centroid(st_geometry(geom)), crs = 4326)[,2],
         decimalLongitude=st_coordinates(st_centroid(st_geometry(geom)), crs = 4326)[,1]) %>% 
  dplyr::filter(!is.nan(decimalLatitude)) %>% 
  sf::st_as_sf(., crs=4326)
# %>%    mutate(gbifID = row_number(.))

dwc_melted$gbifID <-1:nrow(dwc_melted)
saveRDS(object = dwc_melted,file = paste(DATA_DIR,"dwc_melted.rds",sep="/"))