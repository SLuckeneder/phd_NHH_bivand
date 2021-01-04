

# shape files -------------------------------------------------------------

## geobr package

# install.packages("geobr")
library(geobr)
print(geobr::list_geobr(), n = 30)

years <- c(2000, 2001, 2010, 2013:2019) 
for (yr in years){
  mun <- geobr::read_municipality(code_muni = "all",year = yr, simplified = FALSE, showProgress = TRUE) 
  sf::st_write(mun, paste0("project/ibge_data/geobr/mun_base_", yr, ".shp"))
}
for (yr in years){
  borders <- geobr::read_country(year = yr, simplified = FALSE, showProgress = TRUE) 
  sf::st_write(borders, paste0("project/ibge_data/geobr/borders_base_", yr, ".shp"))
}
# for (yr in years){
#   borders <- geobr::read_state(year = yr, simplified = FALSE, showProgress = TRUE)
#   sf::st_write(borders, paste0("project/ibge_data/geobr/states_base_", yr, ".shp"))
# }




# regional GDP ------------------------------------------------------------

# URLs (download and rename manually, because util.downloadAndUnzi() has encoding issues)
# ftp://ftp.ibge.gov.br/Pib_Municipios/2012/base/base_1999_2012_xlsx.zip
# ftp://ftp.ibge.gov.br/Pib_Municipios/2017/base/base_de_dados_2010_2017_xls.zip





# annual shape files ------------------------------------------------------

# https://github.com/tbrugz/ribge would be cool, but does not work out of the box
# hence I partly use it as an inspiration and partly copy code

states <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT", "PA", "PB", 
            "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO")
sn <- as.character(c(12, 27, 13, ))

### 2005, 2007
# weird

## 2000, 2001, 2010, 2013 - 2014
for (yr in c(2000, 2001, 2010, 2013, 2014)){
  
  cat("\n downloading", yr)
  
  root <- paste0("https://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_", yr, "/")
  
  store <- list()
  for(s in seq_along(states)) {
    temp <- tempfile(tmpdir = "project/ibge_data/temp", fileext = ".zip")
    
    if (yr %in% c(2000, 2010)){
      download.file(paste0(root, "/", tolower(states[s]), "/", tolower(states[s]), "_municipios.zip"), temp)
    }
    if (yr %in% c(2001)){
      download.file(paste0(root, "/", tolower(states[s]), "/", sn[s], "mu2500g.zip"), temp)
    }
    if (yr %in% c(2013, 2014)){
      download.file(paste0(root, "/", states[s], "/", tolower(states[s]), "_municipios.zip"), temp)
    }
    
    unzip(temp, exdir = "project/ibge_data/temp")
    
    if (yr %in% c(2000)){
      mun <- sf::st_read(paste0("project/ibge_data/temp/", sn[s] ,"MU500G.shp"))
    } 
    if (yr %in% c(2001)){
      mun <- sf::st_read(paste0("project/ibge_data/temp/", sn[s], "MU2500G.shp"))
    }
    if (yr %in% c(2013, 2014)){
      mun <- sf::st_read(paste0("project/ibge_data/temp/", sn[s], "MUE250GC_SIR.shp"))
    }
    
    store[[states[s]]] <- mun
  }
  
  # merge
  mun_merge <- do.call(what = sf:::rbind.sf, args = store)
  # plot(mun_merge %>% dplyr::select(AREA_1))
  
  # save
  sf::st_write(mun_merge, paste0("project/ibge_data/geo/mun_base_", yr, ".shp"))
  
}


## 2015 - 2019
for (yr in c(2015:2019)){
  
  cat("\n downloading", yr)
  temp <- tempfile(tmpdir = "project/ibge_data/temp", fileext = ".zip")
  if(yr == 2019){
    download.file(paste0("https://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_", yr, "/Brasil/BR/br_municipios_20200807.zip"), temp)
  } else {
    download.file(paste0("https://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_", yr, "/Brasil/BR/br_municipios.zip"), temp)
    }
  unzip(temp, exdir = "project/ibge_data/temp")
  
  if(yr == 2019){
    mun <- sf::st_read("project/ibge_data/temp/BR_Municipios_2019.shp")
  } else {
    mun <- sf::st_read("project/ibge_data/temp/BRMUE250GC_SIR.shp") # 2015:2018
  }
  sf::st_write(mun, paste0("project/ibge_data/geo/mun_base_", yr, ".shp"))
  
}


# population --------------------------------------------------------------

# 2011
pop_municip_2011 <- readxl::read_excel("project/ibge_data/pop_municip_2011.xls", skip = 2) %>%
  `colnames<-`(c("sigula_uf", "codigo_uf", "cod_municipio_short", "nome_munic", "pop")) %>%
  dplyr::filter(! is.na(pop)) %>%
  dplyr::mutate(cod_municipio = paste0(codigo_uf, cod_municipio_short)) %>%
  dplyr::mutate(ano = 2011)

# 2002
pop_municip_2002 <- readxl::read_excel("project/ibge_data/pop_municip_2002.xls", skip = 4) %>%
  `colnames<-`(c("sigula_uf", "codigo_uf", "cod_municipio_short", "nome_munic", "pop")) %>%
  dplyr::filter(! is.na(pop)) %>%
  dplyr::mutate(ano = 2002)
pop_municip_2002 <- dplyr::left_join(pop_municip_2011 %>% dplyr::select("codigo_uf", "cod_municipio", "nome_munic") %>% dplyr::mutate(unid = paste0(codigo_uf, nome_munic)), 
                                     pop_municip_2002 %>% 
                                       dplyr::mutate(unid = paste0(codigo_uf, nome_munic)) %>%
                                       dplyr::select(-codigo_uf, -nome_munic), 
                                     by = "unid") %>%
  dplyr::select(-unid) %>%
  dplyr::mutate(cod_municipio_short = as.character(cod_municipio_short))

dat_ibge_pop <- dplyr::bind_rows(pop_municip_2002, pop_municip_2011)

write.csv(dat_ibge_pop,"project/ibge_data/pop_municip_2002_2011.csv", row.names = FALSE)



