
# https://github.com/tbrugz/ribge would be cool, but does not work out of the box
# hence I partly use it as an inspiration and partly copy code



# annual shape files ------------------------------------------------------

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
    mun <- sf::st_read("project/ibge_data/temp/BRMUE250GC_SIR.shp") # 2017
  }
  sf::st_write(mun, paste0("project/ibge_data/geo/mun_base_", yr, ".shp"))
  
}





# regional GDP ------------------------------------------------------------

# URLs (download and rename manually, because util.downloadAndUnzi() has encoding issues)
# ftp://ftp.ibge.gov.br/Pib_Municipios/2012/base/base_1999_2012_xlsx.zip
# ftp://ftp.ibge.gov.br/Pib_Municipios/2017/base/base_de_dados_2010_2017_xls.zip


