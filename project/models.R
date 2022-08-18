
library(dplyr)
library(tidyr)
library(sf)
library(stringr)

library(geobr)
print(geobr::list_geobr(), n = 5)

from_yr <- 2011
to_yr <- 2017

# data --------------------------------------------------------------------


yr <- ifelse(from_yr == 2011, 2010, from_yr)

if(!file.exists(paste0("project/ibge_data/geobr/base_nat_", yr, ".shp"))){
  base_nat <- geobr::read_country(year = yr, simplified = FALSE) 
  sf::st_write(base_nat, paste0("project/ibge_data/geobr/base_nat_", yr, ".shp"))
} else {
  base_nat <- sf::read_sf("project/ibge_data/geobr/base_nat_2001.shp")
}

if(!file.exists(paste0("project/ibge_data/geobr/base_sta_", yr, ".shp"))){
  base_sta <- geobr::read_state(year = yr, simplified = FALSE)
  sf::st_write(base_sta, paste0("project/ibge_data/geobr/base_sta_", yr, ".shp"))
} else {
  base_sta <- sf::read_sf("project/ibge_data/geobr/base_sta_2001.shp")
}

if(!file.exists(paste0("project/ibge_data/geobr/base_mun_", yr, ".shp"))){
  base_mun <- geobr::read_municipality(code_muni = "all", year = yr, simplified = FALSE) 
  sf::st_write(base_mun, paste0("project/ibge_data/geobr/base_mun_", yr, ".shp"))
} else {
  base_mun <- sf::read_sf("project/ibge_data/geobr/base_mun_2001.shp")
}

base_nat <- base_nat %>% 
  sf::st_transform(crs = sf::st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
base_sta <- base_sta %>% 
  sf::st_transform(crs = sf::st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
base_mun <- base_mun %>% 
  dplyr::mutate(code_mn = as.character(code_mn)) %>%
  sf::st_transform(crs = sf::st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))


bra_mines <- sf::read_sf("project/mine_data/bra_mines.shp")
bra_mines <- bra_mines %>%
  dplyr::mutate(mine_count = ifelse(mine_count == 0, NA, mine_count))
# wdpa_bra <- sf::read_sf("project/wdpa_data/wdpa_bra.shp")

library(tmap)

# regs <- c("MG", "AC", "AM", "RO", "RR", "AP", "PA", "TO", "MT")

p <- tmap::tm_shape(bra_mines) +
  tmap::tm_borders(lwd=0.3, alpha=0.4) +
  tmap::tm_fill(col = "mine_count", style = "pretty", as.count = TRUE,
                colorNA = "white", textNA = "0",
                palette = "-viridis",
                title = "No. of active mines (n = 299)", legend.reverse = TRUE) +
  # tmap::tm_shape(base_sta %>% dplyr::filter(abbrv_s %in% regs)) + tmap::tm_borders(lwd=1) +
  tmap::tm_shape(base_sta) + tmap::tm_borders(lwd=1) +
  tmap::tm_shape(base_nat) + tmap::tm_borders(lwd=2) +
  tmap::tm_layout(legend.position = c("left", "bottom"))
tmap_save(p, "project/mine_data/bra_mines_map.png")



# regs <- c("MG") # Minas Gerais
# regs_wdpa <- stringr::str_subset(unique(wdpa_bra$SUB_LOC), paste(regs, collapse = "|"))
# 
# if (! file.exists("wdpa_data/wdpa_bra_clean_MG_indig.shp")){
#   wdpa_bra_clean_MG_indig <- wdpa_bra %>% 
#     dplyr::filter(SUB_LOC %in% regs_wdpa & TYPE == "Indigenous") %>% sf::st_union()
#   wdpa_bra_clean_MG_other <- wdpa_bra %>% 
#     dplyr::filter(SUB_LOC %in% regs_wdpa & TYPE == "other") %>% sf::st_union()
#   sf::st_write(wdpa_bra_clean_MG_indig, dsn = "wdpa_data/wdpa_bra_clean_MG_indig.shp")
#   sf::st_write(wdpa_bra_clean_MG_other, dsn = "wdpa_data/wdpa_bra_clean_MG_other.shp")
# } else {
#   wdpa_bra_clean_MG_indig <- sf::read_sf("wdpa_data/wdpa_bra_clean_MG_indig.shp")
#   wdpa_bra_clean_MG_other <- sf::read_sf("wdpa_data/wdpa_bra_clean_MG_other.shp")
# }
# 
# wdpa_bra_clean_MG <- rbind(
#   wdpa_bra_clean_MG_indig %>% dplyr::mutate("TYPE" = "Indigenous"),
#   wdpa_bra_clean_MG_other %>% dplyr::mutate("TYPE" = "Other"))


# tmap::tm_shape(bra_mines %>% dplyr::filter(abbrv_s %in% regs)) + 
#   tmap::tm_borders(lwd=0.5, alpha=0.4) +
#   tmap::tm_fill(col = "mine_count", n = 4, style = "pretty", as.count = TRUE, 
#                 colorNA = "white", textNA = "0",
#                 palette = "-viridis",
#                 title = "No. of active mines (n = 96)", legend.reverse = TRUE) +
#   tmap::tm_shape(wdpa_bra_clean_MG) +
#   tmap::tm_fill(col = "TYPE", palette = c("red", "darkgrey"), alpha=0.3, 
#                 title = "Protected area") +
#   tmap::tm_shape(base_sta %>% dplyr::filter(abbrv_s %in% regs)) + tmap::tm_borders(lwd=2) +
#   tmap::tm_layout(main.title = "Minas Gerais", legend.position = c("left", "top")) + 
#   tmap::tm_scale_bar(position=c("right", "bottom"))


# regs <- c("AC", "AM", "RO", "RR", "AP", "PA", "TO", "MT") # North Region + Mato Grosso
# regs_wdpa <- str_subset(unique(wdpa_bra$SUB_LOC), paste(regs, collapse = "|"))
# 
# if (! file.exists("wdpa_data/wdpa_bra_clean_north_indig.shp")){
#   wdpa_bra_clean_north_indig <- wdpa_bra %>% 
#     dplyr::filter(SUB_LOC %in% regs_wdpa & TYPE == "Indigenous") %>% sf::st_union()
#   wdpa_bra_clean_north_other <- wdpa_bra %>% 
#     dplyr::filter(SUB_LOC %in% regs_wdpa & TYPE == "other") %>% sf::st_union()
#   sf::st_write(wdpa_bra_clean_north_indig, dsn = "wdpa_data/wdpa_bra_clean_north_indig.shp")
#   sf::st_write(wdpa_bra_clean_north_other, dsn = "wdpa_data/wdpa_bra_clean_north_other.shp")
# } else {
#   wdpa_bra_clean_north_indig <- sf::read_sf("wdpa_data/wdpa_bra_clean_north_indig.shp")
#   wdpa_bra_clean_north_other <- sf::read_sf("wdpa_data/wdpa_bra_clean_north_other.shp")
# }


# wdpa_bra_clean_north <- rbind(
#   wdpa_bra_clean_north_indig %>% dplyr::mutate("TYPE" = "Indigenous"),
#   wdpa_bra_clean_north_other %>% dplyr::mutate("TYPE" = "Other"))
# 
# tmap::tm_shape(bra_mines %>% dplyr::filter(abbrv_s %in% regs)) + 
#   tmap::tm_borders(lwd=0.5, alpha=0.4) +
#   tmap::tm_fill(col = "mine_count", n = 4, style = "pretty", as.count = TRUE, 
#                 colorNA = "white", textNA = "0", palette = "-viridis",
#                 title = "No. of active \nmines (n = 103)", legend.reverse = TRUE) +
#   tmap::tm_shape(wdpa_bra_clean_north) +
#   tmap::tm_fill(col = "TYPE", palette = c("red", "darkgrey"), alpha=0.3, title = "Protected area") +
#   tmap::tm_shape(base_sta %>% dplyr::filter(abbrv_s %in% regs)) + tmap::tm_borders(lwd=2) +
#   tmap::tm_layout(main.title = "North Region and Mato Grosso", 
#                   legend.position = c("left", "bottom"), legend.stack = "horizontal") + 
#   tmap::tm_scale_bar(position=c("right", "bottom"))


# dat_ibge_1 <- readxl::read_excel("project/ibge_data/gdp_munip_1999-2012.xlsx")
# colnames(dat_ibge_1) <- c("ano", "codigo_uf", "nome_uf",
#                           "cod_municipio", "nome_munic", "nome_metro",
#                           "codigo_meso", "nome_meso", "codigo_micro", "nome_micro",
#                           "vab_agropecuaria", "vab_industria", 
#                           "vab_servicos_exclusivo", "vab_adm_publica", 
#                           "impostos", "pib_total",
#                           "pop_last", "pib_per_capita_last")
# dat_ibge_1 <- dat_ibge_1 %>%
#   dplyr::filter(ano < 2010) %>%
#   dplyr::mutate(cod_municipio = as.character(cod_municipio),
#                 ano = as.numeric(ano)) 
# dat_ibge_pop <- read.csv("project/ibge_data/pop_municip_2002_2011.csv")
# dat_ibge_1 <- dat_ibge_1 %>% dplyr::mutate(unid = paste0(ano, cod_municipio)) %>%
#   dplyr::left_join(dat_ibge_pop %>% dplyr::mutate(unid = paste0(ano, cod_municipio)) %>% dplyr::select(unid, pop), by = "unid") %>%
#   dplyr::mutate(pib_per_capita = pib_total * 1000 / pop)

dat_ibge_2 <- readxl::read_excel("project/ibge_data/gdp_munip_2010-2017.xls")
colnames(dat_ibge_2) <- c("ano", "codigo_regiao", "nome_regiao",
                          "codigo_uf", "sigla_uf", "nome_uf",
                          "cod_municipio", "nome_munic", "nome_metro",
                          "codigo_meso", "nome_meso", "codigo_micro", "nome_micro",
                          "codigo_reg_geo_imediata", "nome_reg_geo_imediata", "mun_reg_geo_imediata",
                          "codigo_reg_geo_intermediaria", "nome_reg_geo_intermediaria", "mun_reg_geo_intermediaria",
                          "codigo_concentracao_urbana", "nome_concentracao_urbana", "tipo_concentracao_urbana",
                          "codigo_arranjo_populacional", "nome_arranjo_populacional",
                          "hierarquia_urbana", "hierarquia_urbana_principais",
                          "codigo_regiao_rural", "nome_regiao_rural", "regiao_rural_classificacao",
                          "amazonia_legal", "semiarido", "cidade_de_sao_paulo",
                          "vab_agropecuaria", "vab_industria", "vab_servicos_exclusivo", "vab_adm_publica", "vab_total",
                          "impostos", "pib_total",
                          "pib_per_capita",
                          "atividade_vab1", "atividade_vab2", "atividade_vab3")
dat_ibge_2 <- dat_ibge_2 %>%
  dplyr::mutate(cod_municipio = as.character(cod_municipio),
                codigo_meso = as.character(codigo_meso),
                codigo_micro = as.character(codigo_micro)) %>%
  dplyr::mutate(pop =  pib_total * 1000 / pib_per_capita)

# dat_ibge <- dplyr::bind_rows(dat_ibge_1, dat_ibge_2)
dat_ibge <- dat_ibge_2

dat_gdp <- dat_ibge %>% dplyr::filter(ano == from_yr) %>% 
  dplyr::mutate(pib_total = pib_total / 1000)

dat_gdp <- base_mun %>% dplyr::left_join(dat_gdp, by = c("code_mn" = "cod_municipio"))

# dat_growth <- dat_ibge %>% dplyr::filter(ano %in% c(2002, 2011)) %>% 
#   dplyr::select(cod_municipio, ano, pib_total) %>%
#   tidyr::spread(key = "ano", value = "pib_total") %>%
#   dplyr::mutate(g = (`2011` - `2002`) / `2002` * 100) %>%
#   dplyr::select(cod_municipio, g)
dat_growth <- dat_ibge %>% dplyr::filter(ano %in% c(from_yr, to_yr)) %>% 
  dplyr::select(cod_municipio, ano, pib_per_capita) %>%
  tidyr::spread(key = "ano", value = "pib_per_capita") %>%
  `colnames<-`(c("cod_municipio", "from_yr", "to_yr")) %>%
  dplyr::mutate(g_cap = (to_yr - from_yr) / from_yr * 100) %>%
  dplyr::select(cod_municipio, g_cap)

dat_gdp <- dat_gdp %>% 
  dplyr::left_join(dat_growth, by = c("code_mn" = "cod_municipio"))

p <- tmap::tm_shape(dat_gdp) +
  tmap::tm_facets(nrow = 1) +
  tmap::tm_fill(col = c("pib_total", "g_cap"), n=8, style="quantile", palette = "-magma", title = "") +
  tmap::tm_layout(legend.position = c("left", "bottom"),
                  panel.labels=c("2011 per capita GDP (current BRL)", "2011-2017 growth rate (%)"),
                  panel.label.bg.color = "white")
  # tmap::tm_layout(legend.position = c("left", "bottom"),
  #                 panel.labels=c("2002 per capita GDP (current BRL)", "2002-2011 growth rate (%)"))
tmap_save(p, "project/mine_data/bra_gdp_map.png")


dat_pop <- dat_ibge %>% dplyr::filter(ano == from_yr)
dat_pop <- base_mun %>% dplyr::left_join(dat_pop, by = c("code_mn" = "cod_municipio"))

dat_popgrowth <- dat_ibge %>% dplyr::filter(ano %in% c(from_yr, to_yr)) %>% 
  dplyr::select(cod_municipio, ano, pop) %>%
  tidyr::spread(key = "ano", value = "pop") %>%
  `colnames<-`(c("cod_municipio", "from_yr", "to_yr")) %>%  
  dplyr::mutate(g_pop = (to_yr - from_yr) / from_yr * 100) %>%
  dplyr::select(cod_municipio, g_pop)

dat_pop <- dat_pop %>% 
  dplyr::left_join(dat_popgrowth, by = c("code_mn" = "cod_municipio"))

# tmap::tm_shape(dat_pop) + 
#   tmap::tm_facets(nrow = 1) + 
#   tmap::tm_fill(col = c("pop", "g_pop"), n=8, style="quantile", title = "") +
#   tmap::tm_layout(legend.position = c("left", "bottom"),
#                   panel.labels=c("2002 population", "2002-2011 population growth"))


dat_sect <- dat_ibge %>% dplyr::filter(ano == from_yr) %>% 
  dplyr::mutate(cod_municipio = as.character(cod_municipio)) %>% 
  dplyr::mutate(vab_agropecuaria = ifelse(vab_agropecuaria < 0, 0, vab_agropecuaria)) %>%
  dplyr::mutate(vab_total = vab_agropecuaria + vab_industria + vab_servicos_exclusivo + vab_adm_publica) %>%
  dplyr::mutate(vab_agropecuaria_perc = vab_agropecuaria / vab_total,
                vab_industria_perc = vab_industria / vab_total,
                vab_servicos_exclusivo_perc = vab_servicos_exclusivo / vab_total,
                vab_adm_publica_perc = vab_adm_publica / vab_total)

dat_sect <- base_mun %>% dplyr::left_join(dat_sect, by = c("code_mn" = "cod_municipio"))

# tmap::tm_shape(dat_sect) + 
#   tmap::tm_facets(nrow = 2) + 
#   tmap::tm_fill(col = c("vab_agropecuaria_perc", "vab_industria_perc", "vab_servicos_exclusivo_perc", "vab_adm_publica_perc"), 
#                 n=8, style="quantile", title = "", palette = "-magma") +
#   tmap::tm_layout(legend.position = c("left", "bottom"),
#                   panel.labels=c("2002 GVA acgriculture (%)", "2002 GVA industry (%)", "2002 GVA services (%)", "2002 GVA public sector (%)"))

# W -----------------------------------------------------------------------


library(spdep)

# nb_q <- spdep::poly2nb(dat_gdp, queen=TRUE)
# summary(nb_q)
# base_mun <- base_mun[-c(1523, 3496),]

dat_gdp <- dat_gdp[!is.na(dat_gdp$pib_per_capita),]
dat_sect <- dat_sect %>% dplyr::filter(code_mn %in% dat_gdp$code_mn)
dat_pop <- dat_pop %>% dplyr::filter(code_mn %in% dat_gdp$code_mn)

coords <- sf::st_centroid(sf::st_geometry(dat_gdp), of_largest_polygon=TRUE)
nb_5nnb <- spdep::knearneigh(coords, k=5)
nb_5nnb <- spdep::knn2nb(nb_5nnb, sym=F)
plot(sf::st_geometry(base_mun), border="grey", lwd=0.5)
plot(nb_5nnb, coords=st_coordinates(coords), add=TRUE, points=FALSE, lwd=0.2)

lwW <- spdep::nb2listw(nb_5nnb, style="W")
save(lwW, file = paste0("project/weights_matrix_", from_yr, "_", to_yr, ".Rdata"))

# spatial autocorrelation -------------------------------------------------


# out <- dplyr::bind_rows(
#   broom::tidy(spdep::moran.test(dat_gdp$pib_per_capita, listw=lwW, randomisation=FALSE, alternative="two.sided"))[1:5],
#   broom::tidy(spdep::moran.test(dat_gdp$g_cap, listw=lwW, 
#                                 randomisation=FALSE, alternative="two.sided"))[1:5],
#   broom::tidy(spdep::moran.test(dat_pop$g_pop, listw=lwW, 
#                                 randomisation=FALSE, alternative="two.sided"))[1:5],
#   broom::tidy(spdep::moran.test(dat_sect$vab_agropecuaria_perc, listw=lwW, 
#                                 randomisation=FALSE, alternative="two.sided"))[1:5],
#   broom::tidy(spdep::moran.test(dat_sect$vab_industria_perc, listw=lwW, 
#                                 randomisation=FALSE, alternative="two.sided"))[1:5],
#   broom::tidy(spdep::moran.test(dat_sect$vab_servicos_exclusivo_perc, listw=lwW, 
#                                 randomisation=FALSE, alternative="two.sided"))[1:5],
#   broom::tidy(spdep::moran.test(dat_sect$vab_adm_publica_perc, listw=lwW, 
#                                 randomisation=FALSE, alternative="two.sided"))[1:5])
# names(out)[1:3] <- c("Moran's I", "Expectation", "Variance")
# 
# 
# out <- dplyr::bind_cols("Variable" = c("2002 GDP (per capita, BRL)", 
#                                        "2002-2011 per cap. GDP growth (%)", 
#                                        "2002-2011 population growth (%)", 
#                                        "2002 GVA acgriculture (%)", "2002 GVA industry (%)", 
#                                        "2002 GVA services (%)", "2002 GVA public sector (%)"), out)
# 
# 
# 
# spdep::moran.plot(dat_gdp$pib_per_capita, listw=lwW)
# spdep::moran.plot(dat_gdp$g_cap, listw=lwW)

# local Morans I

# localm_pib <- spdep::localmoran(dat_gdp$pib_per_capita, listw=lwW, alternative = "two.sided") %>% 
#   dplyr::as_data_frame() 
# colnames(localm_pib) <- c(colnames(localm_pib)[-5], "p_pib")
# localm_pib <- sf::st_as_sf(localm_pib %>% 
#                              dplyr::mutate(geometry = sf::st_geometry(dat_gdp)) %>%
#                              dplyr::mutate(code_mn = dat_gdp$code_mn)) %>% 
#   dplyr::mutate(lsa_pib = ifelse(Ii > 0, "positive", "negative"))
# 
# localm_g <- spdep::localmoran(dat_gdp$g_cap, listw=lwW, alternative = "two.sided") %>% 
#   dplyr::as_data_frame() 
# colnames(localm_g) <- c(colnames(localm_g)[-5], "p_g")
# localm_g <- localm_g %>% dplyr::mutate(code_mn = dat_gdp$code_mn) %>%
#   dplyr::mutate(lsa_g = ifelse(Ii > 0, "positive", "negative"))
# 
# localm <- dplyr::left_join(localm_pib, localm_g, by = "code_mn") %>%
#   dplyr::mutate(lsa_pib = ifelse(p_pib < 0.05, lsa_pib, NA),
#                 lsa_g = ifelse(p_g < 0.05, lsa_g, NA)) 
# 
# tmap::tm_shape(base_nat) + tmap::tm_borders(lwd=2) +
# tmap::tm_shape(localm) + 
#   tmap::tm_facets(nrow = 1) + 
#   tmap::tm_borders(lwd=0.1, alpha=0.4) +
#   tmap::tm_fill(col = c("lsa_pib", "lsa_g"), style = "cat", palette = "Set1",
#                 title = "Local Moran's I",
#                 colorNA = "white", textNA = "not significant",) +
# tmap::tm_layout(legend.position = c("left", "bottom"),
#                   panel.labels=c("2002 GDP (per capita, BRL)", "2002-2011 growth rate (%)"))


# classical lm ------------------------------------------------------------

reg_data <- dplyr::left_join(
  dat_gdp %>% dplyr::select(code_mn, g_cap, pib_per_capita),
  dat_pop %>% dplyr::select(code_mn, g_pop) %>% sf::st_drop_geometry(), by = "code_mn") %>%
  dplyr::left_join(dat_sect %>% dplyr::select(code_mn, vab_agropecuaria_perc, vab_industria_perc, vab_servicos_exclusivo_perc, vab_adm_publica_perc) %>% 
                     sf::st_drop_geometry(), by = "code_mn")

reg_data <- reg_data %>% 
  dplyr::left_join(bra_mines %>% 
                     sf::st_drop_geometry() %>% 
                     dplyr::mutate(code_mn = as.character(code_mn)) %>%
                     dplyr::select(code_mn, mine_count), 
                   by = c("code_mn" = "code_mn")) %>% dplyr::select(-code_mn) %>%
  sf::st_drop_geometry() %>%
  dplyr::mutate(mine = ifelse(is.na(mine_count), 0, 1)) %>%
  dplyr::select(-mine_count)

# export data
data <- reg_data %>% dplyr::mutate(int = 1) %>% dplyr::select(1, 9, 8, 2, 3, 4, 5, 6) %>% as.matrix()
save(data, file = paste0("project/data_matrix_", from_yr, "_", to_yr, ".Rdata"))

# OLS
m <- g_cap ~ log(pib_per_capita) + g_pop + mine + vab_agropecuaria_perc + vab_industria_perc + vab_servicos_exclusivo_perc
m_ols <- lm(formula = m, data = reg_data); summary(m_ols)

# test residuals
# spdep::lm.morantest(m_ols, listw=lwW, alternative="two.sided")




# # SAR ---------------------------------------------------------------------
# 
# 
# m_sar <- spatialreg::lagsarlm(formula = m, data = reg_data, listw = lwW); summary(m_sar)
# 
# 
# # SLX ---------------------------------------------------------------------
# 
# m_slx <- spatialreg::lmSLX(formula = m, data = reg_data, listw = lwW); summary(m_slx)


# SDM ---------------------------------------------------------------------

m_sdm <- spatialreg::lagsarlm(formula = m, data = reg_data, listw = lwW, Durbin = TRUE); summary(m_sdm)



# tests -------------------------------------------------------------------


reg_data$res <- residuals(m_sar)
spdep::moran.mc(reg_data$res, lwW, 999)

reg_data$res <- residuals(m_slx)
spdep::moran.mc(reg_data$res, lwW, 999)

reg_data$res <- residuals(m_sdm)
spdep::moran.mc(reg_data$res, lwW, 999)


# impacts -----------------------------------------------------------------

spdep::impacts(m_sar, listw = lwW)
spdep::impacts(m_sdm, listw = lwW)

