# Rscript - intro_to_geostatistics_with_ibge_data

# Setup -------------------------------------------------------------------

rm(list = ls())
gc(verbose = T)
options(encoding = "UTF-8", stringsAsFactors = F)

# Libraries ---------------------------------------------------------------

if(!require(dplyr)){install.packages("dplyr")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(leaflet)){install.packages("leaflet")}
if(!require(sf)){install.packages("sf")}
if(!require(st)){install.packages("st")}
if(!require(htmltools)){install.packages("htmltools")}
if(!require(rio)){install.packages("rio")}

# Data --------------------------------------------------------------------

# Trabalhando com dados dos Setores Censitárias de Santa Catarina
# GeoCiências > Download > organizacao_do_territorio > malha_territorial > malhas_de_setores_censitarios_divisoes_intramunicipais > (...)

# ShapeFile do Estado de Santa Catarina
# shp_sc_c17 <- sf::st_read("data/sc_setores_censitarios_censo_agro_2017/SC_SETORES_2017_CENSOAGRO.shp")
shp_sc_c10 <- sf::st_read("data/sc_setores_censitarios_censo_2010/42SEE250GC_SIR.shp")

# Classe do objeto
class(shp_sc_c10)

# Correção devido a erros no encoding
shp_sc_c10$NM_BAIRRO <- iconv(shp_sc_c10$NM_BAIRRO, "latin1", "UTF-8")

# Filtro para cidade de Florianópolis
shp_fln <- shp_sc_c10 %>% dplyr::filter(CD_GEOCODM == 4205407)

# Agrupamento dos Setores Censitários por bairros
nb_fln <- shp_fln %>% 
  dplyr::group_by(NM_BAIRRO) %>% 
  dplyr::summarise(
    CD_GEOCODB = as.numeric(dplyr::first(CD_GEOCODB))
  )

# Baixando os dados (falsos) para juntar aos polígonos
skt <- utils::read.csv(file = "data/dados/fln_fakedata.csv")

# Juntando os dados
# sf::st_join()      Para joins especiais (contem*)
fln <- dplyr::left_join(nb_fln, skt[, c("CD_GEOCODB", "skatistas_p100k")], by = "CD_GEOCODB") %>% 
  dplyr::select(NM_BAIRRO, CD_GEOCODB, skatistas_p100k, geometry)

# Pontos de interesse
points <- data.frame(
  lat = c(-48.508450, -48.520692),
  lng = c(-27.571092, -27.600332),
  info = c("Aquarela", "UFSC")
)


# Maps --------------------------------------------------------------------

# plot(nb_fln)

# Labels para os polígonos
labs <- paste0("<b>Bairro: </b>", as.character(fln$NM_BAIRRO), "<br/>",
               "<b>Mais: </b>", as.character("(...)"))

# PopUp dos pontos
pop_up <- paste0("<b>Local: </b>", as.character(points$info), "<br/>",
                 "<b>Latitude: </b>", as.character(points$lat), "<br/>",
                 "<b>Longitude: </b>", as.character(points$lng), "<br/>",
                 "<b>Mais: </b>", as.character("(...)"), "<br/>")

normalize <- function(x){
  x <- x[!is.na(x)]
  return((x-min(x))/(max(x)-min(x)))
}

pal <- leaflet::colorNumeric(c("blue", "red"), 0:1)

leaflet::leaflet() %>% 
  leaflet::addTiles() %>% 
  leaflet::addPolygons(
    data = fln,
    stroke = T,
    weight = 0.6,
    # fillColor = c("red", "blue"),
    # fillColor = RColorBrewer::brewer.pal(n = 8, name = "RdBu"),
    fillColor = ~ pal(normalize(fln$skatistas_p100k)),     # Neste podemos substituir por alguma estatística de interesse. Dessa maneira, o gradiente se altera de acordo com a estatística.
    fill = TRUE,
    fillOpacity = 0.5,
    label = lapply(labs, htmltools::HTML)
  ) %>% 
  leaflet::addAwesomeMarkers(
    data = points,
    lng = ~ lat, 
    lat = ~ lng, 
    popup = ~ pop_up
  ) %>%
  leaflet::addCircles(
    lng = -48.546474, 
    lat = -27.591974,
    radius = 1000,
    color = "green",
    stroke = FALSE, 
    fillOpacity = 0.5,
    popup = "<b>Centro<b/>"
  )

# Rascunho ----------------------------------------------------------------
4205407 # Floripa
4216602 # São José
4211900 # Palhoça
4202305 # Biguaçu
