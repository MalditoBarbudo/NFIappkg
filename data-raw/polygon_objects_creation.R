municipalities_polygons <- sf::read_sf('data-raw/shapefiles/bm5mv20sh0tpm1_20180101_0.shp') %>%
  rmapshaper::ms_simplify(0.01) %>%
  sf::st_transform('+proj=longlat +datum=WGS84') %>%
  dplyr::select(admin_municipality = NOMMUNI, geometry)

regions_polygons <- sf::read_sf('data-raw/shapefiles/bm5mv20sh0tpc1_20180101_0.shp') %>%
  rmapshaper::ms_simplify(0.01) %>%
  sf::st_transform('+proj=longlat +datum=WGS84') %>%
  dplyr::select(admin_region = NOMCOMAR, geometry)

veguerias_polygons <- sf::read_sf('data-raw/shapefiles/bm5mv20sh0tpv1_20180101_0.shp') %>%
  rmapshaper::ms_simplify(0.01) %>%
  sf::st_transform('+proj=longlat +datum=WGS84') %>%
  dplyr::select(admin_vegueria = NOMVEGUE, geometry)

provinces_polygons <- sf::read_sf('data-raw/shapefiles/bm5mv20sh0tpp1_20180101_0.shp') %>%
  rmapshaper::ms_simplify(0.01) %>%
  sf::st_transform('+proj=longlat +datum=WGS84') %>%
  dplyr::select(admin_province = NOMPROV, geometry)

catalonia_polygons <- sf::read_sf('data-raw/shapefiles/catalunya.shp') %>%
  rmapshaper::ms_simplify(0.01) %>%
  sf::st_transform('+proj=longlat +datum=WGS84') %>%
  dplyr::select(admin_aut_community = NOM_CA, geometry)

enpe_polygons <- sf::read_sf('data-raw/shapefiles/enpe_2017.shp') %>%
  rmapshaper::ms_simplify(0.01) %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

pein_polygons <- sf::read_sf('data-raw/shapefiles/pein_2017.shp') %>%
  rmapshaper::ms_simplify(0.01) %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

xn2000_polygons <- sf::read_sf('data-raw/shapefiles/xn2000_2017.shp') %>%
  rmapshaper::ms_simplify(0.01) %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

devtools::use_data(
  municipalities_polygons, regions_polygons, veguerias_polygons, provinces_polygons,
  catalonia_polygons, enpe_polygons, pein_polygons, xn2000_polygons,


  internal = TRUE, overwrite = TRUE
)
