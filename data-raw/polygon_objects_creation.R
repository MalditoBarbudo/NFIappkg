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

# enpe_polygons
natural_interest_area_polygons <- sf::read_sf('data-raw/shapefiles/enpe_2017.shp') %>%
  rmapshaper::ms_simplify(0.01) %>%
  sf::st_transform('+proj=longlat +datum=WGS84') %>%
  dplyr::select(admin_natural_interest_area = nom, geometry)

# pein_polygons
special_protection_natural_area_polygons <- sf::read_sf('data-raw/shapefiles/pein_2017.shp') %>%
  rmapshaper::ms_simplify(0.01) %>%
  sf::st_transform('+proj=longlat +datum=WGS84') %>%
  dplyr::select(admin_special_protection_natural_area = nom, geometry)

# xn2000_polyogns
natura_network_2000_polygons <- sf::read_sf('data-raw/shapefiles/xn2000_2017.shp') %>%
  rmapshaper::ms_simplify(0.01) %>%
  sf::st_transform('+proj=longlat +datum=WGS84')%>%
  dplyr::select(admin_natura_network_2000 = nom_n2, geometry)

devtools::use_data(
  municipalities_polygons, regions_polygons, veguerias_polygons, provinces_polygons,
  catalonia_polygons, natural_interest_area_polygons,
  special_protection_natural_area_polygons, natura_network_2000_polygons,


  internal = TRUE, overwrite = TRUE
)
