# Helpers

dedupe <- function(r) {
  shiny::makeReactiveBinding("val")
  shiny::observe(val <<- r(), priority = 10)
  shiny::reactive(val)
}

hri_builder <- function(data_inputs) {
  nfi <- toupper(data_inputs$nfi)
  viz_shape <- data_inputs$viz_shape
  functional_group <- data_inputs$functional_group
  diameter_classes <- data_inputs$diameter_classes
  filter_expressions <- vapply(
    data_inputs$filter_expressions,
    rlang::expr_text,
    character(1)
  )

  shiny::tagList(
    shiny::h5(glue::glue('Used data: {nfi}')),
    shiny::h5(glue::glue('Summarised by {viz_shape}')),
    shiny::h5(glue::glue('Breakdown level: {functional_group}')),
    shiny::h5(glue::glue('Extra breakdown by diameter classes?: {diameter_classes}')),
    shiny::h5(glue::glue("Used filters: ")),
    lapply(filter_expressions, function(x) {
      shiny::p(x)
    })
  )
}

var_names_input_builder <- function(vars, lang, nfidb, summ = FALSE) {

  if (summ) {

    vars_id <- stringr::str_remove(vars, '_mean$|_se$|_min$|_max$|_n$')
    vars_stat <- stringr::str_extract(vars, '_mean$|_se$|_min$|_max$|_n$') %>%
      stringr::str_remove('_')

    vars_trans <- dplyr::tbl(nfidb, 'VARIABLES_THESAURUS') %>%
      dplyr::select(dplyr::one_of('var_id', glue::glue('translation_{lang}'))) %>%
      dplyr::filter(var_id %in% vars_id) %>%
      dplyr::distinct() %>%
      dplyr::collect() %>%
      as.data.frame()

    dummy_creator <- function(x, y) {
      name <- vars_trans[vars_trans$var_id == x, glue::glue('translation_{lang}')]
      glue::glue("{name} {y}")
    }

    vars_names <- vars_id %>%
      purrr::map2_chr(
        vars_stat,
        ~ dummy_creator(.x, .y)
      )

    names(vars) <- vars_names

  } else {
    vars_trans <- dplyr::tbl(nfidb, 'VARIABLES_THESAURUS') %>%
      dplyr::select(dplyr::one_of('var_id', glue::glue('translation_{lang}'))) %>%
      dplyr::filter(var_id %in% vars) %>%
      dplyr::distinct() %>%
      dplyr::collect() %>%
      as.data.frame()

    vars_names <- vars %>%
      purrr::map_chr(
        ~ vars_trans[vars_trans$var_id == .x, glue::glue('translation_{lang}')]
      )

    names(vars) <- vars_names
  }

  return(vars[!is.na(names(vars))])
}
