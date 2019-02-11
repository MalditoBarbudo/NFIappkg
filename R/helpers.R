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

text_translate <- function(text, lang, texts_thes) {

  text[is.na(text)] <- 'NA_'

  text_df <- texts_thes %>%
    dplyr::select(dplyr::one_of('text_id', glue::glue("text_{lang}"))) %>%
    dplyr::filter(text_id %in% text) %>%
    # dplyr::collect() %>%
    as.data.frame()

  text %>%
    purrr::map_chr(
      ~ text_df[text_df$text_id == .x, glue::glue('text_{lang}')]
    )
}

var_names_input_builder <- function(
  vars, lang, var_thes, texts_thes, tables_names, summ = FALSE
) {

  if (is.null(lang)) {
    lang <- 'eng'
  }

  if (summ) {

    vars_id <- stringr::str_remove(vars, '_mean$|_se$|_min$|_max$|_n$')
    vars_stat <- stringr::str_extract(vars, '_mean$|_se$|_min$|_max$|_n$') %>%
      stringr::str_remove('_') %>%
      text_translate(lang, texts_thes) %>%
      tolower()

    vars_trans <- var_thes %>%
      dplyr::select(
        dplyr::one_of(
          'var_id', glue::glue('translation_{lang}'), 'var_order_app', 'var_table'
        )
      ) %>%
      dplyr::filter(var_id %in% vars_id, var_table %in% tables_names) %>%
      dplyr::distinct() %>%
      as.data.frame()

    dummy_creator <- function(x, y) {
      name <- vars_trans[vars_trans$var_id == x, glue::glue('translation_{lang}')]
      if (is.na(y)) {
        name
      } else {
        glue::glue("{name} {y}")
      }
    }

    vars_names <- vars_id %>%
      purrr::map2_chr(
        vars_stat,
        ~ dummy_creator(.x, .y)
      )

    names(vars) <- vars_names

  } else {
    vars_trans <- var_thes %>%
      dplyr::select(
        dplyr::one_of(
          'var_id', glue::glue('translation_{lang}'), 'var_order_app', 'var_table'
        )
      ) %>%
      dplyr::filter(var_id %in% vars, var_table %in% tables_names) %>%
      dplyr::select(-dplyr::one_of(
          'var_table'
      )) %>%
      dplyr::distinct() %>%
      as.data.frame()

    vars_names <- vars %>%
      purrr::map_chr(
        ~ vars_trans[vars_trans$var_id == .x, glue::glue('translation_{lang}')]
      )

    names(vars) <- vars_names
  }

  # we need the variables ordered with sense, first the admin and id variables, later the
  # proper table variables, the clim/topo/feat variables... So, let's order them
  vars_trans %>%
    dplyr::arrange(var_order_app) %>%
    dplyr::pull(var_id) %>%
    match(vars, .) %>%
    order() -> order_of_vars

  ordered_res <- vars[order_of_vars]

  return(ordered_res)
}

# Aggregator of inputs
var_inputs_aggregator <- function(ready_vars, lang, texts_thes) {

  list(
    admin = ready_vars[stringr::str_detect(ready_vars, 'admin_')],
    id = ready_vars[stringr::str_detect(ready_vars, '_id')],
    proper_table = ready_vars[
      !stringr::str_detect(ready_vars, 'admin_') &
      !stringr::str_detect(ready_vars, '_id') &
      !stringr::str_detect(ready_vars, 'clim_') &
      !stringr::str_detect(ready_vars, 'topo_') &
      !stringr::str_detect(ready_vars, 'feat_') &
      !stringr::str_detect(ready_vars, 'coord_') &
      !stringr::str_detect(ready_vars, 'old_')
    ],
    clim = ready_vars[stringr::str_detect(ready_vars, 'clim_')],
    topo = ready_vars[stringr::str_detect(ready_vars, 'topo_')],
    feat = ready_vars[stringr::str_detect(ready_vars, 'feat_')],
    coord = ready_vars[stringr::str_detect(ready_vars, 'coord_')],
    old = ready_vars[stringr::str_detect(ready_vars, 'old_')]
  ) %>%
    magrittr::set_names(c(
      text_translate('admin', lang, texts_thes),
      text_translate('id', lang, texts_thes),
      text_translate('proper_table', lang, texts_thes),
      text_translate('clim', lang, texts_thes),
      text_translate('topo', lang, texts_thes),
      text_translate('feat', lang, texts_thes),
      text_translate('coord', lang, texts_thes),
      text_translate('old', lang, texts_thes)
    ))

}

# Call this function with an input (such as `textInput("text", NULL, "Search")`) if you
# want to add an input to the navbar (from dean attali,
# https://github.com/daattali/advanced-shiny)
navbarPageWithInputs <- function(..., inputs) {
  navbar <- shiny::navbarPage(...)
  form <- shiny::tags$form(class = "navbar-form", inputs)

  # browser()

  navbar[[3]][[1]]$children[[1]]$children[[2]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]]$children[[2]], form
  )
  navbar
}