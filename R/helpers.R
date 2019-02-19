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

  if (nrow(text_df) < 1) {
    stop(glue::glue("{text} not found in thesaurus"))
  }

  text %>%
    purrr::map_chr(
      ~ text_df[text_df$text_id == .x, glue::glue('text_{lang}')]
    )
}

var_names_input_builder <- function(
  vars, lang, var_thes, texts_thes, tables_names, numerical_thes, summ = FALSE, ordered = TRUE
) {

  if (is.null(lang)) {
    lang <- 'eng'
  }

  # browser()

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
      dplyr::left_join(
        numerical_thes %>%
          dplyr::select(var_id, var_table, var_units), by = c('var_id', 'var_table')
      ) %>%
      dplyr::mutate(var_units = dplyr::if_else(
        is.na(var_units), glue::glue(''), glue::glue("[{var_units}]")
      )) %>%
      dplyr::distinct() %>%
      as.data.frame()

    dummy_creator <- function(x, y) {
      name <- vars_trans[vars_trans$var_id == x, glue::glue('translation_{lang}')][1]
      units <- vars_trans[vars_trans$var_id == x, 'var_units'][1]
      if (is.na(y)) {
        name
      } else {
        if (stringr::str_detect(y, 'number')) {
          glue::glue("{name} {y} [n]")
        } else {
          glue::glue("{name} {y} {units}")
        }
      }
    }

    vars_names <- vars_id %>%
      purrr::map2_chr(
        vars_stat,
        .f = dummy_creator
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
      dplyr::left_join(
        numerical_thes %>%
          dplyr::select(var_id, var_table, var_units), by = c('var_id', 'var_table')
      ) %>%
      dplyr::mutate(
        var_units = dplyr::if_else(
          is.na(var_units), NA_character_, as.character(glue::glue("[{var_units}]"))
        )
      ) %>%
      tidyr::unite(
        col = var_name,
        !!rlang::sym(glue::glue("translation_{lang}")), var_units,
        sep = ' '
      ) %>%
      dplyr::mutate(var_name = stringr::str_remove(var_name, ' NA')) %>%
      dplyr::select(-dplyr::one_of(
          'var_table'
      )) %>%
      dplyr::distinct() %>%
      as.data.frame()

    vars_names <- vars %>%
      purrr::map_chr(
        ~ vars_trans[vars_trans$var_id == .x, 'var_name']
      )

    names(vars) <- vars_names
  }

  # browser()

  # get rid of the duplicated vars (plot_id)
  vars <- vars[!duplicated(vars)]

  if (isTRUE(ordered)) {
    # we need the variables ordered with sense, first the admin and id variables, later the
    # proper table variables, the clim/topo/feat variables... So, let's order them
    vars_trans %>%
      dplyr::arrange(var_order_app) %>%
      dplyr::pull(var_id) %>%
      match(vars, .) %>%
      order() -> order_of_vars

    ordered_res <- vars[order_of_vars]

    return(ordered_res)
  } else {
    vars
  }
}

# Aggregator of inputs
var_inputs_aggregator <- function(ready_vars, lang, texts_thes) {

  list(
    id = ready_vars[stringr::str_detect(ready_vars, '_id')] %>%
      magrittr::extract(!stringr::str_detect(., 'admin_|old_')),
    admin = ready_vars[stringr::str_detect(ready_vars, 'admin_')] %>%
      magrittr::extract(!stringr::str_detect(., '_id')),
    proper_table = ready_vars[
      !stringr::str_detect(ready_vars, 'admin_') &
      !stringr::str_detect(ready_vars, '_id') &
      !stringr::str_detect(ready_vars, 'clim_') &
      !stringr::str_detect(ready_vars, 'topo_') &
      !stringr::str_detect(ready_vars, 'feat_') &
      !stringr::str_detect(ready_vars, 'coords_') &
      !stringr::str_detect(ready_vars, 'old_')
    ],
    clim = ready_vars[stringr::str_detect(ready_vars, 'clim_')],
    topo = ready_vars[stringr::str_detect(ready_vars, 'topo_')],
    feat = ready_vars[stringr::str_detect(ready_vars, 'feat_')],
    coord = ready_vars[stringr::str_detect(ready_vars, 'coords_')],
    old = ready_vars[stringr::str_detect(ready_vars, 'old_')]
  ) %>%
    magrittr::set_names(c(
      text_translate('id', lang, texts_thes),
      text_translate('admin', lang, texts_thes),
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

# info plot builder
infoplot_builder <- function(
  plot_data_all, plot_data_sel, plot_data_unsel,
  fg_id, data_inputs, map_inputs, viz_sel, viz_size, admin_sel,
  lang, var_thes, texts_thes, tables_to_look_at,
  numerical_thes, summ_title, click, session
) {

  # title viz_sel and title click group (needed by text_translate to build
  # the plot title)
  title_viz_sel <- names(
    var_names_input_builder(
      viz_sel, lang(), var_thes, texts_thes, tables_to_look_at(),
      numerical_thes, summ_title
    )
  )
  title_click_group <- text_translate(click$group, lang(), texts_thes) %>%
    tolower()

  # build the plot expression, with glue but before, reduce the functional_group
  # if many
  if (fg_id %in% names(plot_data_all)) {
    plot_data_all %>%
      dplyr::group_by(!! rlang::sym(fg_id)) %>%
      dplyr::select(dplyr::one_of('plot_id', viz_sel)) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::arrange(dplyr::desc(n)) %>%
      dplyr::pull(!! rlang::sym(fg_id)) -> fg_list

    if (length(fg_list) > 5) {

      # is there a fuctional group value already selected
      if (
        data_inputs$viz_functional_group_value != '' &
        !(data_inputs$viz_functional_group_value %in% fg_list[1:5])
      ) {
        fg_list <- c(data_inputs$viz_functional_group_value, fg_list[1:5])
        # warning the user about the trimming
        shinyWidgets::sendSweetAlert(
          session,
          title = 'Too much functional group levels to safely plot them all',
          text = glue::glue(
            "Showing only the 5 levels more abundant",
            " as well as {data_inputs$viz_functional_group_value}"
          )
        )
      } else {
        fg_list <- fg_list[1:5]
        # warning the user about the trimming
        shinyWidgets::sendSweetAlert(
          session,
          title = 'Too much functional group levels to safely plot them all',
          text = glue::glue("Showing only the 5 levels more abundant")
        )
      }

      plot_data_sel <- plot_data_sel %>%
        dplyr::filter(!! rlang::sym(fg_id) %in% fg_list)

      plot_data_unsel <- plot_data_unsel %>%
        dplyr::filter(!! rlang::sym(fg_id) %in% fg_list)

      plot_data_all <- plot_data_all %>%
        dplyr::filter(!! rlang::sym(fg_id) %in% fg_list)
    }
  }

  ## If the variable is numerical, lets do some violin and jitter plots:
  if (data_inputs$viz_color %in% (numerical_thes %>% dplyr::pull(var_id))) {
    plot_expression <- glue::glue(
      "plot_data_all %>%
        ggplot2::ggplot(ggplot2::aes(x = ' ', y = {viz_sel}))"
    )

    # geom_jiter, different if we have viz_size
    if (viz_size %in% names(plot_data_all)) {
      plot_expression <- glue::glue(
        "{plot_expression} +
            ggplot2::geom_jitter(
              data = plot_data_unsel,
              ggplot2::aes(size = {viz_size}), width = 0.1, height = 0,
              alpha = 0.3, color = 'grey88', show.legend = FALSE
            ) +
            ggplot2::geom_jitter(
              data = plot_data_sel, ggplot2::aes(size = {viz_size}), width = 0.1,
              height = 0, alpha = 1, color = 'red', show.legend = FALSE
            )"
      )
    } else {
      plot_expression <- glue::glue(
        "{plot_expression} +
            ggplot2::geom_jitter(
              data = plot_data_unsel,
              width = 0.1, height = 0, alpha = 0.3, size = 4,
              color = 'grey88', show.legend = FALSE
            ) +
            ggplot2::geom_jitter(
              data = plot_data_sel, width = 0.1, size = 4,
              height = 0, alpha = 1, color = 'red', show.legend = FALSE
            )"
      )
    }

    # geom_violin, we can't use quantiles because when we breakdown by diamclass
    # there is errors. Also we have to check for one or two rows data, in which case we
    # don't use geom violin
    if (nrow(plot_data_all) > 2) {
      plot_expression <- glue::glue(
        "{plot_expression} +
        ggplot2::geom_violin(
          adjust = 0.5,
          fill = 'transparent', colour = 'grey88'
        )"
      )
    }

    # facet_grid based on the existence of diamclass_id and fg_id
    if (all(c('diamclass_id', fg_id) %in% names(plot_data_all))) {
      plot_expression <- glue::glue(
        "{plot_expression} +
        ggplot2::facet_grid(
          {fg_id}~diamclass_id
        ) +
        ggplot2::labs(
          y = title_viz_sel,
          x = '',
          title = glue::glue(text_translate('info_plot_title', lang(), texts_thes)),
          subtitle = glue::glue(text_translate('info_plot_subtitle_double_facetted', lang(), texts_thes))
        )"
      )
    } else {
      if('diamclass_id' %in% names(plot_data_all)) {
        # browser()
        plot_expression <- glue::glue(
          "{plot_expression} +
          ggplot2::facet_grid(
            .~diamclass_id
          ) +
          ggplot2::labs(
            y = title_viz_sel,
            x = '',
            title = glue::glue(text_translate('info_plot_title', lang(), texts_thes)),
            subtitle = glue::glue(text_translate('info_plot_subtitle_dc_facetted', lang(), texts_thes))
          )"
        )
      } else {
        if (fg_id %in% names(plot_data_all)) {
          plot_expression <- glue::glue(
            "{plot_expression} +
            ggplot2::facet_grid(
              .~{fg_id}
            ) +
            ggplot2::labs(
              y = title_viz_sel,
              x = '',
              title = glue::glue(text_translate('info_plot_title', lang(), texts_thes)),
              subtitle = glue::glue(text_translate('info_plot_subtitle_fg_facetted', lang(), texts_thes))
            )"
          )
        } else {
          plot_expression <- glue::glue(
            "{plot_expression} +
              ggplot2::labs(
                y = title_viz_sel,
                x = '',
                title = glue::glue(text_translate('info_plot_title', lang(), texts_thes))
              )"
          )
        }
      }
    }
  } else {
    ## if viz_sel is not numeric, then the plot changes
    # also if the shape clicked is a polygon, we need the plot data instead
    # (because in the summarised data there is no categorical data)
    if (click$group != 'plots') {
      viz_sel <- data_inputs$viz_color
      plot_data_all <- map_inputs$main_data[['selected']] %>%
        dplyr::select(dplyr::one_of(admin_sel, viz_sel, fg_id)) %>%
        {
          if (exists('fg_list')) {
            dplyr::filter(., !! rlang::sym(fg_id) %in% fg_list)
          } else {.}
        } %>%
        dplyr::mutate(fill = dplyr::if_else(
          !!rlang::sym(admin_sel) == click$id, 'fill', 'nofill'
        ))

      plot_expression <- glue::glue(
        "plot_data_all %>%
          ggplot2::ggplot(ggplot2::aes(x = {viz_sel})) +
          ggplot2::geom_bar(ggplot2::aes(fill = fill), show.legend = FALSE) +
          ggplot2::scale_fill_manual(values = c('red', 'grey88'))"
      )

      # dont forget to change the title_viz_sel object, as it is not summ and
      # is not viz_sel, but data_inputs$viz_color
      title_viz_sel <- names(
        var_names_input_builder(
          data_inputs$viz_color, lang(), var_thes, texts_thes, tables_to_look_at(),
          numerical_thes, FALSE
        )
      )
    } else {

      # create the needed info to fill only the plot clicked value for viz sel
      pal_ref <- plot_data_all %>%
        dplyr::pull(!!rlang::sym(viz_sel)) %>% unique() %>% sort()
      click_value <- plot_data_all %>%
        dplyr::filter(plot_id == click$id) %>%
        dplyr::pull(!!rlang::sym(viz_sel))

      pal_vals <- rep('grey88', length(pal_ref))
      pal_vals[which(pal_ref == click_value)] <- 'red'



      plot_expression <- glue::glue(
        "plot_data_all %>%
          ggplot2::ggplot(ggplot2::aes(x = {viz_sel})) +
          ggplot2::geom_bar(ggplot2::aes(fill = {viz_sel}), show.legend = FALSE) +
          ggplot2::scale_fill_manual(values = pal_vals)"
      )
    }

    # if functional group different from plot, then facet. In this case there is
    # no need to facet by diameter class as is the same data for all dc's
    if (fg_id %in% names(plot_data_all)) {
      plot_expression <- glue::glue(
        "{plot_expression} +
            ggplot2::facet_grid(
              .~{fg_id}
            ) +
            ggplot2::labs(
              x = title_viz_sel,
              y = 'n',
              title = glue::glue(text_translate('info_plot_title', lang(), texts_thes)),
              subtitle = glue::glue(text_translate('info_plot_subtitle_fg_facetted', lang(), texts_thes))
            )"
      )
    } else {
      plot_expression <- glue::glue(
        "{plot_expression} +
            ggplot2::labs(
              x = title_viz_sel,
              y = 'n',
              title = glue::glue(text_translate('info_plot_title', lang(), texts_thes))
            )"
      )
    }

  }

  plot_expression <- glue::glue(
    "{plot_expression} +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        text = ggplot2::element_text(size = 14, color = 'gray88'),
        axis.text = ggplot2::element_text(color = 'gray88'),
        strip.text = ggplot2::element_text(color = 'gray88'),
        panel.background = ggplot2::element_rect(fill = 'gray12', colour = NA),
        plot.background = ggplot2::element_rect(fill = 'gray12', colour = NA),
        strip.background = ggplot2::element_rect(fill = 'gray12', colour = NA),
        panel.grid = ggplot2::element_line(colour = 'gray58'),
        panel.grid.minor.y = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(size = ggplot2::rel(0.5), colour = 'gray25')
      )"
  )

  plot_res <- plot_expression %>%
    rlang::parse_expr() %>%
    rlang::eval_tidy()

  return(plot_res)
}