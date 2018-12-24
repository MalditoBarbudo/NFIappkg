#' @title mod_infoOutput and mod_info
#'
#' @description A shiny module to create and populate the data inputs
#'
#' @param id shiny id
#'
#' @export
mod_infoOutput <- function(id) {
  ns <- shiny::NS(id)
  return()
}

#' mod_info server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param map_inputs map module reactives, including returned data
#' @param data_inputs data inputs module reactives
#' @param nfidb pool object with the db connection
#'
#' @export
mod_info <- function(
  input, output, session,
  map_inputs, data_inputs, nfidb
) {

  # reactive to look for map_shape_click and create the UI for the plots and
  # the info
  # info_plots_builder <- shiny::eventReactive(
  #   eventExpr = map_inputs$map_shape_click,
  #   valueExpr = {
  #
  #     click <- map_inputs$map_shape_click
  #     main_data <- map_inputs$main_data
  #
  #     if (click$group == 'plots') {
  #       # plot plot
  #       y_var <- data_inputs$viz_color
  #       plot_selected <- click$id
  #
  #       # 1. clicked plots in selected plots
  #       cpisp_plot <- main_data[['selected']] %>%
  #         dplyr::select(dplyr::one_of('plot_id', y_var)) %>%
  #         dplyr::collect() %>%
  #         dplyr::mutate(
  #           color_var = dplyr::if_else(
  #             plot_id == plot_selected, 'a', 'b'
  #           )
  #         ) %>%
  #         ggplot2::ggplot(
  #           ggplot2::aes(x = !!rlang::sym(y_var))
  #         ) +
  #         ggplot2::geom_dotplot(
  #           ggplot2::aes(fill = color_var), binaxis = 'x', dotsize = 0.7
  #         ) +
  #         ggplot2::geom_density(fill = 'red', alpha = 0.4, adjust = 0.5)
  #
  #       res_combined_plot <- cowplot::plot_grid(cpisp_plot)
  #
  #
  #     } else {
  #       # polygon composed plot
  #       # 1. clicked polygon in polygons
  #       y_var <- glue::glue("{data_inputs$viz_color}_mean")
  #       polygon_var <- glue::glue("admin_{data_inputs$admin_div}")
  #       polygon_selected <- click$id
  #
  #       pinp_plot <- main_data[['summarised']] %>%
  #         dplyr::select(dplyr::one_of(y_var, polygon_var)) %>%
  #         dplyr::collect() %>%
  #         magrittr::set_names(., c(y_var, 'polygon_variable')) %>%
  #         dplyr::mutate(color_var = dplyr::if_else(
  #           polygon_variable == polygon_selected, 'a', 'b'
  #         )) %>%
  #         ggplot2::ggplot(
  #           ggplot2::aes(y = !!rlang::sym(y_var), x = polygon_var)
  #         ) +
          # ggplot2::geom_violin(
          #   draw_quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95), adjust = 0.5
          # ) +
          # ggplot2::geom_dotplot(
          #   ggplot2::aes(fill = color_var), binaxis = 'y', stackdir = 'center',
          #   show.legend = FALSE
          # ) %>%
  #         ggplot2::coord_flip()
  #
  #       # iris %>% ggplot(aes(y = Petal.Length)) +
  #       #   geom_violin(aes(x = 'a'), draw_quantiles = c(0.25, 0.5, 0.75), adjust = 0.5) +
  #       #   geom_dotplot(aes(x = 'a', fill = Species), binaxis = "y", stackdir = "center")
  #
  #       # 2. clicked polygon plots plot
  #       y_var <- data_inputs$viz_color
  #       cpopl_plot <- main_data[['selected']] %>%
  #         dplyr::select(dplyr::one_of(y_var, polygon_var, 'plot_id')) %>%
  #         dplyr::filter(!! rlang::sym(polygon_var) == !! polygon_selected) %>%
  #         dplyr::collect() %>%
  #         ggplot2::ggplot(
  #           ggplot2::aes(x = !!rlang::sym(y_var))
  #         ) +
  #         ggplot2::geom_dotplot(binaxis = 'x', dotsize = 0.7) +
  #         ggplot2::geom_density(fill = 'red', alpha = 0.4, adjust = 0.5)
  #
  #       # iris %>% ggplot(aes(x = Petal.Length)) +
  #       #   geom_dotplot(binaxis = "x", stackdir = "up", dotsize = 0.5) +
  #       #   geom_density(fill = 'red', alpha = 0.2, adjust = 0.5)
  #
  #       # 3. polygon itself plot
  #       # pi_plot <- map_inputs$map_data %>%
  #       #   dplyr::filter(!! rlang::sym(polygon_var) == !! polygon_selected) %>%
  #       #   ggplot2::ggplot() %>%
  #       #   ggplot2::geom_sf()
  #
  #       res_combined_plot <- cowplot::plot_grid(
  #         pinp_plot, cpopl_plot,
  #         nrow = 1, align = 'h'
  #       )
  #     }
  #
  #     return(res_combined_plot)
  #
  #   }
  # )

  output$info_plot <- shiny::renderPlot({
    # info_plots_builder()

    click <- map_inputs$map_shape_click

    if (click$group != 'plots') {
      viz_color <- glue::glue(
        "{data_inputs$viz_color}{data_inputs$viz_statistic}"
      )
      join_var <- glue::glue("admin_{data_inputs$admin_div}")
    } else {
      viz_color <- data_inputs$viz_color
      join_var <- 'plot_id'
    }

    if (data_inputs$viz_shape == 'plot' & click$group != 'plots') {
      map_data <- map_inputs$main_data[['summarised']] %>%
        dplyr::select(dplyr::one_of(join_var, viz_color)) %>%
        dplyr::collect()
    } else {
      map_data <- map_inputs$map_data
    }

    map_data %>%
      dplyr::mutate(color_var = dplyr::if_else(
        !! rlang::sym(join_var) == click$id, 'a', 'b'
      )) %>%
      ggplot2::ggplot(
        ggplot2::aes(x = '_', y = !! rlang::sym(viz_color))
      ) +
      ggplot2::geom_violin(
        draw_quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95), adjust = 0.5
      ) +
      # ggplot2::geom_dotplot(
      #   ggplot2::aes(), binaxis = 'y', stackdir = 'center',
      #   show.legend = FALSE
      # )
      ggplot2::geom_jitter(
        ggplot2::aes(colour = color_var, alpha = color_var), size = 4,
        width = 0.1, height = 0
      ) +
      ggplot2::scale_alpha_manual(values = c(1, 0.4)) +
      ggplot2::scale_colour_manual(values = c('red', 'gray66'))
  })

  output$info_table <- gt::render_gt({
    click <- map_inputs$map_shape_click
    main_data <- map_inputs$main_data

    if (click$group == 'plots') {
      main_data[['selected']] %>%
        dplyr::filter(plot_id == click$id) %>%
        dplyr::select(dplyr::one_of(
          'plot_id',
          'admin_province',
          'topo_altitude_asl', 'topo_fdm_slope_percentage',
          'topo_fdm_aspect_cardinal_8',
          'clim_tmean_year', 'clim_prec_year', 'clim_pet_year'
        )) %>%
        head(1) %>%
        dplyr::mutate_if(is.numeric, round) %>%
        dplyr::collect() %>%
        tidyr::gather('Characteristics', 'Value') %>%
        gt::gt(rowname_col = 'Characteristics') %>%
        gt::tab_header(
          title = glue::glue('Plot #{click$id}:')
        ) %>%
        gt::tab_options(
          table.background.color = 'transparent'
        )
    } else {
      polygon_var <- glue::glue("admin_{data_inputs$admin_div}")
      main_data[['summarised']] %>%
        dplyr::filter(!! rlang::sym(polygon_var) == click$id) %>%
        dplyr::select(dplyr::one_of(
          polygon_var,
          'topo_altitude_asl_mean', 'topo_fdm_slope_percentage_mean',
          'clim_tmean_year_mean', 'clim_prec_year_mean', 'clim_pet_year_mean'
        )) %>%
        head(1) %>%
        dplyr::collect() %>%
        tidyr::gather('Characteristics', 'Value') %>%
        gt::gt(rowname_col = 'Characteristics') %>%
        gt::tab_header(
          title = glue::glue('{click$id} {data_inputs$admin_div}:')
        )
    }
  })

  shiny::observeEvent(
    eventExpr = map_inputs$map_shape_click,
    handlerExpr = {
      # ns
      ns <- session$ns

      # sweetalert
      shinyWidgets::sendSweetAlert(
        session = session,
        title = 'Click info',

        # here in text we insert the UI again
        text = shiny::tagList(
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::plotOutput(ns('info_plot'))
            ),
            shiny::column(
              6,
              gt::gt_output(ns('info_table'))
            )
          )
        ),
        html = TRUE,
        btn_labels = 'Discard'
      )
    }
  )
}