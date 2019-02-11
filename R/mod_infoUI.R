#' @title mod_infoUI and mod_info
#'
#' @description A shiny module to create and populate the data inputs
#'
#' @param id shiny id
#'
#' @export
mod_infoUI <- function(id) {
  # ns
  ns <- shiny::NS(id)

  # ui skeleton
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        6,
        # plot column
        shiny::br(),
        shiny::plotOutput(ns("info_plot"), height = "700px")
      ),
      shiny::column(
        6,
        shiny::br(),
        gt::gt_output(ns('info_table'))
      )
    ),
    shiny::fluidRow(
      shiny::column(
        2, offset = 10,
        shiny::hr(),
        shinyWidgets::actionBttn(
          ns('close'),
          'Close',
          icon = shiny::icon('check-circle'), style = 'stretch',
          block = FALSE, size = 'sm'
        )
      )
    )
  )
}

#' mod_info server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param map_inputs map module reactives, including returned data
#' @param data_inputs data inputs module reactives
#' @param nfidb pool object with the db connection
#' @param var_thes variables thesaurus df
#'
#' @export
mod_info <- function(
  input, output, session,
  map_inputs, data_inputs, nfidb, var_thes, texts_thes, lang
) {

  prep_data <- dedupe(shiny::reactive({
    click <- map_inputs$map_shape_click

    if (is.null(click)) {
      return()
    }

    if (click$group == 'plots') {
      viz_sel <- data_inputs$viz_color
      fg_id <- glue::glue("{data_inputs$functional_group}_id")
      if (fg_id == 'plot_id') {fg_id <- ''}
      viz_size <- data_inputs$viz_size

      prep_data <- map_inputs$main_data[['selected']] %>%
        dplyr::select(dplyr::one_of(
          'plot_id', viz_sel, viz_size, 'diamclass_id', fg_id,
          'admin_province',
          'topo_altitude_asl', 'topo_fdm_slope_percentage',
          'topo_fdm_aspect_cardinal_8',
          'clim_tmean_year', 'clim_prec_year', 'clim_pet_year'
        ))

    } else {
      viz_sel <- glue::glue("{data_inputs$viz_color}{data_inputs$viz_statistic}")
      fg_id <- glue::glue("{data_inputs$functional_group}_id")
      if (fg_id == 'plot_id') {fg_id <- ''}
      viz_size <- ''
      admin_sel <- glue::glue("admin_{data_inputs$admin_div}")

      prep_data <- map_inputs$main_data[['summarised']] %>%
        dplyr::select(dplyr::one_of(
          admin_sel, viz_sel, viz_size, 'diamclass_id', fg_id,
          'topo_altitude_asl_mean', 'topo_fdm_slope_percentage_mean',
          'clim_tmean_year_mean', 'clim_prec_year_mean', 'clim_pet_year_mean'
        ))
    }

    return(prep_data)
  }))

  tables_to_look_at <- shiny::reactive({

    shiny::validate(
      shiny::need(data_inputs$nfi, 'No NFI version selected')
    )

    nfi <- data_inputs$nfi

    if (nfi == 'nfi_2_nfi_3') {
      nfi <- 'COMP_NFI2_NFI3'
    } else {
      if (nfi == 'nfi_3_nfi_4') {
        nfi <- 'COMP_NFI3_NFI4'
      } else {
        nfi <- toupper(nfi)
      }
    }

    functional_group <- data_inputs$functional_group %>% toupper()
    diameter_classes <- data_inputs$diameter_classes

    if (isTRUE(diameter_classes)) {
      dc <- 'DIAMCLASS_'
    } else {
      dc <- ''
    }

    table_names <- c(
      glue::glue("{functional_group}_{nfi}_{dc}RESULTS"),
      'PLOTS',
      glue::glue("PLOTS_{nfi}_DYNAMIC_INFO")
    )
    return(table_names)
  })

  # reactive to prepare the plot and the table and return them as a list
  plot_and_table <- shiny::eventReactive(
    eventExpr = map_inputs$map_shape_click,
    valueExpr = {

      click <- map_inputs$map_shape_click

      if (click$group == 'plots') {

        viz_sel <- data_inputs$viz_color
        fg_id <- glue::glue("{data_inputs$functional_group}_id")
        if (fg_id == 'plot_id') {fg_id <- ''}
        viz_size <- data_inputs$viz_size

        plot_data_all <- prep_data()

        plot_data_sel <- plot_data_all %>%
          dplyr::filter(plot_id == click$id)

        plot_data_unsel <- plot_data_all %>%
          dplyr::filter(plot_id != click$id)

        table_data <- plot_data_sel %>%
          dplyr::select(dplyr::one_of(
            'plot_id', 'admin_province',
            'topo_altitude_asl', 'topo_fdm_slope_percentage',
            'topo_fdm_aspect_cardinal_8',
            'clim_tmean_year', 'clim_prec_year', 'clim_pet_year'
          )) %>%
          head(1) %>%
          dplyr::mutate_if(is.numeric, round)

        summ_title <- FALSE
      # end if plots, start of != plots
      } else {

        viz_sel <- glue::glue("{data_inputs$viz_color}{data_inputs$viz_statistic}")
        fg_id <- glue::glue("{data_inputs$functional_group}_id")
        if (fg_id == 'plot_id') {fg_id <- ''}
        viz_size <- ''
        admin_sel <- glue::glue("admin_{data_inputs$admin_div}")

        plot_data_all <- prep_data()

        plot_data_sel <- plot_data_all %>%
          dplyr::filter(!! rlang::sym(admin_sel) == click$id)

        plot_data_unsel <- plot_data_all %>%
          dplyr::filter(!! rlang::sym(admin_sel) != click$id)

        table_data <-  plot_data_sel %>%
          dplyr::ungroup() %>%
          dplyr::select(dplyr::one_of(
            admin_sel,
            'topo_altitude_asl_mean', 'topo_fdm_slope_percentage_mean',
            'clim_tmean_year_mean', 'clim_prec_year_mean', 'clim_pet_year_mean'
          )) %>%
          head(1) %>%
          dplyr::mutate_if(is.numeric, round)

        summ_title <- TRUE
      }

      # validate if we have data
      shiny::validate(
        shiny::need(nrow(plot_data_all) > 0, 'No data to show'),
        shiny::need(
          nrow(plot_data_sel) > 0,
          'No data in the clicked shape, please click in another'
        ),
        shiny::need(nrow(table_data) > 0, 'No data to show the general info')
      )

      # build the plot expression, with glue
      # but before, reduce the functional_group if many
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

          plot_expression <- glue::glue(
            "plot_data_all %>%
            dplyr::filter({fg_id} %in% fg_list) %>%
            ggplot2::ggplot(ggplot2::aes(x = ' ', y = {viz_sel})) +"
          )
        } else {
          plot_expression <- glue::glue(
            "plot_data_all %>%
            ggplot2::ggplot(ggplot2::aes(x = ' ', y = {viz_sel})) +"
          )
        }
      } else {
        plot_expression <- glue::glue(
          "plot_data_all %>%
            ggplot2::ggplot(ggplot2::aes(x = ' ', y = {viz_sel})) +"
        )
      }

      # geom_jiter, different if we have viz_size
      if (viz_size %in% names(plot_data_all)) {
        plot_expression <- glue::glue(
          "{plot_expression}
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
          "{plot_expression}
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
            # draw_quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95),
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
                subtitle = glue::glue(text_translate('info_plot_subtitle_double_facetted', lang(), texts_thes))
              )"
        )
      }
      else {
        if('diamclass_id' %in% names(plot_data_all)) {
          # browser()
          plot_expression <- glue::glue(
            "{plot_expression} +
               ggplot2::facet_grid(
                 .~diamclass_id
               ) +
                 ggplot2::labs(
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
                   subtitle = glue::glue(text_translate('info_plot_subtitle_fg_facetted', lang(), texts_thes))
                 )"
            )
          }
        }
      }

      title_viz_sel <- names(
        var_names_input_builder(viz_sel, lang(), var_thes, texts_thes, tables_to_look_at(), summ_title)
      )
      title_click_group <- text_translate(click$group, lang(), texts_thes) %>%
        tolower()

      info_plot <- rlang::eval_tidy(rlang::parse_expr(plot_expression)) +
        ggplot2::labs(
          title = glue::glue(text_translate('info_plot_title', lang(), texts_thes)),
          y = title_viz_sel,
          x = ''
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          text = ggplot2::element_text(size = 14, color = 'gray88'),
          axis.text = ggplot2::element_text(color = 'gray88'),
          strip.text = ggplot2::element_text(color = 'gray88'),
          panel.background = ggplot2::element_rect(fill = "gray12", colour = NA),
          plot.background = ggplot2::element_rect(fill = "gray12", colour = NA),
          strip.background = ggplot2::element_rect(fill = "gray12", colour = NA),
          panel.grid = ggplot2::element_line(colour = 'gray58'),
          panel.grid.minor.y = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_line(size = ggplot2::rel(0.5), colour = 'gray25')
        )

      # info_table
      info_table <- table_data %>%
        tidyr::gather('Characteristics', 'Value') %>%
        dplyr::mutate(
          Characteristics = names(var_names_input_builder(
            stringr::str_remove(.$Characteristics, '_mean'), lang(), var_thes, texts_thes, tables_to_look_at())
          )
        ) %>%
        gt::gt(rowname_col = 'Characteristics') %>%
        gt::tab_header(
          title = glue::glue(text_translate("info_tab_header", lang(), texts_thes))
        ) %>%
        gt::tab_options(
          table.background.color = 'transparent',
          heading.title.font.size = 22,
          table.font.size = 16,
          table.border.top.width = 0,
          stub_group.border.top.width = 0,
          stub_group.border.bottom.width = 0,
          field.border.top.width = 0,
          field.border.bottom.width = 0,
          heading.border.bottom.width = 2,
          heading.border.bottom.color = 'black'
        ) %>%
        gt::tab_style(
          style = gt::cells_styles(
            text_size = 0
          ),
          location = gt::cells_column_labels(columns = TRUE)
        ) %>%
        gt::tab_style(
          style = gt::cells_styles(
            text_font = 'Montserrat',
            text_color = 'black',
            text_weight = 'bold',
            text_align = 'right'
          ),
          locations = gt::cells_title(groups = 'title')
        ) %>%
        gt::tab_style(
          style = gt::cells_styles(
            text_font = 'Montserrat',
            text_color = 'black',
            text_align = 'right'
          ),
          locations = gt::cells_stub(rows = TRUE)
        ) %>%
        gt::tab_style(
          style = gt::cells_styles(
            text_font = 'Montserrat',
            text_color = 'black',
            text_weight = 'bold',
            text_align = 'left'
          ),
          locations = gt::cells_data(columns = dplyr::vars(Value))
        )

      return(list(info_plot = info_plot, info_table = info_table))
    }
  )

  # outputs
  output$info_plot <- shiny::renderPlot({
    plot_and_table()$info_plot
  })

  output$info_table <- gt::render_gt({
    plot_and_table()$info_table
  })

  # reactives to return the close button
  info_reactives <- shiny::reactiveValues()
  shiny::observe({
    info_reactives$close <- input$close
  })

  return(info_reactives)
}