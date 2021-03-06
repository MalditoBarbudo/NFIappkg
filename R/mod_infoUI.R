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

  # ui skeleton (rows)
  shiny::tagList(
    shiny::fluidRow(
      shiny::br(),
      shiny::plotOutput(ns("info_plot"))
    ),
    shiny::fluidRow(
      shiny::br(),
      formattable::formattableOutput(ns('info_table'), width = "75%")
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
  map_inputs, data_inputs, nfidb, var_thes, texts_thes, numerical_thes, lang
) {

  prep_data <- shiny::reactive({
    click <- map_inputs$map_shape_click

    if (is.null(click)) {
      return()
    }

    if (click$group == 'plots') {
      viz_sel <- data_inputs$viz_color
      fg_id <- glue::glue("{data_inputs$functional_group}_id")
      dom_nfi <- switch(
        data_inputs$dominant_nfi,
        none = '',
        nfi2 = '_nfi2',
        nfi3 = '_nfi3',
        nfi4 = '_nfi4'
      )
      if (fg_id == 'plot_id') {
        # check if dominant group is present
        if (data_inputs$dominant_group == 'none') {
          fg_id <- ''
        } else {
          fg_id <- glue::glue(
            "{data_inputs$dominant_criteria}_{data_inputs$dominant_group}_dominant{dom_nfi}"
          )
        }
      }
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
      if (fg_id == 'plot_id') {
        # check if dominant group is present
        if (data_inputs$dominant_group == 'none') {
          fg_id <- ''
        } else {
          dom_nfi <- switch(
            data_inputs$dominant_nfi,
            none = '',
            nfi2 = '_nfi2',
            nfi3 = '_nfi3',
            nfi4 = '_nfi4'
          )
          fg_id <- glue::glue(
            "{data_inputs$dominant_criteria}_{data_inputs$dominant_group}_dominant{dom_nfi}"
          )
        }
      }
      viz_size <- ''
      if (data_inputs$admin_div == 'file') {
        admin_sel <- 'poly_id'
      } else {
        admin_sel <- glue::glue("admin_{data_inputs$admin_div}")
      }

      prep_data <- map_inputs$main_data[['summarised']] %>%
        dplyr::select(dplyr::one_of(
          admin_sel, viz_sel, viz_size, 'diamclass_id', fg_id,
          'topo_altitude_asl_mean', 'topo_fdm_slope_percentage_mean',
          'clim_tmean_year_mean', 'clim_prec_year_mean', 'clim_pet_year_mean'
        ))
    }

    return(prep_data)
  })

  tables_to_look_at <- shiny::reactive({

    shiny::validate(
      shiny::need(data_inputs$nfi, 'No NFI version selected')
    )

    tables_to_look_at_helper(data_inputs)
  })

  # reactive to prepare the plot and the table and return them as a list
  plot_and_table <- shiny::eventReactive(
    eventExpr = map_inputs$map_shape_click,
    valueExpr = {

      click <- map_inputs$map_shape_click

      # browser()

      if (click$group == 'plots') {

        viz_sel <- data_inputs$viz_color
        fg_id <- glue::glue("{data_inputs$functional_group}_id")
        if (fg_id == 'plot_id') {
          # check if dominant group is present
          if (data_inputs$dominant_group == 'none') {
            fg_id <- ''
          } else {
            dom_nfi <- switch(
              data_inputs$dominant_nfi,
              none = '',
              nfi2 = '_nfi2',
              nfi3 = '_nfi3',
              nfi4 = '_nfi4'
            )
            fg_id <- glue::glue(
              "{data_inputs$dominant_criteria}_{data_inputs$dominant_group}_dominant{dom_nfi}"
            )
          }
        }
        viz_size <- data_inputs$viz_size

        plot_data_all <- prep_data()

        plot_data_sel <- plot_data_all %>%
          dplyr::filter(plot_id == click$id)

        plot_data_unsel <- plot_data_all %>%
          dplyr::filter(plot_id != click$id)

        table_data <- plot_data_sel %>%
          dplyr::select(dplyr::one_of(
            'plot_id', 'admin_province', viz_sel, viz_size,
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
        if (fg_id == 'plot_id') {
          # check if dominant group is present
          if (data_inputs$dominant_group == 'none') {
            fg_id <- ''
          } else {
            dom_nfi <- switch(
              data_inputs$dominant_nfi,
              none = '',
              nfi2 = '_nfi2',
              nfi3 = '_nfi3',
              nfi4 = '_nfi4'
            )
            fg_id <- glue::glue(
              "{data_inputs$dominant_criteria}_{data_inputs$dominant_group}_dominant{dom_nfi}"
            )
          }
        }
        viz_size <- ''
        if (data_inputs$admin_div == 'file') {
          admin_sel <- 'poly_id'
        } else {
          admin_sel <- glue::glue("admin_{data_inputs$admin_div}")
        }

        plot_data_all <- prep_data()

        plot_data_sel <- plot_data_all %>%
          dplyr::filter(!! rlang::sym(admin_sel) == click$id)

        plot_data_unsel <- plot_data_all %>%
          dplyr::filter(!! rlang::sym(admin_sel) != click$id)

        table_data <-  plot_data_sel %>%
          dplyr::ungroup() %>%
          dplyr::select(dplyr::one_of(
            admin_sel, viz_sel,
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

      info_plot <- infoplot_builder(
        plot_data_all, plot_data_sel, plot_data_unsel,
        fg_id, data_inputs, map_inputs, viz_sel, viz_size, admin_sel,
        lang, var_thes, texts_thes, tables_to_look_at,
        numerical_thes, summ_title, click, session
      )

      # info_table
      info_table <- table_data %>%
        tidyr::gather('Characteristics', 'Value') %>%
        dplyr::mutate(
          Characteristics = stringr::str_remove(Characteristics, '_mean|_se|_max|_min|_n'),
          Characteristics = names(var_names_input_builder(
            Characteristics, lang(), var_thes, texts_thes, tables_to_look_at(),
            numerical_thes, ordered = FALSE
          ))
        ) %>%
        ##################################################################################
        ## gt is TOO FAR EXPERIMENTAL yet, so its better to use another html table engine
        # gt::gt(rowname_col = 'Characteristics') %>%
        # gt::tab_header(
        #   title = glue::glue(text_translate("info_tab_header", lang(), texts_thes))
        # ) %>%
        # gt::tab_options(
        #   table.background.color = 'transparent',
        #   heading.title.font.size = 22,
        #   table.font.size = 16,
        #   table.border.top.width = 0,
        # #   stub_group.border.top.width = 0,
        # #   stub_group.border.bottom.width = 0,
        # #   field.border.top.width = 0,
        # #   field.border.bottom.width = 0,
        #   heading.border.bottom.width = 2,
        #   heading.border.bottom.color = '#c8cac8'
        # ) %>%
        # gt::tab_style(
        #   style = gt::cells_styles(
        #     text_size = 0
        #   ),
        #   location = gt::cells_column_labels(columns = TRUE)
        # ) %>%
        # gt::tab_style(
        #   style = gt::cells_styles(
        #     text_font = 'Montserrat',
        #     text_color = '#c8cac8',
        #     text_weight = 'bold',
        #     text_align = 'right'
        #   ),
        #   locations = gt::cells_title(groups = 'title')
        # ) %>%
        # gt::tab_style(
        #   style = gt::cells_styles(
        #     text_font = 'Montserrat',
        #     text_color = '#c8cac8',
        #     text_align = 'right'
        #   ),
        #   locations = gt::cells_stub(rows = TRUE)
        # ) %>%
        # gt::tab_style(
        #   style = gt::cells_styles(
        #     text_font = 'Montserrat',
        #     text_color = '#c8cac8',
        #     text_weight = 'bold',
        #     text_align = 'left'
        #   ),
        #   locations = gt::cells_data(columns = dplyr::vars(Value))
        # )
        ##################################################################################
        formattable::formattable(
          list(
            Characteristics = formattable::formatter(
              "span", style = formattable::style(
                "font-family" = "Montserrat", color = "#c8cac8",
                "font-size" = "14pt", "font-weight" = "normal"
              )
            ),
            Value = formattable::formatter(
              "span", style = formattable::style(
                "font-family" = "Montserrat", color = "#c8cac8",
                "font-size" = "16pt", "font-weight" = "bold"
              )
            )
          ),
          align = c('r', 'l'),
          table.attr = "class=\"table table-condensed lfc_formattable\""
        )

      return(list(info_plot = info_plot, info_table = info_table))
    }
  )

  # outputs
  output$info_plot <- shiny::renderPlot({
    plot_and_table()$info_plot
  })

  output$info_table <- formattable::renderFormattable({
    plot_and_table()$info_table
  })

  # reactives to return the close button
  info_reactives <- shiny::reactiveValues()
  shiny::observe({
    info_reactives$close <- input$close
  })

  return(info_reactives)
}