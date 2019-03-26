#' @title mod_returnedDataOutput and mod_returnedData
#'
#' @description Shiny module to get the data as tbl_sql
#'
#' @param id
#'
#' @export
mod_returnedDataOutput <- function(id) {
  ns <- shiny::NS(id)
  return()
}

#' @title mod_returnedData server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param data_inputs reactives from dataInput module
#' @param map_inputs map input with custom_polygon sf object, if any
#' @param nfidb pool object to access the nfi db
#'
#' @param cache_list inMemory cache
#'
#' @import tidyNFI
#' @importFrom dplyr n
#'
#' @export
#'
#' @rdname mod_returnedDataOuput
mod_returnedData <- function(
  input, output, session,
  data_inputs, map_inputs = NULL, nfidb, lang, texts_thes,
  cache_list
) {

  # main data generator
  main_data <- shiny::eventReactive(
    ignoreNULL = FALSE, ignoreInit = FALSE,
    eventExpr = data_inputs$apply_data,
    valueExpr = {

      shiny::validate(
        shiny::need(data_inputs$nfi, 'No NFI version selected')
      )

      # browser()
      nfi <- data_inputs$nfi
      admin_div <- data_inputs$admin_div
      functional_group <- data_inputs$functional_group
      diameter_classes <- data_inputs$diameter_classes
      filter_vars <- data_inputs$filter_vars
      filter_expressions <- data_inputs$filter_expressions
      custom_polygon <- map_inputs$custom_polygon
      dominant_group <- data_inputs$dominant_group
      dominant_criteria <- data_inputs$dominant_criteria

      # let's check if we can use the cache data:

      browser()
      if (is_chached(
        nfi, cache_list$get("nficached"),
        admin_div, cache_list$get("admindivcached"),
        functional_group, cache_list$get("functionalgroupcached"),
        diameter_classes, cache_list$get("diameterclassescached"),
        filter_vars, cache_list$get("filtervarscached"),
        filter_expressions, cache_list$get("filterexpressionscached"),
        custom_polygon, cache_list$get("custompolygoncached"),
        dominant_group, cache_list$get("dominantgroupcached"),
        dominant_criteria, cache_list$get("dominantcriteriacached")
      )) {
        if (!is.null(cache_list$get("datacached"))) {
          res <- cache_list$get("datacached")
        }
      } else {
        res <- returned_data(
          nfidb, session,
          nfi,
          admin_div,
          functional_group,
          diameter_classes,
          dominant_group,
          dominant_criteria,
          filter_vars,
          filter_expressions,
          custom_polygon, lang, texts_thes
        )

        cache_list$set('datacached', res)
        cache_list$set("nficached", data_inputs$nfi)
        cache_list$set("admindivcached", data_inputs$admin_div)
        cache_list$set("functionalgroupcached", data_inputs$functional_group)
        cache_list$set("diameterclassescached", data_inputs$diameter_classes)
        cache_list$set("filtervarscached", data_inputs$filter_vars)
        cache_list$set("filterexpressionscached", data_inputs$filter_expressions)
        cache_list$set("custompolygoncached", map_inputs$custom_polygon)
        cache_list$set("dominantgroupcached", data_inputs$dominant_group)
        cache_list$set("dominantcriteriacached", data_inputs$dominant_criteria)

      }

      return(res)
    }
  )

  returned_data_reactives <- shiny::reactiveValues()
  shiny::observe({
    returned_data_reactives$main_data <- main_data()
  })

  return(returned_data_reactives)
}