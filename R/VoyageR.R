.supportedFormats <- c("vegawidget", "json")

#' Launch Voyager as a Shiny Gadget
#'
#' @param data Dataset to load
#' @param browser Open in an external browser
#' @param format In what format should plots be returned: "vegawidget" to return
#' an interactive or rendered version, "json" to return the spec as a character
#' in json format
#' @param envir Environment to use when searching for datasets
#' (defaults to the global environment)
#'
#' @return If a visualisation has been specified its JSON will be returned as a string
#' @export
#'
#' @examples
#' data("mtcars")
#'
#' # Don't run the app when testing examples
#' if (interactive()) {
#'   # Launch app and return a vegawidget
#'   VoyageR(mtcars)
#' }
#'
#' if (interactive()) {
#'   # Return the vega plot specification as json
#'   spec_json <- VoyageR(mtcars, format = "json")
#'   vegawidget::as_vegaspec(spec_json)
#' }
VoyageR <- function(data = NULL, format = "vegawidget", browser = FALSE, envir = .GlobalEnv) {
  if (!format %in% .supportedFormats) {
    stop("Unsupported format specified.")
  }

  SELECT_INPUT_NULL_STR <- c("Select Data" = "none")
  SELECT_INPUT_ENV_PREFIX <- "env_"
  SELECT_INPUT_DATA_ARG <- "data_arg"

  # ==== Dataset Choices ====

  # Get the string used for the argument data using crazy R magic
  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      stop("Unsupported data format provided. Please provide data as a dataframe.")
    }
    label_data <- as.character(substitute(data))

    choice_data <- SELECT_INPUT_DATA_ARG
    names(choice_data) <- label_data
  } else {
    choice_data <- c()
  }

  # Prepare options / choices for data dropdown
  ls_all <- ls(envir = envir)
  ls_dfs <- ls_all[as.logical(sapply(ls_all, function (x){ is.data.frame(get(x)) }))]

  if (length(ls_dfs) > 0) {
    choices_env <- paste0(SELECT_INPUT_ENV_PREFIX, ls_dfs)
    names(choices_env) <- ls_dfs
  } else {
    choices_env <- c()

    if (is.null(data)) {
      stop("No data available, please ensure there are dataframes in the environment and that data is provided.")
    }
  }

  select_choices <- list(
    ` ` = SELECT_INPUT_NULL_STR,
    `Provided Data` = choice_data,
    `Environment` = choices_env
  )

  selected_choice <- NULL
  if (!is.null(data)) {
    # Data has been provided
    selected_choice <- choice_data
  } else if (length(choices_env) == 1) {
    # Only a single df in environment, so select it
    selected_choice <- choices_env
  }


  # Make files in www/ available to shiny
  shiny::addResourcePath("www", system.file("www", package="VoyageR"))
  # When running locally: addResourcePath("www", "inst/www")

  # ==== Shiny UI & Server ====
  ui <- miniUI::miniPage(
    shinyjs::useShinyjs(),
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "www/font-awesome-4.7.0/css/font-awesome.min.css"),
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "www/style_voyager.css"),
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css"),

    # Left = cancel; Right = done;
    miniUI::gadgetTitleBar(
      title = "",
      left = shiny::tags$div(
        shiny::selectInput(
          inputId = "dataset",
          label = NULL,
          selected = selected_choice,
          multiple = FALSE,
          choices = select_choices
        ),
        style = "margin: 5px;"
      )
    ),
    miniUI::miniContentPanel(
      shiny::tags$div(id = "root"),
      padding = 0
    ),

    shiny::tags$script(src = "www/bundle.js"),
  )

  server <- function(input, output, session) {

    # Dataset selection
    shiny::observeEvent(input$dataset, {
      message(paste("Selecting Dataset:", input$dataset))

      if (input$dataset == SELECT_INPUT_NULL_STR) {
        # Exit this function
        return()
      } else if (input$dataset == SELECT_INPUT_DATA_ARG) {
        dataset_name <- label_data
        dataset <- data
      } else if (startsWith(input$dataset, SELECT_INPUT_ENV_PREFIX)) {
        dataset_name <- substring(input$dataset, nchar(SELECT_INPUT_ENV_PREFIX) + 1)
        dataset <- get(dataset_name, envir = envir)
      }

      data_object <- list(
        fileName = dataset_name,
        values = dataset
      )
      dataset_json <- jsonlite::toJSON(data_object)

      # Update Dataset in voyager
      shinyjs::runjs(paste0("voyagerInstance.updateData(", dataset_json, ")"))
    })

    # Listen for 'done' events indicating that the 'done' button has been pressed.
    shiny::observeEvent(input$done, {
      # The argument to getSpec corresponds to whether or not to include data in the spec
      shinyjs::runjs("Shiny.setInputValue('finalSpec', JSON.stringify(voyagerInstance.getSpec(true)))")
    })

    # Triggered when finalSpec has been loaded (only stop app then)
    shiny::observeEvent(input$finalSpec, {
      finalSpec_json <- input$finalSpec

      # Detect whether a visualisation has been specified (crudely, but fast)
      has_viz <- grepl('"mark":', finalSpec_json, fixed = TRUE)

      # Return the final vega spec JSON
      if (has_viz) {
        final_output <- finalSpec_json # default to return raw json spec
        if (format == "vegawidget") {
          final_output <- vegawidget::as_vegaspec(finalSpec_json)
        }

        set_last_spec(final_output)
        shiny::stopApp(final_output)
      } else {

        set_last_spec(NULL)
        shiny::stopApp()
      }
    })

  }

  # ==== Launch ====

  if (browser) {
    # Open in extrnal browser
    viewer <- shiny::browserViewer()
  } else {
    # Open dialog in Rstudio
    viewer <- shiny::dialogViewer(dialogName = "VoyageR", width = 1280, height = 720)
  }

  # Launch VoyageR via shiny
  return(shiny::runGadget(ui, server, viewer = viewer))

}
