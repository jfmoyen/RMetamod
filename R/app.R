#' Run the app
#' @import shiny
#' @export
#'
#'
MyApp <- function(){
  shinyApp(Rmetamod_ui, Rmetamod_server)
  }
MyApp()
