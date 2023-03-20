#' Run the app
#' @import shiny
#' @export
MetamodApp <- function(){
  shinyApp(Rmetamod_ui, Rmetamod_server)
}

MetamodApp()
