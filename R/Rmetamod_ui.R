#####################
####  INTERFACE  ####
#####################
#' UI definition
#'
#' @importFrom  bslib bs_theme
#' @importFrom colourpicker colourInput
#' @importFrom shinyWidgets radioGroupButtons
#' @import shiny
#' @export
#'
#' @seealso shiny::shinyApp

Rmetamod_ui <- fluidPage(
  theme = bslib::bs_theme(version = 4,
                          font_scale = 0.8,
                          bootswatch = "yeti"),
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
    h3 {margin-top: 0rem;

    }
      .well { padding-top: 0.4rem;
              padding-bottom: 0.4rem;
              padding-left: 1rem;
              padding-right: 1rem}

      #inline label{ display: table-cell; text-align: center; vertical-align: middle; }
      #inline .form-group { display: table-row;}

    "))
  ),

  # Application title
  #titlePanel("G\u00E9othermes orog\u00E9niques"),

  fluidRow(

    ########################### LEFT #################################
    column(width = 3,
           wellPanel(
             h3("Param\u00E8tres"),
             fluidRow(
               column(width=4,actionButton("default","Param. par d\u00E9faut") ),
               column(width=4,downloadButton("download",label="Enreg. param.") ),
               column(width=4,fileInput("upload",label="",buttonLabel="charger param.",placeholder=NULL) )
             ),
             sliderInput("z_moho",
                         "Epaisseur de la cro\u00FBte [km]",
                         min = 0,
                         max = 80,
                         value = 30),
             sliderInput("k",
                         "Conductivit\u00E9 (k) [W.K-1.m-1]",
                         min = 0.5,
                         max = 4,
                         value = 2),
             sliderInput("q_moho",
                         "Flux au Moho [mW.m-3]",
                         min = 0,
                         max = 60,
                         value = 30),
             sliderInput("HP",
                         "Production de chaleur [\u03BCW.m-3]",
                         min = 0,
                         max = 2,
                         step=0.1,
                         value = 0.65),


             h3("Constantes"),
             p("Modifiez \u00E0 vos risques et p\u00E9rils..."),
             numericInput(inputId="N",
                          label="Nombre de cellules",
                          value=100),
             numericInput(inputId="T_surf",
                          label="Temp\u00E9rature de surface [\u00B0C]",
                          value=0),
             numericInput(inputId="rho",
                          label="rho [kg.m-3]",
                          value=2700),
             numericInput(inputId="Cp",
                          label="Cp [J.K-1.kg-1]",
                          value=1000),
             ##For debug
             actionButton("debug","Open browser for debugging")
           )
    ),

    ########################### MIDDLE #################################
    column(
      width = 6,
      ###### THE GRAPH AND ITS SUBTITLE ######

      plotOutput("geothermPlot", hover = "plot_hover"),
      htmlOutput("coords"),


      ###### THE MODE OF CALCULATION ######
      wellPanel(
        #h3("Mode de calcul"),

        ##### SELECTOR #####
        shinyWidgets::radioGroupButtons(
          inputId = "mode_calc",
          label = "",
          choices = c(
            "Faire varier un param\u00E8tre" = "param",
            "Evolution dans le temps" = "time"
          )
        ),

        ##### MODE PARAMETER TEST #####
        conditionalPanel(
          condition = "input.mode_calc == 'param' ",
          h3("Calculer plusieurs geothermes en fonction d'un param\u00E8tre"),

          column(width=4,
                 selectInput(
                   inputId = "geoth_param",
                   label = "Param\u00E8tre \u00E0 faire varier:  ",
                   choices = c(
                     "Epaisseur" = "z_moho",
                     "Conductivit\u00E9" =
                       "k",
                     "Flux" = "q_moho",
                     "Production de chaleur" =
                       "HP"
                   )
                 )
          ),

          fluidRow(
            # column(width = 4, tags$div(id = "inline",uiOutput("geoth_param_from_UI"))),
            # column(width = 4, tags$div(id = "inline",uiOutput("geoth_param_to_UI"))),


            column(width = 4, tags$div(id = "inline",
                                       numericInput("geoth_param_from",
                                                    "Valeur minimale:",
                                                    value = 30,
                                                    width="40%"))
                   ),

            column(width = 4, tags$div(id = "inline",
                                       numericInput("geoth_param_to",
                                                    "Valeur maximale:",
                                                    value = 45,
                                                    width="40%"))
            ),





            column(
              width = 4,
              tags$div(id = "inline",numericInput(
                inputId = "n_curves",
                label = "Nombre de courbes",
                min = 1,
                value = 1,
                width = "40%"
              ))
            )
          )
        ),

        ###END MODE PARAM

        ##### MODE PT PATHS #####
        conditionalPanel(
          condition = "input.mode_calc == 'time' ",

          h3("Evolution dans le temps"),

          fluidRow(
            column(
              width = 3,
              tags$div(id = "inline",numericInput(
                "t_max",
                "Dur\u00E9e max.[Ma]",
                value = 100,
                step = 10,
                width = "80%"
              ))
            ),
            column(
              width = 4,
              tags$div(id = "inline",numericInput(
                inputId = "n_timestep",
                label = "Nombre d'\u00E9tapes",
                min = 1,
                value = 1,
                width = "60%"
              ))
            ),

            column(width=4,
                   checkboxInput("plot_geotherm",
                                 label = "Tracer les g\u00E9othermes",
                                 value = T),

                   checkboxInput("plot_path",
                                 label = "Tracer les trajets P-T",
                                 value = F)
            )

          ),

          fluidRow(
            column(width=8,
                   sliderInput(
                     "u",
                     "Vitesse verticale [mm.an-1]",
                     min = -10,
                     max = 10,
                     step = 0.1,
                     value = 0
                   )
            )


          ),


          conditionalPanel(
            condition = "input.plot_path == 1",
            fluidRow(
              column(width=8,
                     uiOutput("z0_slider")
              ),
              column(width=4,
                     tags$div(id = "inline",numericInput(
                       inputId = "n_path",
                       label = "Nombre de trajets PT",
                       min = 1,
                       value = 1,
                       width = "40%"
                     ) )
              )

            ),

            conditionalPanel(condition = "input.n_path > 1",
                             fluidRow(column(
                               width = 4,
                               selectInput(
                                 inputId = "path_param",
                                 label =
                                   "Param\u00E8tre \u00E0 faire varier",
                                 choices = c("Epaisseur" = "z_moho",
                                             "Conductivit\u00E9" = "k",
                                             "Flux" = "q_moho",
                                             "Production de chaleur" = "HP",
                                             "Vitesse verticale" = "u",
                                             "Profondeur initiale" = "z0"
                                 )
                               )
                             ),
                             column(width = 4, tags$div(id = "inline",uiOutput("path_param_from_UI"))),
                             column(width =
                                      4, tags$div(id = "inline",uiOutput("path_param_to_UI")))
                             ))
          )

        ) ###END MODE PATH
      )),

    ########################### RIGHT #################################
    column(width = 3,
           wellPanel(

             h3("Options graphiques"),

             ##### BACKGROUND #####
             h4("Arri\u00E8re-plan"),
             selectInput(inputId = "background",
                         label="",
                         choices=c("Faci\u00E8s m\u00E9tamorphiques"="faciesMeta",
                                   "Faci\u00E8s ultra-m\u00E9tamorphiques"="faciesUltraMeta",
                                   "R\u00E9actions"="React",
                                   "Dorsale oc\u00E9anique"="Ride")),

             ##### GEOTHERM SETTINGS #####
             conditionalPanel( condition = "input.plot_geotherm == 1",
                               h4("G\u00E9othermes"),

                               numericInput("geo_width",
                                            "Largeur des courbes",
                                            value = 1,
                                            step=0.2),

                               colourpicker::colourInput(inputId="geo_color1",
                                           label="Couleur",
                                           allowTransparent=T,
                                           value="red"),

                               conditionalPanel(condition = "input.n_curves > 1 || input.n_timestep > 1",
                                                colourpicker::colourInput(inputId="geo_color2",
                                                            label="Couleur max",
                                                            allowTransparent=T,
                                                            value="blue")
                               )
             ), #End geotherm conditional panel

             ##### PATH SETTINGS #####
             conditionalPanel( condition = "input.plot_path == 1",
                               h4("Trajets P-T"),

                               numericInput("path_width",
                                            "Largeur des courbes",
                                            value = 0.6,
                                            step=0.2),

                               colourpicker::colourInput(inputId="path_color1",
                                           label="Couleur",
                                           allowTransparent=T,
                                           value="green"),

                               conditionalPanel(condition = "input.n_path > 1 ",
                                                colourpicker::colourInput(inputId="path_color2",
                                                            label="Couleur max",
                                                            allowTransparent=T,
                                                            value="darkgreen")
                               )
             ) #End path conditional panel

           )) # End right side
  ) # This is the main row

) # End ui
