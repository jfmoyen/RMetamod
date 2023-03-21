#####################
#### SERVER SIDE ####
#####################


### This is to avoid check() notes when using un-quoted arguments in dplyr/ggplot syntax
utils::globalVariables(c("Temperature"))


#' Server-side function
#'
#' @import shiny
#' @import crustalHeat
#' @importFrom rlist list.save list.load
#' @importFrom scales seq_gradient_pal
#' @importFrom shinyWidgets updateColorPickr
#' @export
#'
#' @param input see shiny documentation
#' @param output see shiny documentation
#' @param session see shiny documentation
#' @seealso shiny::shinyApp

Rmetamod_server <- function(input, output, session) {

  #### Debug ####

  observeEvent(input$debug,{
    browser()
  })

  #### Clean the input ####
  # Make a "clean" version of input, to be manipulated, saved, etc.
  input_clean<-reactive(
    list(
      z_moho=input$z_moho, k=input$k, q_moho=input$q_moho, HP=input$HP,
      N=input$N, T_surf=input$T_surf, rho=input$rho, Cp=input$Cp,
      geoth_param=input$geoth_param, geoth_param_from=input$geoth_param_from, geoth_param_to=input$geoth_param_to, n_curves=input$n_curves,
      background=input$background, geo_width=input$geo_width, geo_color1=input$geo_color1, geo_color2=input$geo_color2
    )
  )


  #### Reset ####
  observeEvent(input$default, {
    updateSliderInput(inputId="z_moho",value=30)
    updateSliderInput(inputId="k",value=2)
    updateSliderInput(inputId="q_moho",value=30)
    updateSliderInput(inputId="HP",value=0.65)

    updateNumericInput(inputId="N",value=100)
    updateNumericInput(inputId="T_surf",value=0)
    updateNumericInput(inputId="rho",value=2700)
    updateNumericInput(inputId="Cp",value=1000)


    #updateNumericInput(inputId="n_curves",value=1)

    #updateNumericInput(inputId="u_mm_an",value=0)

  })

  #### Download ####
  output$download <- downloadHandler(
    filename = "metamod_parameters.yaml",
    content = function(file) {
      rlist::list.save(input_clean(),file,type="yaml")
    }
  )

  #### Upload ####
  observeEvent(input$upload,
               {
                 req(input$upload)

                 loadedParams <- rlist::list.load(input$upload$datapath)

                 # There is no "generic" update Input so we have to do it all manually !
                 updateSliderInput(inputId="z_moho",value=loadedParams$z_moho)
                 updateSliderInput(inputId="k",value=loadedParams$k)
                 updateSliderInput(inputId="q_moho",value=loadedParams$q_moho)
                 updateSliderInput(inputId="HP",value=loadedParams$HP)

                 updateNumericInput(inputId="geoth_param_to",value=loadedParams$geoth_param_to)
                 updateNumericInput(inputId="T_surf",value=loadedParams$T_surf)
                 updateNumericInput(inputId="rho",value=loadedParams$rho)
                 updateNumericInput(inputId="Cp",value=loadedParams$Cp)

                 updateSelectInput(inputId="geoth_param",selected=loadedParams$geoth_param)
                 updateNumericInput(inputId="geoth_param_from",value=loadedParams$geoth_param_from)
                 updateNumericInput(inputId="geoth_param_to",value=loadedParams$geoth_param_to)
                 updateNumericInput(inputId="n_curves",value=loadedParams$n_curves)

                 updateSelectInput(inputId="background",selected=loadedParams$background)
                 updateNumericInput(inputId="geo_width",value=loadedParams$geo_width)
                 shinyWidgets::updateColorPickr(session,inputId="geo_color1",value=loadedParams$geo_color1)
                 shinyWidgets::updateColorPickr(session,inputId="geo_color2",value=loadedParams$geo_color2)

               }
               )


  #### Hide/show geotherm legend
  observeEvent(input$mode_calc,
               {
                 if(input$mode_calc == "param")
                 {
                   updateCheckboxInput(inputId="plot_geotherm",value=1)
                   updateCheckboxInput(inputId="plot_path",value=0)
                   updateTextInput(inputId = "n_timestep",value=1)
                 }
                 if(input$mode_calc == "time")
                 {}
               }

  )


  #### Update range of parameters to explore ####

observeEvent(input$geoth_param,
             {
               # Careful here, as the bundle depends both on geoth_param and its values
               # don't forget to isolate...
               updateNumericInput(inputId = "geoth_param_from",value=input[[input$geoth_param]])
               updateNumericInput(inputId = "geoth_param_to",value=input[[input$geoth_param]]*1.5)
               })

  observeEvent(input$path_param,
               {
                 # Careful here, as the bundle depends both on geoth_param and its values
                 # don't forget to isolate...
                 updateNumericInput(inputId = "path_param_from",value=input[[input$path_param]])
                 updateNumericInput(inputId = "path_param_to",value=input[[input$path_param]]*1.5)
               })


  output$z0_slider <- renderUI({
    sliderInput("z0",
                "Pofondeur initiale",
                min = 0,
                max = input$z_moho ,
                step=0.1,
                value = input$z_moho/2)
  })

  #### Get background ####
  bg <- reactive({
      get(input$background)
  })

  ##### PARAMETRIZED MODE #####
  parametrizedBundle <- reactive(
    {

    #Do nothing before the interface is ready !
    req(input$geoth_param_from,input$geoth_param_to)

    geoth_varying_param <- isolate(input$geoth_param)

    # cat("param:",geoth_varying_param,
    #      " / from:",input$geoth_param_from,
    #     " / to:",input$geoth_param_to,
    #     " / n:",input$n_curves,"\n")
    #
    # As we call the bundle function, we take the opportunity to format everything
    # in SI units, as expected by the heat functions

    ee<-steady_geotherm_bundle(T_surf = input$T_surf,
                           rho = input$rho,
                           Cp = input$Cp,
                           z_moho = input$z_moho,
                           k = input$k,
                           q_moho = input$q_moho,
                           HP = input$HP,
                           varying = geoth_varying_param,
                           varies_from = input$geoth_param_from,
                           varies_to = input$geoth_param_to,
                           n_curves = input$n_curves,
                           N = input$N,
                           UItoSI = UItoSI)

    return(ee)
 })

  ##### EVOLVING MODE #####
  # # Time steps
  # times <- reactive({
  #   seq(from=0,to=input$t_max,
  #       length.out = input$n_timestep)
  # })
  #
  # # Real function
  # evolvingBundle <- reactive({
  #
  #   print("bundling")
  #   # Wrapper to plot multiple curves
  #   evolve_one_geotherm<-function(z_moho,
  #                                 T_surf, ka, q_moho, rhoCp, HP, u, times){
  #
  #     grid <- setup.grid.1D(x.down = 0, x.up = z_moho, N = input$N)
  #     T.ini <- seq(600,0,length.out=input$N)
  #
  #     st<-steady.1D(y=T.ini,func=Heat.Model,
  #                   T_surf=T_surf, ka=ka, q_moho=q_moho, rhoCp=rhoCp, HP=HP, u=0, grid=grid,
  #                   scaling=1e7,nspec=1)
  #
  #     out <- ode.1D(y= st$y,
  #                   times=times,
  #                   T_surf=T_surf,
  #                   ka=ka,
  #                   q_moho=q_moho,
  #                   rhoCp=rhoCp,
  #                   HP=HP,
  #                   u=u,
  #                   func=Heat.Model,
  #                   grid=grid,
  #                   nspec=1)
  #
  #     out %>%
  #       as_tibble %>%
  #       pivot_longer(cols=2:ncol(out),names_to = "cell", values_to="Temperature") %>%
  #       mutate(time=as.numeric(time),
  #              Temperature=as.numeric(Temperature),
  #              cell=as.numeric(cell),
  #              Depth = (cell * input$z_moho_km / input$N - input$z_moho_km) *1000,
  #              Time_Ma = time/86400/365 ) %>%
  #       {.} -> out2
  #     print("bundled")
  #     return(out2)
  #
  #   }
#
#     # Do nothing before interface is ready
#     #req(input$n_timestep)
#
#     ### Prepare times
#
#     extended_times <- sort(union(seq(from=0,to=input$t_max,
#                                      length.out = max(0,50 - input$n_timestep) ),times() ))
#
#     evolve_one_geotherm(
#       z_moho = input$z_moho_km * 1000,
#       T_surf = input$T_surf,
#       rhoCp = input$rhoCp,
#       ka = input$k / input$rhoCp,
#       q_moho = input$q_moho_mw * 1e-3,
#       HP = input$HP_muW * 1e-6,
#       u = input$u_mm_an * 1e-3 / (86400*365),
#       times = extended_times * 1e6 * 365 * 86400
#     )
#
#
#     ## Generate tibble with cases
#
#
#     # Make a sequence for the parameter that varies
#     replacement = seq(from= input$geoth_param_from_inner ,
#                       to =  input$geoth_param_to_inner,
#                       length.out =  input$n_curves )
#
#     # Build a simple tibble with the constants
#     cases <- tibble(z_moho_km=input$z_moho_km,
#                     T_surf=input$T_surf,
#                     k=input$k,
#                     q_moho_mw=input$q_moho_mw,
#                     rhoCp=input$rhoCp,
#                     HP_muW=input$HP_muW)
#
#     # These stay constant
#     keep_me <- setdiff(names(cases),input$geoth_param)
#
#     # Replace the col with the variable, inserting the range of values needed.
#     # Recalculate kappa and the variables that are not given as SI in the process
#     cases %<>% select(all_of(keep_me)) %>%
#       cbind(replacement) %>%
#       as_tibble() %>%
#       rename("{input$geoth_param}" := replacement) %>%
#       mutate(ka = k/rhoCp,
#              z_moho = z_moho_km * 1000,
#              q_moho = q_moho_mw * 1e-3,
#              HP = HP_muW * 1e-6 )
#
#     ## The fun starts !
#     cases %>%
#       rowwise() %>%
#       mutate(evol=list( one_steady_geotherm(z_moho ,
#                                             T_surf,
#                                             ka  ,
#                                             q_moho ,
#                                             rhoCp  ,
#                                             HP)  )) %>%
#       unnest(evol) %>%
#       {.} -> out
#
#     return(out)
#
#   })

  ### Prepare colour scale
  geotherm_colours <- reactive({

    scales::seq_gradient_pal(input$geo_color1,
                            input$geo_color2,
                            "Lab")(seq(0,1,
                            length.out=input$n_curves))
  })

  #### Plot ####
  output$geothermPlot <- renderCachedPlot({

   # if(input$mode_calc=="param"){
      ##### PLOT IN PARAM MODE #####

      # Prepare the canvas
      paramPlot <- plotCanvas( bg() )

      # Add curves
      paramPlot <- addGeotherms(paramPlot,
                                parametrizedBundle() ,
                                lwd=input$geo_width,
                                colscale=geotherm_colours() )

      paramPlot

      # Version inline
      # bg_image <- rasterGrob(bg()$image,width=unit(1,"npc"), height=unit(1,"npc"))
      #
      # pp<-parametrizedBundle() %>% ggplot()+
      #   annotation_custom(bg_image)+
      #   scale_x_continuous(limits=c(bg()$Tmin,bg()$Tmax),expand=c(0,0),
      #                      name="Température (°C)",
      #                      breaks=bg()$Tpos,
      #                      labels=bg()$Tpos,
      #                      position="top")+
      #   scale_y_continuous(limits=c(bg()$zmin,bg()$zmax),expand=c(0,0),
      #                      breaks=bg()$zmax+(bg()$zpos*1000),
      #                      labels=bg()$zpos,
      #                      name="Profondeur (km)")+
      #   geom_line(aes(x=Temperature,y=bg()$zmax-Depth,
      #                 colour= as.character(!!rlang::parse_expr(input$geoth_param))),
      #             linewidth=input$geo_width)+
      #   scale_color_manual(values=geotherm_colours() )+
      #   guides(colour=guide_legend(title = input$geoth_param )) +
      #   theme_linedraw()

  #  }


    # if(input$mode_calc=="time"){
    #   ##### PLOT IN TIME MODE #####
    #
    #   ### Prepare colour scale
    #   geotherm_colours <- scales::seq_gradient_pal(input$geo_color1,
    #                                                input$geo_color2,
    #                                                "Lab")(seq(0,1,
    #                                                           length.out=input$n_curves))
    #
    #   ### The graph itself
    #
    #   bg_image <- rasterGrob(bg()$image,width=unit(1,"npc"), height=unit(1,"npc"))
    #
    #   pp<-evolvingBundle() %>% ggplot()+
    #     annotation_custom(bg_image)+
    #     scale_x_continuous(limits=c(bg()$Tmin,bg()$Tmax),expand=c(0,0),
    #                        name="Température (°C)",
    #                        breaks=bg()$Tpos,
    #                        labels=bg()$Tpos,
    #                        position="top")+
    #     scale_y_continuous(limits=c(bg()$zmin,bg()$zmax),expand=c(0,0),
    #                        breaks=bg()$zmax+(bg()$zpos*1000),
    #                        labels=bg()$zpos,
    #                        name="Profondeur (km)")+
    #     geom_line(aes(x=Temp,y=bg()$zmax-Depth,
    #                   colour= as.character(!!rlang::parse_expr(input$geoth_param))),
    #               linewidth=input$geo_width)+
    #     scale_color_manual(values=geotherm_colours)+
    #     guides(colour=guide_legend(title = input$geoth_param )) +
    #     theme_linedraw()
    #
    #
    # }

   # print(pp)

  },
  sizePolicy = sizeGrowthRatio(width = 600, height = 450, growthRate = 1.2),
  cacheKeyExpr = input_clean() )
 # width=600) #this is the end of the RenderPlot function !


  ### Curser coordinates
  output$coords <- renderText({

    paste("<b>Coordonn\u00E9es : ",
          round((bg()$zmax*1000-as.numeric(input$plot_hover$y))/1000,1),
          "km ",
          round(as.numeric(input$plot_hover$x),0),
          "\u00B0C</b>")
  })

  ### Info - T moho etc ####
  output$tmoho <- renderText({

    paste("Temp. moho :",input$dTdz * input$z_moho,"\u00B0C")

  })

  output$totalz <- renderText({

    paste("Dep. vertical total :", input$u * UItoSI["u"] * input$t_max * UItoSI["time"] * UItoSI["z_moho"] , "km")

  })

} # end of server()
