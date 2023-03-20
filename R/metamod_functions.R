##### GEOTHERM and P-T PATHS CALCULATIONS #####

require(ReacTran)
require(magrittr)
require(grid)

Ma_to_sec <- function(t_Ma){
  #' Convenience function to convert Ma to seconds
  return(t_Ma * 1e6 * 86400 * 365 )
}

sec_to_Ma <- function(tsec){
  #' Convenience function to convert seconds to Ma
  return(t_sec / (1e6 * 86400 * 365) )
}

############### Heat transfert equation ####################

heatModel <- function(time, Temp, pars,
                       T_surf, k, q_moho, rho, Cp, HP, u,
                       grid,
                       scaling=1) {
  #' The heat transfert equation in a simple case: 1D-convection + advection with heat production
  #' and constant physical parameters. The boundary conditions are defined here as a base (moho) flux.
  #' @param time: boiler-plate for ReacTran (time)
  #' @param Temp: boiler-plate for ReacTran (temperature)
  #' This also allows to make both available inside the function, which is useful 
  #' if you need to do time or temperature dependent changes (such as k = f(T) )
  #' (hook for future development)
  #' @param pars: boiler-plate for ReacTran, I suspect this is a better way to pass T_surf and friends...
  #' @param T_surf: surface temperature [°C] or [K]
  #' @param k: Heat conductivity [W.m-1.K-1]
  #' @param q_moho: Heat flux at the moho [W.m-2]
  #' @param rho: Specific mass of the crust [km.m-3]
  #' @param Cp: Heat capacity of the crust [J.K-1.kg-1]
  #' @param HP: Heat production of the rocks [W.m-3]
  #' @param u: Vertical velocity [m.s-1]
  #' @param grid: a ReacTran 1D grid object
  #' @param scaling: The size of the time unit. Since 1 sec is very short in crustal heat propagation,
  #' the changes are minute and the steady-state solder struggles to find a solution.
  #' A higher scaling value, of perphaps 1e7, does help.
  #' @note This function expects its parameters in S.I. units. In particular everything involving a time,
  #' even though a power [W], is in seconds and probably has to be converted from Ma upstream.
  #' 
  #' All the physical parameters being constant, they are defined as reals (NOT ReacTran grids, for instance)
  #' @return a list with one component, dTdt, the rate of change of temperature for each depth. 
  #' Not for human consumption, meant to be fed to the ReacTran solvers.
  
  rhoCp <- rho * Cp
  ka <- k / rhoCp
  
  tran <- tran.1D(C = Temp, C.down = T_surf,
                  flux.up = - q_moho,
                  D = ka, 
                  VF = rhoCp,
                  v = u,
                  dx = grid )
  
  return(list(dTdt = (tran$dC + HP/rhoCp) * scaling ))
}

############### Steady state case ####################

steady_geotherm_bundle <- function(T_surf, rho, Cp,
                                   z_moho=0, k=0, q_moho=0, HP=0,
                                   varying, varies_from, varies_to, n_curves,
                                   N,
                                   UItoSI=NULL,
                                   model = heatModel){
  #' Calculate a series of geotherm, varying one of the input parameters
  #' Parameters allowed to vary are z_moho, k, q_moho and HP

  #' @param z_moho: depth to the moho, in [m]
  #' @param N: number of cells in the model
  #' @param T_surf: surface temperature [°C] or [K]
  #' @param k: Heat conductivity [W.m-1.K-1]
  #' @param q_moho: Heat flux at the moho [W.m-2]
  #' @param rho: Specific mass of the crust [km.m-3]
  #' @param Cp: Heat capacity of the crust [J.K-1.kg-1]
  #' @param HP: Heat production of the rocks [W.m-3]
  #' @param varying (string): the name of the parameter that varies. It changes from `varies_from`
  #' to `varies_to`, generating a total of `n_curves` curves.
  #' The value that is may have been supplied is ignored
  #' @param varies_from: minimum value of the varying parameter
  #' @param varies_to: maximum value of the varying parameter
  #' @param n_curves: total number of geotherms to compute
  #' @param N: Number of cells in the model
  #' @param UIsoSI: the conversion factors from user-specified values to SI values.
  #' This is a named vector with the names of the parameters, i.e. `z_moho`, `k`, `q_moho`, `rho`, `Cp` and `HP`.
  #' If not supplied, the function will assume that they are all 1, i.e. values are in SI already.
  #' @param model: The modelling function
  #' @note This function calculates in SI units, but the values can be specified in user-defined (UI) units
  #' as long as the conversion factors UI to SI are supplied. Pay attention to mW, km, Ma, etc.
  #' @return a tibble in "long" format, with depth (in SI units), temperature and the parameters used for each curve
  
  # If needed, we make a vector of conversion factors equal to 1 (no conversion needed)
  if(is.null(UItoSI)){
    UItoSI <- c(1,1,1,1,1,1,1)
    names(UItoSI) <- c("z_moho","q_moho","HP","k","rho","Cp","u")
  }
  
  # Inner function
  .oneSteadyGeotherm <- function(z_moho, N=N,
                                 T_surf, k, q_moho, rho, Cp, HP, model=model){
    # Inner function whose goal in life is to be applied to all cases
    
      # Build the ReacTran grid
      grid <- setup.grid.1D(x.down = 0, x.up = z_moho, N = N)
      
      # The steady state solver requires an initial temperature (but this is not critical)
      # Below is a reasonable guess
      T.ini <- seq(600,0,length.out=N)
      
      # We use scaling here because the time unit is really small, and the changes minute between two steps !
      st<-steady.1D(y=T.ini,func=model,
                    T_surf=T_surf, k=k, q_moho=q_moho, rho = rho, Cp=Cp, HP=HP, u=0, grid=grid,
                    scaling=1e7,nspec=1)
    
      # Format nicely
      return(tibble(Depth = grid$x.mid,Temperature = st$y) )
    
  }

  # Make a sequence for the parameter that varies
  replacement = seq(from= varies_from,
                    to =  varies_to,
                    length.out =  n_curves )
  
  # Build a simple tibble with the constants
  cases <- tibble(z_moho,
                  T_surf, 
                  k=k,
                  q_moho, 
                  rho,
                  Cp,
                  HP)

  # These stay constant
  keep_me <- setdiff(names(cases),varying)
  
  # Replace the col with the variable, inserting the range of values needed.
  cases %<>% select(all_of(keep_me)) %>%
    cbind(replacement) %>%
    as_tibble() %>%
    rename("{varying}" := replacement) 

  ## The fun starts ! We map the geotherm function to each line of the tibble
  # This is where we convert UI units to SI units
  # This return a tibble of tibbles, that we "flatten"
  cases %>%
    rowwise() %>%
    mutate(evol=list( .oneSteadyGeotherm(N=N,
                                         z_moho * UItoSI["z_moho"],
                                         T_surf, 
                                         k=k * UItoSI["k"],
                                         q_moho * UItoSI["q_moho"], 
                                         rho * UItoSI["rho"],
                                         Cp * UItoSI["Cp"],
                                         HP * UItoSI["HP"],
                                         model=model)  )) %>%
    unnest(evol) %>%
    {.} -> out

  # Carry the name of the varying parameter
  attr(out,"varying") <- varying
  
    return(out)
}

############### Plotting ####################

plotCanvas <- function(bg){
  #' Prepare a canvas, with the appropriate background and axes
  #' @param bg the background. A list containing the following:
  #' * `image`, in a format that can be read by the system
  #' * `Tmin`, `Tmax`, the temperature of the left and right border of the image
  #' * `zmin`, `zmax`, the depth of the top and bottom border of the image, in km. 
  #'    They are depth, so positive numbers, going down.
  #' * `Tpos`, `zpos`, the position (and labels) of the ticks on each axis. `zpos` is in km. Positive,going down.
  #' @return (invisibly) a ggplot object with an empty canvas, including a background image and properly set-up axes.
  #' The empty plot is returned invisibly, you need to plot it explicitly if you want it !
   
  bg_image <- rasterGrob(bg$image,width=unit(1,"npc"), height=unit(1,"npc"))
  
  pp <- ggplot() +
    annotation_custom(bg_image)+
    scale_x_continuous(limits=c(bg$Tmin,bg$Tmax),expand=c(0,0),
                       name=stringi::stri_encode("Température (°C)", "UTF-8"),
                       breaks=bg$Tpos,
                       labels=bg$Tpos,
                       position="top")+
    scale_y_continuous(limits=c(bg$zmin*1000,bg$zmax*1000),expand=c(0,0),
                       breaks=bg$zmax*1000-(bg$zpos*1000),
                       labels=bg$zpos,
                       name="Profondeur (km)")+
    theme_linedraw()
  #browser()
  invisible(pp)
  }


addGeotherms <- function(p,geoth,curveVar=NULL,lwd=1,colscale=NULL){
  #' Add geotherms to an existing plot canvas
  #' @param p the plot canvas
  #' @param geoth a tibble, containing the geotherm values. It requires at least
  #' columns `Depth`, `Temperature` and the varying parameter (defined in `var`), corresponding to the color code.
  #' @param curveVar the paramters that varies and defines the curve bundle. It can be automatically read from attributes
  #' @param lwd the line width (constant)
  #' @param colscale the color scale. Discrete. Make sure it has the same length as the number of cases. 
  #' If not supplied, it tries to give sensible defaults.
  #' @value a ggplot object containing the plot. It is returned invisibly so you need to plot it manually.
  
  # Find the maximum Y value
  ymax <- ggplot_build(p)$layout$panel_params[[1]]$y.range[2]
  
  # The parameter that varies is (normally) stored as an attribute
  # if not user-specified we try to get it, and complain if we do not find it...
  if(is.null(curveVar)){
    if("varying" %in% names(attributes(geoth))){
      curveVar <- attr(geoth,"varying")
    }else{
      stop("Error in addGeotherms. Geotherm tibble has no \"varying\" attribute\n
           and curveVar was not specified")
    }
  } 

  # Add legend key
  geoth %<>% mutate(colkey=format(!!rlang::parse_expr(curveVar),digits=3)) 
  
  # If needed, invent a sensible colour scale
  if(is.null(colscale)){
    ncols <- n_distinct(geoth$colkey)
    colscale <- scales::seq_gradient_pal("blue","gold","Lab")(seq(0,1,length.out=ncols))
  }
   
  pp <- p + geom_line(data=geoth,aes(x=Temperature,y=ymax-Depth,
                     colour= colkey),
                 linewidth=lwd)+
       scale_color_manual(values=colscale )+ 
       guides(colour=guide_legend(title = curveVar ))
  
  invisible(pp)
}



  