#---------------------------------------------------------
# Script to produce 3d surfaces representing impacts

# first test

install.packages("plotly")
library(plotly)

# grid
x <- seq(-5, 3, length.out = 100)
y <- seq(-5, 5, length.out = 100)

# gaussian function
gauss <- function(x, y, mx, my, sx=1, sy=1){
  exp(-((x-mx)^2/(2*sx^2) + (y-my)^2/(2*sy^2)))
}

# build surface
z <- outer(x, y, function(x,y)
  gauss(x,y,-2,-2,1,1) +
    gauss(x,y,2,1,1,1)
)

plot_ly(x=x, y=y, z=z) |> 
  plotly::add_surface()

install.packages("plot3D")
library(plot3D)

persp3D(x, y, z,
        colkey = FALSE,
        theta = 30,
        phi = 20)


# first test with plotly

gaussian_surface <- function(points, 
                             grid_size = 100,
                             xlim = c(-10,10),
                             ylim = c(-10,10),
                             sigma = 1){
  
  # create grid
  x <- seq(xlim[1], xlim[2], length.out = grid_size)
  y <- seq(ylim[1], ylim[2], length.out = grid_size)
  
  z <- matrix(0, grid_size, grid_size)
  
  # add gaussian from each point
  for(i in 1:nrow(points)){
    
    mx <- points[i,1]
    my <- points[i,2]
    amp <- ifelse(ncol(points) >= 3, points[i,3], 1)
    
    z <- z + outer(x, y, function(x,y)
      amp * exp(-((x-mx)^2 + (y-my)^2)/(2*sigma^2))
    )
  }
  
  return(list(x=x,y=y,z=z))
}


plot_gaussian_surface <- function(surface, zlim=NULL){
  
  library(plotly)
  
  p <- plot_ly(x = surface$x,
               y = surface$y,
               z = surface$z,
               type="surface",
  )
  
  if(!is.null(zlim)){
    p <- p %>% layout(
      scene = list(
        zaxis = list(range = zlim)
      )
    )
  }
  
  p
}

points <- matrix(c(
  -5, 10, 0.1,
  5,  10, 0.2,
  0, -4, 0.15
), ncol = 3, byrow = TRUE)

surface <- gaussian_surface(points,
                            sigma = 1.5,
                            grid_size = 120)

plot_gaussian_surface(surface, zlim=c(0,0.5))



#-----------

# second test

plot_gaussian_surface <- function(surface, zlim=NULL){
  
  library(plotly)
  
  plot_ly(
    x = surface$x,
    y = surface$y,
    z = surface$z,
    type = "surface",
    
    colorscale = "Reds",
    # colorscale = list(
    #   c(0, "lightgrey"),
    #   # c(0.5, "yellow"),
    #   c(1, "red")
    # ),
    reversescale = FALSE,
    
    opacity = 1,   # ← set transparency here
    
    contours = list(  # ← surface grid lines
      x = list(
        show = TRUE,
        start = min(surface$x),
        end = max(surface$x),
        size = 0.5,
        color = "black"
      ),
      y = list(
        show = TRUE,
        start = min(surface$y),
        end = max(surface$y),
        size = 0.5,
        color = "black"
      )
    )
    
  ) %>%
    layout(
      scene = list(
        zaxis = list(range = zlim, visible = FALSE),
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE)
      )
    )
  
}

points <- matrix(c(
  -5, 9.5, 0.1,
  5,  9.5, 0.2,
  0, -4, 0.15
), ncol = 3, byrow = TRUE)

surface <- gaussian_surface(points,
                            xlim=c(-10,10),
                            ylim=c(-10,10),
                            grid_size=120,
                            sigma=1.5)

plot_gaussian_surface(surface, zlim=c(0,0.5))

#-------------------------------

# now for real

library(plotly)

gaussian_surface <- function(points,
                             grid_size = 150,
                             xlim = c(-10, 10),
                             ylim = c(-10, 10),
                             sigma = 1) {
  
  x <- seq(xlim[1], xlim[2], length.out = grid_size)
  y <- seq(ylim[1], ylim[2], length.out = grid_size)
  
  z <- matrix(0, grid_size, grid_size)
  
  for (i in 1:nrow(points)) {
    mx <- points[i, 1]
    my <- points[i, 2]
    amp <- ifelse(ncol(points) >= 3, points[i, 3], 1)
    
    z <- z + outer(
      x, y,
      function(x, y) amp * exp(-((x - mx)^2 + (y - my)^2) / (2 * sigma^2))
    )
  }
  
  list(x = x, y = y, z = z)
}

plot_gaussian_surface_style <- function(surface,
                                        zlim = NULL,
                                        xline_every = 1,
                                        yline_every = 1) {
  
  plot_ly(
    x = surface$x,
    y = surface$y,
    z = surface$z,
    type = "surface",
    # colorscale = list(
    #   c(0.00, "#fff7bc"),
    #   c(0.20, "#fee391"),
    #   c(0.40, "#fec44f"),
    #   c(0.60, "#fe9929"),
    #   c(0.80, "#d95f0e"),
    #   c(1.00, "#993404")
    # ),
    colorscale = "Reds",
    showscale = FALSE,
    opacity = 1,
    contours = list(
      x = list(show = TRUE, start = min(surface$x), end = max(surface$x),
               size = xline_every, color = "rgba(90,90,90,0.55)",
               width = 1, highlight = FALSE),
      y = list(show = TRUE, start = min(surface$y), end = max(surface$y),
               size = yline_every, color = "rgba(90,90,90,0.55)",
               width = 1, highlight = FALSE)
    ),
    lighting = list(
      ambient = 0.75,
      diffuse = 0.85,
      roughness = 0.95,
      specular = 0.05,
      fresnel = 0.02
    ),
    lightposition = list(x = 100, y = 100, z = 200)
  ) %>%
    layout(
      scene = list(
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        zaxis = list(visible = FALSE, range = zlim),
        aspectratio = list(x = 1.8, y = 1.2, z = 0.4),
        camera = list(
          eye = list(x = -1.6, y = -1.3, z = 0.75)
        ),
        bgcolor = "white"
      ),
      paper_bgcolor = "white",
      margin = list(l = 0, r = 0, b = 0, t = 0)
    )
}

#--------
# now using this last one

# wind turbines

points <- matrix(c(
  -5, 9.5, 1,
  5,  9.5, 2,
  0, -4, 1.5
), ncol = 3, byrow = TRUE)

surface <- gaussian_surface(
  points,
  xlim=c(-10,10),
  ylim=c(-10,10),
  grid_size=120,
  sigma=1.5
)

plot_gaussian_surface_style(surface, zlim = c(0, 2), xline_every = 1, yline_every = 1)

#----------

# mine

points <- matrix(c(
  0, 9.5, 1.8
), ncol = 3, byrow = TRUE)

surface <- gaussian_surface(
  points,
  xlim=c(-10,10),
  ylim=c(-10,10),
  grid_size=120,
  sigma=3.5
)

plot_gaussian_surface_style(surface, zlim = c(0, 2), xline_every = 1, yline_every = 1)

#----------

# wind turbines and mine
# here we expand the function to allow especific sigma for each feature
# nad have points defined as a data.frame instead of matrix

gaussian_surface <- function(points,
                             grid_size = 150,
                             xlim = c(-10, 10),
                             ylim = c(-10, 10),
                             sigma_default = 1) {
  
  x <- seq(xlim[1], xlim[2], length.out = grid_size)
  y <- seq(ylim[1], ylim[2], length.out = grid_size)
  
  z <- matrix(0, grid_size, grid_size)
  
  for (i in 1:nrow(points)) {
    mx  <- points$x[i]
    my  <- points$y[i]
    amp <- if ("amp" %in% names(points)) points$amp[i] else 1
    sig <- if ("sigma" %in% names(points)) points$sigma[i] else sigma_default
    
    z <- z + outer(
      x, y,
      function(x, y) {
        amp * exp(-((x - mx)^2 + (y - my)^2) / (2 * sig^2))
      }
    )
  }
  
  list(x = x, y = y, z = z)
}

# remake figure for turbines only, with sigma slightly bigger for three turbines

points <- data.frame(
  x = c(-5, 5, 0),
  y = c(9.5, 9.5, -4),
  amp = c(1, 2, 1.5),
  sigma = c(1.5, 1.6, 1.5)
)

surface <- gaussian_surface(
  points,
  xlim=c(-10,10),
  ylim=c(-10,10),
  grid_size=120,
  # sigma=1.5
)

plot_gaussian_surface_style(surface, zlim = c(0, 2), xline_every = 1, yline_every = 1)

# plot for everything all together

# points <- matrix(c(
#   -5, 9.5, 1,
#   5,  9.5, 2,
#   0, -4, 1.5,
#   15, 9.5, 1.8
# ), ncol = 3, byrow = TRUE)

# mine and turbines separated

points <- data.frame(
  x = c(-5, 5, 0, 20),
  y = c(9.5, 9.5, -4, 8.5),
  amp = c(1, 2, 1.5, 1.8),
  sigma = c(1.5, 1.6, 1.5, 3.5)
)

surface <- gaussian_surface(
  points,
  xlim=c(-10,30),
  ylim=c(-10,10),
  grid_size=120,
  # sigma=3.5
)

plot_gaussian_surface_style(surface, zlim = c(0, 2), xline_every = 1, yline_every = 1)

# mine and turbines close to each other

points <- data.frame(
  x = c(-5, 5, 0, 12),
  y = c(9.5, 9.5, -4, 8.5),
  amp = c(1, 2, 1.5, 1.8),
  sigma = c(1.5, 1.6, 1.5, 3.5)
)

surface <- gaussian_surface(
  points,
  xlim=c(-10,30),
  ylim=c(-10,10),
  grid_size=120,
  # sigma=3.5
)

plot_gaussian_surface_style(surface, zlim = c(0, 2.5), xline_every = 1, yline_every = 1)


