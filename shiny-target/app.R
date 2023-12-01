library(shiny)
library(tidyverse)
library(patchwork)

red = "#cc0000"
  
# Function to solve the gesture
solve_gesture <- function(max_t, tg) {
  # time step
  dt <- 0.01
  
  # empty vectors for position and velocity
  X <- c()
  V <- c()
  
  # initial states
  X[1] <- 0
  V[1] <- 0
  
  # Gesture
  TG <- tg
  KG <- 6
  
  # Neutral attractor
  TN <- 0
  KN <- 6
  
  # activation function
  ae <- 450
  A <- rep(0, max_t)
  r <- 40
  A[100:(99 + r)] <- cumsum(rep(1 / r, r))
  A[(100 + r):(ae - r)] <- 1
  A[(ae - r):(ae - 1)] <- 1 - cumsum(rep(1 / r, r))
  
  TM <- seq(1, max_t)
  
  # let gesture evolve
  for (t in TM[2:length(TM)]) {
    Tt <- A[t] * TG + (1 - A[t]) * TN
    Kt <- A[t] * KG + (1 - A[t]) * KN
    B <- 2 * sqrt(Kt)
    dx <- V[t - 1]
    X[t] <- X[t - 1] + dt * dx
    dv <- -B * V[t - 1] - Kt * (X[t - 1] - Tt)
    V[t] <- V[t - 1] + dt * dv
  }
  data.frame(X, V, A, TM, ae)
}

# Minima, maxima, and steps of parameters
tg_min <- -1.3
tg_max <- -0.7
tg_step <- 0.1
max_t  <- 750

# If data should be generated on the fly.
# Otherwise it is loaded
generate_data <- T

if (generate_data) {
  df <- data.frame()
  for (tg in seq(tg_min, tg_max, tg_step)) {
    tg <- round(tg, 1)
    G <- solve_gesture(max_t, tg)
    df <- rbind(df, data.frame(tg, G))
  }
  write.csv(df, "data.csv")
} else {
  df <- read.csv("data.csv")
}

slider_styling <- HTML(
  ".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #cc0000;  border-top: 1px solid #cc0000; border-bottom: 1px solid #cc0000}"
)

# UI of the app
ui <- fluidPage(titlePanel(""),
                tags$style(slider_styling),
                fluidRow(
                  column(4,
                         wellPanel(
                           h4("Target of red opening gesture", style = "color: #cc0000"),
                           sliderInput(
                             inputId = "tg",
                             label = "",
                             min = tg_min,
                             max = tg_max,
                             step = tg_step,
                             value = -1
                           )
                         )),
                  column(8,
                         plotOutput(outputId = "evoPlot", height = "650px"))
                ))

# Server function of the shiny app that generates the plot
server <- function(input, output) {
  output$evoPlot <- renderPlot({
    
    # Get correct gestures from the data frame
    d1 <-
      df %>% filter(tg == input$tg) %>%
      mutate(type = "modified")
    d2 <-
      df %>% filter(tg == -1) %>%
      mutate(type = "reference")
    
    d <- rbind(d1, d2)
    
    ae1 <- unique(d1$ae)
    ae2 <- unique(d2$ae)
    
    
    layout(matrix(c(1,2,3), ncol=1), widths=c(3,3,3), heights=c(1,2,2), TRUE) 
    par(mar = c(3,3,2,1), cex = 1)
    
    plot(d1$TM, d1$A*2.1, type = "n", yaxt = "n", ann = F)
    rect(xleft = 100, xright = ae1, ybottom = 0, ytop = 1, col = rgb(0.8,0,0,0.3), border = red)
    text(x = 100+(ae1-100)/2, y = 0.5, labels = "labial opening")
    rect(xleft = ae1, xright = max_t, ybottom = 0, ytop = 1, col = rgb(0.8,0,0,0.3), border = red)
    text(x = ae1+(max_t-ae1)/2, y = 0.5, labels = "labial closure")
    rect(xleft = 100, xright = ae2, ybottom = 1.1, ytop = 2.1, col = "lightgrey", border = "dimgrey")
    text(x = 100+(ae2-100)/2, y = 1.6, labels = "labial opening")
    rect(xleft = ae2, xright = max_t, ybottom = 1.1, ytop = 2.1, col = "lightgrey", border = "dimgrey")
    text(x = ae1+(max_t-ae1)/2, y = 1.6, labels = "labial closure")
    title(main = "gestural score (activations)", line = 0.5, adj = 0)
    
    plot(d1$TM, d1$X, type = "n", xlab = "", ylab = "", ylim = c(-1.3, 0.1), yaxt = "n")
    ytick <- seq(-1, 0, by = 0.5)
    axis(side = 2, at = ytick, labels = T)
    
    grid(nx = NULL,
         ny = NULL,
         lty = 2, col = "grey", lwd = 0.7)
    lines(d1$TM, d1$X, type = "l", col = red)
    lines(d2$TM, d2$X, col = "dimgrey")
    title(ylab = "position", line = 2)
    title(main = "position", line = 0.5, adj = 0)
    
    text(x = max_t-25, y = 0.07, labels = "closed")
    text(x = max_t-25, y = -1.27, labels = "open")
    
    plot(d1$TM, abs(d1$V), type = "n", ylim = c(0, 1.3), xlab = "", ylab = "", yaxt = "n")
    grid(nx = NULL,
         ny = NULL,
         lty = 2, col = "grey", lwd = 0.7)
    lines(d1$TM, abs(d1$V), type = "l", col = red)
    lines(d2$TM, abs(d2$V), col = "dimgrey")
    
    ytick <- seq(0, 1.2, by = 0.4)
    axis(side = 2, at = ytick, labels = T)
    text(x = max_t-25, y = 1.25, labels = "faster")
    text(x = max_t-25, y = 0.1, labels = "slower")
    
    title(ylab = "absolute velocity", line = 2)
    title(xlab = "simulated time", line = 2)
    title(main = "absolute velocity", line = 0.5, adj = 0)

    
  })
}

shinyApp(ui = ui, server = server)