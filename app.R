library(shiny)
library(bslib)

# UI and theme
app_theme <- bs_theme(
  version = 5,
  bootswatch = "sandstone"
)

ui <- fluidPage(
  theme = app_theme,
  titlePanel("Comparing Standard Normal and t-Distributions"),
  tabsetPanel(
    # First tab: Plot Distributions
    tabPanel("Plot Distributions",
             sidebarLayout(
               sidebarPanel(
                 checkboxInput("add_t", "Overlay t-distribution", value = TRUE),
                 conditionalPanel(
                   condition = "input.add_t == true",
                   numericInput("df", "Degrees of Freedom (df):", value = 3, min = 1),
                   # information
                   br(),
                   HTML(paste0(
                     "Developed for <strong>DSE1101</strong>: Introduction to Data Science for Economics."
                   ))
                 )
               ),
               mainPanel(
                 plotOutput("distPlot"),
                 br(),
                 helpText(HTML(
                   "<strong>Note:</strong> We compare the <strong>standard normal distribution</strong> and the <strong>t-distribution</strong>.
                    Both distributions are symmetric and centered at zero. 
      The standard normal has a fixed shape, while the t-distribution varies depending on the degrees of freedom (df). 
      As df increases, the t-distribution approaches the standard normal."
                 ))),
               
             )
    ),
    
    # Second tab: Cumulative probability
    tabPanel("Cumulative Probability",
             sidebarLayout(
               sidebarPanel(
                 numericInput("x_val", "Value of x:", value = 1),
                 radioButtons("dist_choice", "Distribution:",
                              choices = c("Standard normal" = "normal", "t-distribution" = "t")),
                 conditionalPanel(
                   condition = "input.dist_choice == 't'",
                   numericInput("df_cum", "Degrees of Freedom (df):", value = 30, min = 1)
                 ),
                 # information
                 br(),
                 HTML(paste0(
                   "Developed for <strong>DSE1101</strong>: Introduction to Data Science for Economics."
                 ))
               ),
               mainPanel(
                 plotOutput("cumPlot"),
                 br(),
                 helpText(HTML(
                   "<strong>Note:</strong> As the degrees of freedom increase, the <strong>t-distribution</strong> approaches the standard normal. 
                    When df is large (≥ 30 as a practical rule of thumb), the normal distribution is often used as a convenient approximation for calculating probabilities."
                 )),
                 uiOutput("r_code_header"),
                 verbatimTextOutput("cum_code")
               )
             )
    )
  )
)

server <- function(input, output) {
  
  # Tab 1 Distribution comparison
  output$distPlot <- renderPlot({
    x <- seq(-4, 4, length.out = 400)
    plot(x, dnorm(x), type = "l", lwd = 3.5, col = "steelblue",
         ylab = "Density", xlab = "x", main = "Standard Normal vs t-Distribution")
    
    if (input$add_t) {
      lines(x, dt(x, df = input$df), col = "greenyellow", lwd = 3.5, lty = "dashed")
      legend("topleft", legend = c("Standard normal distribution", "t-distribution"), 
             col = c("steelblue", "greenyellow"),
             lwd = 3, lty = c("solid", "dashed"))
    } else {
      legend("topleft", legend = "Standard normal", col = "steelblue", lwd = 3)
    }
  })
  
  # Tab 2 Cumulative probability
  output$cumPlot <- renderPlot({
    x_val <- input$x_val
    x <- seq(-5, 5, length.out = 500)
    dist_type <- input$dist_choice
    
    if (dist_type == "normal") {
      y <- dnorm(x)
      plot(x, y, type = "l", lwd = 3.5, col = "steelblue", ylim = c(0, max(y) * 1.05),
           main = paste0("Standard normal distribution"),
           xlab = "x", ylab = "Density")
      
      # Shade area to the left of x_val
      polygon(c(x[x <= x_val], x_val),
              c(y[x <= x_val], 0),
              col = adjustcolor("steelblue", alpha.f = 0.3),
              border = NA)
      
      abline(v = x_val, col = "steelblue", lty = 2)
      
      p_val <- round(pnorm(x_val), 4)
      text(x = -4, y = max(y) * 0.9, 
           labels = bquote(italic(P)(X <= .(x_val)) == .(p_val)), 
           pos = 4, col = "steelblue", cex = 1.2)
      
    } else {
      df <- input$df_cum
      y <- dt(x, df)
      plot(x, y, type = "l", lwd = 3.5, col = "forestgreen", ylim = c(0, max(y) * 1.05),
           main = paste0("t-distribution with df = ", df),
           xlab = "x", ylab = "Density")
      
      polygon(c(x[x <= x_val], x_val),
              c(y[x <= x_val], 0),
              col = adjustcolor("forestgreen", alpha.f = 0.3),
              border = NA)
      
      abline(v = x_val, col = "forestgreen", lty = 2)
      
      p_val <- round(pt(x_val, df), 4)
      text(x = -4, y = max(y) * 0.9, 
           labels = bquote(italic(P)(X <= .(x_val)) == .(p_val)), 
           pos = 4, col = "forestgreen", cex = 1.2)
    }
  })
  
  # Cumulative probability code
  output$r_code_header <- renderUI({
    strong("Base R code for cumulative probability:")
  })
  
  
  output$cum_code <- renderPrint({
    x <- input$x_val
    if (input$dist_choice == "normal") {
      cat(paste0("x <- ", x, "\n"))
      cat("pnorm(x)\n")
    } else {
      df <- input$df_cum
      cat(paste0("x <- ", x, "\n"))
      cat(paste0("df <- ", df, "\n"))
      cat("pt(x, df)\n")
    }
  })

}

shinyApp(ui, server)