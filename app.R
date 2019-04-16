library(shiny)
library(tidyverse)

# Define UI ----
ui <- fluidPage(
  titlePanel("title panel"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps with 
               information from the 2010 US Census."),
      numericInput("n_y",
                   label = "Number of young players",
                   value = 5,
                   min = 0),
      numericInput("n_e",
                   label = "Number of elderly players",
                   value = 5,
                   min = 0),
      numericInput("t_max",
                   label = "Number of iterations",
                   value = 20,
                   min = 0),
      sliderInput("prop_y_vac",
                  label = "Proportion of vaccinated young players",
                  min = 0,
                  max = 100,
                  value = 50,
                  ticks = FALSE,
                  post = "%"),
      sliderInput("prop_e_vac",
                  label = "Proportion of vaccinated elderly players",
                  min = 0,
                  max = 100,
                  value = 20,
                  ticks = FALSE,
                  post = "%"),
      sliderInput("y_effic",
                  label = "Vaccine efficacy for young players",
                  min = 0,
                  max = 100,
                  value = 80,
                  ticks = FALSE,
                  post = "%"),
      sliderInput("e_effic",
                  label = "Vaccine efficacy for elderly players",
                  min = 0,
                  max = 100,
                  value = 50,
                  ticks = FALSE,
                  post = "%"),
      numericInput("f_cost_y",
                   label = "Flu cost for young players",
                   value = 100,
                   min = 0),
      numericInput("f_cost_e",
                   label = "Flu cost for elderly players",
                   value = 400,
                   min = 0),
      numericInput("points",
                   label = "Starting points",
                   value = 4000,
                   min = 0),
      numericInput("vac_cost",
                   label = "Vaccine cost",
                   value = 20,
                   min = 0)
      ),
    mainPanel(
      plotOutput("plot1"),
      # textOutput("y_list"),
      tableOutput("graph_df")
      # textOutput("p_list")
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  # Store values from sidebar into variables
  n_y <- reactive({input$n_y})
  n_e <- reactive({input$n_e})
  n <- reactive({input$n_y + input$n_e})
  t_max <- reactive({input$t_max})
  
  prop_y_vac <- reactive({input$prop_y_vac / 100})
  prop_e_vac <- reactive({input$prop_e_vac / 100})
  y_effic <- reactive({input$y_effic / 100})
  e_effic <- reactive({input$e_effic / 100})
  f_cost_y <- reactive({input$f_cost_y})
  f_cost_e <- reactive({input$f_cost_e})
  
  points <- reactive({input$points})
  vac_cost <- reactive({input$vac_cost})
  
  y_list <- reactive({
    sample(1:n(), input$n_y)
  })
  output$y_list <- reactive({y_list()})
  e_list <- reactive({
    setdiff(1:n(), y_list())
  })
  
  p_type_df <- reactive({
      data.frame(p = 1:n()) %>% 
      mutate(p_type = ifelse(p %in% y_list(), "young", "elderly"))
  })
  output$p_type_df <- renderTable({p_type_df()})
  
  y_vac_list <- reactive({
    sample(y_list(), prop_y_vac() * n_y())
  })
  e_vac_list <- reactive({
    sample(e_list(), prop_e_vac() * n_e())
  })
  n_y_vac <- reactive({length(y_vac_list())})
  n_e_vac <- reactive({length(e_vac_list())})
  p_vac_df <- reactive({
    data.frame(p = 1:n()) %>% 
      mutate(p_vac = ifelse(p %in% c(y_vac_list(), e_vac_list()), TRUE, FALSE))
  })
  output$p_vac_df <- renderTable({p_vac_df()})
  
  p_list <- reactive({
    reset_p_list(list(), n(), p_type_df(), p_vac_df(), points())
  })
  output$p_list <- renderPrint({p_list()})
  
  pe <- reactive({
    calculate_pe(p_list(), n_y_vac(), n_e_vac())
  })

  graph_df <- reactive({
    run_sim(t_max(), n(), p_list(), vac_cost(), pe(), y_effic(), e_effic(), f_cost_y(), f_cost_e()) %>% 
      mutate(player_type = factor(player_type, levels = c("young", "elderly")))
  })
  
  output$graph_df <- renderTable({graph_df()})
  
  plot1 <- reactive({
    ggplot(graph_df(), aes(x = t, y = points)) +
      geom_line(aes(color = vac, group = as.factor(ID))) +
      facet_grid(~ player_type) +
      scale_color_discrete(name = "Vaccinated?",
                           breaks = c(TRUE, FALSE),
                           labels = c("Yes", "No")) +
      scale_x_continuous(limits = c(0, max(t))) +
      scale_y_continuous(limits = c(0, 4000))
  })
  output$plot1 <- renderPlot({plot1()})
}

# PE function
calculate_pe <- function(p_list, n_y_vac, n_e_vac) {
  return((2.25*y_effic*n_y_vac + 0.4*e_effic*n_e_vac)/length(p_list))
}

# Reset player list function
reset_p_list <- function(p_list, n, p_type_df, p_vac_df, points) {
  for (p in 1:n) {
    p_list[[p]] <- vector("list", 5)
    names(p_list[[p]]) <- c("ID", "player_type", "vac", "infected", "points")
    p_list[[p]]$ID <- p
    p_list[[p]]$player_type <- p_type_df[p, 2]
    p_list[[p]]$vac <- p_vac_df[p, 2]
    p_list[[p]]$infected <- NULL
    p_list[[p]]$points <- points
  }
  return(p_list)
}

# Run simulation
run_sim <- function(t_max, n, p_list, vac_cost, pe, y_effic, e_effic, f_cost_y, f_cost_e) {
  graph_df <- data.frame(t = integer(0),
                         ID = integer(0),
                         player_type = character(0),
                         vac = logical(0),
                         points = integer(0))
  for (p in 1:n) {
    graph_df <- rbind(graph_df, data.frame(t = 0,
                                           ID = p,
                                           player_type = p_list[[p]]$player_type,
                                           vac = p_list[[p]]$vac,
                                           points = p_list[[p]]$points))
  }
  
  for (t in 1:t_max) {
    for (p in 1:n) {
      #subtract points if vaccinated
      p_list[[p]]$points <- ifelse(p_list[[p]]$vac, p_list[[p]]$points - vac_cost, p_list[[p]]$points)
      
      # determine of players are infected
      rn <- runif(1) # generate random number
      prob_flu <- ifelse(p_list[[p]]$player_type == "young" & p_list[[p]]$vac, (1-pe)*(1-y_effic),
                         ifelse(p_list[[p]]$player_type == "elderly" & p_list[[p]]$vac, (1-pe)*(1-e_effic),
                                1-pe))
      p_list[[p]]$infected <- ifelse(rn < prob_flu, TRUE, FALSE)
      p_list[[p]]$points <- ifelse(p_list[[p]]$player_type == "young" & p_list[[p]]$infected, p_list[[p]]$points - f_cost_y,
                                   ifelse(p_list[[p]]$player_type == "elderly" & p_list[[p]]$infected, p_list[[p]]$points - f_cost_e,
                                          p_list[[p]]$points))
      graph_df <- rbind(graph_df, data.frame(t = t,
                                             ID = p,
                                             player_type = factor(p_list[[p]]$player_type, levels = c("young", "elderly")),
                                             vac = factor(p_list[[p]]$vac, levels = c(TRUE, FALSE)),
                                             points = p_list[[p]]$points))
    }
  }
  return(graph_df)
}

# Run the app ----
shinyApp(ui = ui, server = server)