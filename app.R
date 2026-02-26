# model Place of Safety capacity required
# present as Shiny webapp
# Feb 2026

# to run locally - when in parent directory
# library(shiny)
# runApp('POScapacity')


library(shiny)
library(bslib) # UI toolkit
library(ggplot2)
library(magrittr)
library(dplyr)

ui <- page_fillable(
  title = "Place of Safety demand",
  
  card(card_header('Place of Safety demand'),
  # Sidebar panel for inputs ----
  layout_sidebar(
    sidebar = sidebar(
    sliderInput(
      inputId = "demand",
      label = "Average monthly demand:",
      min = 1,
      max = 300,
      value = 50
    ),
    sliderInput(
      inputId = "max_spaces",
      label = "Number of places of safety:",
      min = 1,
      max = 30,
      value = 5
    )
  ),
  plotOutput(outputId = "distPlot"))
  ),
  
  layout_columns(
  card(card_header('Effect of changing capacity'),
    plotOutput(outputId = 'capacityPlot')),
  card(card_header('Effect of changing demand'),
    plotOutput(outputId = 'demPlot'))
  )
)


days_exceed_capacity <- function(total_days, daily_average, max_spaces){

  p_exceed = 1 - ppois(max_spaces, daily_average)
  days_exceed = round(total_days * p_exceed)
  return(days_exceed)
}

demandPlot <- function(daily_average, max_spaces){
  # distribution of expected demand following poisson
    spaces = seq(0, max_spaces + 2)
    
    # distribution as days per 30 rather than proportion
    expected_usage = dpois(spaces, lambda = daily_average) * 30
    
    days_excess_y = days_exceed_capacity(365, daily_average, max_spaces)

    plt = ggplot() +
      geom_col(aes(x = spaces, y = expected_usage, fill = (spaces <= max_spaces))) +
      xlab('daily assessments') +
      ylab('number of days each month') +
      labs(title = 'expected daily demand given current monthly average',
            caption = paste0('Days annually when insufficient spaces: ', days_excess_y)) +
      geom_vline(xintercept = max_spaces + .5) +
      theme_minimal() +
      theme(legend.position = 'none')
    return(plt)
}


capacityPlot <- function(daily_average, max_spaces){
  # sensitivity to change in max capacity
  tib = tibble(daily_demand = daily_average,
              capacity = seq(max_spaces - 1, max_spaces + 1)) %>%
              rowwise() %>%
              mutate(annual_exceed = days_exceed_capacity(365, daily_demand, capacity))
  
  plt = ggplot(tib) +
    geom_col(aes(x=capacity, y = annual_exceed),
      fill = hcl(h = 15, l = 65, c = 100)) + # matches default first color
    theme_minimal() +
    ylab('Days per annum capacity exceeded') +
    xlab('Maximum capacity available')
  ylim_original = layer_scales(plt)$y$get_limits()
  plt2 = plt +
    geom_hline(yintercept = c(4, 12, 26, 52), col = 'blue') +
    scale_y_continuous(sec.axis = sec_axis(~ .,
              breaks = c(4, 12, 26, 52),
              labels = c('quarterly', 'monthly', 'fortnightly', 'weekly')),
              limits = ylim_original)
  return(plt2)
}

demPlot <- function(daily_average, max_spaces){
  # sensitivity to change in demand for assessments
  tib = tibble(daily_demand = seq(daily_average * .9, daily_average * 1.1,
                                      length.out = 5),
              capacity = max_spaces) %>%
              rowwise() %>%
              mutate(annual_exceed = days_exceed_capacity(365, daily_demand, capacity)) %>%
              mutate(percent_demand = 100 * daily_demand / daily_average)
  
  plt = ggplot(tib) +
    geom_col(aes(x=percent_demand, y = annual_exceed),
      fill = hcl(h = 15, l = 65, c = 100)) + # matches default first color
    theme_minimal() +
    ylab('Days per annum capacity exceeded') +
    xlab('Demand (% of current average)')
  ylim_original = layer_scales(plt)$y$get_limits()
  plt2 = plt +
    geom_hline(yintercept = c(4, 12, 26, 52), col = 'blue') +
    scale_y_continuous(sec.axis = sec_axis(~ .,
              breaks = c(4, 12, 26, 52),
              labels = c('quarterly', 'monthly', 'fortnightly','weekly')),
              limits = ylim_original)
  return(plt2)
}


server <- function(input, output) {

  # This expression that generates a plot is wrapped in a call
  # to renderPlot:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$demand) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
  
    demand_monthly = input$demand
    daily_average = demand_monthly / 30
    max_spaces = input$max_spaces
    demandPlot(daily_average, max_spaces)
    })
  output$capacityPlot <- renderPlot({
  
    demand_monthly = input$demand
    daily_average = demand_monthly / 30
    max_spaces = input$max_spaces
    capacityPlot(daily_average, max_spaces)
    })
  output$demPlot <- renderPlot({
  
    demand_monthly = input$demand
    daily_average = demand_monthly / 30
    max_spaces = input$max_spaces
    demPlot(daily_average, max_spaces)
    })
}

shinyApp(ui = ui, server = server)
