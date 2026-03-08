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


assessments_exceed_capacity <- function(total_days, daily_average, capacity){
  # total assessments beyond available capacity
  assessments_unmet_total = 0
  extra_demand = 1
  while (extra_demand > 0){
    demand = capacity + extra_demand
    p_this_demand = dpois(demand, daily_average)
    days_this_demand = p_this_demand * total_days
    assessments_unmet = round(days_this_demand * extra_demand)
    assessments_unmet_total = assessments_unmet_total + assessments_unmet
    extra_demand = extra_demand + 1
    if (days_this_demand < .1) {extra_demand = 0} # to end loop
  }
  return(assessments_unmet_total)
}

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
    assessments_excess_y = assessments_exceed_capacity(365, daily_average, max_spaces)

    plt = ggplot() +
      geom_col(aes(x = spaces, y = expected_usage, fill = (spaces <= max_spaces))) +
      xlab('daily assessments') +
      ylab('number of days each month') +
      labs(title = 'expected daily demand given current monthly average',
            caption = paste0(days_excess_y,
            ' days annually when insufficient spaces',
                     '; ', assessments_excess_y,
                     ' assessments missed')) +
      geom_vline(xintercept = max_spaces + .5) +
      annotate(x = max_spaces + .5, y = +Inf, label = 'current capacity', vjust = 2, geom = 'label') +
      theme_minimal() +
      theme(legend.position = 'none')
    return(plt)
}

addFrequencyLines <- function(plt) {
  ylim_original = layer_scales(plt)$y$get_limits()
  b = c(4, 12, 26, 52)
  b_subset = b[b < max(ylim_original)]
  plt2 = plt +
    geom_hline(yintercept = b_subset, col = 'blue') +
    scale_y_continuous(sec.axis = sec_axis(~ .,
              breaks = b,
              labels = c('quarterly', 'monthly', 'fortnightly', 'weekly')),
              limits = ylim_original)
  return(plt2)
}

capacityPlot <- function(daily_average, max_spaces, to_plot){
  # sensitivity to change in max capacity
  tib = tibble(daily_demand = daily_average,
              capacity = to_plot) %>%
              rowwise() %>%
              mutate(annual_exceed = days_exceed_capacity(365, daily_demand, capacity))
  
  plt = ggplot(tib) +
    geom_col(aes(x=capacity, y = annual_exceed),
      fill = hcl(h = 15, l = 65, c = 100)) + # matches default first color
    theme_minimal() +
    ylab('Days per annum capacity exceeded') +
    xlab('Maximum capacity available') +
    labs(title = 'Frequency capacity exceeded')
  plt2 = addFrequencyLines(plt) +
        geom_vline(xintercept = max_spaces) +
      annotate(x = max_spaces, y = +Inf, label = 'current capacity', vjust = 2, geom = 'label')

  return(plt2)
}

capacityPlotAssessments <- function(daily_average, max_spaces, to_plot){
  # sensitivity to change in max capacity - n assessments
  tib = tibble(daily_demand = daily_average,
              capacity = to_plot) %>%
              rowwise() %>%
              mutate(annual_exceed = assessments_exceed_capacity(365, daily_demand, capacity))
  
  plt = ggplot(tib) +
    geom_col(aes(x=capacity, y = annual_exceed),
      fill = hcl(h = 15, l = 65, c = 100)) + # matches default first color
    theme_minimal() +
    ylab('Assessments per annum beyond capacity') +
    xlab('Maximum capacity available') +
    labs(title = 'Assessments missed') +
    geom_vline(xintercept = max_spaces) +
    annotate(x = max_spaces, y = +Inf, label = 'current capacity', vjust = 2, geom = 'label')
  return(plt)
}

demPlot <- function(daily_average, max_spaces, percent_demand){
  # sensitivity to change in demand for assessments
  
  tib = tibble(percent_demand = percent_demand,
              capacity = max_spaces) %>%
              rowwise() %>%
              mutate(daily_demand = percent_demand / 100 * daily_average) %>%
              mutate(annual_exceed = days_exceed_capacity(365, daily_demand, capacity))
  
  plt = ggplot(tib) +
    geom_col(aes(x=percent_demand, y = annual_exceed),
      fill = hcl(h = 15, l = 65, c = 100)) + # matches default first color
    theme_minimal() +
    ylab('Days per annum capacity exceeded') +
    xlab('Demand (% of current average)') +
    labs(title = 'Frequency capacity exceeded') +
    scale_x_continuous(breaks = percent_demand)

  plt2 = addFrequencyLines(plt) +
      geom_vline(xintercept = 100) +
      annotate(x = 100, y = +Inf, label = 'current demand', vjust = 2, geom = 'label')
  return(plt2)
}

demPlotAssessments <- function(daily_average, max_spaces, percent_demand){
  # sensitivity to change in demand for assessments
  tib = tibble(percent_demand,
              capacity = max_spaces) %>%
              rowwise() %>%
              mutate(daily_demand = percent_demand / 100 * daily_average) %>%
              mutate(annual_exceed = assessments_exceed_capacity(365, daily_demand, capacity))
  
  plt = ggplot(tib) +
    geom_col(aes(x=percent_demand, y = annual_exceed),
      fill = hcl(h = 15, l = 65, c = 100)) + # matches default first color
    theme_minimal() +
    ylab('Assessments per annum beyond available capacity') +
    xlab('Demand (% of current average)')  +
    labs(title = 'Assessments missed') +
    geom_vline(xintercept = 100) +
    annotate(x = 100, y = +Inf, label = 'current demand', vjust = 2, geom = 'label') +
    scale_x_continuous(breaks = percent_demand)

  return(plt)
}

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
      max = 200,
      value = 30
    ),
    sliderInput(
      inputId = "max_spaces",
      label = "Number of places of safety:",
      min = 1,
      max = 20,
      value = 2
    )
  ),
  plotOutput(outputId = "distPlot"))
  ),
  
  layout_columns(
  card(card_header('Effect of changing capacity'),
    plotOutput(outputId = 'capacityPlotAssmt'),
    plotOutput(outputId = 'capacityPlot')
    ),
  card(card_header('Effect of changing demand'),
    plotOutput(outputId = 'demPlotAssmt'),
    plotOutput(outputId = 'demPlot')
    )
  ),
  p("The code to generate this page is available at https://github.com/psychoxmodels/POScapacity",
  style = "font-size:8px; font-style:italic")
)

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
    capacities_to_plot = seq(max_spaces - 1, max_spaces + 1)
    capacityPlot(daily_average, max_spaces, capacities_to_plot)
    })
    
 output$capacityPlotAssmt <- renderPlot({
    demand_monthly = input$demand
    daily_average = demand_monthly / 30
    max_spaces = input$max_spaces
    capacities_to_plot = seq(max_spaces - 1, max_spaces + 1)
    capacityPlotAssessments(daily_average, max_spaces, capacities_to_plot)
    })
    
  output$demPlot <- renderPlot({
    demand_monthly = input$demand
    daily_average = demand_monthly / 30
    max_spaces = input$max_spaces
    demands_to_plot = seq(50, 150, length.out = 5)
    demPlot(daily_average, max_spaces, demands_to_plot)
    })
    
  output$demPlotAssmt <- renderPlot({
    demand_monthly = input$demand
    daily_average = demand_monthly / 30
    max_spaces = input$max_spaces
    demands_to_plot = seq(50, 150, length.out = 5)
    demPlotAssessments(daily_average, max_spaces, demands_to_plot)
    })
}

shinyApp(ui = ui, server = server)
