library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(gganimate)
library(openxlsx)
library(shinydashboard)
library(plotly)
library(DT)
library(tidyr)
library(sqldf)



# Pre-load and process the data
capacity_demand <- read_excel("merged_data.xlsx")

# Create a table of 'demand_fulfilled' counts
demand_satisfied <-  capacity_demand %>% 
                    filter(forecast!=0)  
demand_data <- table(demand_satisfied$demand_fulfilled)

  
# Convert the table to a data frame for ggplot2
demand_data_df <- as.data.frame(demand_data)
names(demand_data_df) <- c("index", "values")

#Variables for Value box
val1=nrow(capacity_demand %>% filter(demand_fulfilled %in% c("Yes","No","Not Available")))
val2=nrow(capacity_demand %>% filter(demand_fulfilled == "Unused capacity"))
val3=nrow(capacity_demand %>% filter(demand_fulfilled =="Not Available"))

threshold <- 500

capacity_shortfall <- subset(capacity_demand, avail < 0)

capacity_shortfall <- capacity_shortfall %>% 
  mutate(new_capacity = max_capacity - avail)

capacity_surplus   <- subset(capacity_demand, avail > 0)
capacity_surplus$opt_capacity <- ifelse(capacity_surplus$forecast < capacity_surplus$standard_capacity, 
                                        capacity_surplus$standard_capacity, 
                                        capacity_surplus$max_capacity)

capacity_surplus$new_opt_capacity <- ifelse(capacity_surplus$avail < threshold, 
                                        capacity_surplus$opt_capacity, 
                                        (capacity_surplus$forecast+threshold))

# Group by geonode_id and sku_id, then calculate sum of avail
capacity_risks <- capacity_shortfall%>%
  group_by(geonode_id, sku_id, month_year) %>%
  summarise(capacityShortfall = sum(avail)*(-1))

capacity_risks_e <- capacity_surplus%>%
  group_by(geonode_id, sku_id, month_year) %>%
  summarise(capacitySurplus = sum(avail))


# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard"), 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Supplier Capacity Analysis",
               tabName = "t1",
               icon = icon("chart-line")),
      menuItem("Capacity Shortfall Analysis",
               tabName = "t2",
               icon = icon("arrow-down")),
      menuItem("Capacity Surplus Analysis",
               tabName = "t3",
               icon = icon("arrow-up"))
    )
  ),
  dashboardBody(
    tabItems(
      # Tab 1:
      tabItem(
        tabName = "t1",
        fluidRow(
 
          box(
            width = 8,
            valueBoxOutput("valueBox1"),
            valueBoxOutput("valueBox2"),
            valueBoxOutput("valueBox3")
          )
        ),
        fluidRow(
          box(
            width = 8,
            title = "Forecasted Demand Fulfillment Analysis",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("plot_graph")
          )
        )
      ),
      
      # Tab 2
      tabItem(
        tabName = "t2",
        navbarPage(
          "",
          tabPanel("Capacity Risk Summary(Shortfall)",
                   fluidPage(
                     fluidRow(
                       h4(
                         box(
                           width = 12,
                           title = "Capacity Risk Summary(Shortfall)",
                           status = "primary",
                           solidHeader = TRUE,
       
                             mainPanel(
                               mainPanel(dataTableOutput("report"))
                             )
                         )
                       )
                     ),
               
                   )
          ),
          
          tabPanel("Capacity(Shortfall) Vs Demand",
                   fluidPage(
                     fluidRow(
                       h4(
                         box(
                           width = 12,
                           title = "Capacity(Shortfall) Vs Demand",
                           status = "primary",
                           solidHeader = TRUE,
                           
                           sidebarLayout(
                             sidebarPanel(
                               width = 3,
                               selectInput("geonode_id",
                                           "Geonode ID",
                                           choices = unique(capacity_risks$geonode_id),
                                           selected = ""),
                               selectInput("sku_id",
                                           "SKU ID",
                                           choices = "",
                                           selected = ""),
                               textOutput("text"),
                               verbatimTextOutput("verb"),
                               textOutput("text1"),
                               verbatimTextOutput("verb1")
                             ),
                               mainPanel(plotOutput("LinePlot"))
                           )
                         )
                       )
                     ),
                     
              fluidRow(
                h4(
                  box(
                    width = 12,
                    title = "Shortfall Resolution(Increase the Production Capacity)",
                    status = "primary",
                    solidHeader = TRUE,
                        mainPanel(plotOutput("scatPlot"))
                    )
                )
              ),

             )
          )
        )
      ),

      # Tab 3
      tabItem(
        tabName = "t3",
        navbarPage(
          "",
          tabPanel("Capacity Risk Summary(Surplus)",
                   fluidPage(
                     fluidRow(
                       h4(
                         box(
                           width = 12,
                           title = "Capacity Risk Summary(Surplus)",
                           status = "primary",
                           solidHeader = TRUE,
                           
                           mainPanel(
                             mainPanel(dataTableOutput("report_e"))
                           )
                         )
                       )
                     ),
                     
                   )
          ),
          
          tabPanel("Capacity(Surplus) Vs Demand",
                   fluidPage(
                     fluidRow(
                       h4(
                         box(
                           width = 12,
                           title = "Capacity(Surplus) Vs Demand",
                           status = "primary",
                           solidHeader = TRUE,
                           
                           sidebarLayout(
                             sidebarPanel(
                               width = 3,
                               selectInput("geonode_id_e",
                                           "Geonode ID",
                                           choices = unique(capacity_risks_e$geonode_id),
                                           selected = ""),
                               selectInput("sku_id_e",
                                           "SKU ID",
                                           choices = "",
                                           selected = ""),
                               textOutput("text_e"),
                               verbatimTextOutput("verb_e"),
                               textOutput("text1_e"),
                               verbatimTextOutput("verb1_e")
                               
                             ),
                             mainPanel(plotOutput("LinePlot_e"))
                           )
                         )
                       )
                     ),
                     fluidRow(
                       h4(
                         box(
                           width = 12,
                           title = " Surplus Resolution(Reduce the Production Capacity)",
                           status = "primary",
                           solidHeader = TRUE,
                           mainPanel(plotOutput("scatPlot_e"))
                         )
                       )
                     ),
                     
                   )
          )
        )
      )
    )
  )
)


# Define server logic
server <- function(input, output,session) {

  observeEvent(
    input$geonode_id,
    updateSelectInput(
      session,
      "sku_id",
      "SKU ID",
      choices = unique(capacity_risks$sku_id[capacity_risks$geonode_id==input$geonode_id])))
  
  
  observeEvent(
    input$geonode_id_e,
    updateSelectInput(
      session,
      "sku_id_e",
      "SKU ID",
      choices = unique(capacity_risks_e$sku_id[capacity_risks_e$geonode_id==input$geonode_id_e])))
  

  output$valueBox1 <- renderValueBox({
    valueBox(
      val1, 
      subtitle = "Total Forecasted Demands", 
      icon = icon("calendar"), 
      color = "purple"
    )
  })
  
  output$valueBox2 <- renderValueBox({
    valueBox(
      val2, 
      subtitle = "Unused Capacity", 
      icon = icon("battery-empty"), 
      color = "green"
    )
  })
  
  output$valueBox3 <- renderValueBox({
    valueBox(
      val3,
      subtitle = "Unmet Capacity", 
      icon = icon("ban"), 
      color = "orange"
    )
  })
  
  output$plot_graph <- renderPlot({
    p <- ggplot(demand_data_df, aes(x = index, y = values,fill = factor(index))) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = values), vjust = -0.5, hjust = 0.5) +
      labs(x = 'Demand Fulfilled', y = '# of Forecast Demands', title = 'Forecasted Demand Fulfillment Analysis',fill = "Status") +
      theme_minimal()
    
    p
  })

  
  output$report <- renderDataTable({
      report <- capacity_risks
    
    DT::datatable(report, options = list(pageLength = 10))
                                           
  })
  

output$LinePlot <- renderPlot({

  graphd <-  capacity_shortfall %>%
             filter(geonode_id == input$geonode_id) %>%
             filter(sku_id == input$sku_id)

p1 <-  ggplot(data = graphd) +
    geom_line(aes(x = start_date, y = max_capacity,color = "Production Capacity")) +
    geom_line(aes(x = start_date, y = forecast, color = "Forecasted Demand"))+
    labs(title = "Production Capacity Vs Forecasted Demand",
        x = "Date",
        y ="Production Capacity/Forecasted Demand")

p1

})


output$scatPlot <- renderPlot({
    graphd <-capacity_shortfall %>%
    filter(geonode_id == input$geonode_id) %>%
    filter(sku_id == input$sku_id)
    
  
    data_long <- pivot_longer(graphd, cols = c(new_capacity, forecast), names_to = "type", values_to = "value")
    
    # Create the side-by-side bar chart
    ggplot(data_long, aes(x = month_year, y = value, fill = type)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
      labs(x = "Date", y = "Production Capacity/Forecasted Demand", fill = "Type", title = "Capacity vs Demand by Category") +
      theme_minimal()
})


output$report_e <- renderDataTable({
  report <- capacity_risks_e
  DT::datatable(report, options = list(pageLength = 10))
})


output$LinePlot_e <- renderPlot({
  graphd <-  capacity_surplus  %>%
    filter(geonode_id == input$geonode_id_e) %>%
    filter(sku_id == input$sku_id_e)

  
  p1 <-  ggplot(data = graphd) +
    geom_line(aes(x = start_date, y = opt_capacity,color = "Production Capacity")) +
    geom_line(aes(x = start_date, y = forecast, color = "Forecasted Demand"))+
    labs(title = "Production Capacity Vs Forecasted Demand",
         x = "Date",
         y ="Production Capacity/Forecasted Demand")
  
  p1
  
})


output$scatPlot_e <- renderPlot({
  graphd <-  capacity_surplus  %>%
    filter(geonode_id == input$geonode_id_e) %>%
    filter(sku_id == input$sku_id_e)

  p1 <-  ggplot(data = graphd) +
    geom_line(aes(x = start_date, y = new_opt_capacity,color = "Production Capacity")) +
    geom_line(aes(x = start_date, y = forecast, color = "Forecasted Demand"))+
    labs(title = "Production Capacity Vs Forecasted Demand",
         x = "Date",
         y ="Production Capacity/Forecasted Demand")
  
  p1
})

output$text <- renderText({ "Supplier(Location)" })
output$verb <- renderText({
                        sql_query <- sprintf("SELECT location FROM capacity_demand WHERE geonode_id = '%s' LIMIT 1",
                                             input$geonode_id)


                        Supplier <- sqldf(sql_query)
                        Supplier <- as.character(Supplier$location[1])
                        })

output$text1 <- renderText({ "Orgnization" })
output$verb1 <- renderText({
                        sql_query <- sprintf("SELECT org_id FROM capacity_demand WHERE geonode_id = '%s' LIMIT 1",
                        input$geonode_id)
  
  
                        Org <- sqldf(sql_query)
                        Org <- as.character(Org$org_id[1])
                      })


output$text_e <- renderText({ "Supplier(Location)" })
output$verb_e <- renderText({
                        sql_query <- sprintf("SELECT location FROM capacity_demand WHERE geonode_id = '%s' LIMIT 1",
                       input$geonode_id_e)
  
  
                    Supplier <- sqldf(sql_query)
                    Supplier <- as.character(Supplier$location[1])

                      })
output$text1_e <- renderText({ "Orgnization" })
output$verb1_e <- renderText({
                              sql_query <- sprintf("SELECT org_id FROM capacity_demand WHERE geonode_id = '%s' LIMIT 1",
                              input$geonode_id_e)
  
  
                               Org <- sqldf(sql_query)
                               Org <- as.character(Org$org_id[1])
                            })

#   output$LinePlot <- renderImage({
#     # Data preparation
#     graphd <- standard_capacity_risks %>%
#       filter(geonode_id == input$geonode_id) %>%
#       filter(sku_id == input$sku_id)
# 
#     # Create the animated plot
#     p <- ggplot(graphd, aes(x = start_date, y = forecast, group = sku_id)) +
#       geom_line(aes(color = "Forecasted Demand")) +
#       geom_line(aes(x = start_date, y = max_capacity, color = "Max Capacity")) +
#       labs(title = "Date: {frame_along}")
# 
# 
#     # Animate the plot
#     anim <- p + transition_reveal(start_date)
# 
# 
#     # Save the animation
#     outfile <- tempfile(fileext = ".gif")
#     anim_save(filename = outfile, animation = anim, fps = 20, width = 1000, height = 800)
# 
#     # Return the animation file path
#     list(src = outfile, contentType = "image/gif")
#   }, deleteFile = TRUE)

}

# Run the application
shinyApp(ui = ui, server = server)




