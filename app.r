library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

#deployApp()
ui <- fluidPage(
  tabsetPanel(
    tabPanel(title = "Price analysis",
      wellPanel(
        fluidRow(
          headerPanel('Price analysis'),
          column(2, selectInput(inputId = "name",
                         label = "Tour name",
                         c("ToB" = "touch",
                           "BT" = "batwa",
                           "CoC" = "culture",
                           "MBB" = "mama",
                           "MoAT" = "mother",
                           "IoM" = "islands",
                           "TH" = "tom")),
          numericInput(inputId = "price",
                       label = "Price",
                       value = 1
          ),
          numericInput(inputId = "dollarRate",
                       label = "Dollar rate",
                       value = 3650
          ),selectInput(inputId = "allowance",
                        label = "Guides' allowance type",
                        c("Unified" = "unified",
                          "Custom" = "custom")),
          actionButton("display", "Display")),
          column(4,
                 # This outputs the dynamic UI component
                 uiOutput("ui"),
                 uiOutput("ui1"),
                 uiOutput("ui2"),
                 uiOutput("ui3"),
                 uiOutput("ui4")
          ),
          column(5,
                 verbatimTextOutput("recommended"),
                 verbatimTextOutput("profit"),
                 verbatimTextOutput("profit2")
          )
          )),
      fluidRow(
            column(3,
                   tableOutput("tabelaNovic")
            ),
            column(6, offset = 3,
                   plotOutput('plot1')
            )
          ),
      fluidRow(
        column(3,
               tableOutput("tabelaNovic2")
        ), column(6, offset = 3,
                  plotOutput('plot2') )
    
      )), tabPanel(title = "Trend report", 
                   wellPanel(
                     fluidRow(
                       headerPanel('Yearly trend report'),
                       column(2, selectInput(inputId = "nameTrendReport",
                                             label = "Tour name",
                                             c("ToB" = "touch",
                                               "BT" = "batwa",
                                               "CoC" = "culture",
                                               "MBB" = "mama",
                                               "MoAT" = "mother",
                                               "IoM" = "islands",
                                               "TH" = "tom")),
                              actionButton("display2", "Display")
                       )
                     )
                    ),
                   fluidRow(
                     column(3,
                            tableOutput("tabelaLetniTrend")
                     ),
                     column(6,
                            plotOutput('plot3')
                     )
                   )
                   
                   ), tabPanel(title = "Booking categories", 
                               wellPanel(
                                 fluidRow(
                                   headerPanel('Booking categories per year'),
                                   column(2, selectInput(inputId = "booking",
                                                         label = "Choose",
                                                         c("Clients" = "clients_per_year",
                                                           "Bookings" = "bookings_per_year")),
                                                         actionButton("display3", "Display")
                                   )
                                   )
                                 ),
                                 fluidRow(
                                   column(3,
                                          tableOutput("booking")
                                   ),
                                   column(6,
                                          plotOutput('plot4')
                                   )
                                 )
                                 
                               )
    
    
    
  ) 
)


server <- function(input, output) {
  
  output$ui <- renderUI({
    if (input$allowance == 'unified') {
      numericInput(inputId = "enotni",
                   label = "Guides' allowance",
                   value = 1, width = "20%"
      )
    } else {
      numericInput(inputId = "prvi",
                   label = "1",
                   value = 1, width = "20%"
      )
    }
  })
  
  output$ui1 <- renderUI({
    if (input$allowance == 'unified') {
      return()
    } else {
      numericInput(inputId = "drugi",
                   label = "2",
                   value = 1, width = "20%"
      )
    }
  })
  
  output$ui2 <- renderUI({
    if (input$allowance == 'unified') {
      return()
    } else {
      numericInput(inputId = "tretji",
                   label = "3",
                   value = 1, width = "20%"
      )
    }
  })
  
  output$ui3 <- renderUI({
    if (input$allowance == 'unified') {
      return()
    } else {
      numericInput(inputId = "cetrti",
                   label = "4",
                   value = 1, width = "20%"
      )
    }
  })
  
  output$ui4 <- renderUI({
    if (input$allowance == 'unified') {
      return()
    } else {
      numericInput(inputId = "peti",
                   label = "5+",
                   value = 1, width = "20%"
      )
    }
  })
  
  novice <- eventReactive(input$display, {
    single_price <- input$price
    dollar_rate <- input$dollarRate
    #guides_commision <- input$guidesCommission
    tura <- input$name
    novice <- read.csv(paste(input$name, "_raw.csv", sep='')) #potem tu preberem csv z input$name
    if (input$allowance == 'unified') {
      novice$guides_cost <- input$enotni * novice$guides
    } else {
      novice$guides_cost <- input$peti * novice$guides
      novice$guides_cost[1] <- input$prvi
      novice$guides_cost[2] <- input$drugi
      novice$guides_cost[3] <- input$tretji
      novice$guides_cost[4] <- input$cetrti
    }
    if (tura == 'mama'  || tura == 'islands' || tura == 'mother') {
      novice$cost_pp <- round((novice$guides_cost + novice$dorm +
                               novice$costs_ugx / dollar_rate) / novice$participants, 2)
    } else {
      novice$cost_pp <- round((novice$guides_cost + 
                               novice$costs_ugx / dollar_rate) / novice$participants, 2)
    }
    
    novice$profit <- (novice$price_pp_USD - novice$cost_pp) * novice$participants * novice$nr_tours
    novice$profit_single_price <- (single_price - novice$cost_pp) * novice$participants * novice$nr_tours
    novice <- novice[,c('participants', 'nr_tours', 'price_pp_USD', 'cost_pp', 'profit', 'profit_single_price' )]
    novice
  })
  
  stroski <- eventReactive(input$display, {
    tura <- input$name
    single_price <- input$price
    #guides_commision <- input$guidesCommission
    dollar_rate <- input$dollarRate
    stroski <- read.csv(paste(tura,'_costs.csv', sep=''), sep=';', header=F)
    stroski <- t(stroski)
    rownames(stroski) <- c()
    colnames(stroski) = stroski[1, ]
    stroski = stroski[-1, ]
    stroski <- data.frame(stroski)
    stroski$guides <- as.numeric(as.character(stroski$guides))
    
    if (input$allowance == 'unified') {
      stroski$guides_cost <- input$enotni * stroski$guides
    } else {
      stroski$guides_cost <- input$peti * stroski$guides
      stroski$guides_cost[1] <- input$prvi
      stroski$guides_cost[2] <- input$drugi
      stroski$guides_cost[3] <- input$tretji
      stroski$guides_cost[4] <- input$cetrti
    }
    
    
    if (tura == 'mama'  || tura == 'islands' || tura == 'mother') {
      stroski <- stroski %>% transmute(participants = parse_number(participants),
                                       guides_cost = parse_number(guides_cost),
                                       dorm = parse_number(dorm),
                                       costs_ugx = parse_number(costs_ugx))
      stroski$cost_pp <- round((stroski$guides_cost + stroski$dorm +
                                  stroski$costs_ugx / dollar_rate) / stroski$participants, 2)
    } else {
      stroski <- stroski %>% transmute(participants = parse_number(participants),
                                       guides_cost = parse_number(guides_cost),
                                       costs_ugx = parse_number(costs_ugx))
      stroski$cost_pp <- round((stroski$guides_cost + 
                                  stroski$costs_ugx / dollar_rate) / stroski$participants, 2)
    }
    
    
    stroski <- stroski %>% transmute(participants = parse_number(participants), 
                                     guides_cost = parse_number(guides_cost),
                                     cost_pp = parse_number(cost_pp)) %>%
      mutate(profit = (single_price - cost_pp) * participants, VAT = profit * 0.82, comm = profit * 0.9,
             VAT_comm = profit * 0.82 * 0.9)
    stroski
  })
  
  leta <- eventReactive(input$display2, {
    tura <- input$nameTrendReport
    leta <- read.csv(paste(tura, "_leta.csv", sep='')) 
    leta
  })
  
  book <- eventReactive(input$display3, {
    tura <- input$booking
    book <- read.csv(paste(tura, ".csv", sep=''))
    book
  })
  
  text <- eventReactive(input$display, {
    novice <- novice()
    recommended_price <- round(sum(novice$participants*novice$nr_tours*novice$price_pp_USD) / sum(novice$participants*novice$nr_tours),2)
    paste("RECOMMENDED PRICE PER PERSON:", round(recommended_price, 2), '$')
  })
  
  text2 <- eventReactive(input$display, {
    novice <- novice()
    skupni_profit <- sum(novice$profit)
    paste("ACTUAL HISTORICAL PROFIT:", round(skupni_profit, 0), '$')
  })
  
  text3 <- eventReactive(input$display, {
    novice <- novice()
    skupni_profit_enotna <- sum(novice$profit_single_price)
    paste("HYPOTHETICAL HISTORICAL PROFIT WITH A SINGLE PRICE:", round(skupni_profit_enotna, 0), '$')
  })
  
  
  plot1 <- eventReactive(input$display, {
    novice <- novice()
    p <- ggplot(novice,aes(x = participants)) + geom_line(aes(y = profit, colour = "profit normal pricing"),size=1.2)
    p <- p + geom_line(aes(y = profit_single_price, colour = "profit single price"), size=1.2)
    p
  })
  
  plot2 <- eventReactive(input$display, {
    stroski <- stroski()
    p <- ggplot(stroski,aes(x = participants)) + geom_line(aes(y = cost_pp * participants, colour = "total costs per trek"),size=1.2)
    p <- p + geom_line(aes(y = guides_cost, colour = "guides' allowance"), size=1.2)
    p
  })
  
  output$tabelaNovic <- renderTable({
    novice()
  }, caption="HISTORY", caption.placement = "top")
  
  output$recommended  <- renderText({
    text()
  })
  
  output$profit  <- renderText({
    text2()
  })
  
  output$profit2  <- renderText({
    text3()
  })
  
  
  output$plot1  <- renderPlot({
    plot1()
  })
  
  output$plot2  <- renderPlot({
    plot2()
  })
  
  output$tabelaNovic2 <- renderTable({
    stroski()
  }, caption="FUTURE",
  caption.placement = "top")
  
  output$tabelaLetniTrend <- renderTable({
    leta()
  }, caption="REPORT",
  caption.placement = "top")
  
  output$plot3  <- renderPlot({
    barplot(leta()$nr_participants, names.arg = leta()$year, col='blue')
  })
  
  output$booking <- renderTable({
    book()
  }, caption="REPORT",
  caption.placement = "top")
  
  output$plot4  <- renderPlot({
    tmp <- as.matrix(sapply(book(), as.numeric))
    barplot(height = t(tmp[1:8,2:4]), names.arg = book()$year, beside=TRUE)
  })
  
  
  
  
  
}


shinyApp(ui = ui, server = server)

