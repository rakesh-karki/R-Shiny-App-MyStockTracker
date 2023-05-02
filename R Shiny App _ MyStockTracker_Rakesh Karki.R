### YOU WILL NEED TO CREATE AND PLUG YOUR OWN API CODE AT LINE 225 FOR THE THIS APP TO WORK PROPERLY ###

#### My Stock Tracker : Rakesh Karki"#####
################################################################################

# Create the list of required packages
packages = c("shiny", "shinyWidgets","tidyquant", "StanHeaders","prophet", 
             "shinycssloaders","quantmod", "forecast","tseries","timeSeries",
             "dplyr","fGarch","DT", "ggplot2", "tidyverse","syuzhet","tidyr", 
             "stringr", "data.table","httr","jsonlite", "plotly","tidytext")

# Load or install / load the required packages
package.check <- lapply(
  packages,
  FUN = function(x) {
         if (!require(x, character.only = TRUE)) {
            install.packages(x, dependencies = TRUE)
            library(x, character.only = TRUE)
    }
  }
)

Sys.setlocale('LC_ALL','C')   # updates the locale 

################# UI ###########################################################

ui <- fluidPage(
  setBackgroundColor("ghostwhite"),
    tags$style(
        ".first-p { color: blue; font-size: 20px; height:20px}"),
    tags$style(HTML("
    .tabbable > .nav > li > a                  {background-color: lightgrey;  color:black}
    .tabbable > .nav > li[class=active]    > a {background-color: green; color:white}
  "
  )),
    p(class = "first-p", (strong("My Stock Tracker - (C) Rakesh Karki"))), 
    titlePanel(NULL),
        tabsetPanel(
        tabPanel(strong("Stock Historical Performance"), fluid = TRUE,
            sidebarLayout(
              sidebarPanel(
              selectInput("selectedbenchmark", "Select Stock Benchmark", 
                          choices = list("DOW", "DOWGLOBAL", "SP400", "SP500", "SP600"), 
                          selected = "SP500"),
              
              selectizeInput( "locInput", "Select Stock(s) to Plot (Max 4)", 
                            choices = NULL,selected = NULL, multiple = TRUE, 
                            options = list(maxItems = 4)),
              dateRangeInput("dates", "Select Date range", 
                          start = (today() - years(1))), width = 3,
              selectInput("selectedPlot", "Select Plot Type", 
                          choices = list("Candlestick Chart", "Candlestick Chart - With BB", 
                                         "Line Chart", "Bar Chart"), 
                          selected = "Candlestick Chart")
            ),
            mainPanel(
              withSpinner(plotOutput("distPlot1")), width = 9, height= "900px"
            ))
        ),
        
        tabPanel(strong("Stock Price Forecasting"), fluid= TRUE,
          sidebarLayout(
            sidebarPanel(
              selectInput("selectedstockforecast", "Select A Stock for Forecast", 
                        choices = "AAPL", 
                        selected = "AAPL"),
              sliderInput("Daysforecast", "Days of forecast",
                          min = 1, max = 30, value = 15, step = 5, 
                          post = " day(s)"), width = 3,
              selectInput("selected_model", "Select Forecasting Model", 
                        choices= list("ARIMA","Prophet","Neural Network"), 
                        selected = "ARIMA")
        ),
        mainPanel(
        withSpinner(
          plotOutput("distPlot2")), width = 9, height= "900px",
          dataTableOutput("predicted_stockprice")
        
        ))),
        tabPanel(
          strong("Stock News Sentiment Analysis"), fluid= TRUE,
            sidebarLayout(
              sidebarPanel(
                selectInput("Sentimentstock", "Select a Stock for Setiment Analysis", 
                          choices = "AAPL", 
                          selected = "AAPL"),
                
                sliderInput("newsdays", "Days of News?",
                          min = 1, max = 28, value = 10, step = 2, 
                          post = " day(s)"), width = 3
                   ),
              
          mainPanel(
                withSpinner(plotOutput("distPlot3")), width = 9, height= "900px",
                dataTableOutput("Sentimentoutput")
          )))
        ))

################# Server #######################################################

server <- function(input, output, session) {
  observe({
    myindex<- tq_index(input$selectedbenchmark)$symbol
    
    updateSelectizeInput(session, "locInput",
                      choices = myindex, 
                      selected = list(myindex[1], myindex[2]))
    
    updateSelectInput(session, "selectedstockforecast",
                      choices = myindex, 
                      selected = myindex[1])
    
    updateSelectInput(session, "Sentimentstock",
                      choices = myindex, 
                      selected = myindex[1])
  })
  
################ Reactive1 #####################################################

  vals <- reactiveValues()
    observe({
      if(is.null(input$locInput)) {
        return()
      }  
        vals$W <- tq_get(input$locInput,
                         from = input$dates[1],
                         to = input$dates[2],
                         get = "stock.prices")
        
        vals$P <- tq_get(input$selectedstockforecast,
                         from = (today() - years(2)),
                         get = "stock.prices")
        
        vals$Q<- data.frame(ds = vals$P$date, y = as.numeric(vals$P$close))
        
        vals$prophetpred <- prophet(vals$Q, daily.seasonality = F)
        
        vals$future <- make_future_dataframe(vals$prophetpred, periods = input$Daysforecast)
        
        vals$forecastprophet <- predict(vals$prophetpred, vals$future)
    })
    
################# Plot1 ######################################################## 
    
    output$distPlot1 <- renderPlot({
      shiny::validate(need(input$locInput, 'PLEASE SELECT STOCK/S TO PLOT'))
      
      if (input$selectedPlot =="Candlestick Chart") {
            vals$W %>%
              ggplot(aes(date, close, group = symbol)) +
              geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
              geom_ma(ma_fun = SMA, n = 10, color = "darkblue", size = 1) +
              labs(title = "Candlestick Chart with 10 days moving average", 
                  y = "Closing Price(USD)", x = "") + 
                  facet_wrap(~ symbol, ncol = 2, scale = "free_y") + 
                  theme_tq()
      }
      
        else if (input$selectedPlot =="Candlestick Chart - With BB") {
            vals$W %>%
                ggplot(aes(x = date, y = close, open = open, high = high, low = low, 
                           close = close, group = symbol)) + geom_barchart() +
                geom_bbands(ma_fun = SMA, sd = 2, n = 10, linetype = 5) +
                labs(title = "Candlestick Chart with BB", y = "Closing Price(USD)", x = "") + 
                facet_wrap(~ symbol, ncol = 2, scale = "free_y") + 
                theme_tq()
        }
      
        else if (input$selectedPlot =="Line Chart") {
            vals$W %>% ggplot(aes(date, close, color=symbol)) +
                geom_line()+
                labs(title = "Line Chart", y = "Closing Price(USD)", x = "") + 
                facet_wrap(~ symbol, ncol = 2, scale = "free_y") +
                theme_tq()
        }
      
        else if (input$selectedPlot =="Bar Chart") {
            vals$W %>% ggplot(aes(date, close)) +
                geom_barchart(aes(open = open, high = high, low = low, close = close))+
                labs(title = "Bar Chart", y = "Closing Price(USD)", x = "") + 
                facet_wrap(~ symbol, ncol = 2, scale = "free_y") +
                theme_tq()
        }
    })
    
################# Plot2 ########################################################
    
    output$distPlot2 <- renderPlot({
      shiny::validate(need(input$selectedstockforecast != '', 'PLEASE SELECT A STOCK TO PLOT'))
      
      if (input$selected_model =="Prophet"){
        plot(vals$prophetpred, vals$forecastprophet, xlabel = "Date", ylabel = "Closing Price(USD)") + 
          ggtitle(paste0(input$selectedstockforecast,": Stock Price Prediction"))
      }
      
      else if (input$selected_model =="ARIMA"){
        plot(forecast(auto.arima(vals$P$close, lambda = "auto"), h = input$Daysforecast))
      }
      
      else if (input$selected_model =="Neural Network"){
        plot(forecast(nnetar(vals$P$close, size = length(vals$P$close)/(1.5^(-10)*
                                                (length(vals$P$close)+30)), 
        lambda = BoxCox.lambda(vals$P$close)), h= input$Daysforecast, PI = TRUE))
      }
        })
    
#### Predicted stock Price Table ###############################################
    
    output$predicted_stockprice <- renderDataTable({
      if (input$selected_model =="ARIMA"){data.frame(forecast(auto.arima(vals$P$close, 
                                              lambda = "auto"), h=input$Daysforecast))}
      
      else if (input$selected_model =="Prophet"){data.frame(vals$forecastprophet)[
        (1:input$Daysforecast), c(1,2,12,13,14,15,16)]}
      
      else if (input$selected_model =="Neural Network"){data.frame(
      forecast(nnetar(vals$P$close, size= length(vals$P$close)/(1.5^(-10)*(length(vals$P$close)+30)), 
                        lambda = BoxCox.lambda(vals$P$close)), h= input$Daysforecast, PI = TRUE) 
      )}})
    
####### Reactive2 ############################################################## 
    
    senti <- reactiveValues()
    observe({
      My_API_Key <- "YOUR API CODE GOES HERE"   # visit https://newsapi.org/ for your own API
      
      news_url = paste0("https://newsapi.org/v2/everything?q=",
                        input$Sentimentstock,
                        "&from=",today()-ddays(input$newsdays),
                        "&sortBy=relevance&pageSize=100&language=en&apiKey=", My_API_Key)
      
      results <- GET(url = news_url)
      mynews <- content(results, "text")
      
      mynews %<>%
        fromJSON(flatten = TRUE) %>%
        as.data.frame() %>%
        select(c(articles.title, articles.description, articles.content, articles.publishedAt))
      
      senti$news<-mynews
      
      news_words <- senti$news %>%
        select(c("articles.title","articles.description", "articles.content", "articles.publishedAt")) %>%
        unnest_tokens(word, articles.description) %>%
        filter(!word %in% append(stop_words$word, values = "chars"), str_detect(word, "^[a-z']+$"))
      
      news_words <- str_replace_all(news_words,"[^[:graph:]]", " ")
      
      Sentiment <-get_nrc_sentiment(news_words)
      
      senti_df<-data.frame(t(Sentiment))
      senti_df_rowsum <- data.frame(rowSums(senti_df))
      names(senti_df_rowsum)[1] <- "Total_count"
      senti_df_rowsum <- cbind("sentiment" = rownames(senti_df_rowsum), senti_df_rowsum)
      rownames(senti_df_rowsum) <- NULL
      senti_df_Plot<-senti_df_rowsum[1:10,]
      
      senti$ggplot<- ggplot(data=senti_df_Plot, aes(x=sentiment, y=Total_count, 
                          fill= sentiment)) + geom_bar(stat = "identity" ) + 
        geom_text(aes(label = scales::percent(Total_count/sum(Total_count))), 
        vjust = -0.2, size = 4, position = position_dodge(0.9))+
        ggtitle(paste0(input$Sentimentstock,": News (newsapi.org) Sentiment Analysis Summary"))+
        theme(axis.text=element_text(size=14),legend.position = "none")
    })

#####Plot3 #####################################################################
    
    output$distPlot3 <- renderPlot({senti$ggplot}) 
    
##### Table Plot ###############################################################
    
    output$Sentimentoutput <- renderDataTable(senti$news)
    }

################################################################################

shinyApp(ui = ui, server = server)

################################################################################
# Code References:
# http://rstudio-pubs-static.s3.amazonaws.com/493413_f3d12f11474a4484b2791dd0fd0a9bf5.html
# https://rpubs.com/kapage/523169
# https://www.tabvizexplorer.com/news-headlines-text-mining-and-sentiment-analysis/
