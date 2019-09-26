library(tidyverse)
library(quantmod)
library(shiny)
library(plotly)
library(TTR)
ui = fluidPage(
  headerPanel(title = "William's Alligator"),
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = 'ticker', label = 'Enter Stock Ticker', value = 'MSFT'),
      numericInput(inputId = 'ndays',label = 'Select number of days', value = 100, min = 1, max = 9682),
      selectInput(inputId = 'f1', label = 'Select Frequency', choices = c("daily", 'monthly', 'weekly'), selected = 'daily'),
      selectInput(inputId = 's1', label = 'Select OHLC', choices = c('Open', 'High', 'Low','Close')),
      sliderInput(inputId = 'ma1', label = '1st MA:Teeth (red)', min = 1, max = 50, value = 5),
      sliderInput(inputId = 'ma2', label = '2nd MA:Jaw (blue)', min = 1, max = 50, value = 8),
      sliderInput(inputId = 'ma3', label = '3rd MA:Lips (green)', min = 1, max = 50, value = 3),
      numericInput(inputId = 'nrsi', label = 'Enter the number of periods for RSI', min = 1, max = 50, value = 14),
      sliderInput(inputId = 'rsi_l', label ='Select RSI range (lower)', value = 25, min = 0, max = 100),
      sliderInput(inputId = 'rsi_h', label ='Select RSI range (upper)', value = 75, min = 0, max = 100),
      checkboxInput(inputId = 'stock_table', value = TRUE, label = 'Show Stock Table')
    ),mainPanel(
      plotlyOutput(outputId  = 'p1', width = 1400, height = 800),
      plotlyOutput(outputId = 'r1', width = 1290, height = 500),
      verbatimTextOutput(outputId = 't1'),
      verbatimTextOutput(outputId = 'rc'),
      verbatimTextOutput(outputId = 'c1')
    )
  )
)
server <- function(input, output){
  library(TTR)
  library(tidyverse)
  require(plotly)
  stock_ts = reactive({  getSymbols(Symbols = input$ticker, auto.assign = FALSE, periodicity = input$f1) })
  stock_df =  reactive({ as.data.frame(stock_ts()) })
  stock_1 =  reactive({ data.frame(Date = as.Date(rownames(stock_df())), Open = stock_df()[,1], High = stock_df()[,2], Low = stock_df()[,3], Close = stock_df()[,4]) })
  
  xcol <- reactive({ as.Date(tail(stock_1()$Date,n = input$ndays)) })
  ycol <-  reactive({tail(stock_1()[,input$s1],n = input$ndays) })
  
  stock_2 <- reactive({ data.frame(Date = xcol(), Close = ycol()) })
  p1 = reactive({ ggplot(data = stock_2(), aes(x = Date, y = Close)) + geom_line()})
  sma = reactive({ p1() + geom_line(aes(x = Date, y = SMA(Close, n = input$ma1), color = I('red'))) + geom_line(aes(x = Date, y = SMA(Close, n = input$ma2), color = I('blue'))) + geom_line(aes(x = Date, y = SMA(Close, n = input$ma3), color = I('green'))) })
  p2 = reactive({  sma() + labs(x = 'Date', y = input$s1) })
  
  
  output$p1 <- renderPlotly({p2()})
  
  rsi = reactive({ RSI(tail(stock_1()[,5],n = input$ndays)) })
  output$r1 = renderTable(rsi())
  rsi <- reactive({ data.frame(Date = xcol(), rsi = RSI(tail(stock_1()[,5],n = input$ndays),n = input$nrsi))})
  rs_plot = reactive({ ggplot(data= rsi(), aes(x = Date, y = rsi)) + geom_line() + geom_hline(yintercept = input$rsi_l, color = I('red')) + geom_hline(yintercept = input$rsi_h, color = I('red')) })
  output$r1 = renderPlotly(rs_plot())
  
  sm1 = reactive({ last(SMA(stock_2()[,2], n = input$ma1)) })
  sm2 = reactive({ last(SMA(stock_2()[,2], n = input$ma2)) })
  sm3 = reactive({ last(SMA(stock_2()[,2], n = input$ma3)) })
  cl = reactive({  last(stock_2()[,2]) })
  algo = reactive({  if(cl() > sm1() & cl() > sm2() & cl() > sm3()){
    print('The Stock is in upward trend, BUY.')
  }else if(round(cl(), digits = 1) == round(sm2(), digits  = 1) | round(sm1(), digits = 1) == round(cl(), digits = 1) | round(cl(), digits = 0) == round(sm3(), digits  = 0) ) {
    print('The Trend is irregular')
  }else{
    print('The stock is in downward trend. SELL')
  } })
  output$t1 = renderPrint(algo())
  
  obz = reactive({last(rsi()[,2]) })
  algo_2 = reactive({
    if(obz() >= input$rsi_h){
      print('the stock is over-bought')
      }else if(obz() <= input$rsi_l){
        print('the stock is over-sold')
        }else{
          print('the stock in moderately demanded')
        } })
  output$rc = renderPrint(algo_2())
  output$s1 =  renderPrint(stock_2()) 
  
  algo_3 = reactive({
    if(obz() <= 25 & algo() == 'The Stock is in upward trend, BUY.'){
      print('strong BUY')
    }else if(obz() > 25 & obz() <= 45 & algo() == 'The Stock is in upward trend, BUY.'){
      print('BUY')
    }else if(obz() > 45 & obz() <= 74 & algo() == 'The Stock is in upward trend, BUY.'){
      print('the stock is entering overbought zone. Do not buy')
    }else if(obz() > 74 & algo() == 'the stock is entering overbought zone. Do not buy'){
    print('the stock is in overbought zone. Do not buy')
  }else if(obz() <= 45 & algo() == 'The Trend is irregular'){
    print('The stock is in over-sell region. However there is no signal to buy the stock. Wait to take any positions')
  }else if(obz() >= 75 & algo() == 'The stock is in downward trend. SELL'){
    print('strong SELL')
  }else if(obz() > 46 & obz() <= 74 & algo() == 'The stock is in downward trend. SELL'){
    print('SELL')
  }else if(obz() <= 25 & algo() == 'The stock is in downward trend. SELL'){
    print('wait the stock is likely to bounce back.')
  }else if(obz() > 25 & obz() < 46 & algo() == 'The stock is in downward trend. SELL'){
    print('the stock is entering over-sold region. wait the stock is likely to bounce back.')
  }else{print('the stock is entering or entered over-bought region. However the trend of stock is irregular')} })
  
  output$c1 = renderPrint(algo_3())
}
shinyApp(ui = ui, server = server)
  