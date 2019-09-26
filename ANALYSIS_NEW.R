cpi_ind <- read.csv(file = "~/Desktop/DATA_CPI_PPI/CPI_INDIA.csv")
ppi_ind <- read.csv(file = "~/Desktop/DATA_CPI_PPI/PPI_INDIA.csv")
names(cpi_ind) <- c("date", "cpi")
names(ppi_ind) <- c("date", "ppi")
cpi_ppi <- cbind(cpi_ind, ppi_ind$ppi)
require(xts)
cpi_ppi_xts_1 <- xts(cpi_ppi[ ,c(2, 3)], order.by = as.Date(cpi_ppi$date))
cpi_ppi_xts <- cpi_ppi_xts_1[index(cpi_ppi_xts_1) <= "2006-12-01", ]
cpi_ppi_xts <- as.data.frame(cpi_ppi_xts_1)
cpi_ppi_complete <- cbind(rownames(cpi_ppi_xts), cpi_ppi_xts)

rownames(cpi_ppi_complete) = NULL
names(cpi_ppi_complete) <- c("date", "cpi", "ppi")
require(class)
require(stats)
cpi_ppi_complete$date <- as.Date(cpi_ppi_complete$date)
#find the correlation between the cpi and ppi
cor_cpi_ppi <- cor(cpi_ppi_complete$cpi, cpi_ppi_complete$ppi)
cor_cpi_ppi


split <- round(nrow(cpi_ppi_complete) * 0.8)
train_cpi_ppi <- cpi_ppi_complete[1:split, ]
test_cpi_ppi <-cpi_ppi_complete[(1 + nrow(train_cpi_ppi)):nrow(cpi_ppi_complete), ]

#model for prediction
m1 <- lm(ppi ~ cpi, data = train_cpi_ppi)
pred <- predict(object = m1, newdata = test_cpi_ppi)

#calculate the r_squared and rmse
err_2 <- (pred - test_cpi_ppi$ppi) ^ 2
rmse <- sqrt(mean(err_2))

#print the rmse
rmse

#calculate the r_squared
rss <- sum(err_2)
sst <- test_cpi_ppi$ppi- mean(test_cpi_ppi$ppi)
sst_tot <- sum(sst ^ 2)
r_squared <- (1 - (rss / sst_tot))


#after this model make a model with a lag on cpi of 1 year

cpi_ppi_lag <- lag(cpi_ppi_xts_1$cpi, k = 1)
cpi_ppi_lag <- as.data.frame(cpi_ppi_lag)
rownames(cpi_ppi_lag) = NULL
cpi_ppi_lag_complete <- cbind(cpi_ppi_complete[ ,c(1, 3)], cpi_ppi_lag)
rownames(cpi_ppi_lag_complete) = NULL


#remove the first row to make a model
cpi_ppi_lag_complete <- cpi_ppi_lag_complete[-1, ]

#finding the correlation between the two variables
cor_lag_cpi_ppi <- cor(cpi_ppi_lag_complete$cpi, cpi_ppi_lag_complete$ppi)

#splitting the data into train and test set
split_2 <- round(nrow(cpi_ppi_lag_complete) * 0.8)
train_lag <- cpi_ppi_lag_complete[1:split_2, ]
test_lag <- cpi_ppi_lag_complete[(1 + nrow(train_lag)):nrow(cpi_ppi_lag_complete), ]

#making the autoregressive model

m2 <- lm(ppi ~ cpi, data = train_lag)
pred_2 <- predict(object = m2, newdata = test_lag)

#calculating the rmse and the r_squared
error_2 <- (pred_2 - test_lag$ppi) ^ 2
rmse_2 <- sqrt(mean(error_2))

#making it interactive
require(shiny)


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      numericInput(
        inputId = "predict", label = "Please enter CPI value to predict", value = 100
      )
    ), mainPanel(
      textOutput(outputId = "output_predict")
    )
  )
)
server <- function(input, output){
  data <- reactive({data.frame(cpi = input$predict)})
  output$output_predict <- renderPrint(predict(m2, newdata = data()))
}
shinyApp(ui = ui, server = server)

