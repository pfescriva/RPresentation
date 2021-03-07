library(shiny)
library(shinythemes)
suppressPackageStartupMessages(library(forecast))
suppressPackageStartupMessages(library(astsa))
suppressPackageStartupMessages(library(lmtest))
suppressPackageStartupMessages(library(fUnitRoots))
suppressPackageStartupMessages(library(FitARMA))
suppressPackageStartupMessages(library(strucchange))
suppressPackageStartupMessages(library(reshape))
suppressPackageStartupMessages(library(Rmisc))
suppressPackageStartupMessages(library(fBasics))
library(MASS)
library(reticulate)
library(imputeTS)
library(lubridate)
library(tidyverse)
library(dygraphs)
library(xts)
library(tseries)

################
#### Data Preprocessing


data(PeaceBridge2003)
#str(PeaceBridge2003)

PeaceBridge2003$Date = seq(as.Date("2003/01/01"), as.Date("2015/12/01"), "months")

PeaceBridge2003_1 = PeaceBridge2003
PeaceBridge2003_2 = PeaceBridge2003
PeaceBridge2003_3 = PeaceBridge2003
PeaceBridge2003_4 = PeaceBridge2003
PeaceBridge2003_5 = PeaceBridge2003

PeaceBridge2003_1$Group = "1"
PeaceBridge2003_2$Group = "2"
PeaceBridge2003_3$Group = "3"
PeaceBridge2003_4$Group = "4"
PeaceBridge2003_5$Group = "5"

data = rbind(PeaceBridge2003_1, PeaceBridge2003_2, PeaceBridge2003_3,
             PeaceBridge2003_4, PeaceBridge2003_5)

#### Let's assume we had started with the dataset data above

split_df = split(data, data$Group)
forecast = matrix(rep(0, 12*length(split_df)), nrow = 12)
names(forecast) = c(1:5)


####

library(forecast)

for (i in 1:length(split_df)){
  
  data = split_df[[i]]
  
  fit = auto.arima(data$Traffic, D=12)
  
  forecast[,i] = tail(
    as.numeric(forecast(fit, h = 12)$fitted), n = 12)
}

forecasts = as_tibble(forecast)
names(forecasts) = as.factor(c(1:5))

forecasts_mean = apply(forecasts, 2, mean)
names(forecasts_mean) = c(1:5)



####

forecast2 = matrix(rep(0, 12*length(split_df)), nrow = 12)

names(forecast2) = c(1:5)


for (i in 1:length(split_df)){
  
  data = split_df[[i]]
  
  fit = nnetar(data$Traffic, D=12)
  
  forecast2[,i] = tail(
    as.numeric(forecast(fit, h = 12)$fitted), n = 12)
}

forecasts2 = as_tibble(forecast2)
names(forecasts2) = as.factor(c(1:5))

forecasts2_mean = apply(forecasts2, 2, mean)
names(forecasts2_mean) = c(1:5)

library(shiny)
library(shinythemes)
library(plotly)
library(dygraphs)

ui = fluidPage(theme = shinytheme("superhero"),
               numericInput(inputId = "n",
                            "Sample size", value = 1),
               dygraphOutput("one"),
               plotlyOutput("two"))

server = function(input, output) {
  
  output$one = renderDygraph({dygraph((
    cbind(xts(forecasts2[, as.character(input$n)],
              order.by = seq(as.Date("2016-01-01"), as.Date("2016-12-01"), by = 'month')), xts(forecasts[, as.character(input$n)],
                                                                                                      order.by = seq(as.Date("2016-01-01"), as.Date("2016-12-01"), by = 'month')))),
    main = "Internal EAD Forecast") %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
    dyRangeSelector() %>%
    dyRoller(rollPeriod = 1)})

output$two = renderPlotly({
  tibble(ARIMA = forecasts_mean[as.character(input$n)],
         NNet = forecasts2_mean[as.character(input$n)]) %>% plot_ly(orientation = 'h', name = 'Testing MAE', x = c(forecasts_mean[as.character(input$n)],
                                                                                                                   forecasts2_mean[as.character(input$n)]),
                                                                    y = c("ARIMA", "NNet"), colors=cols, mode="markers") %>% layout(title = "Total Accidents Year",
                                                                                                                                    barmode = 'group',
                                                                                                                                    xaxis = list(title = ""),
                                                                                                                                    yaxis = list(title = "")) %>%
    layout(plot_bgcolor='black') %>%
    layout(paper_bgcolor='black')
  
})
}

shinyApp(ui = ui, server = server)
