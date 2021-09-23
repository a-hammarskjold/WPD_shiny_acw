library(shiny)

data = read.csv("Data\\shiny_data.csv")
NHtracts = readOGR("Data\\NHtracts\\NHtracts.shp")

ui = fluidPage(
  titlePanel("2010-2018 WPD Arrests Data"),
  # *Input() functions
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "year",
                  label = "Years",
                  min = 2010,
                  max = 2018,
                  sep = "",
                  value = 2010,
                  animate =  animationOptions(loop = TRUE)),
      radioButtons(inputId = "group",
                   label = "Data",
                   choices = c('Total Arrests','White Only Arrests','Black Only Arrests')),
      radioButtons(inputId = "adj",
                   label = "Data Adjustment",
                   choices = c("None","As a Percent of the Population","As a Percent of Total Arrests","Standardized Incidence Ratio","Poisson Regression"))),
    # *Output() functions
    mainPanel(plotOutput(outputId = "map"))
  ))

server = function(input,output) {
  output$map <- renderPlot({
    if (input$adj == "None"){
      if (input$group == "Total Arrests"){
        fillmap2(NHtracts,paste(input$year,input$group),data$arrests_total[which(data$year==input$year)],map.lty=0,leg.loc='below',y.scl=data$arrests_total,leg.cex=1,leg.rnd=3)
      }
      if (input$group == "White Only Arrests"){
        fillmap2(NHtracts,paste(input$year,input$group),data$arrests_W[which(data$year==input$year)],map.lty=0,leg.loc='below',y.scl=data$arrests_W,leg.cex=1,leg.rnd=3)
      }
      if (input$group == "Black Only Arrests"){
        fillmap2(NHtracts,paste(input$year,input$group),data$arrests_B[which(d.inla$year==input$year)],map.lty=0,leg.loc='below',y.scl=data$arrests_B,leg.cex=1,leg.rnd=3)
      }
    }
    
    if (input$adj == "As a Percent of the Population"){
      if (input$group == "Total Arrests"){
        fillmap2(NHtracts,paste(input$year,input$group,input$adj),data$pct_arr_tot[which(data$year==input$year)],map.lty=0,leg.loc='below',y.scl=data$pct_arr_tot,leg.cex=1,leg.rnd=3)
      }
      if (input$group == "White Only Arrests"){
        fillmap2(NHtracts,paste(input$year,input$group,input$adj),data$pct_arr_wow[which(data$year==input$year)],map.lty=0,leg.loc='below',leg.cex=1,leg.rnd=3)
      }
      if (input$group == "Black Only Arrests"){
        fillmap2(NHtracts,paste(input$year,input$group,input$adj),data$pct_arr_bob[which(data$year==input$year)],map.lty=0,leg.loc='below',leg.cex=1,leg.rnd=3)
      }
    }
    
    if (input$adj == "As a Percent of Total Arrests"){
      if (input$group == "Total Arrests"){
        fillmap2(NHtracts,paste(input$year,input$group,input$adj),data$pct_arr_t[which(data$year==input$year)],map.lty=0,leg.loc='below',leg.cex=1,leg.rnd=3)
      }
      if (input$group == "White Only Arrests"){
        fillmap2(NHtracts,paste(input$year,input$group,input$adj),data$pct_arr_w[which(data$year==input$year)],map.lty=0,leg.loc='below',leg.cex=1,leg.rnd=3)
      }
      if (input$group == "Black Only Arrests"){
        fillmap2(NHtracts,paste(input$year,input$group,input$adj),data$pct_arr_b[which(data$year==input$year)],map.lty=0,leg.loc='below',leg.cex=1,leg.rnd=3)
      }
    }
    
    if (input$adj == "Standardized Incidence Ratio"){
      if (input$group == "Total Arrests"){
        fillmap2(NHtracts,paste(input$year,input$group,input$adj),data$sir_tot[which(data$year==input$year)],map.lty=0,leg.loc='below',leg.cex=1,leg.rnd=3)
      }
      if (input$group == "White Only Arrests"){
        fillmap2(NHtracts,paste(input$year,input$group,input$adj),data$sir_w[which(data$year==input$year)],map.lty=0,leg.loc='below',leg.cex=1,leg.rnd=3)
      }
      if (input$group == "Black Only Arrests"){
        fillmap2(NHtracts,paste(input$year,input$group,input$adj),data$sir_b[which(data$year==input$year)],map.lty=0,leg.loc='below',leg.cex=1,leg.rnd=3)
      }
    }
    
    if (input$adj == "Poisson Regression"){
      if (input$group == "Total Arrests"){
        fillmap2(NHtracts,paste(input$year,input$group,input$adj),exp(data$sumres_t[which(data$year==input$year)]),map.lty=0,leg.loc='below',y.scl=exp(data$sumres_t),leg.cex=1,leg.rnd=3)
      }
      if (input$group == "White Only Arrests"){
        fillmap2(NHtracts,paste(input$year,input$group,input$adj),exp(data$sumres_w[which(data$year==input$year)]),map.lty=0,leg.loc='below',y.scl=exp(data$sumres_w),leg.cex=1,leg.rnd=3)
      }
      if (input$group == "Black Only Arrests"){
        fillmap2(NHtracts,paste(input$year,input$group,input$adj),exp(data$sumres_b[which(data$year==input$year)]),map.lty=0,leg.loc='below',y.scl=exp(data$sumres_b),leg.cex=1,leg.rnd=3)
      }
    }
  })
}

shinyApp(ui=ui,server=server)