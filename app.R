library(shiny)
library(viridis)
library(rgdal)

fillmap2<-function(map, figtitle, y , leg.loc="beside", y.scl=NULL,
                   main.cex=1.5,main.line=0,map.lty=1,leg.rnd=0,
                   leg.cex=1){
  
  # 0: dark 1: light light Current shading ranges from darkest to light gray white (to distinguish with lakes).
  y.uq=sort(unique(c(y,y.scl)))
  cols<-viridis(length(y.uq),direction=-1)
  shading=y
  for (i in 1:length(y)){
    shading[i]<-cols[which(y.uq==y[i])]
  }
  
  par(mar=c(0,0,2,0))
  if (leg.loc=="beside"){
    layout(matrix(1:2,ncol=2),width=c(.8,.2))
  } else 
    if (leg.loc=="below"){
      layout(matrix(1:2,nrow=2),height=c(.6,.4))
    } else (print("leg.loc options are below or beside"))
  
  plot(map,col=shading,axes=F, lty=map.lty)
  title(main=figtitle,cex.main=main.cex,line=main.line) 
  
  par(mar=c(5, 4, 4, 2) + 0.1)
  plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = '')
  cols.5=cols[seq(1,length(y.uq),length.out=5)]
  lab.5=cols.5
  for (i in 1:5){lab.5[i]=y.uq[which(cols==cols.5[i])[1]]}
  lab.5=round(as.numeric(lab.5),leg.rnd)
  par(mar=c(0,0,0,0))
  if (leg.loc=="beside"){
    legend_image <- as.raster(matrix(cols, ncol=1))
    text(x=1.6, 
         y = seq(0,length(y.uq),length.out=5)/length(y.uq),
         labels = rev(lab.5), cex=leg.cex)
    rasterImage(legend_image, 0, 0, 1,1)
  } else{
    legend_image <- as.raster(matrix(cols, nrow=1))
    text(y=-0.25, 
         x = seq(0,length(y.uq),length.out=5)/(length(y.uq)*.5),
         labels = lab.5, cex=leg.cex)
    rasterImage(legend_image, 0, 0, 2,1)
  }
}


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