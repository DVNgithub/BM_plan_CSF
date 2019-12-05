#------------------------------------------------------------
# for paths set up, loading libraries
#------------------------------------------------------------

# run shiny app locally
if (FALSE) {
  rm(list=ls())
  setwd("//lrlhps/data/ttx_projects/onc/IDO1/prog/dev/shiny_app")
  library(shiny)
  runApp('BM_plan_CSF_sim_app.r')
}#end

if (TRUE) {
  set.seed(1234)
  
  options(shiny.trace = TRUE)
  
  library(shiny)
  library(shinyjs)
  library(grid)
  library(gplots)
  library(ggplot2)
  library(scales)
  library(gridExtra)
  library(gtools)
  library(reshape)
  library(plyr)
  library(dplyr)
  library(stats)
  library(tidyr)
  
}#end

#------------------------------------------------------------
# for GUI
#------------------------------------------------------------
if (TRUE) {
  # from DVN
  # The parameter is ORR 
  # the output could be three lines, 0.25, 0.5 ,0.75 quantile
  # The parameter is ORR, uniform or Beta, parameter for uniform and beta
  testfunc <- function(n, qt, orr, 
                       type.dist="uniform",
                       par1=0, par2=1) {
    # n: number of random vector
    # qt: quantile (0.25, 0.5, 0.75)
    # type.dist: type of distribution (uniform or beta)
    # par1, par2: parameters for randomized distribution
    if(tolower(type.dist) %in% "uniform") {
      BM <- runif(n, par1, par2)
    } else {
      BM <- rbeta(n, par1, par2)
    }
    ct <- quantile(BM, qt)
    
    resp <- NULL
    for(i in 1:length(BM)){
      if(BM[i]>= ct){
        ttt <- sample(0:1, 1, prob=c(1-orr, orr))
      } else{ttt <- 0}
      resp[i] <- ttt
    }
    return(c(ct, sum(resp)))
  }#end
  
  
  out.func <- function(Nsim=10000, 
                       N=c(10, 20, 40),
                       qt=c(0.50, 0.75),
                       orr=0.2,
                       type.dist="uniform",
                       par1=0, par2=1,
                       xval=c(1:10)) {
    ret <- NULL
    for(i in 1:length(N)) {
      ret0 <- NULL
      for(k in 1:length(qt)) {
        temp <- sapply(1:Nsim, function(x){testfunc(n=N[i], qt=qt[k], orr=orr, type.dist = type.dist, par1 = par1, par2 = par2)})
        df <- NULL
        for(m in 1:length(xval)) {
          yval <- mean(temp[2,]>= xval[m])
          tb0 <- data.frame(Nsim, N=N[i], qt=qt[k], xval=xval[m], yval)
          if(is.null(df)) df <- tb0 else df <- rbind(df, tb0)
        }#end
        if(is.null(ret0)) ret0 <- df else ret0 <- rbind(ret0, df)
      }#end k
      if(is.null(ret)) ret <- ret0 else ret <- rbind(ret, ret0)
    }#end for
    return(ret)
  }#end 
  
  
  # this function for getting data, doing plot
  # using these parameters for Shiny app
  csf.sim.result <- function(Nsim=10000, 
                             N=c(10, 20, 40),
                             qt=c(0.50, 0.75),
                             orr=0.2,
                             type.dist="uniform",
                             par1=0, par2=1,
                             xval=c(1, 10), xbreak=1) {
    
    xval <- c(xval[1]:xval[2])
    
    # for testing
    if (FALSE) {
      Nsim=10000
      N=c(10, 20, 40)
      qt=c(0.25, 0.50, 0.75)
      orr=0.2
      type.dist="uniform"
      par1=0; par2=1
      xval=c(1:10)
    }
    
    col.line <- c('red', 'blue', 'green', 'orange', 'orchid', 'purple', 'brown')
    
    ret <- out.func(Nsim=Nsim, N=N, qt=qt, orr=orr, type.dist=type.dist, par1=par1, par2=par2, xval=xval)
    dat <- mutate(ret, 
                  grp=paste0("Quantile: ", qt),
                  grp2=paste0("N = ", N, ", ORR = ", orr))
    
    # do line plot
    p <- ggplot(dat, aes(x=xval, y=yval, group=grp, color=grp)) + 
          geom_point(shape=1, size=1) + 
          geom_line(linetype=1, size=0.4) +   
          scale_color_manual("", values=col.line[1:length(qt)]) +
          facet_wrap(~ grp2) +
          xlab("Number of observation") +
          ylab("Prob. of success") +
          labs(title="") +
          scale_x_continuous(breaks=seq(0, max(xval), xbreak)) +
          scale_y_continuous(breaks=seq(0, 1, 0.1)) +
          theme_bw() +
          theme(legend.position = "none",
                legend.key = element_blank(), 
                legend.text = element_text(size=7), legend.key.width=unit(1,"line"),
                axis.title.y=element_text(margin=margin(0,15,0,0), size=12, face = "bold"),
                axis.title.x=element_text(margin=margin(15,0,0,0), size=12, face = "bold"),
                axis.text.x=element_text(vjust=0.5, hjust=0.5, size=11, face = "bold"),
                axis.text.y=element_text(vjust=0.5, hjust=0.5, size=11, face = "bold"),
                axis.ticks.y=element_line(size=0.1),
                plot.title = element_text(size = 9, hjust = 0.5),
                panel.grid.major = element_blank())
    
    return(list(df=ret, plot.out=p))
    
  }#end
}#end functions

#------------------------------------------------------------
# for server
#------------------------------------------------------------
if (TRUE) {
  shinyServer <-function(input, output, session) {
    #shinyServer(function(input, output, session) {
    # DVN adding
    ret_csf_sim <- reactive({
      csf.sim.result(input$Nsim, input$N, input$qt, input$orr, input$type.dist, 
                     input$par1, input$par2, input$xval, input$xbreak)
    })
    
    output$csf_output_sim <- renderTable({
      tab <- ret_csf_sim()
      print(tab$df)
    },rownames = TRUE)
    
    output$csf_plot_sim <- renderPlot({
      tab <- ret_csf_sim()
      
      grid.newpage()
      print(tab$plot.out, vp = viewport(x = unit(0.50, "npc"), y = unit(0.52, "npc"), width=0.7, height=0.9, just = "centre"))
      
    })
  }#end server
  
}#end server

#------------------------------------------------------------
# for GUI
#------------------------------------------------------------
if (TRUE) {
  shinyUI <- fluidPage(
    shinyjs::useShinyjs(),
    titlePanel("Analytical CSF and Risk Evaluation Tool"),
    
    # from DVN
    tabsetPanel(
      tabPanel("Biomarker Plan for CSF", 
               fluidRow(
                 column(4,
                        h4("Input"),
                        wellPanel(
                          numericInput("Nsim","Number of simulation", min=100, value=10000, step = 100),
                          numericInput("N","Sample Size of the Study Cohort", min=0,value=20, step = 1),
                          numericInput("qt","Quantile", min=0, max=1, value=0.50, step = 0.25),
                          sliderInput("orr", "Historical Control ORR", min=0, max=1, value = 0.2, step=0.01),
                          radioButtons("type.dist", "Distribution type",
                                       choices = list("Uniform", "Beta"), selected = "Uniform", inline = TRUE),
                          
                          conditionalPanel(
                            condition = "input.type.dist == 'Uniform'",
                            fluidRow(
                              column(3,
                                     numericInput("par1", "alpha", min=0,value=0)
                              ),
                              column(3,
                                     numericInput("par2", "beta", min=0,value=1)
                              )
                            )
                          ),
                          sliderInput("xval", "Numer of observarion range", min=1, max=20, value=c(1, 10), step = 1),
                          sliderInput("xbreak", "Number of observation break down (for plotting)", min=1, max=4, value = 1, step=1)
                        )
                 ), #end input
                 
                 column(8,
                        h4("Output"),
                        wellPanel(
                          tags$b("Operational Characteristics"),
                          tags$p(),
                          tableOutput("csf_output_sim"),
                          plotOutput("csf_plot_sim")
                        )
                 )
               )
      ) #end
      
    )#end tabsetPanel
    
  ) #end 
}#end ui

shinyApp(ui=shinyUI, server=shinyServer)

#-------------------------------------------------------------


