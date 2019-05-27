library(shiny)
library(corrplot)
library(ggplot2)
library(rmetalog)
library(fBasics)

genetri <- function(Nsim,a,b,c) {
  U <- runif(Nsim)                             # uniform u values
  X=rep(0,Nsim)           
  for (i in 1:Nsim)
    if (U [i] < ((c-a)/(b-a))){
      X[i] <-  a+sqrt(U[i]*(b-a)*(c-a))        # check is done based on U value accordig to the theory
    }else {
      X[i] <-  b-sqrt((1-U[i])*(b-a)*(b-c))               
    }
  return(X)
}
# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Probabilistic Reserve Evaluation"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      fluidRow(
               column(12,div(class = "option-group",
                            radioButtons("dis_v", "Gross Rock Volume (Million Barrel)",
                                         choices = c("Normal" = "norm",
                                                     "Uniform" = "unif",
                                                     "Log-normal" = "lnorm",
                                                     "Exponential" = "exp",
                                                     "Triangle" ="tri"
                                                     )),
                            
                            conditionalPanel("input.dis_v === 'norm'",
                                             sliderInput("mean_normal_v", "Mean", min=10, max=10000, value=100),
                                             sliderInput("sd_normal_v", "SD", min=10, max=1000, value=20)),
                            
                            
                            conditionalPanel("input.dis_v === 'unif'",
                                             sliderInput("min_uni_v", "min", min=10, max=10000, value=200),
                                             sliderInput("max_uni_v", "max", min=10, max=10000, value=200)),
                            
                            conditionalPanel("input.dis_v === 'lnorm'",
                                             sliderInput("mean_lognorm_v", "Mean", min=10, max=10000, value=200),
                                             sliderInput("sd_lognorm_v", "SD", min=100, max=1000, value=200)),
                            
                            conditionalPanel("input.dis_v === 'exp'",
                                             sliderInput("rate_v", "Rate", min=0.0001, max=0.1, value=0.1)),
                            
                            conditionalPanel("input.dis_v === 'tri'",
                                             sliderInput("a_tri_v", "a", min=100, max=10000, value=200),
                                             sliderInput("b_tri_v", "b", min=100, max=10000, value=400),
                                             sliderInput("c_tri_v", "c", min=100, max=10000, value=300))
                            
                            
               )
               ,column(12,plotOutput('hist')))
      ),
      fluidRow(
               column(12,
                      radioButtons("dis_NTG", "Net To Gross Volume Ratio",
                                   choices = c("Normal" = "norm",
                                               "Uniform" = "unif",
                                               "Log-normal" = "lnorm",
                                               "Exponential" = "exp",
                                               "Triangle" ="tri")),
                      
                      conditionalPanel("input.dis_NTG === 'norm'",
                                       sliderInput("mean_normal_ng", "Mean", min=0, max=1, value=0.5),
                                       sliderInput("sd_normal_ng", "SD", min=0, max=1, value=0.1)
                                       
                      ),
                      conditionalPanel("input.dis_NTG === 'unif'",
                                       sliderInput("min_uni_ng", "min", min=0, max=1, value=0.5),
                                       sliderInput("max_uni_ng", "max", min=0, max=1, value=0.5)
                      ),
                      conditionalPanel("input.dis_NTG === 'lnorm'",
                                       sliderInput("mean_lognorm_ng", "Mean", min=0, max=1, value=0.5),
                                       sliderInput("sd_lognorm_ng", "SD", min=0, max=1, value=0.1)
                      ),
                      conditionalPanel("input.dis_NTG === 'exp'",
                                       sliderInput("rate_ng", "Rate", min=0.0001, max=0.1, value=0.1)
                      ),
                      conditionalPanel("input.dis_NTG === 'tri'",
                                       sliderInput("a_tri_ng", "a", min=0, max=1, value=0.5),
                                       sliderInput("b_tri_ng", "b", min=0, max=1, value=0.7),
                                       sliderInput("c_tri_ng", "c", min=0, max=1, value=0.4)
                                       
                      ),
                      column(12,plotOutput('hist1')))
      ),
      
      fluidRow(
        column(12,
               radioButtons("dis_P", "Porosity",
                            choices = c("Normal" = "norm",
                                        "Uniform" = "unif",
                                        "Log-normal" = "lnorm",
                                        "Exponential" = "exp",
                                        "Triangle" ="tri",
                                        "Metalog" ="met")),
               
               conditionalPanel("input.dis_P === 'norm'",
                                sliderInput("mean_normal_p", "Mean", min=0, max=1, value=0.5),
                                sliderInput("sd_normal_p", "SD", min=0, max=1, value=0.1)
                                
               ),
               conditionalPanel("input.dis_P === 'unif'",
                                sliderInput("min_uni_p", "min", min=0, max=1, value=0.5),
                                sliderInput("max_uni_p", "max", min=0, max=1, value=0.5)
               ),
               conditionalPanel("input.dis_P === 'lnorm'",
                                sliderInput("mean_lognorm_p", "Mean", min=0, max=1, value=0.5),
                                sliderInput("sd_lognorm_p", "SD", min=0, max=1, value=0.1)
               ),
               conditionalPanel("input.dis_P === 'exp'",
                                sliderInput("rate_p", "Rate", min=0.0001, max=0.1, value=0.1)
               ),
               conditionalPanel("input.dis_P === 'tri'",
                                sliderInput("a_tri_p", "a", min=0, max=1, value=0.5),
                                sliderInput("b_tri_p", "b", min=0, max=1, value=0.7),
                                sliderInput("c_tri_p", "c", min=0, max=1, value=0.4)),
               
              
               column(12,plotOutput('hist2')))
      ),
      
      
      
      fluidRow(
        column(12,
               radioButtons("dis_S", "Oil Saturation",
                            choices = c("Normal" = "norm",
                                        "Uniform" = "unif",
                                        "Log-normal" = "lnorm",
                                        "Exponential" = "exp",
                                        "Triangle" ="tri")),
               
               conditionalPanel("input.dis_S === 'norm'",
                                sliderInput("mean_normal_s", "Mean", min=0, max=1, value=0.5),
                                sliderInput("sd_normal_s", "SD", min=0, max=1, value=0.1)
                                
               ),
               conditionalPanel("input.dis_S === 'unif'",
                                sliderInput("min_uni_s", "min", min=0, max=1, value=0.5),
                                sliderInput("max_uni_s", "max", min=0, max=1, value=0.5)
               ),
               conditionalPanel("input.dis_S === 'lnorm'",
                                sliderInput("mean_lognorm_s", "Mean", min=0, max=1, value=0.5),
                                sliderInput("sd_lognorm_s", "SD", min=0, max=1, value=0.1)
               ),
               conditionalPanel("input.dis_S === 'exp'",
                                sliderInput("rate_p", "Rate", min=0.0001, max=0.1, value=0.1)
               ),
               conditionalPanel("input.dis_S === 'tri'",
                                sliderInput("a_tri_s", "a", min=0, max=1, value=0.5),
                                sliderInput("b_tri_s", "b", min=0, max=1, value=0.7),
                                sliderInput("c_tri_s", "c", min=0, max=1, value=0.4)
                                
               ),
               column(12,plotOutput('hist3')))
      ),
      
      
      fluidRow(
        column(12,
               radioButtons("dis_VC", "Conversion Factor",
                            choices = c("Normal" = "norm",
                                        "Uniform" = "unif",
                                        "Log-normal" = "lnorm",
                                        "Exponential" = "exp",
                                        "Triangle" ="tri")),
               
               conditionalPanel("input.dis_VC === 'norm'",
                                sliderInput("mean_normal_vc", "Mean", min=2, max=4, value=2),
                                sliderInput("sd_normal_vc", "SD", min=0.1, max=1, value=0.1)
                                
               ),
               conditionalPanel("input.dis_VC === 'unif'",
                                sliderInput("min_uni_vc", "min", min=0, max=3, value=1),
                                sliderInput("max_uni_vc", "max", min=0, max=3, value=1)
               ),
               conditionalPanel("input.dis_VC === 'lnorm'",
                                sliderInput("mean_lognorm_vc", "Mean", min=0, max=3, value=1),
                                sliderInput("sd_lognorm_vc", "SD", min=0, max=3, value=1)
               ),
               conditionalPanel("input.dis_VC === 'exp'",
                                sliderInput("rate_vc", "Rate", min=0.0001, max=0.1, value=0.1)
               ),
               conditionalPanel("input.dis_VC === 'tri'",
                                sliderInput("a_tri_vc", "a", min=0, max=3, value=0.5),
                                sliderInput("b_tri_vc", "b", min=0, max=3, value=2),
                                sliderInput("c_tri_vc", "c", min=0, max=3, value=0.4)
                                
               ),
               column(12,plotOutput('hist4')))
      ),
    
      
      fluidRow(
        column(12,
               radioButtons("dis_RF", "Recovery Factor",
                            choices = c("Normal" = "norm",
                                        "Uniform" = "unif",
                                        "Log-normal" = "lnorm",
                                        "Exponential" = "exp",
                                        "Triangle" ="tri")),
               
               conditionalPanel("input.dis_RF === 'norm'",
                                sliderInput("mean_normal_rf", "Mean", min=0, max=1, value=0.5),
                                sliderInput("sd_normal_rf", "SD", min=0, max=1, value=0.1)
                                
               ),
               conditionalPanel("input.dis_RF === 'unif'",
                                sliderInput("min_uni_rf", "min", min=0, max=1, value=0.5),
                                sliderInput("max_uni_rf", "max", min=0, max=1, value=0.5)
               ),
               conditionalPanel("input.dis_RF === 'lnorm'",
                                sliderInput("mean_lognorm_rf", "Mean", min=0, max=1, value=0.5),
                                sliderInput("sd_lognorm_rf", "SD", min=0, max=1, value=0.1)
               ),
               conditionalPanel("input.dis_RF === 'exp'",
                                sliderInput("rate_rf", "Rate", min=0.0001, max=0.1, value=0.1)
               ),
               conditionalPanel("input.dis_RF === 'tri'",
                                sliderInput("a_tri_rf", "a", min=0, max=1, value=0.5),
                                sliderInput("b_tri_rf", "b", min=0, max=1, value=0.7),
                                sliderInput("c_tri_rf", "c", min=0, max=1, value=0.4)
                                
               ),
               column(12,plotOutput('hist5')))
      ),
      
      
      sliderInput("n",
                  "Number of observations:",
                  value = 10000,
                  min = 10000,
                  max = 100000, step = 10000),
      
      
      sliderInput("error",
                  "Maximum percentage of error in the mean:",
                  value = 1,
                  min = 1,
                  max = 10, step = 1)
      
      
      
      #plotOutput('hist')
    
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("plot")),
                  tabPanel("Cumulative Distribution", plotOutput("cum")),
                  tabPanel("Summary Statistics", tableOutput("summary")),
                  tabPanel("Boxplot", plotOutput("boxplot")),
                  tabPanel("Sensivity Analysis", plotOutput("sens")),
                  tabPanel("#Minimum Iteration", tableOutput("ite"))

      )
      
    )
  )
)

server <- function(input, output) {
  
  
  d_v <- reactive({
    dist_v <- switch(input$dis_v,
                     norm = rnorm(input$n,input$mean_normal_v,input$sd_normal_v),
                     unif = runif(input$n, input$min_uni_v, input$max_uni_v),
                     #lnorm = rlnorm(input$n, log(input$mean_lognormal^2 / sqrt(input$sd_lognormal^2 + input$mean_lognormal^2)), sqrt(log(1 + (input$sd_lognormal^2 / input$mean_lognormal^2)))),
                     lnorm = rlnorm(input$n,log((input$mean_lognorm_v^2) / (sqrt(input$sd_lognorm_v^2 + input$mean_lognorm_v^2))),
                                    sqrt(log(1 + (input$sd_lognorm_v^2 / input$mean_lognorm_v^2)))),
                     exp = rexp(input$n,input$rate_v),
                     tri=genetri(input$n,a=input$a_tri_v,b=input$b_tri_v,c=input$c_tri_v),
                     rnorm)
    
    #dist_v(input$n,input$mean_normal,input$sd_normal)
  })
  
  output$hist <- renderPlot({
    dist <- input$dist
    n <- input$n
    hist(d_v(), col = "#75AADB", border = "white",breaks = 100, main='Histogram_ Gross Rock
         Volume (Million Barrel)', xlab='GRV_Million Barrel')
  })
  
  d_NTG <- reactive({
    dist_NTG <- switch(input$dis_NTG,
                       norm = rnorm(input$n,input$mean_normal_ng,input$sd_normal_ng),
                       unif = runif(input$n, input$min_uni_ng, input$max_uni_ng),
                       #lnorm = rlnorm(input$n, log(input$mean_lognormal^2 / sqrt(input$sd_lognormal^2 + input$mean_lognormal^2)), sqrt(log(1 + (input$sd_lognormal^2 / input$mean_lognormal^2)))),
                       lnorm = rlnorm(input$n,log((input$mean_lognorm_ng^2) / (sqrt(input$sd_lognorm_ng^2 + input$mean_lognorm_ng^2))),
                                      sqrt(log(1 + (input$sd_lognorm_ng^2 / input$mean_lognorm_ng^2)))),
                       exp = rexp(input$n,input$rate_ng),
                       tri=genetri(input$n,a=input$a_tri_ng,b=input$b_tri_ng,c=input$c_tri_ng),
                       rnorm)
    
    #dist_v(input$n,input$mean_normal,input$sd_normal)
  })
  output$hist1 <- renderPlot({
    dist <- input$dist
    n <- input$n
    hist(d_NTG(), col = "#75AADB", border = "white",breaks = 100, main='Histogram_ Net To Gross Ratio', xlab='NTG_Ratio')
  })
  
  data <- reactive({
    
    file1 <- input$file
    
    if(is.null(file1)){return()} 
    
    read.table(file=file1$datapath)
    
  })
  
  mins <- reactive({
    minss <- input$met_min
  })
  
  maxs <- reactive({
    minss1 <- input$met_max
  })
  bnd <- reactive({
    sw <- c(mins(),maxs())
  })
  
  my_metalog <- reactive(
    {
      my_metalog  <- metalog(
        data()[,1],
        term_limit = 9,
        term_lower_bound = 2,
        bounds = c(0,1),
        boundedness = 'b',
        step_len = 0.01)
    })

  sff <- reactive(
     {
      zz <- rmetalog(my_metalog(), n = 100000, term = 9)
    })
  
  d_P <- reactive({
    dist_P <- switch(input$dis_P,
                     norm = rnorm(input$n,input$mean_normal_p,input$sd_normal_p),
                     unif = runif(input$n, input$min_uni_p, input$max_uni_p),
                     #lnorm = rlnorm(input$n, log(input$mean_lognormal^2 / sqrt(input$sd_lognormal^2 + input$mean_lognormal^2)), sqrt(log(1 + (input$sd_lognormal^2 / input$mean_lognormal^2)))),
                     lnorm = rlnorm(input$n,log((input$mean_lognorm_p^2) / (sqrt(input$sd_lognorm_p^2 + input$mean_lognorm_p^2))),
                                    sqrt(log(1 + (input$sd_lognorm_p^2 / input$mean_lognorm_p^2)))),
                     exp = rexp(input$n,input$rate_p),
                     tri=genetri(input$n,a=input$a_tri_p,b=input$b_tri_p,c=input$c_tri_p),
                     met = sff(),
                     rnorm)
    
    #dist_v(input$n,input$mean_normal,input$sd_normal)
  })
  
  output$hist2 <- renderPlot({
    dist <- input$dist
    n <- input$n
    hist(d_P(), col = "#75AADB", border = "white",breaks = 100, main='Histogram_ Porosity', xlab='Porosity')
  })
  
  
  
  ###############
  #output$hist22 <- renderPlot({
   #dist <- input$dist
   # n <- input$n
    #hist(data()[,1], col = "#75AADB", border = "white",breaks = 20, main='Histogram_ Porosity_Data', xlab='Porosity-data')
  #})
  
  
  
  
  ############
  
  
  
  
  d_S <- reactive({
    dist_S <- switch(input$dis_S,
                     norm = rnorm(input$n,input$mean_normal_s,input$sd_normal_s),
                     unif = runif(input$n, input$min_uni_s, input$max_uni_s),
                     #lnorm = rlnorm(input$n, log(input$mean_lognormal^2 / sqrt(input$sd_lognormal^2 + input$mean_lognormal^2)), sqrt(log(1 + (input$sd_lognormal^2 / input$mean_lognormal^2)))),
                     lnorm = rlnorm(input$n,log((input$mean_lognorm_s^2) / (sqrt(input$sd_lognorm_s^2 + input$mean_lognorm_s^2))),
                                    sqrt(log(1 + (input$sd_lognorm_s^2 / input$mean_lognorm_s^2)))),
                     exp = rexp(input$n,input$rate_s),
                     tri=genetri(input$n,a=input$a_tri_s,b=input$b_tri_s,c=input$c_tri_s),
                     rnorm)
    
    #dist_v(input$n,input$mean_normal,input$sd_normal)
  })
  output$hist3 <- renderPlot({
    dist <- input$dist
    n <- input$n
    hist(d_S(), col = "#75AADB", border = "white",breaks = 100, main='Histogram_ Oil Saturation', xlab='S_o')
  })
  
  
  d_VC <- reactive({
    dist_VC <- switch(input$dis_VC,
                     norm = rnorm(input$n,input$mean_normal_vc,input$sd_normal_vc),
                     unif = runif(input$n, input$min_uni_vc, input$max_uni_vc),
                     #lnorm = rlnorm(input$n, log(input$mean_lognormal^2 / sqrt(input$sd_lognormal^2 + input$mean_lognormal^2)), sqrt(log(1 + (input$sd_lognormal^2 / input$mean_lognormal^2)))),
                     lnorm = rlnorm(input$n,log((input$mean_lognorm_vc^2) / (sqrt(input$sd_lognorm_vc^2 + input$mean_lognorm_vc^2))),
                                    sqrt(log(1 + (input$sd_lognorm_vc^2 / input$mean_lognorm_vc^2)))),
                     exp = rexp(input$n,input$rate_vc),
                     tri=genetri(input$n,a=input$a_tri_vc,b=input$b_tri_vc,c=input$c_tri_vc),
                     rnorm)
    
    #dist_v(input$n,input$mean_normal,input$sd_normal)
  })
  output$hist4 <- renderPlot({
    dist <- input$dist
    n <- input$n
    hist(d_VC(), col = "#75AADB", border = "white",breaks = 100, main='Histogram_ Volume Conversion', xlab='Bg/Bo')
  })
  
  d_RF <- reactive({
    dist_RF <- switch(input$dis_RF,
                     norm = rnorm(input$n,input$mean_normal_rf,input$sd_normal_rf),
                     unif = runif(input$n, input$min_uni_rf, input$max_uni_rf),
                     #lnorm = rlnorm(input$n, log(input$mean_lognormal^2 / sqrt(input$sd_lognormal^2 + input$mean_lognormal^2)), sqrt(log(1 + (input$sd_lognormal^2 / input$mean_lognormal^2)))),
                     lnorm = rlnorm(input$n,log((input$mean_lognorm_rf^2) / (sqrt(input$sd_lognorm_rf^2 + input$mean_lognorm_rf^2))),
                                    sqrt(log(1 + (input$sd_lognorm_rf^2 / input$mean_lognorm_rf^2)))),
                     exp = rexp(input$n,input$rate_rf),
                     tri=genetri(input$n,a=input$a_tri_rf,b=input$b_tri_rf,c=input$c_tri_rf),
                     rnorm)
    
    #dist_v(input$n,input$mean_normal,input$sd_normal)
  })
  
  
  output$hist5 <- renderPlot({
    dist <- input$dist
    n <- input$n
    hist(d_RF(), col = "#75AADB", border = "white",breaks = 100, main='Histogram_Recovery Factor', xlab='RF')
  })
  ress <- reactive({
    res <- (d_v()*d_NTG()*d_P()*d_S()*d_RF()/d_VC()) 
  })
  datfram <- reactive({
    data.frame(
      histo=c(ress()))
  })
  
  output$plot <- renderPlot({
    PP1 <- ggplot(datfram(), aes(x=histo)) + 
      geom_histogram(aes(y=..density..),color="darkblue", fill="lightblue",bins = 100) +
      geom_vline(aes(xintercept=mean(histo)), color="red", linetype="dashed", size=1) +
      ylab("Density") + xlab("Technical Reserve, million STB") + ggtitle("Results of Simulation: PDF")
    print(PP1)
  })

  
  dff <- reactive({
    data.frame(Metric = c('Mean','SD','P10','P50','P90','Skewness','Kurtosis'), Value = c(mean(ress()),sd(ress()),quantile(ress(),c(0.1,0.5,0.9)),skewness(ress()),kurtosis(ress())))
    })
  
  output$summary <- renderTable({
    dff()
  })
  
  #output$boxplot <- renderPlot({
   # boxplot(ress(), las =2, col = 'red',ylab = "Technical Reserve, million STB")
  #})
  
  
  output$boxplot <- renderPlot({
    PP2 <- ggplot(datfram(), aes(y=histo)) + 
      geom_boxplot(color="darkblue", fill="lightblue") +
      ylab("Technical Reserve, million STB") + ggtitle("Box Plot, Result of Simulation")
    print(PP2)
  })
  
  
  
  
  
  
  
  dat <- reactive({
    dat <- data.frame(
      variable=c('Gross Rock Volume','Net to Gross Volume Ratio','Porosity','Oil Saturation','Conversion Factor','Recovery Factor'),
      Correlation_Coefficient=c(cor(d_v(),ress()),cor(d_NTG(),ress()),cor(d_P(),ress()),cor(d_S(),ress()),cor(d_VC(),ress()),cor(d_RF(),ress())))
  })
  
  output$sens <- renderPlot({
    PP <- ggplot(dat(),aes(variable,Correlation_Coefficient, fill='color')) +
      geom_bar(stat = 'identity', width = 0.3) +
      coord_flip()
    print(PP)
  })

  #cumr <- reactive({
   # res <- ecdf(ress()) 
  #})
  
  

  output$cum <- renderPlot({
    PC <- ggplot(NULL, aes(x=ress())) + 
      geom_step(stat="ecdf", size=1,color="darkblue") + 
      scale_y_continuous("CDF",breaks = c(0.1,0.2,0.3,0.4,0.5,0.6
                                         ,0.7,0.8,0.9,1)) +
      scale_x_continuous("Technical Reserve, million STB")
    print(PC)
    
  })
  
  
  totite <- reactive({
    ((100*sd(ress())*1.96)/(mean(ress())*input$error))^2
  })
  
  d_min <- reactive({
    data.frame(Metric = c("Minimum Number of Simulations required to meet the Assigned Maximum Error"), Value = c(totite()))
  })
  
  output$ite <- renderTable({
    d_min()
  })
  
  #output$tot <- reactive({
  #((100*sd(ress())*1.96)/(mean(ress())*input$error))^2
  #})
  
  
 # data <- reactive({
   
#    file1 <- input$file
    
 #   if(is.null(file1)){return()} 
    
  #  read.table(file=file1$datapath)
    
    
  #})
  
  
  #my_metalog <- reactive(
  #  {
   #   my_metalog  <- metalog(
    #    data()[,1],
     #   term_limit = 9,
      #  term_lower_bound = 2,
       # bounds = c(0, 1),
        #boundedness = 'b',
        #step_len = 0.01)
    #})
  #sff <- reactive(
   # {
    #  zz <- rmetalog(my_metalog(), n = 10000, term = 9)
    #})
  
  
  #output$plot1 <- renderPlot({
    
   # if(is.null(data())){return ()}
    
    #hist(sff(),breaks = 200)
    
  #})
  
  
#  output$cum <- renderPlot({
 #   plot(cumr())
#  })
  
}
shinyApp(ui, server)
