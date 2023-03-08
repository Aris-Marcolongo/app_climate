library(shiny)
library(ggplot2)
library(plotly)


# Loading data (global) 
pca_vectors_boreal <- readRDS("data/pca_vectors_Boreal.rds")
pca_vectors_temperate  <- readRDS("data/pca_vectors_Temperate.rds")
pca_vectors_tropical  <- readRDS("data/pca_vectors_Tropical.rds")
feat_imp_boreal <- readRDS("data/feat_imp_Boreal.rds")
feat_imp_temperate  <- readRDS("data/feat_imp_Temperate.rds")
feat_imp_tropical  <- readRDS("data/feat_imp_Tropical.rds")
feat_boreal <- readRDS("data/years_low_Boreal.rds")
feat_temperate  <- readRDS("data/years_low_Temperate.rds")
feat_tropical  <- readRDS("data/years_low_Tropical.rds")
rec_boreal <- readRDS("data/rec_low_Boreal.rds")
rec_temperate  <- readRDS("data/rec_low_Temperate.rds")
rec_tropical  <- readRDS("data/rec_low_Tropical.rds")


# Plotting utilities
plot_site_vector <- function(site, ivec){
  data <- switch(site, 
                 "boreal" = pca_vectors_boreal,
                 "temperate" = pca_vectors_temperate,
                 "tropical" = pca_vectors_tropical)
  month <- -5:11
  print(dim(data))
  ppt <- data[ivec,1,1:17]
  at <- data[ivec,2,1:17]
  par <- data[ivec,3,1:17]
  df <- data.frame(month,ppt,at,par)
  ggplot(data=df, aes(x=month, y=ppt))+geom_line(color='blue')+
    geom_line(data = df, aes(x = month, y = at), color = "orange")+
    geom_line(data = df, aes(x = month, y = par), color = "green") +
    labs(x="Months",y="Weight") 
}

plot_feat_imp <- function(site){
  data <- switch(site, 
                 "boreal" = feat_imp_boreal,
                 "temperate" = feat_imp_temperate,
                 "tropical" = feat_imp_tropical)
  component <- 1:51
  featimp <- data[1:51]
  df <- data.frame(component,featimp)
  ggplot(data=df, aes(x=component, y=featimp))+
    geom_line(color='black')+
    labs(x="Component",y="Feat_imp") 
}

plot_reconstructed <- function(df,df_rec,color){
  ggplot(data=df, aes(x=month, y=driver))+geom_line(color=color)+
    geom_line(data = df_rec, aes(x = month, y = driver_rec), color = "black", linetype="dashed")+
    labs(x="Months",y="GPP-production") 
}


# User interface ----
ui <- fluidPage(
  
  tabsetPanel(
    
    tabPanel('Plot Single Years',
      titlePanel("Input"),
      sidebarLayout(
        sidebarPanel(
          p("For the selected site and year the GPP production as given by integrated gradients is reported.", 
            style = "font-family: 'times'; font-si16pt"),
          p("Color legend: BLUE : PPT, ORANGE: AT, GREEN: PAR", 
            style = "font-family: 'times'; font-si16pt"),
          p("The reconstructed GPP production, using only the pca components relevant for predicting yearly GPP, is overlapped with a dashed black line.", 
            style = "font-family: 'times'; font-si16pt"),          
          p("Check other tab for a summary of the pca components chosen by the random forest", 
            style = "font-family: 'times'; font-si16pt"),   
          selectInput("site", 
                      label = "Site",
                      choices = c("temperate", "boreal","tropical"),
                      selected = "temperate"),
          textInput("year", 
                       label = 'Years (comma separated)',
                        value = "1,2"),
          width=3
        ),
        mainPanel(fluidRow(
                    splitLayout(cellWidths = c("100%"), 
                                plotly::plotlyOutput("plot1",height = '100%')
                                )
                  ),
                  width=9
        )
      )
    ),
    
    tabPanel('Principal components',
             titlePanel("Principal components"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("site_pca", 
                             label = "Site",
                             choices = c("temperate", "boreal","tropical"),
                             selected = "temperate"),
                 width=3
               ),
               mainPanel(
              plotly::plotlyOutput("plot2",height = '100%'),
              plotly::plotlyOutput("plot3",height = '100%'),
               width=9
               )
             )
    ),  
    
  )
)


# Server logic ----
server <- function(input, output) {
  
      output$plot1 <- renderPlotly({
            lista <- strsplit(input$year,',')[[1]]
            lplots<- vector(mode='list', length=3*length(lista))
            # load data of the site
            data <- switch(input$site, 
                           "boreal" = feat_boreal,
                           "temperate" = feat_temperate,
                           "tropical" = feat_tropical)
            data_rec <- switch(input$site, 
                               "boreal" = rec_boreal,
                               "temperate" = rec_temperate,
                               "tropical" = rec_tropical)
            for (i in 1:length(lista)){
                iy <- strtoi(lista[i])
                ppt <- data[iy,1,1:17]
                at <- data[iy,2,1:17]
                par <- data[iy,3,1:17]
                ppt_rec <- data_rec[iy,1,1:17]
                at_rec <- data_rec[iy,2,1:17]
                par_rec <- data_rec[iy,3,1:17]
                ylim_a <- min(min(ppt),min(at),min(par),min(ppt_rec),min(at_rec),min(par_rec))
                ylim_b <- max(max(ppt),max(at),max(par),max(ppt_rec),max(at_rec),max(par_rec))
                for (j in 1:3){
                     color <- switch(j, 
                                 "1" = 'blue',
                                 "2" = 'orange',
                                 "3" = 'green')
                     driver <- switch(j, 
                                     "1" = ppt,
                                     "2" = at,
                                     "3" = par)
                     driver_rec <- switch(j, 
                                      "1" = ppt_rec,
                                      "2" = at_rec,
                                      "3" = par_rec)
                     month <- -5:11
                     df <- data.frame(month,driver)
                     df_rec <- data.frame(month,driver_rec)
                     if (i==1){
                          text <- switch(j, 
                                      "1" = 'PPT',
                                      "2" = 'AT',
                                      "3" = 'PAR')
                          lplots[[(i-1)*3+j]] <- ggplotly(
                                  plot_reconstructed(df,df_rec,color)+ylim(ylim_a, ylim_b), 
                                  height = 400*length(lista)
                                 ) %>% add_annotations(
                           text = paste(text,". Year: ",lista[i]),
                           x = 0,
                           y = 1,
                           yref = "paper",
                           xref = "paper",
                           xanchor = "left",
                           yanchor = "top",
                           yshift = 20,
                           showarrow = FALSE,
                           font = list(size = 15)
                         ) }
                     else {
                       lplots[[(i-1)*3+j]] <- ggplotly(plot_reconstructed(df,df_rec,color)+ylim(ylim_a, ylim_b), 
                                                      height = 400*length(lista))%>% add_annotations(
                                                        text = paste("Year: ",lista[i]),
                                                        x = 0,
                                                        y = 1,
                                                        yref = "paper",
                                                        xref = "paper",
                                                        xanchor = "left",
                                                        yanchor = "top",
                                                        yshift = 20,
                                                        showarrow = FALSE,
                                                        font = list(size = 15)
                                                      )
                     }
                }
            }
            if (length(lista)==2){
              subplot(lplots, nrows = length(lista), margin = 0.03)
            } else {
              subplot(lplots, nrows = length(lista))
            }
      })
    
      
      output$plot2 <- renderPlotly({
        yintercept <- switch(input$site_pca, 
                         "temperate" = 0.075,
                         "boreal" = 0.075,
                         "tropical" = 0.06)        
        ggplotly(plot_feat_imp(input$site_pca)
                 + geom_point()
                 + geom_hline(yintercept=yintercept, linetype="dashed", color = "red"), 
                 height = 300) %>% add_annotations(
                   text = paste("Feature importance"),
                   x = 0,
                   y = 1,
                   yref = "paper",
                   xref = "paper",
                   xanchor = "left",
                   yanchor = "top",
                   yshift = 20,
                   showarrow = FALSE,
                   font = list(size = 15)
                 )
      })
      
      
      output$plot3 <- renderPlotly({
        chosen <- switch(input$site_pca, 
                        "temperate" = list(c(1,3,2)),
                        "boreal" = list(c(1,5,11,4)),
                        "tropical" = list(c(3,1,2,6,10)))
        ncomp <- length(chosen[[1]])
        nrows <- switch(input$site_pca, 
                        "temperate" = 1,
                        "boreal" = 1,
                        "tropical" = 2)
        height <- switch(input$site_pca, 
                         "temperate" = 300,
                         "boreal" = 300,
                         "tropical" = 600)
        lplots<- vector(mode='list', length=ncomp)
        for (i in 1:ncomp){
            text <- paste(chosen[[1]][i])
            if (i==1) {
              text <- paste(text,"| BLUE : PPT, ORANGE: AT, GREEN: PAR")
            }
            lplots[[i]]<-ggplotly( plot_site_vector(input$site_pca,chosen[[1]][i]), height=height)  %>% add_annotations(
                                  text = text,
                                  x = 0,
                                  y = 1,
                                  yref = "paper",
                                  xref = "paper",
                                  xanchor = "left",
                                  yanchor = "top",
                                  yshift = 20,
                                  showarrow = FALSE,
                                  font = list(size = 15)
                                )
        }
        subplot(lplots, nrows = nrows, margin = 0.03)
      })
      
}
# Run app ----
shinyApp(ui, server)