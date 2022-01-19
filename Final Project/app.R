
library(shiny)
library(shinythemes)
library(dplyr)
library(rgdal)
library(leaflet)
library(stargazer)

server <- function(input, output) {
  
  philly <- read.csv("Philly_schools.csv")
  colnames(philly)[colnames(philly) == "SCHOOL_ZIP"] <- "CODE"
  
  philly2 <- philly %>%
    group_by(CODE) %>%
    summarise(avg_attendance = round(mean(Attendance, na.rm = T),digits = 0),
              avg_low_inc_fam = round(mean(Low_income_family, na.rm = T), digits = 0),
              avg_total_sus = round(mean(Total_suspensions, na.rm = T), digits = 0))
  
  zip <- readOGR("Zipcodes_Poly", layer = "Zipcodes_Poly", encoding = "UTF-8")
  zip@data$CODE <- as.character(zip@data$CODE)
  zip@data$CODE <- as.numeric(zip@data$CODE)
  zip@data <- left_join(zip@data, philly2, by = "CODE")
  
  codepalette <- colorBin(palette = "YlOrRd", 
                          zip@data$avg_total_sus)
  
  zip_popup <- paste0("<strong>Zip Code: </strong>", 
                      zip@data$CODE, 
                      "<br><br><strong>Avg % of Attendance: </strong>", 
                      zip@data$avg_attendance,"%",
                      "<br><strong>Avg % Low Income Family: </strong>",
                      zip@data$avg_low_inc_fam,"%",
                      "<br><strong>Avg # of Suspensions: </strong>",
                      zip@data$avg_total_sus)
  
  map <- leaflet(zip) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(stroke = TRUE, 
                smoothFactor = 0.3, 
                fillOpacity = .8,
                fillColor = ~codepalette(avg_total_sus),
                color = "grey",
                weight = 1,
                popup = zip_popup) %>%
    addLegend("bottomright", 
              pal = codepalette,
              values = ~avg_total_sus,  
              title= "Average # of Suspensions: Lowest to Highest",
              opacity = 1)
  
  output$map <- renderLeaflet(map)

  #Regression Analysis
  
  inputformula <- reactive({
    
    as.formula(paste("Total_suspensions", " ~ ", paste(input$CODE2, collapse = "+")))
  })
  
  model <- reactive({
    lm(inputformula(), data = philly)
  })
  
  #Covariates Labelling
  
  covar.labels <- reactive({
    covars <- character()
    if ('Attendance' %in% input$CODE2) {
      covars <- c(covars, "Attendance %")
    }
    
    if ('Low_income_family' %in% input$CODE2) {
      covars <- c(covars, "Low income Family %")
    }
    
    if ('Drugs' %in% input$CODE2) {
      covars <- c(covars, "Drug Infractions per 100 Students")
    }
    if ('Withdrawals' %in% input$CODE2) {
      covars <- c(covars, "# of Withdrawals")
    }
    
    if ('Teacher_attendance' %in% input$CODE2) {
      covars <- c(covars, "Teachers Attendance")
    }
    
    if ('Gifted_education' %in% input$CODE2) {
      covars <- c(covars, "Students Receiving Gifted Educatione")
    }
    
    if ('Average_salary' %in% input$CODE2) {
      covars <- c(covars, "Average Teacher salary")
    }
    
    return(covars)
  })
  
  #Output to table format  
  output$table <- renderText({
    covars <- covar.labels()
    stargazer(model(), type = "html", 
              covariate.labels = covars, omit.stat = c("f","ser","aic","adj.rsq"),
              dep.var.labels = "Suspensions Prediction")
    
  })
}
  
#UI

ui <- shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  navbarPage(
    "Philadelphia School Districts Analysis",
    # First tab
    tabPanel(
      "Welcome",
        HTML('<center><img src = "philadelphia-skyline.jpg" width = "1600" height = "700"></center>'),
        br(),
      fluidRow(
        column(3),
        column(6,
               shiny::HTML("<br><br><center> <h1>What you'll find here</h1> </center><br>"),
               shiny::HTML("<h4>An interactive tool to help you explore the many school districts in Philadelphia and how socio-economic factors might play an impact on the number of suspensions in Philadelphia. 
                           Included in this application are an interactive map where you can find information on average attendance rate and various socio-economic factors associated with each Philadelphia zip code and a regression model that 
                           will allow you to predict the number of suspensions based on the socio-economics factors in a school district.</h5>")
        ),
        column(3)
      ),
      br(),
      h6("DISCLAIMER: Please note that the information presented here is for academic grading purposes only. The data presented here is part of the course materials provided by the Data Analytics Certificate Program of the University of Pennyslvania.")
    ),
    # Second tab
    tabPanel(
      "Interactive Map",
      headerPanel(h2("Interactive Map of Philadelphia School Districts", align = 'center')),
      br(),
      h4("(Left Click into each district to find more information on attendance rate and various socio-economic factors)", align = 'center'),
      leafletOutput("map",
                    width = "100%",
                    height = "600px")
          ),
    tabPanel(
      "Regressions Analysis",
      headerPanel(h3("Multivariate Regression Analysis on the Relationship between Socio-Economic Variables and Number of Suspensions in Philadelphia", align = 'center')),
      sidebarLayout(
        position = "left",
        sidebarPanel(
          h2("Start here"),
          br(),
          checkboxGroupInput(
            "CODE2",
            label = "Select any of the independent variables below to calculate the Suspensions Prediction Model. Select/Unselect your choices at any time.",
            c("Attendance %" = "Attendance",
              "Low Income Family %" = "Low_income_family",
              "Drug Infractions per 100 Students" = "Drugs",
              "# of Withdrawals" = "Withdrawals",
              "Teachers Attendance" = "Teacher_attendance",
              "Students Receiving Gifted Education" = "Gifted_education",
              "Average Teacher Salary" = "Average_salary"),
            selected = "Attendance"
          )
        ),
        mainPanel(br(),
                  tabsetPanel(
                    type = "tabs",
                    tabPanel(
                      "Regression Table",
                      h3("Table of Regression Coefficients"),
                      HTML('</br>'),
                      tableOutput("table")
                    )
                  )
        )
      )
    )
        )
      )
)


  

shinyApp(ui = ui, server = server)