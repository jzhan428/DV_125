library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(plotly)
library(tidyverse)
library(shinythemes)
library(ggthemes)
library(shinyWidgets)


loc_data = read_csv("location.csv")
companies =  loc_data %>%
  distinct(company) %>%
  pull(company)

R_data = read_csv("ROA_ROE.csv")

stockdata <- read_csv("stock price.csv")
stockdata$Date = as.Date(stockdata$Date, "%m/%d/%Y")
stockdata = pivot_longer(data = stockdata,cols = 2:6,names_to = "company",values_to = "price")
stockdata$price = round(stockdata$price, digits = 2)
companies_2 =  stockdata %>%
  distinct(company) %>%
  pull(company)

rawdata = read_csv("data.csv")
colnames(rawdata) = c("Year", "Company", "Revenue", "Income", "DilutedEPS")
rawdata$Revenue = round(rawdata$Revenue, digits = 2)
rawdata$Income = round(rawdata$Income, digits = 2)

Desc = read_csv("desc.csv")

ui <- dashboardPage(
  dashboardHeader(title = "Grocery Industry Overview"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Dashboard",
      tabName = "page1",
      icon = icon("fa-solid fa-database")
    ),
    menuItem("Map", tabName = "map", icon = icon("map-marked-alt")),
    menuItem(
      "Statistics",
      icon = icon("server"),
      menuSubItem("stock price", tabName = "page2"),
      menuSubItem("investment performace", tabName = "page3"),
      menuSubItem("fiancial perforamce", tabName = "page4")
    )
    
  )),
  
  
  
  dashboardBody(tabItems(
    tabItem(
      tabName = "page1",
      column(
        width = 12,
        img(src = "grocery.jpg", width = 600),
        align = "center"
      ),
      fluidRow(
        box(
          title = "Our Project's Mission",
          solidHeader = TRUE,
          status = "success",
          width = 12,
          collapsible = TRUE,
          column(12,
                 tags$div(
                   p(
                     "Our group conducts the data from 5 top companies in the grocery industry to visualize their Operation and financial
                               performance. By using our data visualization website, investors who are interested in this industry can get a comprehensive
                               understanding of these five companies and have some insights to optimize their future investment decisions."
                   ),
                   
                 ))
        )
      ),
      fluidRow(
        box(
          title = "Industry Overview",
          solidHeader = TRUE,
          status = "warning",
          width = 12,
          collapsible = TRUE,
          column(12,
                 tags$div(
                   p(
                     "The Supermarkets and Grocery Stores industry accounts
                              for the largest food retail channel in the United States.
                              Operators in this industry retail general lines of food products,
                              including fresh and prepared meats, poultry and seafood,
                              canned and frozen foods, fresh fruits and vegetables and various
                              dairy products."
                   ),
                   
                 ))
        )
      ),
      fluidRow(
        box(
          title = "Team Member",
          solidHeader = TRUE,
          status = "primary",
          width = 12,
          collapsible = TRUE,
          column(
            12,
            tags$li("1.", tags$strong("Jingyi Zhang"), "(jzhan296@jhu.edu) "),
            tags$li("2.", tags$strong("Yawen Zheng"), "(yzheng91@jhu.edu)"),
            tags$li("3.", tags$strong("Ziqing Jia"), "(zjia11@jhu.edu)"),
            tags$li("4.", tags$strong("Mingyue Lu"), "(mlu37@jhu.edu)"),
            tags$li("5.", tags$strong("Haoen Li"), "(hli171@jhu.edu)")
          )
        )
      )
      
      
      
      
      
    ),
    
    tabItem(
      tabName = "map",
      column(width = 12,
             selectInput(
               "select",
               label = h5("Select one company"),
               choices = companies
             )),
      
      column(width = 12,
             leafletOutput("myMap", width = "100%"),
             br()),

      fluidRow(column(6, uiOutput("logo")),
               column(6, uiOutput("desc")))
      
      
    ),
    
    
    tabItem(
      tabName = "page2",
      selectInput(
        "stock",
        label = h5("Choose a company"),
        choices = companies_2
      ),
      
      plotlyOutput("stockprice")
    ),
    
    tabItem(
      tabName = "page3",
      br(),
      br(),
      column(width = 6,
             plotlyOutput("ROA", width = "100%")),
      column(width = 6,
             plotlyOutput("ROE", width = "100%")),
      fluidRow(
        column(
          6,
          strong("Return on assets (ROA)"),
          p(
            "refers to a financial ratio that indicates how profitable a company is in relation to its total assets. Corporate management, analysts, and investors can use ROA to determine how efficiently a company uses its assets to generate a profit."
          ),
          br(),
          br(),
          img(src = "ROA.jpg", width = 300, align = "center")
        ),
        column(
          6,
          strong("Return on equity (ROE)"),
          p(
            "refers to  a measure of the profitability of a business in relation to the equity. Because shareholder's equity can be calculated by taking all assets and subtracting all liabilities, ROE can also be thought of as a return on assets minus liabilities. ROE measures how many dollars of profit are generated for each dollar of shareholder's equity. ROE is a metric of how well the company utilizes its equity to generate profits."
          ),
          img(src = "ROE.jpg", width = 300, align = "center")
        )
        
      )
    ),
    tabItem(
      tabName = "page4",
      br(),
      column(
        width = 6,
        sliderInput(
          "year",
          "Year:",
          min = 2015,
          max = 2021,
          value = 2015,
          step = 1,
          animate = animationOptions(interval = 1000, loop = FALSE)
        ),
        colorSelectorInput(
          inputId = "mycolor",
          label = "Pick a color :",
          choices = c(
            "steelblue",
            "cornflowerblue",
            "firebrick",
            "palegoldenrod",
            "forestgreen"
          )
        ),
        
        
      ),
      
      column(width = 6, plotlyOutput("DilutedEPS")),
      
      
      
      fluidRow(column(6, plotlyOutput("Revenue")),
               column(6, plotlyOutput("Net_Income")))
      
      
      
    )
    
    
  ))
)



server <- function(input, output, session) {
  loc_data_company = reactive({
    loc_data %>%
      filter(company == input$select)
  })
  
  
  fin_data = reactive(rawdata %>%
                        filter(Year == input$year))
  
  Desc_data = reactive(Desc %>%
                         filter(company == input$select))
  
  
  output$myMap = renderLeaflet({
    leaflet(data = loc_data_company())  %>%
      setView(-77.01182, 38.89852, zoom = 10) %>%
      addTiles() %>%
      addMarkers(lng = ~ lat, lat = ~ lng)
  })
  
  output$logo <- renderUI({
    return(div(
               img(
                 src = paste0(input$select, ".jpg"),
                 height = 200),
               align = "center",
               h2("Company Logo")
               ))
    
  })
  
  output$desc <- renderUI({
    return(div(
      h2("Company description"),
      Desc_data[text]
      )
    )
  })
  
  
  
  output$stockprice = renderPlotly({
    SubData_stock = stockdata %>%
      filter(company == input$stock)
    
    SubData_stock %>%
    ggplot(mapping = aes(x = Date, y =price)) +
      geom_line() +
      labs(x = "Date", y = "stock price")+
      theme_economist()
    
  })
  
  output$ROA = renderPlotly({
    ggplot(data = R_data, mapping = aes(x = Company, y = ROA)) +
      geom_bar(stat = "identity",
               fill = "lightblue",
               color = "black") +
      labs(x = "Company", y = "ROA") +
      ggtitle("Comparision of ROA of five companies") +
      theme_economist()
  })
  
  output$ROE = renderPlotly({
    ggplot(data = R_data, aes(x = Company, y = ROE)) +
      geom_bar(stat = "identity",
               fill = "lightblue",
               color = "black") +
      labs(x = "Company", y = "ROE") +
      ggtitle("Comparision of ROE of five companies") +
      theme_economist()
    
  })
  
  output$DilutedEPS = renderPlotly({
    ggplot(data = fin_data(), aes(x = Company, y = DilutedEPS)) +
      geom_bar(stat = "identity",
               fill = input$mycolor,
               color = "black") +
      labs(x = "Company", y = "") +
      ggtitle("Diluted Earning Per Share") +
      ylim(0, 15) +
      theme_clean()
  })
  
  output$Revenue = renderPlotly({
    ggplot(data = fin_data(), aes(x = Company, y = Revenue)) +
      geom_bar(stat = "identity",
               fill = input$mycolor,
               color = "black") +
      labs(x = "Company", y = "") +
      ggtitle("Total Revenue in Billions") +
      ylim(0, 600) +
      theme_clean()
  })
  
  output$Net_Income = renderPlotly({
    ggplot(data = fin_data(), aes(x = Company, y = Income)) +
      geom_bar(stat = "identity",
               fill = input$mycolor,
               color = "black") +
      labs(x = "Company", y = "") +
      ggtitle("Net Income in Billions") +
      ylim(0, 15) +
      theme_clean()
  })
}
shinyApp(ui = ui, server = server)
