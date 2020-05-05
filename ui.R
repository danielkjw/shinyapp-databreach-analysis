
# shiny ui block ####
shinyUI(
    dashboardPage(
    dashboardHeader(title='Data Breach Analysis'),
    # Sidebar block #####
    dashboardSidebar(
        #sidebarpanel
        sidebarUserPanel("Daniel Kim"),
        sidebarMenu(
            menuItem("Where is My Data?", tabName = "hometab", icon = icon("dashboard")),
            menuItem("U.S. Breach Risk & Cost", tabName = "costtab", icon = icon("dollar-sign")),
            menuItem("Breach Methods ", tabName = "analysistab", icon = icon("chart-bar")),
            menuItem("Data", tabName = "datatab", icon = icon("database")),
            menuItem("About Me", tabName = "abouttab", icon = icon("user"))
            
        )
    ),
    # Dashboard Body #####
    dashboardBody(
        #dashboard body ####
        shinyDashboardThemes(
            theme = "blue_gradient"),
        tabItems(
            #Overview ####
            tabItem(tabName="hometab", ####
                    fluidRow( #------------------ROW -------------------------
                        column(width=3, p(style="font-size:25px",strong("Highest loss")),
                        valueBoxOutput("maxcompany",width = "50%")),
                    column(width=6,offset = 2,p(style="font-size:25px", strong("Total Records Lost/Stolen")),
                    valueBox('6,219,819,956',p(style="font-size:25px", strong("United States")),width = "50%",
                             icon = icon("line-graph")))),
                    fluidRow(highchartOutput("hcontainer",height = "500px")
                        ),
                    fluidRow(selectInput(inputId = "global", "Choose a Year", 
                    choices = choice1, selected=2020 )),
                        # selectInput(inputId = "largest", "Choose a Year", 
                        #         choices = choice2 ),
                    fluidRow(column(width = 12, highchartOutput("hmaxcompany",height = "500px"))),
                    fluidRow(
                        box(width =12,
                            title = "Largest Breaches in History"
                            ,status = "primary"
                            ,solidHeader = TRUE 
                            ,collapsible = TRUE
                            ,DTOutput("lgraph")
                        ))
                    ),
          #-----------Cost Page #####  #------------------ROW -------------------------
                #Cost tab
            tabItem(tabName="costtab",
                    fluidRow(column(width=6,highchartOutput("hcontain2",height = "500px")),
                             column(width=6,highchartOutput("hcontain3",height = "500px")) 
                    ),
                    fluidRow(
                        column(width = 12,highchartOutput("hcontain4",height = "500px"))),
                    fluidRow(selectInput(inputId = "global4", "Choose a Statistic", 
                                         choices = c("U.S. Cost"=6,"Global Costs"=8), selected = "U.S. Cost")),
                    fluidRow(column(width=12,highchartOutput("hcontain5",height = "500px"))
                    )),
          tabItem(tabName="datatab",fluidRow(
              box(width =12,
                  title = "Raw Data"
                  ,status = "primary"
                  ,solidHeader = TRUE 
                  ,collapsible = TRUE
                  ,DTOutput("tgraph2")
              )),
              tabItem( tabItem(tabName="abouttab",
                                        fluidRow(
                                            infoBox("abouttab", 10 * 2, icon = icon("credit-card")))))
              
              # infoBoxes with fill=TRUE
              
          )
          
                
          
          )
        

    ))
)

