# # #US Region Type Comparison Page ##################################################################
# 
# 
# 
library(shinydashboard)

 
dashboardPage( skin = "black",
  dashboardHeader(title = "Daniel Kim"),
dashboardSidebar(
sidebarMenu(
  menuItem("Breaches by Location", tabName = "businesstab", icon = icon("chart-bar")),
  menuItem("Targets and Methods ", tabName = "breachtab", icon = icon("chart-bar")),
  menuItem("Breach Cost", tabName = "costtab", icon = icon("dollar-sign")),
  menuItem("Breach Facts", tabName = "hometab", icon = icon("dashboard")),
  menuItem("Data", tabName = "datatab", icon = icon("database")),
  menuItem("About Me", tabName = "abouttab", icon = icon("user"))
    )),
  dashboardBody(  useShinyjs(),
    tabItems(
      #Overview ####
      # Home Tab #####
      tabItem(tabName="hometab",
              # cat(file=stderr(), "1st BROWSER bins\n"),
              h2("Global Statistics"),
              h3("What companies experience the largest data breaches?"),
              fluidRow(column(width= 4,p(style="font-size:25px",strong("Highest loss")),
                       valueBoxOutput("maxcompany",width = "50%")),
                column(width= 4, offset=2,p(style="font-size:25px", strong("Total Records")),
                       valueBox('6,219,819,956', p(style="font-size:25px",strong("United States")),
                                width = "100%",icon = icon("line-graph")))),
              fluidRow(highchartOutput("hcontainer", height = "500px")),
              fluidRow(selectInput(inputId = "global", "Choose a Year",choices = choice1, selected=2020)),
              fluidRow(column(width = 12, highchartOutput("hmaxcompany",height = "500px")))
      ),
      tabItem(tabName = "businesstab",
                titlePanel("Privacy Rights Clearinghouse: Chronology of Data Breaches"), tags$br(),
                h3("Total Breaches by Region and State"), tags$br(),
                sidebarLayout(
                  sidebarPanel(
                    tags$h2("Date Ranges"),tags$br(),h4("Select a Date Range"), tags$br(),
                    sliderInput(inputId ="daterange", "Date Ranges:", min =getdates("2005-01-10") , max = getdates("2019-10-25"),
                                value= c(getdates("2005-01-10"),getdates("2019-10-25")), timeFormat="%Y-%m"),
                    tags$h4("Select a Region"),
                    selectInput(inputId = "regions", label = "Choose a Region in the US",choices = regionchoices,
                                selected = regionchoices[[1]],
                                multiple = TRUE),
                    tags$p("Deselect a choice: Ctrl + left click"),
                    actionButton("reset", "Reset"),
                    tags$br(),
                    selectInput(inputId = "states",label = "Choose a states in the US",
                                choices = regionchoices ,selected = "New York",multiple = FALSE)),
                  mainPanel( fluidRow(tabName="Region",  plotlyOutput("regionbar",height = "500px"))
                            )),
        fluidRow(tabName="States", plotlyOutput("statebar",height = "500px")
                )),
      tabItem(tabName = "breachtab",
              fluidRow(
                boxPlus(
                  title = "Business Type Defintions", 
                  closable = FALSE, 
                  width = 6,
                  enable_label = TRUE,
                  # label_text = 1,
                  label_status = "danger",
                  status = "primary", 
                  solidHeader = TRUE, 
                  collapsible = TRUE,
                  collapsed = TRUE,
                  HTML(paste("<p>BSO	Businesses (Other)</p>",
                             "<p>BSR	Businesses (Retail/Merchant including Online Retail</p>",
                             "<p>EDU	Educational Institutions</p>",
                             "<p>MED	Healthcare, Medical Providers and Medical Insurance Services</p>",
                             "<p>NGO	Nonprofits</p>",
                             "<p>UNKN	Unknown</p>")),
              
                  # status="success",

                "A box with a solid black background"
              ),boxPlus(
                title = "Breach Types Definitions", 
                closable = FALSE, 
                width = 6,
                enable_label = TRUE,
                # label_text = 1,
                label_status = "danger",
                status = "primary", 
                solidHeader = TRUE, 
                collapsible = TRUE,
                collapsed = TRUE,
                HTML(paste("<p>CARD</p>",
                   "<p> Fraud Involving Debit and Credit Cards Not Via Hacking
                           (skimming devices at point-of-service terminals, etc.)</p>",
                   "<p>HACK</p>",
                  "<p> Hacked by an Outside Party or Infected by Malware</p>",
                   
                  "<p>INSD</p>",
                  "<p> Insider (employee, contractor or customer)</p>",
                   
                  "<p>PHYS</p>",
                   "<p>Physical (paper documents that are lost, discarded or stolen)</p>",
                   
                  "<p>PORT</p>",
                   "<p>Portable Device (lost, discarded or stolen laptop, PDA, smartphone, memory stick, CDs, hard drive, data tape, etc.)</p>",
                   
                  "<p>STAT</p>",
                  "<p> Stationary Computer Loss (lost, inappropriately accessed, discarded or stolen computer or server not designed for mobility)</p>",
                   
                  "<p>DISC</p>",
                   "<p>Unintended Disclosure Not Involving Hacking, Intentional Breach or 
                   Physical Loss (sensitive information posted publicly, mishandled or sent to 
                   the wrong party via publishing online, sending in an email, sending in a 
                   mailing or sending via fax) </p>",
                   
                  "<p>UNKN</p>",
                   "<p>Unknown (not enough information about breach to know</p>")),
                
                # status="success",
                
                "A box with a solid black background"
              )),
              fluidPage(
                titlePanel("Total Breaches by Business Type and Method"), tags$br(),
                sidebarLayout(
                  sidebarPanel(
                    h2("Date Ranges"),tags$br(), h4("Select a Date Range"), tags$br(),
                    sliderInput("brdaterange", "Date Ranges:", min =getdates("2005-01-10") , max = getdates("2019-10-25"),
                                value= c(getdates("2005-01-10"),getdates("2019-10-25")), timeFormat="%Y-%m"),
                    h4("Select a Business Type"),
                    selectInput(inputId = "btype", label = "Choose a Business Type in the US",
                                choices = boptions$abbrev, selected = boptions$abbrev[1],multiple = TRUE),
                    p("Deselect a choice: Ctrl + left click"),
                    actionButton("resetbus", "Reset Business Types"),
                    tags$br(),
                    tags$br(),
                    selectInput(inputId = "breachtype",label = "Choose a Breach Method",
                                choices = breach_options$abbrev, selected = breach_options$abbrev[1],multiple = TRUE),
                    actionButton("resetbreach", "Reset Breach Methods")),
                  mainPanel(
                    fluidRow(id = "inTabset", selected ="Business Types", type="tabs",
                             fluidRow(tabName="Breach Methods", plotlyOutput("breachmethodbar", height = "500px")),
                             fluidRow(tabName="Business Types", plotlyOutput("busbar", height = "500px"))

                  )
                ))))
      ,
      tabItem(tabName="costtab",
              h2("Cost Tab"),
              fluidRow(column(width=6,highchartOutput("hcontain2",height = "500px")),
                       column(width=6,highchartOutput("hcontain3",height = "500px"))),
              fluidRow(column(width = 12,highchartOutput("hcontain4",height = "500px"))),
              fluidRow(selectInput(inputId = "global4", "Choose a Statistic",
                                   choices = c("U.S. Cost"=6,"Global Costs"=8), selected = "U.S. Cost")),
              fluidRow(column(width=12,highchartOutput("hcontain5",height = "500px"))))
      ,
      tabItem(tabName="datatab",fluidRow(titlePanel("PRC Breach Data"), tags$br(),DTOutput("tgraph2", height = "100%"))),
      tabItem(tabName="abouttab",fluidRow(
        HTML(paste("<h1>My name is Daniel and I am learning about data</h1>",
                   "<h1>It is quite a journey!</h1>"))
        
        
      ))
    ))
    # fluidRow(
    #   # A static valueBox
    #   valueBox(10 * 2, "New Orders", icon = icon("credit-card")),
    # 
    #   # Dynamic valueBoxes
    #   valueBoxOutput("progressBox"),
    # 
    #   valueBoxOutput("approvalBox")
    # ),
    # fluidRow(
    #   # Clicking this will increment the progress amount
    #   box(width = 4, actionButton("count", "Increment progress"))
    # )
  )


