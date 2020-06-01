# Data Breach Statistics Dashboard
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.


server <- function(input, output,session) { 
  output$progressBox <- renderValueBox({
    valueBox(
      paste0(25 + input$count, "%"), "Progress", icon = icon("list"),
      color = "purple"
    )})
  

    # send data from R to Javascript
    # observeEvent(input$controller, {
    #   session$sendCustomMessage(
    #     type = "update-tabs",
    #     message = input$controller
    #   )
    # })
    # 
  # HOME VIS ####
  #Overview ##########################################################################

  output$hcontainer <- renderHighchart({

    statsdf4 <- statsdf %>%
      filter(data_id == 4 | data_id == 5) %>%
      select(data_id, Year,Half, Region, Total, data_category, data_title, survey_period)

    hchart(statsdf4,hcaes(x=Year,y=Total, group=Region),type="line",color=c("#00AFBB", "#E7B800")) %>%
      hc_exporting(enabled = TRUE) %>%
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                 shared = TRUE, borderWidth = 2) %>%
      hc_title(text="Annual number of data breaches and exposed records in the United States from 2005 to 2019 (in millions)",
               align="center") %>%

      hc_subtitle(text="Number of global data breaches pertaining to identity theft from 1st half 2013 to 1st half 2018",align="center") %>%
      hc_credits(enabled = TRUE,
                 text = "Source: Data Source: IBM; Ponemon Institute; Various sources (CPO Magazine);") %>%
      hc_add_theme(hc_theme_elementary())

})
# Overview Tab Home ####

  # Left Box
  output$maxcompany <- renderValueBox({
    topNumRec1 <- visdata1 %>%
      filter(year == input$global) %>%
      top_n(1,records_lost) %>%
      select(year, Entity, records_lost)
      valueBox(topNumRec1$Entity[[1]],
               paste(topNumRec1$records_lost[[1]]/1000000," million records exposed in",input$global , sep=' '),
               icon = icon("fire")) #, color = "teal"
    })

# Mid Char
output$hmaxcompany <- renderHighchart({
  highrank <- visdata1 %>%
    filter(year == input$global) %>%
    arrange(desc(records_lost)) %>%
    select(year, Entity, records_lost)

  hc <- hchart(highrank,type="column", hcaes(x = Entity, y=records_lost)) %>%
    hc_title(text="Companies with highest breach severity by year",align="center") %>%
    hc_xAxis(max = 5) %>%
    hc_add_theme(hc_theme_economist())
  hc
})

output$hcontain112 <- renderValueBox({
  infoBox(
    "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
    color = "purple"
  )

})
#
  output$lgraph<-renderDT({
    top2%>%
      filter(year == input$global) %>%
      select(Entity,records_lost, year)
  })


 pbreachre <- reactive({
    pbreach %>%
    filter(region %in% input$regions) %>%
      filter(Date >= floor_date(ymd(input$daterange[1]),unit="month") & Date <= ceiling_date(ymd(input$daterange[2]),unit="month"))
   })
  
  pbreachrestate <- reactive({
    pbreach %>%
      filter(region %in% input$states) %>%
      # filter(State %in% input$states) %>%
      filter(Date >= floor_date(ymd(input$daterange[1]),unit="month") & Date <= ceiling_date(ymd(input$daterange[2]),unit="month"))
  })

  output$maxregion <- renderValueBox({
    topNumRec1 <- visdata1 %>%
      filter(year == input$global) %>%
      top_n(1,records_lost) %>%
      select(year, Entity, records_lost)
    valueBox(topNumRec1$Entity[[1]],
             paste(topNumRec1$records_lost[[1]]/1000000," million records exposed in",input$global , sep=' '),
             icon = icon("fire")) #, color = "teal"
  })

   output$regionbar <- renderPlotly({

     p <- pbreachre() %>%
       group_by(Year, Month) %>%
       count(region,sort = TRUE, name = "Incidents")
     
     # s <- ggplot(p, aes(p$region,p$Incidents, fill = p$region)) +
     #   geom_bar(stat = "identity") +
     #   ggtitle("Group specific line chart") +
     #   # geom_text(aes(label=p$Incidents))
     #   # 
     # 
     # fig <- ggplotly(s)
     # fig
       plot_ly(x=p$region,y=p$Incidents, type = 'bar', color = ~p$region) %>%
       layout(title = 'Total Breaches by Region',barmode = 'stack')
       
   })

  #
  # # observeEvent(c(input$states,input$daterange),{
  output$statebar <- renderPlotly({
        p("daterange 2:")
        # print(input$states)
    s <- pbreachrestate() %>%
          # filter(Date >= floor_date(ymd(input$daterange[1]),unit="month") & Date <= ceiling_date(ymd(input$daterange[2]),unit="month")) %>%
          group_by(Year,Month) %>%
          count(State, sort = TRUE, name = "Incidents") %>% 
          arrange(Incidents)
      plot_ly(x=s$State,y=s$Incidents, type='bar', color = ~s$State) %>% 
      layout(barmode = 'stack', categoryorder = "array",
             categoryarray = ~s$Incidents)
    })
  
  
  
  onclick("reset", {
    updateSelectInput(session, "regions", selected = "")
  })
  onclick("showstates", {
    updateSelectInput(session, "states", selected = "")
  })
  

#   # Breach Type Comparison Page ###########################################################################
#
  pbreachre2 <- reactive({
    pbreach %>%
      filter(Business_Type %in% input$btype) %>%
      filter(Date >= floor_date(ymd(input$brdaterange[1]),unit="month") & Date <= ceiling_date(ymd(input$brdaterange[2]),unit="month"))
  })
  
  pbreachrtype2 <- reactive({
    pbreach %>%
      filter(Business_Type %in% input$btype) %>%
      filter(Breach_Method %in% input$breachtype) %>%
      filter(Date >= floor_date(ymd(input$brdaterange[1]),unit="month") & Date <= ceiling_date(ymd(input$brdaterange[2]),unit="month"))
  })
  # summarize(Incidents = n(), Max_Loss = max(TotalRecords), Min_loss = min(TotalRecords), Avg_loss = mean(TotalRecords)) %>%
  
  
  observe({output$busbar <- renderPlotly({
    p("daterange 2:")
    s <- pbreachre2() %>%        group_by(Year,Month) %>%
      group_by(Year,Month) %>%
      count(Business_Type, sort = TRUE, name = "Incidents") %>% 
      arrange(Incidents)
    plot_ly(x=s$Business_Type,y=s$Incidents, type='bar', color = ~s$Business_Type) %>% 
      layout(barmode = 'stack', categoryorder = "array",
             categoryarray = ~s$Incidents)
  })})

  observe({output$busbar <- renderPlotly({
      p("daterange 2:")
      s <- pbreachre2() %>%        group_by(Year,Month) %>%
        group_by(Year,Month) %>%
        count(Business_Type, sort = TRUE, name = "Incidents") %>% 
        arrange(Incidents)
      plot_ly(x=s$Business_Type,y=s$Incidents, type='bar', color = ~s$Business_Type) %>% 
        layout(barmode = 'stack', categoryorder = "array",
               categoryarray = ~s$Incidents)
    })})

  observe({output$breachmethodbar <- renderPlotly({
      p("daterange 2:")
    s <- pbreachrtype2() %>%
        group_by(Year,Month,Business_Type) %>%
        count(Breach_Method, sort = TRUE, name = "Incidents") %>%
        arrange(Incidents)
        # # plot_ly(s, labels = ~Breach_Method, values=s$Incidents, type='pie')
        # fig <- fig %>% count(s$Breach_Method, s$Incidents)
        # fig <- fig %>% plot_ly(x = ~Breach_Method, y = ~n, color = ~Breach_Method)
        # 
        # fig
        plot_ly(x=s$Breach_Method,y=s$Breach_Method, type='bar', color = ~s$Business_Type) %>%
          layout(barmode = 'stack', categoryorder = "array",
                 categoryarray = ~s$Incidents)
    })
  })
  
  
  
  
  # observe({output$breachmethodbar <- renderPlotly({
  #   p("daterange 2:")
  #   s <- pbreachrtype2() %>%
  #     group_by(Year,Month,Business_Type) %>%
  #     count(Breach_Method, sort = TRUE, name = "Incidents") %>%
  #     arrange(Incidents)
  #   plot_ly(x=s$Breach_Method,y=s$Incidents, type='bar', color = ~s$Breach_Method) %>%
  #     layout(barmode = 'stack', categoryorder = "array",
  #            categoryarray = ~s$Incidents)
  # })
  # })
  onclick("resetbus", {
    updateSelectInput(session, "btype", selected = "")
  })
  onclick("resetbreach", {
    updateSelectInput(session, "breachtype", selected = "")
  })
  

  
  #
  # observeEvent(c(input$breachtype,input$btype,input$brdaterange),{
  # output$detailstable = renderDT({
  #   # paste0(input$brdaterange[2] - input$brdaterange[1], " years are selected. <br/>",
  #   #        "Highest Record Lost: ", Max_Loss, " <br/>", "Average Loss losscountries in the dataset measured at ",
  #   #        length(unique(theData()$year)), " occasions.")
  #   # HTML("I want a line break here <br/> since the label is too long")
  # })})
  #

  # observeEvent(c(input$breachtype,input$btype,input$brdaterange),{
  observe({output$detailstable <- renderDT({
      pz <- pbreach %>%
        filter(Breach_Method %in% input$pbreachrtype2()) %>%
        filter(Business_Type %in% input$btype) %>%
        filter(Date >= floor_date(ymd(input$brdaterange[1]),unit="month") & Date <= ceiling_date(ymd(input$brdaterange[2]),unit="month")) %>%
        group_by(Year, Business_Type,Breach_Method) %>%
        summarize(Business_Description = unique(Business_Description) ,
                  Breach_Description = unique(Business_Description),
                  Incidents = n(), Max_Loss = max(TotalRecords), 
                  Min_loss = min(TotalRecords),
                  Avg_loss = mean(TotalRecords,na.rm = TRUE))
  count(Business_Type, sort=True, name='n_business_types') %>%
  count(Breach_Method, sort=True, name='n_breachmethod')
  })
      #
      # methcount1 <- pz %>%
      #   count(Breach_Method, sort=True, name='n_breachmethod')
      #
      # bcount2 <- pz %>%
      #   count(Business_Type,Breach_Method, sort=True, name='n_btypes_by_breachmethod')
      #
      # methcount2 <- pz %>%
      #   count(Breach_Method, Business_Type,sort=True, name='n_btypes_by_breachmethod')

      # hchart(pz, "scatter", hcaes(x = pz$n_business_types, y = pz$n_breachmethod, group = class))
      # 
      # highchart() %>%
      # hc_chart(type = "column") %>%
      # hc_xAxis(categories =  p$Date) %>%
      # hc_add_series(data =  p$Incidents,
      #               name = "temp data",colorByPoint = TRUE)%>%
      # hc_add_theme(hc_theme_google()) %>%
      # # hchart(p, "column", hcaes(x = Year, y = Incidents, group = Breach_Method)) %>%
      #   hc_add_theme(hc_theme_google())
  })





#
#   # Costs Page ###########################################################################
#
  # Show the values in an HTML table ----

  # Business Tab ####
  # output$bizcontain1 <- renderText()
  #


  output$hcontain2 <- renderHighchart({
    prc2top5 %>%
      rename(Year,BreachYear) %>%
      hchart(type = "column", hcaes(x = State, y = Incidents)) %>%
      hc_title(text="Breach Incidents by State 2005 to 2019",align="center") %>%
      # hchart(prc21,'column', hcaes(x = StateActual, y = Incidents, group = BreachYear)) %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_legend(align = "left", verticalAlign = "top",
                layout = "vertical", x = 0, y = 100)
  })



  output$hcontain2 <- renderHighchart({
    prc2top5 %>%
    # rename(Year, BreachYear) %>%
    hchart(type = "column", hcaes(x = State, y = Incidents)) %>%
      hc_title(text="Breach Incidents by State 2005 to 2019",align="center") %>%
    # hchart(prc21,'column', hcaes(x = StateActual, y = Incidents, group = BreachYear)) %>%
    hc_add_theme(hc_theme_google()) %>%
      hc_legend(align = "left", verticalAlign = "top",
                layout = "vertical", x = 0, y = 100)
  })

  output$hcontain3 <- renderHighchart({
      p1 <- prcbreach %>%
        mutate(Date_Public = MadePubDate, State_ = StateActual, Breach_Type = BreachType,Industry = OrganType,
               Total_Records = TotalRecords) %>%
        group_by(BreachYear,State_) %>%
        summarise(Year= unique(BreachYear), Total_Records = mean(Total_Records, na.rm = T))

    zchart <- hchart(p1,type="column", hcaes(x = Year, y=Total_Records)) %>%
      hc_title(text="Total Records Lost by Year",align="center") %>%
      hc_add_theme(hc_theme_economist())

    zchart
  })

  output$hcontain4 <- renderHighchart({
    s1<-statsdf %>%
      filter(data_id %in% c(6,8)) %>%
      select(Year,Region, Total, data_category, data_title, data_id)

    mychart <- s1 %>%
      filter(data_id == input$global4) %>%
      hchart(type="line", hcaes(x = Year, y=Total)) %>%
      hc_title(text="Cost per Lost Record",align="center") %>%
      hc_add_theme(hc_theme_538())
      mychart
  })
  output$hcontain5 <- renderHighchart({
    plast <- prcbreach %>%
      group_by(BreachType) %>%
      select(BreachType, TotalRecords, State, BreachYear, OrganType) %>%
      hchart('bar', hcaes(x = BreachType, y = TotalRecords, group = OrganType)) %>%
      hc_add_theme(hc_theme_538()) %>%
      hc_xAxis(fontSize=16) %>%
      hc_yAxis(fontSize=25) %>%
      hc_legend(align = "top", verticalAlign = "top",
                layout = "horizontal", x = 0, y = 400)
    plast

  })

#   #------------------raw tab####
  output$tgraph2<-renderDT({
    p <- prcbreach %>%
      # mutate(Date = MadePubDate, State_ = StateActual,Breach_Type = BreachType, Industry = OrganType, Total_Records = TotalRecords) %>%
      select(Date = MadePubDate, Year = BreachYear, Month,region,Company, State = StateActual,
             BreachType, Breach_desc, OrganType, Organ_desc, TotalRecords, region)
    DT::datatable(p,  extensions = c('Buttons','Scroller'),
                           options = list(#dom = 'BrtS',
                                          scrollY = 200,
                                          deferRender = TRUE,
                                          scrollX = TRUE,
                                          paging = TRUE,
                                          # autoWidth =TRUE,
                                          pageLength=50,
                                          responsive=TRUE
                           ),width = '100%',height = '100%',
                           fillContainer = TRUE
    )
    # filter(year == input$global) %>%
  })
# 
# }
# 
# 
# 
# 
# #------------------About me tab####
# 

}

# 
# }

