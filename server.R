# Data Breach Statistics Dashboard
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.

# Define server logic required to draw a histogram
function(input, output) { 

  output$hcontainer <- renderHighchart({ ####
    statsdf4 <- statsdf %>%
      filter(data_id == 4 | data_id == 5) %>%
      select(data_id, Year,Half, Region, Total, data_category, data_title, survey_period)
    
    hchart(statsdf4,hcaes(x=Year,y=Total, group=Region),type="line",color=c("#00AFBB", "#E7B800")) %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                 shared = TRUE, borderWidth = 2) %>%
      hc_title(text="Annual number of data breaches and exposed records in the United States from 2005 to 2019 (in millions)",
               align="center") %>%
      
      hc_subtitle(text="Number of global data breaches pertaining to identity theft from 1st half 2013 to 1st half 2018
                  ",align="center") %>%
      hc_credits(enabled = TRUE,
                 text = "Source: Data Source: IBM; Ponemon Institute; Various sources (CPO Magazine);") %>% 
      hc_add_theme(hc_theme_elementary())



    # saved code ####
    # if (input$stacked != FALSE) {
    #   hc <- hc %>%
    #     hc_plotOptions(series = list(stacking = input$stacked))
    # }
    # 
    # if (input$theme != FALSE) {
    #   theme <- switch(input$theme,
    #                   null = hc_theme_null(),
    #                   darkunica = hc_theme_darkunica(),
    #                   gridlight = hc_theme_gridlight(),
    #                   sandsignika = hc_theme_sandsignika(),
    #                   fivethirtyeight = hc_theme_538(),
    #                   economist = hc_theme_economist(),
    #                   chalk = hc_theme_chalk(),
    #                   handdrwran = hc_theme_handdrawn()
    #   )
    #   
    #   hc <- hc %>% hc_add_theme(theme)
      
})
  

output$maxcompany <- renderValueBox({
  topNumRec1 <- visdata1 %>%
    filter(year == input$global) %>%
    top_n(1,records_lost) %>%
    select(year, Entity, records_lost) 

      valueBox(topNumRec1$Entity[[1]], paste(topNumRec1$records_lost[[1]]/1000000,
                                       " million records exposed in",input$global , sep=' '), 
               icon = icon("fire"), color = "teal")
  }) 
  
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
  
  # highchart() %>% ####
  #   hc_add_theme(hc_theme_ffx()) %>%
  #   hc_title(text = "Loans Funded By Day") %>%
  #   hc_add_series(data = daySummary, mapping = hcaes(x=as.Date(FundedDayGroup), y=TotalAmount), type = "column", name = "Daily Loan Revenue",
  #                 tooltip = list(pointFormat = "Daily Revenue ${point.TotalAmount} across {point.TotalCount} deals")) %>%
  #   hc_xAxis(type = "datetime", labels=list(rotation = -45, y = 40) ) %>%
  #   hc_yAxis(title=list(text = "Revenue")) %>%
  #   hc_tooltip(crosshairs = TRUE)

})
  
  output$lgraph<-renderDT({
    top2%>%
      # filter(year == input$global) %>% 
      select(Entity,records_lost, year)
  })

  # output$hist <- renderPlot({
  #   hist(histdata(),xlab="Value",
  #        main=paste(input$number,"random values between 0 and 1"))
  # })
  
  output$progressBox <- renderInfoBox({
    infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
      color = "purple"
    )
  })
  
#------------------cost tab#### --------------------------------------------------------------------------- x
  # Show the values in an HTML table ----
  
  #turn into state map #####

  
  #------------------cost table PRC #### --------------------------------------------------------------------------- x
  


  output$hcontain2 <- renderHighchart({
    prc2top5 %>%
    rename(Year = BreachYear) %>% 
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
  #-----
  
  
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
  
  #------------------raw tab####
  output$tgraph2<-renderDT({
    prcbreach %>%
      mutate(Date_Public = MadePubDate, State_ = StateActual,Breach_Type = BreachType, Industry = OrganType, Total_Records = TotalRecords) %>% 
      select(Date_Public,State_, Breach_Type,Industry,Total_Records)
    # filter(year == input$global) %>% 
  })

  
}




#------------------About me tab####






