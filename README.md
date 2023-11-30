
# โค้ด Shiny App
library(readxl)
library(flexdashboard)
library(shiny)
library(ggplot2)
library(ggalluvial)
library(plotly)
library(igraph)
library(networkD3)
library(dbscan)
library(rcartocolor)
library(treemapify)
library(DT)
library(wesanderson)
library(tidyr)
library(dplyr)
library(shinythemes)
library(stringr)
  
# สร้าง UI
ui <- fluidPage(
  titlePanel("คะแนนสอบกลางภาคและปลายภาค"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("template"),
      fileInput(inputId = 'file', label = 'โปรด upload ไฟล์ MS excel ของท่าน', #input$file1
                accept = c(".xlsx"),
                placeholder = "ท่านยังไม่ได้ upload file ใด ๆ " ),
      actionButton("goButton", "Analyze")),
    #แสดงguage
    mainPanel(
      conditionalPanel(
          condition = "input.goButton > 0",
        column(gaugeOutput("guage1"),width=3),
        column(gaugeOutput("guage2"),width=3),
        column(gaugeOutput("guage3"),width=3)),
      #แสดงdrop down
      conditionalPanel(
        condition = "input.goButton > 0",
        mainPanel(column(
           selectInput(inputId="charts1",
                                 choices=c("รวม","2561","2562","2563","2564","2565"),
                                 label=c("เลือกปีการศึกษา"),width="100%"),
            selectInput(inputId="charts2",
                                 choices=c("รวม","1","2","3","4","5","6","7"),
                                 label=c("เลือกห้อง"),width="100%"),
      #แสดงscatter plot
          plotlyOutput("plot1"), width= 12),
      #แสดงTreemap Aตกคู่ Bตกmid Cตกfin Dผ่านคู่
          column(plotOutput("treemap"),width = 12, height = "300px"))
      ,
      
        #แสดงdrop down
        mainPanel(#fluidRow(
        column(6, selectInput(inputId="charts3",
                                                 choices=c("รวม","2561","2562","2563","2564","2565"),
                                                 label=c("เลือกปีการศึกษา ชุดที่ 1"))
        ),
        column(6,selectInput(inputId="charts4",
                             choices=c("รวม","1","2","3","4","5","6","7"),
                             label=c("เลือกห้อง ชุดที่ 1"))
        ),
        column(6, selectInput(inputId="charts5",
                               choices=c("รวม","2561","2562","2563","2564","2565"),
                               label=c("เลือกปีการศึกษา ชุดที่ 2"))
        ),
        column(6,selectInput(inputId="charts6",
                    choices=c("รวม","1","2","3","4","5","6","7"),
                    label=c("เลือกห้อง ชุดทึ่ 2")) 
        )),
     
        
        #แสดงboxplot และ MCMC
        column(plotlyOutput("plot2"), width=6),
        column(plotlyOutput("plot3"), width=6),
        column(plotlyOutput("plot4"), width=12,height = "300px"),
        #แสดงtable
          DTOutput(outputId = "table", width = 10))
    )
  )
)


    




# สร้าง Server
server <- function(input, output){
  
  
  data <- reactive({
    inFile <- input$file
    if (is.null(inFile))
      return()
    read_excel(inFile$datapath)
  })
  
  url<-a("template", href = "https://docs.google.com/spreadsheets/d/1_sGD7vBX0x2w8vq33bhRD0ttC_VzuBVsx8D2gKostqE/edit#gid=0")
  output$template <- renderUI({
    tagList("ดาวน์โหลด", url,"สำหรับกรอกข้อมูล")
  })
  #gauge1
  output$guage1 <- renderGauge({
    #จัดการข้อมูล
    dat <- data()
    
    
    if (is.null(dat)) {
      return()
    }
   
    #filter gauge1
    mid_risk = dat %>% 
      filter(mid < 15 ) %>% 
      count() %>%
      as.numeric()
    
    gauge(value = mid_risk,
          min = 0,
          max = dim(dat)[1],
          label = "ไม่ผ่านกลางภาค",
          sectors = gaugeSectors(
            success = c(0,0), 
            warning = c(0,31), 
            danger = c(32,32)
          ))
  })
  
  #gauge2
  output$guage2 <- renderGauge({
    #จัดการข้อมูล
    dat <- data()
    
    
    if (is.null(dat)) {
      return()
    }
    
    #filter gauge2
    fin_risk = dat %>% 
      filter(fin < 15 ) %>% 
      count() %>%
      as.numeric()
    
    gauge(value = fin_risk,
          min = 0,
          max = dim(dat)[1],
          label = "ไม่ผ่านปลายภาค",
          sectors = gaugeSectors(
            success = c(0,0), 
            warning = c(0,31), 
            danger = c(32,32)
          ))
  })
  
 #gauge3 
  output$guage3 <- renderGauge({
    #จัดการข้อมูล
    dat <- data()
    
    
    if (is.null(dat)) {
      return()
    }
    
    #filter gauge3
    all_risk = dat %>% 
      filter(all < 30 ) %>% 
      count() %>%
      as.numeric()
    
    gauge(value = all_risk,
          min = 0,
          max = dim(dat)[1],
          label = "ไม่ผ่านคะแนนรวม",
          sectors = gaugeSectors(
            success = c(0,0), 
            warning = c(0,31), 
            danger = c(32,32)
          ))
  })
  
  #plot1
  output$plot1<-renderPlotly({
    dat <- data()
    
    
    if (is.null(dat)) {
      return()
    }
    
    selected_data1 <- switch(input$charts1,
                             
                             "รวม" = dat,
                             "2561" = dat %>% filter(year == "2561"),
                             "2562" = dat %>% filter(year == "2562"),
                             "2563" = dat %>% filter(year == "2563"),
                             "2564" = dat %>% filter(year == "2564"),
                             "2565" = dat %>% filter(year == "2565"))
                             
    selected_select_data1 <- switch(input$charts2,
                             "รวม" = selected_data1,   
                             "1" = selected_data1 %>% filter(room == "1"),
                             "2" = selected_data1 %>% filter(room == "2"),
                             "3" = selected_data1 %>% filter(room == "3"),
                             "4" = selected_data1 %>% filter(room == "4"),
                             "5" = selected_data1 %>% filter(room == "5"),
                             "6" = selected_data1 %>% filter(room == "6"),
                             "7" = selected_data1 %>% filter(room == "7"))
                             
    ggplot(selected_select_data1, aes(x = mid, y = fin, colour = gender)) +
      geom_point() +
      ggtitle("แผนภาพแสดงการกระจายของข้อมูล")+
      theme(text = element_text(family = "ChulaCharasNew"))
  })
  
  #treemap
  output$treemap<-renderPlot({
    dat <- data()
    
    
    if (is.null(dat)) {
      return()
    }
    
    selected_data1 <- switch(input$charts1,
                             
                             "รวม" = dat,
                             "2561" = dat %>% filter(year == "2561"),
                             "2562" = dat %>% filter(year == "2562"),
                             "2563" = dat %>% filter(year == "2563"),
                             "2564" = dat %>% filter(year == "2564"),
                             "2565" = dat %>% filter(year == "2565"))
    
    selected_select_data1 <- switch(input$charts2,
                                    "รวม" = selected_data1,   
                                    "1" = selected_data1 %>% filter(room == "1"),
                                    "2" = selected_data1 %>% filter(room == "2"),
                                    "3" = selected_data1 %>% filter(room == "3"),
                                    "4" = selected_data1 %>% filter(room == "4"),
                                    "5" = selected_data1 %>% filter(room == "5"),
                                    "6" = selected_data1 %>% filter(room == "6"),
                                    "7" = selected_data1 %>% filter(room == "7"))
   
    midfin1 <- selected_select_data1
   
    midfin1$mid <- as.numeric(midfin1$mid) 
    midfin1$fin <- as.numeric(midfin1$fin) 
    midfin1$midmid = ifelse(midfin1$mid < 15,0,1) 
    midfin1$finfin =  ifelse(midfin1$fin < 15,0,1)
    midfin1$year <- as.character(midfin1$year)
    midfin1$midmid <- as.numeric(midfin1$midmid)
    midfin1$finfin <- as.numeric(midfin1$finfin)
    
    midfin1 |> select(code,year,room,midmid,finfin) |>
      mutate(count = midmid + finfin) |>
      group_by(midmid,finfin)%>%
      count() %>%
      ungroup()%>%
      arrange(midmid,finfin)%>%
      mutate(group = LETTERS[1:nrow(.)]) |>
      pivot_longer(cols = c("midmid","finfin"),
                   names_to = "pass_type",
                   values_to = "pass_value")%>%
      mutate(pass_type = str_replace(pass_type,"midmid",
                                     "กลางภาค"))%>%
      mutate(pass_type = str_replace(pass_type,"finfin",
                                     "ปลายภาค"))%>%
      ggplot(aes(area = n, fill = group, subgroup = group, label = pass_type))+
      geom_treemap(alpha = 0.9)+
      geom_treemap_subgroup_border(col = "white")+
      geom_treemap_text(family = "ChulaCharasNew", padding.y = grid::unit(4, "mm"))+
      geom_treemap_subgroup_text(place = "centre", grow = T,
                                 alpha = 0.4, colour ="white", min.size = 0)+
      scale_fill_carto_d(palette = 4)+
      theme(legend.position = "none")+
    ggtitle("Treemap แสดง Aตกคู่ Bตกmid Cตกfin Dผ่านคู่")
  })
  
  #plot2
  output$plot2<-renderPlotly({
    dat <- data()
    
    
    if (is.null(dat)) {
      return()
    }
    
    selected_data3 <- switch(input$charts3,
                             "รวม" = dat,
                             "2561" = dat %>% filter(year == "2561"),
                             "2562" = dat %>% filter(year == "2562"),
                             "2563" = dat %>% filter(year == "2563"),
                             "2564" = dat %>% filter(year == "2564"),
                             "2565" = dat %>% filter(year == "2565"))
    
    selected_select_data3 <- switch(input$charts4,
                                    "รวม" = selected_data3,   
                                    "1" = selected_data3 %>% filter(room == "1"),
                                    "2" = selected_data3 %>% filter(room == "2"),
                                    "3" = selected_data3 %>% filter(room == "3"),
                                    "4" = selected_data3 %>% filter(room == "4"),
                                    "5" = selected_data3 %>% filter(room == "5"),
                                    "6" = selected_data3 %>% filter(room == "6"),
                                    "7" = selected_data3 %>% filter(room == "7"))

    midfin2 <- selected_select_data3
    midfin2$mid <- as.numeric(midfin2$mid) 
    midfin2$fin <- as.numeric(midfin2$fin) 
    
    midfin2 |> select(code,year,room,mid,fin) |> 
      pivot_longer(cols=4:5, names_to = "round", values_to = "score") |>
      mutate(round = factor(round, 
                            levels=c("mid","fin"),
                            labels=c("คะแนนกลางภาค","คะแนนปลายภาค"))
             ,score = as.numeric(score)) |>
      ggplot(aes(x = round, y = score, fill = round)) +
      geom_boxplot() +
      geom_point() +
      theme(text = element_text(family = "ChulaCharasNew"))+
      scale_y_continuous(limits = c(0, 20))+
      ggtitle("boxplotแสดงการแจกแจงของข้อมูลชุดที่ 1")
      #+theme(legend.position = "none")
  }) 
  
  #plot3
  output$plot3<-renderPlotly({
    dat <- data()
    
    
    if (is.null(dat)) {
      return()
    }
    
    selected_data5 <- switch(input$charts5,
                             "รวม" = dat,
                             "2561" = dat %>% filter(year == "2561"),
                             "2562" = dat %>% filter(year == "2562"),
                             "2563" = dat %>% filter(year == "2563"),
                             "2564" = dat %>% filter(year == "2564"),
                             "2565" = dat %>% filter(year == "2565"))
    
    selected_select_data5 <- switch(input$charts6,
                                    "รวม" = selected_data5,   
                                    "1" = selected_data5 %>% filter(room == "1"),
                                    "2" = selected_data5 %>% filter(room == "2"),
                                    "3" = selected_data5 %>% filter(room == "3"),
                                    "4" = selected_data5 %>% filter(room == "4"),
                                    "5" = selected_data5 %>% filter(room == "5"),
                                    "6" = selected_data5 %>% filter(room == "6"),
                                    "7" = selected_data5 %>% filter(room == "7"))
    
    midfin3 <- selected_select_data5
    midfin3$mid <- as.numeric(midfin3$mid) 
    midfin3$fin <- as.numeric(midfin3$fin) 
    
    midfin3 |> select(code,year,room,mid,fin) |> 
      pivot_longer(cols=4:5, names_to = "round", values_to = "score") |>
      mutate(round = factor(round, 
                            levels=c("mid","fin"),
                            labels=c("คะแนนกลางภาค","คะแนนปลายภาค"))
             ,score = as.numeric(score)) |>
      ggplot(aes(x = round, y = score, fill = round)) +
      geom_boxplot() +
      geom_point() +
      theme(text = element_text(family = "ChulaCharasNew"))+
      ggtitle("boxplotแสดงการแจกแจงของข้อมูลชุดที่ 2")+
      scale_y_continuous(limits = c(0, 20))
    
  }) 
  
  #plot4MCMC
  output$plot4<-renderPlotly({
    dat <- data()
    
    
    if (is.null(dat)) {
      return()
    }
    selected_data3 <- switch(input$charts3,
                             "รวม" = dat,
                             "2561" = dat %>% filter(year == "2561"),
                             "2562" = dat %>% filter(year == "2562"),
                             "2563" = dat %>% filter(year == "2563"),
                             "2564" = dat %>% filter(year == "2564"),
                             "2565" = dat %>% filter(year == "2565"))
    
    selected_select_data3 <- switch(input$charts4,
                                    "รวม" = selected_data3,   
                                    "1" = selected_data3 %>% filter(room == "1"),
                                    "2" = selected_data3 %>% filter(room == "2"),
                                    "3" = selected_data3 %>% filter(room == "3"),
                                    "4" = selected_data3 %>% filter(room == "4"),
                                    "5" = selected_data3 %>% filter(room == "5"),
                                    "6" = selected_data3 %>% filter(room == "6"),
                                    "7" = selected_data3 %>% filter(room == "7"))
    
    midfin4 <- selected_select_data3
    midfin4$all <- as.numeric(midfin4$all) 
    midfin4$all <- as.numeric(midfin4$all) 
    
    selected_data5 <- switch(input$charts5,
                             "รวม" = dat,
                             "2561" = dat %>% filter(year == "2561"),
                             "2562" = dat %>% filter(year == "2562"),
                             "2563" = dat %>% filter(year == "2563"),
                             "2564" = dat %>% filter(year == "2564"),
                             "2565" = dat %>% filter(year == "2565"))
    
    selected_select_data5 <- switch(input$charts6,
                                    "รวม" = selected_data5,   
                                    "1" = selected_data5 %>% filter(room == "1"),
                                    "2" = selected_data5 %>% filter(room == "2"),
                                    "3" = selected_data5 %>% filter(room == "3"),
                                    "4" = selected_data5 %>% filter(room == "4"),
                                    "5" = selected_data5 %>% filter(room == "5"),
                                    "6" = selected_data5 %>% filter(room == "6"),
                                    "7" = selected_data5 %>% filter(room == "7"))
    
    midfin5 <- selected_select_data5
    midfin5$all <- as.numeric(midfin5$all) 
    midfin5$all <- as.numeric(midfin5$all) 
    
    selected_name1 <- switch(input$charts3,
                             "รวม" = "รวม",
                             "2561" = "2561",
                             "2562" = "2562",
                             "2563" = "2563",
                             "2564" = "2564",
                             "2565" = "2565")
    
    selected_name2 <- switch(input$charts4,
                             "รวม" =  "รวม",    
                             "1" = "1",
                             "2" = "2",
                             "3" = "3",
                             "4" = "4",
                             "5" = "5",
                             "6" = "6",
                             "7" = "7")
    selected_name3 <- switch(input$charts5,
                             "รวม" = "รวม",
                             "2561" = "2561",
                             "2562" = "2562",
                             "2563" = "2563",
                             "2564" = "2564",
                             "2565" = "2565")
    
    selected_name4 <- switch(input$charts6,
                             "รวม" =  "รวม",    
                             "1" = "1",
                             "2" = "2",
                             "3" = "3",
                             "4" = "4",
                             "5" = "5",
                             "6" = "6",
                             "7" = "7")
    
    ind_model<-"model{

# Likelihood for the two samples
  for (i in 1:N1) {
    y1[i] ~ dnorm(mu[1], 1 / sigma^2)
  }

  for (j in 1:N2) {
    y2[j] ~ dnorm(mu[2], 1 / sigma^2)
  }


# different between group
diff <- mu[2] - mu[1]

for (j in 1:2)
{
mu[j]~dnorm(0,0.01)
}
sigma ~dunif(0,200)

}"
    
    y1<-midfin4$all[midfin4$room==selected_name2]
    y2<-midfin5$all[midfin5$room==selected_name4]
    N1<-length(y1)
    N2<-length(y2)
    
    library(rjags)
    sector_compare<- jags.model(file = textConnection(ind_model), 
                                data=list(y1=y1,y2=y2,N1=N1,N2=N2),
                                n.chains = 4)
    
    sector_sim <- coda.samples(sector_compare,
                               variable.names = c("mu","sigma","diff"),
                               n.iter = 10000,
                               thin = 4)
    
    sector_chain<-data.frame(sector_sim[[1]]) 
    
    
    sector_chain %>%
      pivot_longer(cols=c('mu.1.', 'mu.2.'),
                   names_to='mu',
                   values_to='mean') %>%
      ggplot(aes(x=mean,col=mu))+
      scale_color_manual(name="",values=c("pink","blue"),labels=c("5","6"))+
      geom_density()+
      theme_light()+
      theme(legend.position = "none")+
      ggtitle("กราฟแสดงการแจกแจงระหว่างชุดข้อมูลที่ 1(สีชมพู) และ 2(สีน้ำเงิน) จากการทำ MCMC")
    
  }) 
  
  #table
  output$table <- renderDT({
    datatable(data(), options = list(paging = FALSE, searching = FALSE))
  })
}

# รัน Shiny App
shinyApp(ui, server)


