
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

midfin <- read_excel("/Users/pariwatthamma/Downloads/midfin.xlsx")



  

  
# สร้าง UI
ui <- fluidPage(
  titlePanel("คะแนนสอบกลางภาคและปลายภาค"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("template"),
      fileInput(inputId = 'file', label = 'ขั้นที่ 2 upload ไฟล์ MS excel ของท่าน', #input$file1
                accept = c(".xlsx"),
                placeholder = "ท่านยังไม่ได้ upload file ใด ๆ " ),
      p("ขั้นที่ 3 กดปุ่ม Analyze เพื่อวิเคราะห์ข้อมูล"),
      actionButton("goButton", "Analyze"),
      br(),
      br(),
      h5("คำแนะนำสำหรับการกรอกข้อมูลในไฟล์ template"),
      HTML("
            <ul>
            <li> code = รหัสนักเรียนเป็นตัวเลข 5 ตัวเช่น 65191 เป็นต้น </li>
            <li> name = ชื่อนักเรียน ไม่ต้องใส่คำนำหน้า </li>
            <li> surname = นามสกุลนักเรียน </li>
            <li> gender = เพศนักเรียน </li>
                <ul>
                <li> male = เพศชาย </li>
                <li> female = เพศหญิง </li>
                </ul> 
            <li> year = ปีการศึกษานักเรียนเป็นตัวเลข 4 ตัวเช่น 2561 เป็นต้น </l>
            <li> room = ห้องเรียนตัวเลข 1 ตัว (เลข 1 - 7) </l>
            <li> mid = คะแนนกลางภาคเลข[ทศนิยม 1 ตำแหน่ง](คะแนนเต็ม 45 คะแนน) </l>
            <li> fin = คะแนนปลายภาคเป็นตัวเลข[ทศนิยม 1 ตำแหน่ง](คะแนนเต็ม 45 คะแนน) </l>
            <li> all = คะแนนปลายภาคเลข[ทศนิยม 1 ตำแหน่ง](คะแนนเต็ม 90 คะแนน) </l>
            <li> gpax = เกรดเฉลี่ยในรายวิชาเป็นตัวเลขทศนิยม 1 ตำแหน่ง) </l>
            </ul>
                 "),
      h5("คำแนะนำการอ่านค่าวิเคราะห์ข้อมูล"),
      HTML("
            <ul>
            <li> gauge  แสดงจำนวนนักเรียนที่คะแนนไม่ผ่านเกณฑ์ เป็นต้น </li>
            <li> scatter plot แสดงการกระจายของคะแนนนักเรียน </li>
            <li> treemap แสดงสัดส่วนนักเรียนแต่ละกลุ่ม </li>
                <ul>
                <li> นักเรียนผ่านกลางภาคและปลายภาค สีเขียวทั้งสองช่อง </li>
                <li> นักเรียนผ่านเฉพาะกลางภาค สีเขียวช่วงกลางภาค สีแดงช่องปลายภาค </li>
                <li> นักเรียนผ่านเฉพาะปลายภาค สีเขียวช่องปลายภาค สีแดงช่องกลางภาค </li>
                <li> นักเรียนไม่ผ่านกลางภาคและปลายภาค สีแดงทั้งสองช่อง </li>
                </ul> 
            <li> boxplot แสดงการเปรียบเทียบการกระจายข้อมูลนักเรียนแต่ละชุดข้อมูล </l>
            <li> graph แสดงการเปรียบเทียบการกระจายข้อมูลนักเรียนแต่ละชุด (สถิติแบบเบส์) </l>
            <li> table แสดงข้อมูลแบบตาราง </l>
            </ul>
                 "),
      uiOutput("contact"),
      width = 3),
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
          plotlyOutput("plot1"), width= 12, height = "300px"),
      #แสดงTreemap 
          column(plotOutput("treemap"),width = 12, height = "300px"))
      ,
      
        #แสดงdrop down
        mainPanel(#fluidRow(
        column(6, selectInput(inputId="charts3",
                                                 choices=c("รวม","2561","2562","2563","2564","2565"),
                                                 label=c("เลือกปีการศึกษา ชุดที่ 1"))
        ),
        column(6, selectInput(inputId="charts5",
                              choices=c("รวม","2561","2562","2563","2564","2565"),
                              label=c("เลือกปีการศึกษา ชุดที่ 2"))
        ),
        column(6,selectInput(inputId="charts4",
                             choices=c("รวม","1","2","3","4","5","6","7"),
                             label=c("เลือกห้อง ชุดที่ 1"))
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
          DTOutput(outputId = "table", width = "100%"))
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
  
  url_template<-a("template", href = "https://github.com/toeytho/toeytho.github.io/blob/main/midfin.xlsx")
  output$template <- renderUI({
    tagList("ขั้นที่ 1","ดาวน์โหลด", url_template,"สำหรับกรอกข้อมูล")
  })
  
  url_contact<-a("ติดต่อผู้จัดทำ", href ="https://www.facebook.com/toey.tho/")
  output$contact <- renderUI({
    tagList("งานนี้เป็นส่วนหนึ่งของรายวิชา Data Visualization ", url_contact)
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
    midfin1$midmid = ifelse(midfin1$mid < 14,0,1) 
    midfin1$finfin =  ifelse(midfin1$fin < 14,0,1)
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
      mutate(pass_value = str_replace(pass_value,"0",
                                      "notpass"))%>%
      mutate(pass_value = str_replace(pass_value,"1",
                                      "pass"))%>%
      ggplot(aes(area = n, fill = pass_value, subgroup = group, label = pass_type))+
      geom_treemap(alpha = 0.9)+
      geom_treemap_subgroup_border(col = "white")+
      geom_treemap_text(family = "ChulaCharasNew", padding.y = grid::unit(4, "mm"))+
      geom_treemap_subgroup_text(place = "centre", grow = T,
                                 alpha = 0.4, colour ="white", min.size = 0)+
      scale_fill_manual(values = c("#C5E898","#FA7070"))+
      theme(legend.position = "none")+
    ggtitle("เขียวผ่าน,แดงตก")
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
    
    dat <- data()
    
    if (is.null(dat)) {
      return()
    }
    
   tab <- dat %>% select(code,name,gender,year,room,mid,fin,gpax)
   
   tab <- tab %>% mutate(gender = factor(gender, levels=c("male","female"),
                               labels=c("ชาย","หญิง")))
   tab <- tab %>% mutate(gpax = factor(gpax,levels = c("0","1","1.5","2","2.5","3","3.5","4"),
                                       labels = c("0","1.0","1.5","2.0","2.5","3.0","3.5","4.0")))
      
      names(tab)[1]<-"รหัสนักเรียน" 
      names(tab)[2]<-"ชื่อนักเรียน"
      names(tab)[3]<-"เพศ"
      names(tab)[4]<-"ปีการศึกษา"
      names(tab)[5]<-"ห้อง"
      names(tab)[6]<-"กลางภาค"
      names(tab)[7]<-"ปลายภาค"
      names(tab)[8]<-"เกรดเฉลี่ยสะสม"
      
    datatable(tab, extensions = c('ColReorder','Responsive'), 
              options = list(colReorder = list(realtime = FALSE),
                             columnDefs = list(
                               list(className = 'dt-center', targets = '_all')
                             ))) %>%
      ### จัด format gender
      formatStyle(
        columns = 3,
        backgroundColor = styleEqual(
          levels = c("ชาย","หญิง"),
          values = c('navy', 'pink')
        ),
        color = styleEqual(
          levels = c("ชาย","หญิง"), 
          values = c("white","black")
        )
      ) %>%
      ### จัด format mid
      formatStyle(
        column = 6,
        backgroundColor = styleInterval(
          cuts = 14,  # ตัวแบ่ง 2 ค่า
          values = c("#FA7070","#C5E898")         
          )
      ) %>%
      ### จัด format fin
      formatStyle(
        column = 7,
        backgroundColor = styleInterval(
          cuts = 14,  # ตัวแบ่ง 2 ค่า
          values = c("#FA7070","#C5E898")  
        )
      )%>%
    ### จัด format gpax
    formatStyle(
      column = 8,
      backgroundColor = styleEqual(
        levels = c("0","1.0","1.5","2.0","2.5","3.0","3.5","4.0"),
        values = c("black","#511418","#7a1f23","#a2292f","#51464f","#00626e","#00444c","#00262a")  
      ),
      color = styleEqual(
        levels = c("0","1.0","1.5","2.0","2.5","3.0","3.5","4.0"),
        values = c("white","white","white","white","white","white","white","white")  
      )
    )
    
  })
}

# รัน Shiny App
shinyApp(ui, server)


