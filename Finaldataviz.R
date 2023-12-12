library(tidyr)
library(dplyr)
library(ggplot2)
library(treemapify)
library(rcartocolor)
library(ggrepel)
#item1.1

i1a <- c("ข้าราชการครูและบุคลากรทางการศึกษา","ข้าราชการพลเรือน","ครูเอกชน","พนักงานราชการหรือพนักงานของรัฐ","ลูกจ้างประจำ","ลูกจ้างชั่วคราว")
i1b <- c(22379,1980,106436,15267,532,18)
i1 <- data_frame(i1a,i1b)

i1$i1a <- factor(i1$i1a)
names(i1)[1]<- "name"
names(i1)[2]<- "number"
glimpse(i1)

pi1 <-i1 %>% ggplot(aes(area = number, fill = name, subgroup = number, label = name)) +
  geom_treemap(alpha = 0.9) +
  geom_treemap_subgroup_border(col = "white") +
  geom_treemap_text(family = "ChulaCharasNew", padding.y = grid::unit(4, "mm")) +
  geom_treemap_subgroup_text(place = "bottom", grow = T,
                             alpha = 0.4, colour = "white", min.size = 0) +
  scale_fill_carto_d(palette = 2) +
  theme(text = element_text(family = "ChulaCharasNew")) +
  annotate("point", x = 70, y = 2,
           colour = "purple", size = 2,alpha=0.5)+
  annotate(geom = "text", x=70, y=2, label = "ลูกจ้างชั่วคราว 18", 
           family = "ChulaCharasNew",  col = "maroon", size=3,  fontface = "bold")

pi1
  


#item1.2
i2a <- c("ภาคเอกชน","ภาครัฐ","เพิ่งสำเร็จการศึกษา","ศึกษาต่อในระดับที่สูงกว่าปริญญาตรี","อยู่ระหว่างติดตามข้อมูล","อยู่ระหว่างการรับทุน","ลาออก(ระหว่างรับทุน)","ธุรกิจส่วนตัว","พ้นสภาพนักเรียนทุน","อื่นๆ")
i2b <- c(455,171,97,73,72,35,32,18,15,31)
i2 <- data_frame(i2a,i2b)

i2$i2a <- factor(i2$i2a)
names(i2)[1]<- "name"
names(i2)[2]<- "number"

glimpse(i2)

pi2 <- i2 %>% 
  mutate(percent = sprintf("%.2f", 100 * (number / sum(number)))) %>%
  ggplot(aes(x = as.numeric(percent), y = reorder(name, as.numeric(percent)),col=number)) +
  geom_point() +
  geom_text_repel(aes(label = percent), nudge_x = 0.2) +
  geom_segment(aes(y = name, x = 0, yend = name, xend = as.numeric(percent)))+
    ylab("สถานะการประกอบอาชีพ")+
    xlab("ร้อยละจำนวนแต่ละกลุ่ม")+
  theme_minimal() +
    theme(text = element_text(family="ChulaCharasNew"))+
    ggtitle("แผนภาพประกอบข้อมูลสถานะการประกอบอาชีพ")+
  theme(legend.position = "none")+
  scale_color_gradient(low="pink",high="red")

pi2

#item1.3
i3a <- c("สาธารณูปโภคพื้นฐาน","การศึกษา","สุขภาพและสิ่งแวดล้อม","โครงสร้างพื้นฐานด้านวิทยาศาสตร์","โครงสร้างพื้นฐานด้านเทคโนโลยี","สาธารณูปโภคพื้นฐาน","การศึกษา","สุขภาพและสิ่งแวดล้อม","โครงสร้างพื้นฐานด้านวิทยาศาสตร์","โครงสร้างพื้นฐานด้านเทคโนโลยี")
i3b <- c(2562,2562,2562,2562,2562,2563,2563,2563,2563,2563)
i3c <- c(27,56,55,38,38,26,55,49,39,34)
i3 <- data_frame(i3a,i3b,i3c)

names(i3)[1]<- "name"
names(i3)[2]<- "year"
names(i3)[3]<- "score"

glimpse(i3)

pi3 <- i3 |>  pivot_wider(names_from = "year", values_from = "score") %>%  
  rename("year2562"='2562',"year2563"='2563')|>
  mutate(change = year2563 - year2562)%>%
  pivot_longer(cols=c("year2562","year2563"),
               names_to="year", values_to = "score")%>%
  ggplot(aes(x=year,y=score,group=name,col=change <0))+
  geom_point()+
  geom_line()+
  geom_text_repel(aes(
            label = score))+
  geom_text_repel(data=.%>% filter(year == "year2563"),
                  aes(label=name),
                  nudge_x=0.1,segment.linetype=2, size=3,
                  segment.curvature= -0.5,family="ChulaCharasNew")+
  theme_light()+
  labs(col = "การเปลี่ยนแปลง")+
  scale_color_manual(labels = c("เพิ่มขึ้น", "ลดลง"),values=c("darkgray","maroon"))+
  labs(x="ปีการศึกษา", y="อันดับสมรรถนะแต่ละโครงสร้งพื้นฐาน")+
  theme(text = element_text(family="ChulaCharasNew"))
 
pi3


library(readxl)
library(ggplot2)
library(patchwork)
#item2.1
#data preprocessing
ii1 <- read_excel("/Users/pariwatthamma/Downloads/สทศ-ONET_2564_P6_M3_M6_คะแนนมากกว่า50.xlsx",sheet=2)
glimpse(ii1)


tempi1 <- ii1 |> select(c(4,5,24,29,30,31)) 


names(tempi1)[1]<-"code_school"
names(tempi1)[2]<-"name_school"
names(tempi1)[3]<-"subject"
names(tempi1)[4]<-"mean"
names(tempi1)[5]<-"sd"
names(tempi1)[6]<- "med"

glimpse(tempi1)
head(tempi1,15)

tempi1 <- tempi1 %>% mutate(subject= factor(subject,levels = c("93","95","94","91"),
                                    labels = c("ENG","SCI","MATH","THAI")))
summary(tempi1)

#สำรวจการกระจายของค่าmean
ti1_outlier <- tempi1  %>%
  group_by(subject)%>% 
  summarise(
    q1 = quantile(mean, 0.25,na.rm=T),
    q3 = quantile(mean, 0.75,na.rm=T),
    upper = q3+1.5*IQR(mean,na.rm=T),
    lower = q1-1.5*IQR(mean,na.rm=T))

ti1 <- tempi1 %>%
  left_join(x=.,y=ti1_outlier, by="subject") %>%
  ggplot(aes(x=mean, y=subject,fill=subject))+
  geom_boxplot(outlier.alpha = 0)+
  geom_jitter(data= . %>%
                filter(mean > upper| mean < lower),
              width=0.1,
              height=0.1,
              alpha=0.5)+
  scale_fill_brewer(type="qua",palette = 1)+
  theme_light()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())    

#สำรวจการกระจายของ med   
ti2_outlier <- tempi1  %>%
  group_by(subject)%>% 
  summarise(
    q1 = quantile(med, 0.25,na.rm=T),
    q3 = quantile(med, 0.75,na.rm=T),
    upper = q3+1.5*IQR(med,na.rm=T),
    lower = q1-1.5*IQR(med,na.rm=T))

ti2 <- tempi1 %>%
  left_join(x=.,y=ti2_outlier, by="subject") %>%
  ggplot(aes(x=med, y=subject,fill=subject))+
  geom_boxplot(outlier.alpha = 0)+
  geom_jitter(data= . %>%
                filter(med > upper| med < lower),
              width=0.1,
              height=0.1,
              alpha=0.5)+
  scale_fill_brewer(type="qua",palette = 1)+
  theme_light()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())   
#เปรียบเทียบการกระจายของ mean และ med
ti1/ti2


#item2.2
#data preprocessing
glimpse(ii1)

tempi2 <- ii1 |> select(c(1,6,7,24,29)) 

names(tempi2)[1]<-"year"
names(tempi2)[2]<-"code_local"
names(tempi2)[3]<-"name_local"
names(tempi2)[4]<-"subject"
names(tempi2)[5]<-"mean"

glimpse(tempi2)
head(tempi2,15)

tempi2 <- tempi2 %>% mutate(subject= factor(subject,levels = c("93","95","94","91"),
                                            labels = c("ENG","SCI","MATH","THAI")))
summary(tempi2)
table(tempi2$name_local)%>%count()
length(tempi2$name_local)

#LINE
#ตรวจสอบแนวโน้ม
ttempi2 <- tempi2 %>% group_by(year,code_local,name_local) %>%
  summarise(avg_mean = mean(mean)) %>% 
  pivot_wider(names_from = "year" , values_from = "avg_mean") %>%
  rename("year2560"='2560',"year2561"='2561',"year2562"='2562',"year2563"='2563',"year2564"='2564')%>%
  mutate(slope1 = year2561-year2560) %>% 
  mutate(slope2 = year2562-year2561) %>% 
  mutate(slope3 = year2563-year2562) %>% 
  mutate(slope4 = year2564-year2563) 

ttempi2$slope1 = replace(ttempi2$slope1,is.na(ttempi2$slope1), mean(ttempi2$slope1, na.rm = TRUE))
ttempi2$slope2 = replace(ttempi2$slope2,is.na(ttempi2$slope2), mean(ttempi2$slope2, na.rm = TRUE))
ttempi2$slope3 = replace(ttempi2$slope3,is.na(ttempi2$slope3), mean(ttempi2$slope3, na.rm = TRUE))
ttempi2$slope4 = replace(ttempi2$slope4,is.na(ttempi2$slope4), mean(ttempi2$slope4, na.rm = TRUE))

  ttempi2 %>% mutate(slope=slope1+slope2+slope3+slope4)%>%
  mutate(slope=slope/5)%>%
  select(1,2,3,4,5,6,7,12)%>% 
  pivot_longer(cols = starts_with("year"),names_to= "year",values_to = "avg_mean")%>% 
    ggplot(aes(x = year,
             y=avg_mean,
             group = name_local,
             col=slope <0))+
  geom_line()+
  geom_line(data=. %>% filter(code_local == "3081"),
             aes(x=year, y=avg_mean),col="orange")+
    annotate(geom = "text", x=4.5, y=45, label = "ท้องถิ่นจังหวัดกระบี่", 
             family = "ChulaCharasNew",  col = "orange", size=3,  fontface = "bold")+
  theme_light()+
  labs(col = "แนวโน้มการเปลี่ยนแปลง")+
  scale_color_manual(labels = c("เพิ่มขึ้น", "ลดลง","ไม่แน่นอน"),values=c("skyblue","pink","grey"))+
  labs(x="ปีการศึกษา", y="ค่าเฉลี่ยคะแนนสอบทั้ง 4 วิชา")+
  theme(text = element_text(family="ChulaCharasNew"))

#BOX PLOT
  #ตรวจสอบการกระจายของข้อมูล
tempi2 %>% group_by(year,code_local,name_local) %>%
  summarise(avg_mean = mean(mean)) %>% 
  arrange(-avg_mean)%>% 
  ggplot(aes(x=avg_mean))+
  geom_boxplot()+
  geom_point(data =. %>% filter(name_local == "ท้องถิ่นจังหวัดกระบี่"),
             aes(x=avg_mean, y=0),col="orange")

#TREEMAP
#ตรวจสอบจำนวนแนวโน้มที่มีการเปลี่ยนแปลง
ttempi2 <- tempi2 %>% group_by(year,code_local,name_local) %>%
  summarise(avg_mean = mean(mean)) %>%
  pivot_wider(names_from = "year" , values_from = "avg_mean") %>%
  rename("year2560"='2560',"year2561"='2561',"year2562"='2562',"year2563"='2563',"year2564"='2564')%>%
  mutate(slope1 = year2561-year2560) %>% 
  #mutate(slope1 = ifelse(is.na(slope1), mean(slope1, na.rm = TRUE), slope1))%>%
  mutate(slope2 = year2562-year2561) %>% 
  #mutate(slope2 = ifelse(is.na(slope2), mean(slope2, na.rm = TRUE), slope2))%>%
  mutate(slope3 = year2563-year2562) %>%
  #mutate(slope3 = replace(is.na(slope3), mean(slope3, na.rm = TRUE), slope3))%>%
  mutate(slope4 = year2564-year2563) 
  #mutate(slope4 = ifelse(is.na(slope4), mean(slope4, na.rm = TRUE), slope4))
  
  
  ttempi2$slope1 = replace(ttempi2$slope1,is.na(ttempi2$slope1), mean(ttempi2$slope1, na.rm = TRUE))
  ttempi2$slope2 = replace(ttempi2$slope2,is.na(ttempi2$slope2), mean(ttempi2$slope2, na.rm = TRUE))
  ttempi2$slope3 = replace(ttempi2$slope3,is.na(ttempi2$slope3), mean(ttempi2$slope3, na.rm = TRUE))
  ttempi2$slope4 = replace(ttempi2$slope4,is.na(ttempi2$slope4), mean(ttempi2$slope4, na.rm = TRUE))
  
  ttempi2 %>% mutate(slope=slope1+slope2+slope3+slope4) %>%
  mutate(slope=slope/5) %>%
  mutate(slope = ifelse(is.na(slope), mean(slope, na.rm = TRUE), slope)) %>%
  #mutate(slope = (year2564 - year2560) / 4)
  #mutate(slope= means(year2560, year2561, year2562, year2563,year2564, na.rm = TRUE))
  select(1,2,3,4,5,6,7,12)%>% 
  pivot_longer(cols = starts_with("year"),names_to= "year",values_to = "avg_mean") %>%
  mutate(group = ifelse(slope > 0, "เพิ่มขึ้น", "ลดลง"))%>%
  group_by(group) %>%
  count() %>% 
  ungroup() %>%
  arrange(-n) %>%
  ggplot(aes(area = n, fill = group, subgroup = n, label = "")) +
  geom_treemap(alpha = 0.9) +
  geom_treemap_subgroup_border(col = "white") +
  geom_treemap_text(family = "ChulaCharasNew") +
  geom_treemap_subgroup_text(place = "bottom", grow = T,
                             alpha = 0.4, colour = "white", min.size = 0) +
  labs(fill = "กลุ่มที่มีการเปลี่ยนแปลง")+
  scale_fill_manual(labels = c("เพิ่มขึ้น", "ลดลง","ไม่แน่นอน"),values=c("skyblue","pink","grey"))+
  theme(text = element_text(family="ChulaCharasNew"))


#item2.3
#data preprocessing
tempi4 <- ii1 |> select(c(1,5,15,7,24,29,31,30)) 

glimpse(ii1)
names(tempi4)[1]<-"year"
names(tempi4)[2]<-"name_school"
names(tempi4)[3]<-"name_prov"
names(tempi4)[4]<-"name_local"
names(tempi4)[5]<-"subject"
names(tempi4)[6]<-"mean"
names(tempi4)[7]<-"med"
names(tempi4)[8]<-"sd"

tempi4 <- tempi4 %>% mutate(subject= factor(subject,levels = c("93","95","94","91"),
                                            labels = c("ENG","SCI","MATH","THAI")))

tempi4 <- tempi4 %>% filter(year %in% c("2560", "2562"),name_prov %in% c("กรุงเทพมหานคร","เชียงใหม่"))
table(tempi4$name_prov)
table(tempi4$year)
glimpse(tempi4)
summary(tempi4)

tempi4 <- tempi4  %>% mutate(name_local = ifelse(is.na(name_local), name_prov, name_local)) %>%
  mutate(name_local = ifelse(name_local == "กรุงเทพมหานคร","สช.กรุงเทพมหานคร",name_local))%>%
  mutate(name_local = ifelse(name_local == "เชียงใหม่","สช.เชียงใหม่",name_local))

#2.3.1
#โรงเรียน

tempii4 <- tempi4 %>% select(1:6) %>% 
  group_by(year,name_school,name_prov,subject,name_local) %>%
  summarise(avg_mean = mean(mean)) %>%
  pivot_wider(names_from = "subject",values_from ="avg_mean") %>%
  filter(name_local == "สำนักงานเขตพื้นที่การศึกษามัธยมศึกษา เขต 1")%>%
  group_by(name_school) %>%
  summarise(
    mean_eng = mean(ENG),
    mean_sci =  mean(SCI),
    mean_math =  mean(MATH),
    mean_thai =  mean(THAI)) 

AA <- tempii4$name_school
BB <- as.numeric(tempii4$mean_eng)
CC <- as.numeric(tempii4$mean_sci)
DD <-  as.numeric(tempii4$mean_math)
EE <-  as.numeric(tempii4$mean_thai)

matrix_tempi4 <- matrix(c(as.numeric(BB),as.numeric(CC),as.numeric(DD),as.numeric(EE)),nrow=length(tempii4$name_school),ncol=4)

colnames(matrix_tempi4) <- c("mean_eng","mean_sci","mean_math","mean_thai")
rownames(matrix_tempi4) <- AA

dist_matrix_tempi4 <- data.matrix(dist(matrix_tempi4))
heatmap(dist_matrix_tempi4)

library(qgraph)
dist_matrix_tempi4 <- as.matrix(dist(matrix_tempi4))
dist_matrix_tempi4 <- 1/dist_matrix_tempi4
qgraph(dist_matrix_tempi4,layout='spring',vsize=3)

#jpeg('2.3.1.jpg',width=1000,height=1000,unit='px')

#2.3.1
#เขตพื้นที่

tempii4 <- tempi4 %>% select(1:6) %>% 
  group_by(year,name_school,name_prov,subject,name_local) %>%
  summarise(avg_mean = mean(mean)) %>%
  pivot_wider(names_from = "subject",values_from ="avg_mean") %>% 
  group_by(name_local) %>%
  summarise(
    mean_eng = mean(ENG),
    mean_sci =  mean(SCI),
    mean_math =  mean(MATH),
    mean_thai =  mean(THAI)) 

AA <- tempii4$name_local
BB <- as.numeric(tempii4$mean_eng)
CC <- as.numeric(tempii4$mean_sci)
DD <-  as.numeric(tempii4$mean_math)
EE <-  as.numeric(tempii4$mean_thai)

matrix_tempi4 <- matrix(c(as.numeric(BB),as.numeric(CC),as.numeric(DD),as.numeric(EE)),nrow=length(tempii4$name_local),ncol=4)

colnames(matrix_tempi4) <- c("mean_eng","mean_sci","mean_math","mean_thai")
rownames(matrix_tempi4) <- AA

dist_matrix_tempi4 <- data.matrix(dist(matrix_tempi4))
heatmap(dist_matrix_tempi4)

library(qgraph)
dist_matrix_tempi4 <- as.matrix(dist(matrix_tempi4))
dist_matrix_tempi4 <- 1/dist_matrix_tempi4
qgraph(dist_matrix_tempi4,layout='spring',vsize=3)


#2.3.2
#ค่าเฉลี่ยระดับโรงเรียน
tempi4 %>% group_by(year,name_school,name_prov,name_local) %>%
  summarise(avg_mean = mean(mean)) %>%
  group_by(name_local) %>%
  summarise(
    mean_mean = avg_mean,
    med_mean = avg_mean) %>%
  ggplot(aes(x=mean_mean, y=med_mean))+
  geom_point()

#ค่าเฉลี่ยของเขต
tempi4 %>% group_by(year,name_school,name_prov,name_local) %>%
       summarise(avg_mean = mean(mean)) %>%
  group_by(name_local) %>%
  summarise(
    mean_mean = mean(avg_mean),
    med_mean = median(avg_mean)) %>%
  ggplot(aes(x=mean_mean, y=med_mean))+
  geom_point()

#cv ระดับโรงเรียน
tempi4 %>% group_by(year,name_school,name_prov,name_local) %>%
  summarise(avg_mean = mean(mean),
            avg_med = mean(med),
            avg_sd = mean(sd)) %>%
  group_by(name_local) %>%
  summarise(
    cv_mean = avg_sd/avg_mean,
    cv_med =  avg_sd/avg_med) %>%
  ggplot(aes(x=cv_mean, y=cv_med))+
  geom_point()

#หาสัมประสิทธิ์การแปรผัน cv ระดับเขตพื้นที่
#coefficient of variance 

table(tempi4$name_local)
length(table(tempi4$name_local))

tempi4 %>% group_by(year,name_school,name_prov,name_local) %>%
  summarise(avg_mean = mean(mean),
            avg_med = mean(med),
            avg_sd = mean(sd)) %>%
  group_by(name_local) %>%
  summarise(
    cv_mean = sd(avg_mean)/mean(avg_mean),
    cv_med =  sd(avg_med)/mean(avg_med)) %>% 
  ggplot(aes(x=cv_mean, y=cv_med,col= cv_mean>0.15))+
  geom_point()+
  geom_text_repel(aes(label=name_local,family="ChulaCharasNew"))+
  theme_minimal()+
  labs(col = "ระดับความเลื่อมล้ำ")+
  scale_color_manual(labels = c("มาก", "น้อย","ไม่แน่นอน"),values=c("steelblue","maroon","grey"))+
  labs(x="สัมประสิทธิ์การแปรผันของมัธยฐาน", y="สัมประสิทธิ์การแปรผันค่าเฉลี่ย")+
  theme(text = element_text(family="ChulaCharasNew"))

glimpse(ttempi4)  
sum(is.na(ttempi4[4]))

  





