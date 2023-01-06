# Yasam Maliyeti
Veri Görselleştirme Projesi


---
title: "vgproje"
format: html
editor: visual
---

```{r, message=FALSE, warning=FALSE}
options(repos = list(CRAN="http://cran.rstudio.com/"))
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("dplyr")
library(ggplot2)
library(tidyverse)
library(dplyr)
```

```{r, message=FALSE, warning=FALSE}
library(readxl)
data <- read_excel("C:/Users/serca/Desktop/Yeni Microsoft Excel Çalışma Sayfası (3).xlsx", 
    col_types = c("skip", "text", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric"))
```

```{r, message=FALSE, warning=FALSE}
graph1 <- ggplot(data, aes(x = data$`Average Monthly Net Salary  (After Tax)`))+
  geom_density(fill="azure2", color="black", alpha=0.8)+
  geom_vline(aes(xintercept=405),
            color="brown", linetype="dashed", size=1)+
  geom_label(aes(x=405.03, label="İstanbul\n405.03", y=0.00072), colour="brown",
             vjust = 2, text=element_text(size=0.7), geom="label", fill = "azure2")+
  scale_x_continuous(breaks = seq(0,6000,1000))+
  scale_y_continuous(limits = c(0,0.00075),labels = scales::comma)+
  theme(legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.y = element_text(hjust = 1, size = 8),
        axis.title.x = element_text(hjust = 1, size = 8),
        axis.text = element_text(colour = "black", size = 11))+
  labs(x = "Ortalama Aylık Net Maaş (Dolar)",
       y = "Yoğunluk",
       title = "Önemli Dünya Şehirlerinin Ortalama Net Maaş Yoğunluk Grafiği")
ggsave(graph1, filename = "graph1.png",
       bg = "transparent",
       width = 6.5, height = 3.5, dpi = 3000)
  
```

```{r, message=FALSE, warning=FALSE}
data2 <- filter(data,data$City=='Istanbul, Turkey'| 
                  data$City=='Paris, France'|
                  data$City=='London, United Kingdom'|
                  data$City=='New York, NY, United States'|
                  data$City=='Moscow, Russia'|
                  data$City=='Toronto, Canada'|
                  data$City=='Tokyo, Japan'|
                  data$City=='Berlin, Germany'|
                  data$City=='Nairobi, Kenya'|
                  data$City=='Kiev (Kyiv), Ukraine')
```

```{r, message=FALSE, warning=FALSE}
c <- data2$`Average Monthly Net Salary  (After Tax)`/data2$`McMeal at McDonalds  (or Equivalent Combo Meal)`
graph2 <- ggplot(data2, aes(reorder(x = data2$City, +c),
                  y = c,fill=factor(if_else(
                    data2$City=="Istanbul, Turkey","Highlighted","Normal"))))+
  geom_bar(stat = "identity", color = "black")+
  scale_fill_manual(name = "City", values=c("brown","azure2"))+
  coord_flip()+ 
  scale_y_continuous(breaks = seq(0,600,50), limits=c(0,600))+
  scale_x_discrete(
  breaks = c ("New York, NY, United States" ,"London, United Kingdom" ,
              "Paris, France","Toronto, Canada" , "Tokyo, Japan", 
              "Berlin, Germany" , "Moscow, Russia", "Istanbul, Turkey" ,
              "Kiev (Kyiv), Ukraine" , "Nairobi, Kenya") ,
  labels= c("New York, Amerika" , "Londra, İngiltere" , "Paris, Fransa", 
            "Toronto, Kanada" , "Tokyo, Japonya", "Berlin, Almanya", 
            "Moskova, Rusya", "İstanbul, Türkiye", "Kiev, Ukrayna", 
            "Nairobi, Kenya" ))+
   theme(legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_text(colour = "black", size = 11))+
  labs(title = "Ortalama Bir Maaş İle Alınabilen BigMac Sayıları")

ggsave(graph2, filename = "graph2.png",
       bg = "transparent",
       width = 6.5, height = 3, dpi = 3000)
```

```{r, message=FALSE, warning=FALSE}
graph3 <- ggplot(data2, aes(reorder(
                  x = data2$City, 
                    +data2$`McMeal at McDonalds  (or Equivalent Combo Meal)`),
                  y = data2$`McMeal at McDonalds  (or Equivalent Combo Meal)`,
                  fill=factor(if_else(
                    data2$City=="Istanbul, Turkey","Highlighted","Normal"))))+
  geom_bar(stat = "identity", color = "black")+
  scale_fill_manual(name = "City", values=c("brown","azure2"))+
  coord_flip()+
  scale_y_continuous(breaks = seq(0,10,2), limits=c(0,10))+
  scale_x_discrete(
  breaks = c ("New York, NY, United States" ,"London, United Kingdom" ,
              "Paris, France","Toronto, Canada" , "Tokyo, Japan", 
              "Berlin, Germany" , "Moscow, Russia", "Istanbul, Turkey" ,
              "Kiev (Kyiv), Ukraine" , "Nairobi, Kenya") ,
  labels= c("New York, Amerika" , "Londra, İngiltere" , "Paris, Fransa", 
            "Toronto, Kanada" , "Tokyo, Japonya", "Berlin, Almanya", 
            "Moskova, Rusya", "İstanbul, Türkiye", "Kiev, Ukrayna", 
            "Nairobi, Kenya" ))+
   theme(legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_text(colour = "black", size = 11))+
  labs(title = "BigMac Fiyatları (Dolar)")

ggsave(graph3, filename = "graph3.png",
       bg = "transparent",
       width = 6.5, height = 3, dpi = 3000)
```

```{r, message=FALSE, warning=FALSE}
graph4 <- ggplot(data2, aes(reorder(x = data2$City,
                          +data2$`Apartment  (3 bedrooms)  in City Centre`),
                  y = data2$`Apartment  (3 bedrooms)  in City Centre`,
                  fill=factor(if_else(
                    data2$City=="Istanbul, Turkey","Highlighted","Normal"))))+
  geom_bar(stat = "identity", color = "black")+
  scale_fill_manual(name = "City", values=c("brown","azure2"))+
  coord_flip()+
  scale_y_continuous(breaks = seq(0,7000,1000), limits=c(0,7000))+
  scale_x_discrete(
  breaks = c ("New York, NY, United States" ,"London, United Kingdom" ,
              "Paris, France","Toronto, Canada" , "Tokyo, Japan", 
              "Berlin, Germany" , "Moscow, Russia", "Istanbul, Turkey" ,
              "Kiev (Kyiv), Ukraine" , "Nairobi, Kenya") ,
  labels= c("New York, Amerika" , "Londra, İngiltere" , "Paris, Fransa", 
            "Toronto, Kanada" , "Tokyo, Japonya", "Berlin, Almanya", 
            "Moskova, Rusya", "İstanbul, Türkiye", "Kiev, Ukrayna", 
            "Nairobi, Kenya" ))+
   theme(legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_text(colour = "black", size = 11))+
  labs(title = "Şehir içinde 3+1 Dairelerin Ortalama Kira Fiyatları (Dolar)")

ggsave(graph4, filename = "graph4.png",
       bg = "transparent",
       width = 6.5, height = 3, dpi = 3000)
```

```{r, message=FALSE, warning=FALSE}
graph5 <- ggplot(data2, aes(reorder(x = data2$City,
                          +data2$`Apartment  (3 bedrooms)  in City Centre`/data2$`Average Monthly Net Salary  (After Tax)`),
                  y = data2$`Apartment  (3 bedrooms)  in City Centre`/data2$`Average Monthly Net Salary  (After Tax)`,
                  fill=factor(if_else(
                    data2$City=="Istanbul, Turkey","Highlighted","Normal"))))+
  geom_bar(stat = "identity", color = "black")+
  scale_fill_manual(name = "City", values=c("brown","azure2"))+
  coord_flip()+
  scale_y_continuous(breaks = seq(0,3,0.5), limits=c(0,3))+
  scale_x_discrete(
  breaks = c ("New York, NY, United States" ,"London, United Kingdom" ,
              "Paris, France","Toronto, Canada" , "Tokyo, Japan", 
              "Berlin, Germany" , "Moscow, Russia", "Istanbul, Turkey" ,
              "Kiev (Kyiv), Ukraine" , "Nairobi, Kenya") ,
  labels= c("New York, Amerika" , "Londra, İngiltere" , "Paris, Fransa", 
            "Toronto, Kanada" , "Tokyo, Japonya", "Berlin, Almanya", 
            "Moskova, Rusya", "İstanbul, Türkiye", "Kiev, Ukrayna", 
            "Nairobi, Kenya" ))+
   theme(plot.title = element_text(hjust = 1),
         legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_text(colour = "black", size = 11))+
  labs(title = "Şehir içinde 3+1 Dairelerin Ortalama Kira Fiyatının Ortalama Maaşa Oranı")

ggsave(graph5, filename = "graph5.png",
       bg = "transparent",
       width = 6.5, height = 3, dpi = 3000)
  
```

```{r, message=FALSE, warning=FALSE}
graph6 <- ggplot(data2, aes(reorder(
                  x = data2$City, 
                    +data2$`Toyota Corolla Sedan 1.6l 97kW Comfort  (Or Equivalent New Car)`/data2$`Average Monthly Net Salary  (After Tax)`),
                  y = data2$`Toyota Corolla Sedan 1.6l 97kW Comfort  (Or Equivalent New Car)`/data2$`Average Monthly Net Salary  (After Tax)`,
                  color=factor(if_else(
                    data2$City=="Istanbul, Turkey","Highlighted","Normal"))))+
  geom_point(stat = "identity", aes(size=data2$`Toyota Corolla Sedan 1.6l 97kW Comfort  (Or Equivalent New Car)`))+
  scale_color_manual(name = "City", values=c("brown","black"))+
  coord_flip()+
  scale_y_continuous(breaks = seq(0,80,5), limits=c(0,80))+
  scale_x_discrete(
  breaks = c ("New York, NY, United States" ,"London, United Kingdom" ,
              "Paris, France","Toronto, Canada" , "Tokyo, Japan", 
              "Berlin, Germany" , "Moscow, Russia", "Istanbul, Turkey" ,
              "Kiev (Kyiv), Ukraine" , "Nairobi, Kenya") ,
  labels= c("New York, Amerika" , "Londra, İngiltere" , "Paris, Fransa", 
            "Toronto, Kanada" , "Tokyo, Japonya", "Berlin, Almanya", 
            "Moskova, Rusya", "İstanbul, Türkiye", "Kiev, Ukrayna", 
            "Nairobi, Kenya" ))+
   theme(plot.title = element_text(hjust = 1),
         plot.subtitle = element_text(size = 8, hjust = -0.23),
         legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_text(colour = "black", size = 11))+
  labs(title = "Ortalama Maaş İle Toyota Corolla Sedan 1.6l 97kW Comfort Alma Süresi (Ay)",
       subtitle = "(Sıfır Araç)",
       caption = "(Nokta Büyüklüğü Araç Fiyatını Temsil Eder)")

ggsave(graph6, filename = "graph6.png",
       bg = "transparent",
       width = 7, height = 3.5, dpi = 3000)
```

```{r, message=FALSE, warning=FALSE}
graph7 <- ggplot(data2, aes(reorder(
                  x = data2$City, 
                    +data2$`Price per Square Meter  to Buy Apartment  in City Centre`*100/data2$`Average Monthly Net Salary  (After Tax)`),
                  y = data2$`Price per Square Meter  to Buy Apartment  in City Centre`*100/data2$`Average Monthly Net Salary  (After Tax)`,
                  color=factor(if_else(
                    data2$City=="Istanbul, Turkey","Highlighted","Normal"))))+
  geom_point(stat = "identity", aes(size=data2$`Price per Square Meter  to Buy Apartment  in City Centre`*100))+
  scale_color_manual(name = "City", values=c("brown","black"))+
  coord_flip()+
  scale_y_continuous(breaks = seq(200,600,50), limits=c(200,600))+
  scale_x_discrete(
  breaks = c ("New York, NY, United States" ,"London, United Kingdom" ,
              "Paris, France","Toronto, Canada" , "Tokyo, Japan", 
              "Berlin, Germany" , "Moscow, Russia", "Istanbul, Turkey" ,
              "Kiev (Kyiv), Ukraine" , "Nairobi, Kenya") ,
  labels= c("New York, Amerika" , "Londra, İngiltere" , "Paris, Fransa", 
            "Toronto, Kanada" , "Tokyo, Japonya", "Berlin, Almanya", 
            "Moskova, Rusya", "İstanbul, Türkiye", "Kiev, Ukrayna", 
            "Nairobi, Kenya" ))+
   theme(plot.title = element_text(hjust = -0.30),
         legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_text(colour = "black", size = 11))+
  labs(title = "Ortalama Maaş İle Şehir İçinde 100 m² Daire Alma Süresi (Ay)",
       caption = "(Nokta Büyüklüğü Şehir İçinde Ortalama 100 m² Daire Fiyatını Temsil Eder)")

ggsave(graph7, filename = "graph7.png",
       bg = "transparent",
       width = 7, height = 3.5, dpi = 3000)
```
