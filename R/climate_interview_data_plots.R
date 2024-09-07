# climate data - interviews
#install.packages("wesanderson")
#library(wesanderson)
library(ggplot2)
library(RColorBrewer)
library(ggtext)


#wes_palette("AsteroidCity1")

setwd("~/Desktop/MA/")

#data_for_R <- read_excel("questionnaire data /data for R.xlsx")
# where do you go with your animals if there is drought and there is not enough water?
data_water <- data.frame(
  Category = c("go down", "go up", "sees no issue", "stay there"),
  Value = c(5, 9, 51, 8) # c(6.85, 12.33, 69.86, 10.96)
)

data_water$relative <- round(data_water$Value / sum(data_water$Value)*100,1)


ggplot(data_water, aes(x = reorder(Category,-Value), y = relative)) +
  geom_bar(stat = "identity", fill = '#224afb') +
  labs(title = "Where do you go with your animals if there is drought and there is not enough water?",
       x = "Category", y = "% of herders", subtitle = paste('n=',sum(data_water$Value))) + 
  #scale_fill_manual(values = wes_palette("Cavalcanti1"))+
  guides(fill="none")+
  theme(plot.title = element_textbox_simple(size = rel(1.5)),
        #plot.title = element_text(size = rel(3)),
        plot.subtitle = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.5))
        )
  
###############################
# where do you go with your animals if there is drought and there is not enough grass?
  data_grass <- data.frame(
    Category = c("go down", "go up", "sees no issue", "stay there", "use a second pasture"),
    Value = c(9, 31, 14, 16, 6) # c(11.84, 40.47, 18.42, 21.05, 7.89)
  )
  
data_grass$relative <- round(data_grass$Value/sum(data_grass$Value)*100,1)


  ggplot(data_grass, aes(x = reorder(Category,-Value), y = relative)) +
    geom_bar(stat = "identity", fill = '#224afb') +
    labs(title = "Where do you go with your animals if there is drought and there is not enough grass?",
        x = 'Category', y = "% of herders", subtitle = paste('n=',sum(data_grass$Value))) +  
  #  scale_fill_manual(values = wes_palette("Cavalcanti1"))+
     guides(fill = 'none')+
     theme(plot.title = element_textbox_simple(size = rel(1.5)),
          #plot.title = element_text(size = rel(3)),
          plot.subtitle = element_text(size = rel(1.5)),
          axis.text = element_text(size = rel(1.5)),
          axis.title = element_text(size = rel(1.5))
    )
  
###############################
#Have either a low pasture productivity and less water availability impacted depredation of livestock by snow leopards or wolves? 
  data_depredation_impact <- data.frame(
    Category = c("yes", "no", "don't know", "stays the same"),
    Value = c(12, 20, 8, 5) # c(17.78, 24.44, 20, 11.11, 26.67)
  )
  
  data_depredation_impact$relative <- round(data_depredation_impact$Value/sum(data_depredation_impact$Value)*100,1)
  
  
  ggplot(data_depredation_impact, aes(x = reorder(Category,-Value), y = relative)) +
    geom_bar(stat = "identity", fill = '#224afb') +
    labs(title = "Have either a low pasture productivity and less water availability impacted depredation of livestock by carnivores?",
         x = 'Category', y = "% of herders",subtitle = paste('n=',sum(data_depredation_impact$Value))) +  
   # scale_fill_manual(values = wes_palette("Cavalcanti1"))+
     guides(fill = 'none')+
     theme(plot.title = element_textbox_simple(size = rel(1.5)),
          #plot.title = element_text(size = rel(3)),
          plot.subtitle = element_text(size = rel(1.5)),
          axis.text = element_text(size = rel(1.5)),
          axis.title = element_text(size = rel(1.5))
    )
  
###############################
#Have you noticed any trends in increasing or decreasing rainfall in the last ten years?
  data_increase_decrease_water <- data.frame(
    Category = c("decreasing", "don't know", "stays the same"),
    Value = c(59, 2, 4) #c(90.77, 3.08, 6.15)
  )
  
  data_increase_decrease_water$relative <- round(data_increase_decrease_water$Value/sum(data_increase_decrease_water$Value)*100,1)
  
  ggplot(data_increase_decrease_water, aes(x = reorder(Category,-Value), y = relative)) +
    geom_bar(stat = "identity", fill = '#224afb') +
    labs(title = "Have you noticed any trends in increasing or decreasing rainfall in the last ten years?",
         x = 'Category', y = "% of herders", subtitle = paste('n=',sum(data_increase_decrease_water$Value))) + # scale_fill_manual(values = wes_palette("Cavalcanti1"))+
    guides(fill = 'none')+
    theme(plot.title = element_textbox_simple(size = rel(1.5)),
          #plot.title = element_text(size = rel(3)),
          plot.subtitle = element_text(size = rel(1.5)),
          axis.text = element_text(size = rel(1.5)),
          axis.title = element_text(size = rel(1.5))
    )

###############################
# How has this impacted pasture availability in the years with more or less rainfall?
  data_pasture_availability <- data.frame(
    Category = c("less pastures", "drought", "no problem"),
    Value = c(43, 10, 7) # c(71.67, 16.67, 11.67,)
  )
  
  data_pasture_availability$relative <- round(data_pasture_availability$Value/sum(data_pasture_availability$Value)*100,1)
  
  
  ggplot(data_pasture_availability, aes(x = reorder(Category,-Value), y = relative)) +
    geom_bar(stat = "identity", fill = '#224afb') +
    labs(title = "How has this impacted pasture availability in the years with more or less rainfall?",
        x = 'Category',  y = "% of herders", subtitle = paste('n=',sum(data_pasture_availability$Value))) +  
  #  scale_fill_manual(values = wes_palette("Cavalcanti1"))+
    guides(fill ='none')+
    theme(plot.title = element_textbox_simple(size = rel(1.5)),
          #plot.title = element_text(size = rel(3)),
          plot.subtitle = element_text(size = rel(1.5)),
          axis.text = element_text(size = rel(1.5)),
          axis.title = element_text(size = rel(1.5))
    )
  
###############################
#How have you coped to address low pasture availability? 
  data_coping_pastures <- data.frame(
  Category = c("bought hay", "evade to other pastures", "nothing", "no problem", "other"),
  Value = c(5, 27, 20, 11, 3) # c(7.58, 40.91, 16.67, 30.03, )
  )

  data_coping_pastures$relative <- round(data_coping_pastures$Value/sum(data_coping_pastures$Value)*100,1)
  
  
  
ggplot(data_coping_pastures, aes(x = reorder(Category,-Value), y = relative)) +
  geom_bar(stat = "identity", fill = '#224afb') +
  labs(title = "How have you coped to address low pasture availability?",
       x = 'Category', y = "% of herders",subtitle = paste('n=',sum(data_coping_pastures$Value)) ) +  
 # scale_fill_manual(values = wes_palette("Cavalcanti1"))+
  guides(fill ='none')+
  theme(plot.title = element_textbox_simple(size = rel(1.5)),
        #plot.title = element_text(size = rel(3)),
        plot.subtitle = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.5))
  )

###############################
# What are you concerned about with respect to the pastures and livestock when you think of the future?
data_future <- data.frame(
  Category = c("livestock numbers", "pasture depletion", "caragana expansion", "no concerns", "hay prices", "other"),
  Value = c(27, 41, 5, 10, 9, 24) # c( )
)
total_future = 69
data_future$relative <- round(data_future$Value/total_future*100,1)


ggplot(data_future, aes(x = reorder(Category,-Value), y = relative)) +
  geom_bar(stat = "identity", fill = '#224afb') +
  labs(title = "What are you concerned about with respect to the pastures and livestock when you think of the future?",
      x = '', y = "% of herders",subtitle = paste('n=', total_future )) + 
  scale_fill_brewer(palette = "Dark2")+guides(fill = 'none')+
 #scale_fill_manual(values = wes_palette("BottleRocket1"))
  theme(plot.title = element_textbox_simple(size = rel(1.5)),
        #plot.title = element_text(size = rel(3)),
        plot.subtitle = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.5))
  )


