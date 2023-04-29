# Name: Stuti Garg
# SUID : 796501330
# NETID : stgarg
# Topic : Final Project - IST719 : Information Visualization 
# Dataset : Adidas USA sales dataset
# Source for the dataset : data.world
# Link : https://data.world/stellabigail/adidas-us-sales-datasets


#Importing all the packages
install.packages("readxl")
install.packages("ggplot2")

#Library all the installed packages
library(readxl)
library(ggplot2)

options(scipen = 99)
adidas <- read_excel("Adidas US Sales Datasets.xlsx")

##################################  Total sales by retailers in different regions  ###############################################
agg.sales <- tapply(adidas$`Total Sales`, list(adidas$Retailer,adidas$Region),sum)
barplot(agg.sales, beside = T,
        main = "Total sales by retailers in different regions",
        legend.text = rownames(rownames(agg.sales)),
        col = c("blue","pink","green","red","orange","purple")
)

##################################  Units Sold by retailers in different regions  ###############################################
agg.units <- tapply(adidas$`Units Sold`, list(adidas$Retailer,adidas$Region),sum)
barplot(agg.units, beside = T,
        main = "Units Sold by retailers in different regions",
        legend.text = rownames(rownames(agg.units)),
        col = c("blue","pink","green","red","orange","purple")
)

##################################  Average Price per Unit (PPU) by retailers in different regions  ###############################################
agg.ppu <- tapply(adidas$`Price per Unit`, list(adidas$Retailer,adidas$Region),mean)
barplot(agg.ppu, beside = T,
        main = "Average Price per Unit (PPU) by retailers in different regions",
        legend.text = rownames(rownames(agg.ppu)),
        col = c("blue","pink","green","red","orange","purple")
)

##################################  Which sales method of the retailers was more preferred  ###############################################
agg.sales.method <- tapply(adidas$`Total Sales`, list(adidas$`Sales Method`,adidas$Retailer),sum)
barplot(agg.sales.method, beside = T,
        main = "Which sales method of the retailers was more preferred",
        legend.text = rownames(rownames(agg.sales.method)),
        col = c("blue","red","green")
)

#################################  Total sales vs Operating Profit  ##############################################################
profit.pred <- predict(lm(adidas$`Operating Profit` ~ adidas$`Total Sales`))

ggplot(adidas) +
  aes(y = `Operating Profit`, x= `Total Sales`) +
  geom_point() +
  geom_smooth() 



#################################  Total Units sold in the given time  ##############################################################
profit.pred <- predict(lm(adidas$`Operating Profit` ~ adidas$`Total Sales`))

ggplot(adidas) +
  aes(y = `Operating Profit`, x= `Total Sales`) +
  geom_point() +
  geom_smooth() 


#################################  Apparels Trend  ########################################################################################
apparels <- adidas[adidas$Product %in%  c("Men's Apparel","Women's Apparel"),]

agg.aparels <- tapply(apparels$`Units Sold`, list(apparels$Product,apparels$Retailer),mean)
barplot(agg.aparels, beside = T,
        main = "Aparels trend",
        legend.text = rownames(rownames(agg.aparels)),
        col = c("blue","red")
)


#################################  Products Trend  ###################################################################################
agg.product <- tapply(adidas$`Units Sold`, list(adidas$Product,adidas$Retailer),mean)
barplot(agg.product, beside = T,
        main = "Products Trend",
        legend.text = rownames(rownames(agg.product)),
        col = c("blue","red","yellow","pink","orange","green")
)


