test

> x <- 8
> x*3
[1] 24
> y=x/2
> y
[1] 4
> x=15
> x
[1] 15
> y
ls
[1] 15
> weights<-c(10,20,30)
> x
[1] 15
> y
[1] 4
#challange
> z <- c(weights,x,y)
> z
[1] 10 20 30 15  4
> zm <- mean(z)
> zm
#Data
> getwd()
[1] "C:/Users/Sean/Desktop/SWC-wsu"
> list.files()
[1] "gapminder.csv"  "intro script.R" "SWC-wsu.Rproj" 
setwd("~/Desktop")
[1] 15.8
list.files()
setwd("C:/Users/Sean/Desktop/SWC-wsu")
gapminder<-read.csv("gapminder.csv")
head(gapminder)
> getwd()
[1] "C:/Users/Sean/Desktop/SWC-wsu"
> list.files()
[1] "gapminder.csv"  "intro script.R" "SWC-wsu.Rproj" 
> setwd("~C:/Users/Sean/Desktop/SWC-wsu")
Error in setwd("~C:/Users/Sean/Desktop/SWC-wsu") : 
  cannot change working directory
> gapminder<-read.csv("gapminder.csv")
> head(gapminder)
country continent year lifeExp      pop gdpPercap
1 Afghanistan      Asia 1952  28.801  8425333  779.4453
2 Afghanistan      Asia 1957  30.332  9240934  820.8530
3 Afghanistan      Asia 1962  31.997 10267083  853.1007
4 Afghanistan      Asia 1967  34.020 11537966  836.1971
5 Afghanistan      Asia 1972  36.088 13079460  739.9811
6 Afghanistan      Asia 1977  38.438 14880372  786.1134
> class(gapminder)
[1] "data.frame"
> str(gapminder)
'data.frame':  1704 obs. of  6 variables:
  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
$ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
$ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
$ lifeExp  : num  28.8 30.3 32 34 36.1 ...
$ pop      : num  8425333 9240934 10267083 11537966 13079460 ...
$ gdpPercap: num  779 821 853 836 740 ...
> str(gapminder)
'data.frame':	1704 obs. of  6 variables:
  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
$ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
$ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
$ lifeExp  : num  28.8 30.3 32 34 36.1 ...
$ pop      : num  8425333 9240934 10267083 11537966 13079460 ...
$ gdpPercap: num  779 821 853 836 740 ...
wights[1]
weights[1]
weights[1:3]
#firstrow,firstcollum
gapminder[1,1]
#firstrow, thirdcolum
gapminder[1,3]
#500th row,5th&6th colum
gapminder[500,5:6]
gapminder$pop
#equlivelent to:
gapminder[,5]
gapminder[,pop]
gapminder[,"pop"]
#alldata for finland
gapminder[gapminder$country=="Finland",]
# countriesand years where pop<=10000
gapminder[gapminder$pop<=100000,]
q<-"Sao Tome and Principe"
gapminder[50,4]
gapminder[50,"lifeExp"]
gapminder[4,50]
gapminder$lifeExp[50]

st<-gapminder[gapminder$country==q,]
gapminder[gapminder$]
gapminder[gapminder$lifeExp<==80]
#countries with life exp.>80
gapminder[gapminder$lifExp>80,"country"]
install.packages("dplyr")
install.packages("ggplot2")
$dplyr
?dplyr
library("dplyr")
#select keeps colum
select(gapminder, country,year,pop)
filter(gapminder, country=="Finland")
# to create pipe %>%
gapminder%>%
  filter(pop <= 100000)%>%
  select(country, year)
gapminder_sml <-gapminder %>%
  filter(pop <= 100000)%>%
  select(country, year)
gapminder%>%
  filter(gdpPercap >= 3500)%>%
  select(country, year,gdpPercap)
gapminder%>%
  mutate(totalgdp = gdpPercap *pop)%>%
#shows header
  head
#to update gapminder obj could give same name
# to groupby multiple columns
gapminder %>%
  mutate(totalgdp =gdpPercap* pop)%>%
  group_by(continent,year)%>%
  summarize(meangdp = mean(totalgdp))
# group by values
gapminder%>%
  mutate(totalgdp =gdpPercap * pop)%>%
  group_by(continent,year)%>%
  summarize(meangdp = mean(totalgdp),
            mingdp=min(totalgdp))
gapminder%>%
  group_by(year)%>%
  summarize(mean = mean(lifeExp),
            min= min(lifeExp))
#gapminder is dataset



#summarize lists

#to export data write.csv
write.csv(gapminder_sml,"gapminder_sml.csv")
#now to ggplot

#open library
library("ggplot2")
# Intro to ggplot2
#remove rm()
# load ggplot library

library(ggplot2)
#loadgapminder
gapminder<- read.csv(""~)

#grammer of graphics book
#scatter plot of lifeExp vs gdpPercap
ggplot(gapminder,
       aes(x=gdpPercap, y=lifeExp))+geom_point()
#ggplot aesthetics make connection to some column to some attribute of plot,example y 

#geometric objects make geomeptric objects plots, i.e. points
p <- ggplot(gapminder, aes(x=gdpPercap, y=lifeExp))
p + geom_point()
p2<- p+ geom_point()
print(p2)
# to define scale 
#malke x axis have log scale

p3 <- p + geom_point()+scale_x_log10()
p3
ggplot(gapminder,
      aes(x=log10(gdpPercap), y=lifeExp))+ geom_point()

p3

ggplot(gapminder, aes(x=log10(gdpPercap), y=lifeExp))+ scale_x_log10() + geom_point()

library(dplyr)


#another way to get log scale

names(gapminder)

gapminder%>%
  filter(country=="Iceland", lifeExp<80)
  
library(dplyr)
ggplot(gapminder, aes(x=log10(gdpPercap), y=lifeExp))+ scale_x_log10() + geom_point()
# adding color by country
ggplot(gapminder, aes(x=(gdpPercap), y=lifeExp,color=country))+ scale_x_log10() + geom_point()

gapminder%>%
  filter(year=="2007")%>%
ggplot(aes(x=(gdpPercap), y=lifeExp,color=country))+ scale_x_log10() + geom_point()

# play withaesthetics
ggplot(gapminder, aes(x=gdpPercap, y=lifeExp)) + geom_point(color="red", size=1)

gapminder%>%
  filter(country=="China")%>%
  ggplot(gapminder, aes(x=gdpPercap, y=lifeExp)) + geom_point(aes(color=year, size=2)
 
   #now plot of china or india                                                            
  gm_chinaindia <- gapminder %>%
    filter(country=="China"|country== "India")
  
  ggplot(gm_chinaindia, aes(x=(gdpPercap), y=lifeExp))+ geom_line(aes(group=country)
  + geom_point(aes(color=year))
  
  gapminder%>%
    filter(year==2007)%>%
    ggplot(aes(x=lifeExp)) +geom_histogram(binwidth=2.5, fill="red",color="black")
  
  #box plots
  gapminder%>%
    filter(year==2007)%>%
    ggplot(aes(y=lifeExp, x=continent))+geom_boxplot(color="blue", fill="white", size=1)+coord_flip()
  
  gapminder%>%
    filter(year==2007)%>%
    ggplot(aes(y=lifeExp, x=continent))+ geom_point
  (position=position_jitter(width=0.1, height=0))
   
   #faceting
  ggplot(gapminder,aes(x=gdpPercap, y=lifeExp))+ 
    geom_point() + scale_x_log10() + facet_grid(continent~year)
  
    #faceting is conditioning, by catagorical values of some other catagory
  #facet wrap, have ~ inbetween
  ggplot(gapminder,aes(x=gdpPercap, y=lifeExp))+ 
    geom_point() + scale_x_log10() + facet_wrap(~year)
  
  
  # themes
  ggplot(gapminder,aes(x=gdpPercap, y=lifeExp))+ 
    geom_point() + scale_x_log10() + facet_wrap(~year)
    p + theme_bw()
  
 p <-gapminder %>%
   filter(country %in% c("United State","China")) %>%
   ggplot(aes(x=gdpPercap, y=lifeExp)) +
 geom_point()
 p
 p <-gapminder %>%
   filter(country %in% c("United State","China","Nigeria")) %>%
   ggplot(aes(x=gdpPercap, y=lifeExp)) +
   geom_point(aes(color=country))
 p
