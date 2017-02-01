t1 <- Sys.time()
setwd("~/R/skryptydoR/ny_times/dane/")
library(dplyr)
library(tidyr)
ROK=2016

library(ggplot2)
#DAY <- read.table("ILCHICAG.txt")

#names(DAY) <- c("Month", "Day", "Year", "Temp")
DAY <- poznan
DAY <- DAY %>% filter(Month!=2 | Day!=29)


DAY$date <- as.Date(paste(DAY$Year,DAY$Month, DAY$Day, sep = "-"))
DAY$doy <- as.numeric(as.character(format(DAY$date, "%j")))


head(DAY)
is.leapyear <- function(year){
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}


DAY$leapyear <- ifelse(is.leapyear(DAY$Year),1,0)
DAY$doy <- ifelse((DAY$leapyear==1 & DAY$doy>60), yes=DAY$doy-1, no=DAY$doy)

# create dataframe that represents 1995-2013 historical data
Past <- DAY %>%
  dplyr::group_by(Year, Month) %>%
  arrange(Day) %>%
  ungroup() %>%
  dplyr::group_by(Year) %>%
  mutate(newDay = doy) %>%   # label days as 1:365 (will represent x-axis)         
  ungroup() %>%
  dplyr::filter(Temp != -99 & Year != ROK) %>%     # filter out missing data (identified with '-99' value) & current year data
  dplyr::group_by(newDay) %>%
  mutate(upper = max(Temp), # identify max value for each day
         lower = min(Temp), # identify min value for each day
         avg = mean(Temp),  # calculate mean value for each day
         se = sd(Temp)/sqrt(length(Temp))) %>%  # calculate standard error of mean
  #mutate(avg_upper = avg+(2.101*se),  # calculate 95% CI for mean
   #      avg_lower = avg-(2.101*se)) %>%  # calculate 95% CI for mean
  mutate(avg_upper = quantile(Temp, 0.75),  # calculate 95% CI for mean
         avg_lower = quantile(Temp, 0.25)) %>%  # calculate 95% CI for mean
  ungroup() %>%   arrange(date)

#Past
# create dataframe that represents current year data
Present <- DAY %>%
  filter(Temp != -99 & Year == ROK)  # filter out missing data & select current year data

#head(Present)


# create dataframe that represents the lowest temp for each day for the historical data
PastLows <- Past %>%
  group_by(newDay) %>% 
  summarise(Pastlow = min(Temp)) # identify lowest temp for each day from 1995-2013

# create dataframe that identifies the days in 2014 in which the temps were lower than all previous 19 years
Present$newDay <- Present$doy
PresentLows <- Present %>%
  left_join(PastLows) %>%  # merge historical lows to current year low data
  mutate(record = ifelse(Temp<Pastlow, "Y", "N")) %>% # identifies if current year was record low
  filter(record == "Y")  # filter for days that represent current year record lows

# create dataframe that represents the highest temp for each day for the historical data
PastHighs <- Past %>%
  group_by(newDay) %>%
  summarise(Pasthigh = max(Temp))  # identify highest temp for each day from 1995-2013

# create dataframe that identifies the days in 2014 in which the temps were higher than all previous 19 years
PresentHighs <- Present %>%
  left_join(PastHighs) %>%  # merge historical highs to current year low data
  mutate(record = ifelse(Temp>Pasthigh, "Y", "N")) %>% # identifies if current year was record high
  filter(record == "Y")  # filter for days that represent current year record highs

# function to turn y-axis labels into degree formatted values
dgr_fmt <- function(x, ...) {
  parse(text = paste(x, "*degree", sep = ""))
}

# create y-axis variable
a <- dgr_fmt(seq(-35,35, by=5))

# create a small dataframe to represent legend symbol for 2014 Temperature
legend_data <- data.frame(x=seq(175,182),y=rnorm(8,-10,1.2))


p <- ggplot(Past, aes(newDay, Temp)) +
  theme(plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) +
  geom_linerange(Past, mapping=aes(x=newDay, ymin=lower, ymax=upper), colour = "wheat2", alpha=.1)

# Past2 <- Past %>% group_by(Day, Month) %>% summarise_all(., .funs=(mean))
# Past2 <- Past2 %>% arrange(Month, Day)
#print(p)
#p <- ggplot(Past, aes(1:366, Temp))
#p+geom_linerange(Past2, mapping=aes(x=1:365, ymin=lower, ymax=upper))


p <- p + 
  geom_linerange(Past, mapping=aes(x=newDay, ymin=avg_lower, ymax=avg_upper), colour = "wheat4")

#print(p)

p <- p + 
  geom_line(Present, mapping=aes(x=newDay, y=Temp, group=1)) +
  geom_vline(xintercept = 0, colour = "wheat4", linetype=1, size=1)

#print(p)


p <- p + 
  geom_hline(yintercept = -30, colour = "white", linetype=1) +
  geom_hline(yintercept = -25, colour = "white", linetype=1) +
  geom_hline(yintercept = -15, colour = "white", linetype=1) +
  geom_hline(yintercept = -10, colour = "white", linetype=1) +
  geom_hline(yintercept = -5, colour = "white", linetype=1) +
  geom_hline(yintercept = 0, colour = "white", linetype=1) +
  geom_hline(yintercept = 5, colour = "white", linetype=1) +
  geom_hline(yintercept = 10, colour = "white", linetype=1) +
  geom_hline(yintercept = 15, colour = "white", linetype=1) +
  geom_hline(yintercept = 20, colour = "white", linetype=1) +
  geom_hline(yintercept = 25, colour = "white", linetype=1) +
  geom_hline(yintercept = 30, colour = "white", linetype=1) +
  geom_hline(yintercept = 35, colour = "white", linetype=1)

#print(p)



p <- p + 
  geom_vline(xintercept = 31, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 59, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 90, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 120, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 151, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 181, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 212, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 243, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 273, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 304, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 334, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 365, colour = "wheat4", linetype=3, size=.5) 

#print(p)


p <- p +
  coord_cartesian(ylim = c(-35,35)) +
  scale_y_continuous(breaks = seq(-35,35, by=5), labels = a) +
  scale_x_continuous(expand = c(0, 0), 
                     breaks = c(15,45,75,105,135,165,195,228,258,288,320,350),
                     labels = c("Styczeń", "Luty", "Marzec", "Kwiecień","Maj", "Czerwiec", "Lipiec", "Sierpień", "Wrzesień",                                "Październik", "Listopad", "Grudzień"))

#print(p)


p <- p +
  geom_point(data=PresentLows, aes(x=newDay, y=Temp), colour="blue3") +
  geom_point(data=PresentHighs, aes(x=newDay, y=Temp), colour="firebrick3")

#print(p)

p <- p +
  ggtitle(paste("Pogoda w Poznaniu", ROK)) +
  theme(plot.title=element_text(face="bold",hjust=.012,vjust=.8,colour="#3C3C3C",size=20)) +
  annotate("text", x = 19, y = 98, label = "Temperatura", size=4, fontface="bold")

# oblicz staty
head(DAY)
lata <- DAY %>% group_by(Year) %>% summarise( srednia=mean(Temp)) %>% arrange(desc(srednia))

miesiace <- DAY %>% filter(Year>= 1971 & Year<=2000) %>%  group_by(Month) %>% summarise(srednia =mean(Temp))
anomalie_mm <- DAY %>% filter(Year==ROK) %>% group_by(Month) %>% summarise(srednia =mean(Temp))
anomalie_mm <- round(anomalie_mm-miesiace,1)

brejki = c(15,45,75,105,135,165,195,228,258,288,320,350)
p <- p +  annotate("text", x = brejki, y = -30, label = format(anomalie_mm$srednia,nsmall = 1), size=3, colour=ifelse(anomalie_mm$srednia<0, yes="blue", no="red")) 


p <- p +
  annotate("text", x = 71, y = 34, 
           label = "Dane przedstawiają średnią dobową temperaturę powietrza od 1951 r.", size=3, colour="gray30") +
  annotate("text", x = 67, y = 31, 
           label = paste("Zestawienie obliczono na podst. danych do dn. ",format(max(Present$date),"%d %B")), size=3, colour="gray30") +
  annotate("text", x = 69, y = 28, 
           label = paste0("Średnia roczna temperatura w ", ROK, "r. wyniosła ", round(mean(Present$Temp),2), " *C " #, ROK, " the 9th coldest"
                          ), size=3, colour="gray30") + 
  annotate("text", x = 69, y = 25, label = paste0("Rok ", ROK, " był ", which(lata$Year==ROK), ". wśród najcieplszych lat po roku 1951"), size=3, colour="gray30")

#print(p)

no_of_coldest <- nrow(PresentLows)
no_of_hottest <- nrow(PresentHighs)



p <- p +
  annotate("segment", x = 30, xend = 40, y = -5, yend = -10, colour = "blue3") +
  annotate("text", x = 65, y = -20, label = paste0("Zaobserwowano ", no_of_coldest, " dni "), size=3, colour="blue3") +
  annotate("text", x = 56, y = -22, label = "najchłodniejszych od 1951", size=3, colour="blue3") +
  annotate("segment", x = PresentHighs$doy[nrow(PresentHighs)], xend = 305, y = PresentHighs$Temp[nrow(PresentHighs)], yend = 23, colour = "firebrick3") +
  annotate("text", x = 333, y = 24, label = paste("Zaobserwowano ", no_of_hottest, " dni "), size=3, colour="firebrick3") +
  annotate("text", x = 333, y = 22, label = "najcieplejszych od 1951", size=3, colour="firebrick3")

#print(p)



p <- p +  annotate("segment", x = 182, xend = 182, y = -15, yend = -3.9, colour = "wheat2", size=3) +
    annotate("segment", x = 182, xend = 182, y = -7.8, yend = -11.1, colour = "wheat4", size=3) +
    geom_line(data=legend_data, aes(x=x,y=y)) +
    annotate("segment", x = 184, xend = 186, y = -7.9, yend = -7.9, colour = "wheat4", size=.5) +
    annotate("segment", x = 184, xend = 186, y = -11, yend = -11, colour = "wheat4", size=.5) +
    annotate("segment", x = 185, xend = 185, y = -11, yend = -7.94, colour = "wheat4", size=.5) +
    annotate("text", x = 206, y = -9.58, label = "NORMALNY ZAKRES", size=2, colour="gray30") +
    annotate("text", x = 155, y = -9.58, label = paste0(ROK, " TEMPERATURA"), size=2, colour="gray30") +
    annotate("text", x = 203, y = -3.9, label = "REKORD CIEPŁA", size=2, colour="gray30") +
    annotate("text", x = 203, y = -15, label = "REKORD CHŁODU", size=2, colour="gray30")


p <- p+   annotate("rect", xmin = 310, xmax = 365, ymin = -36.5, ymax = -33.5, size=.5, alpha=0.2) + 
  annotate("text", x = 338, y = -35, label = "(c) Bartosz Czernecki 2017", size=3, colour="black") +
#  annotate("rect", xmin = 0, xmax = 65, ymin = -36.5, ymax = -33.5, size=.5, alpha=0.2) + 
  annotate("text", x = 42, y = -35, label = "Na podstawie koncepcji Edwarda Tufte'a (2001) \noraz rpubs.com/bradleyboehmke/weather_graphic", size=2.5, colour="darkgray") 
 
#svg(filename = "alamakota.svg", width = 14, height = 7 , pointsize = 18)
svg(filename = "alamakota.svg", width = 10.5, height = 5.25 , pointsize = 18)
print(p)
dev.off()

print(t1-Sys.time())
