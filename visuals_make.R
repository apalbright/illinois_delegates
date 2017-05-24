#Alex Albright (thelittledataset.com & @AllbriteAllday)

#Load libraries
library(ggplot2);library(ggrepel); library(ggthemes);library(plyr);library(reshape);library(grid);library(scales);library(RColorBrewer);library(gridExtra)

#Define theme for all visuals - thanks to Max Woolf (@minimaxir) for tips from his blog on how to format all this
 my_theme <- function() {

  # Define colors for the chart
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[4]
  color.panel = palette[3]
  color.axis.text = palette[9]
  color.axis.title = palette[9]
  color.title = palette[9]

  # Create basic construction of chart
  theme_bw(base_size=9, base_family="Georgia") + 

  # Set the entire chart region to a light gray color
  theme(panel.background=element_rect(fill=color.panel, color=color.background)) +
  theme(plot.background=element_rect(fill=color.background, color=color.background)) +
  theme(panel.border=element_rect(color=color.background)) +

  # Format grid
  theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
  theme(panel.grid.minor=element_blank()) +
  theme(axis.ticks=element_blank()) +

  # Format legend
  theme(legend.position="right") +
  theme(legend.background = element_rect(fill=color.panel)) +
  theme(legend.text = element_text(size=7,color=color.axis.title)) + 
  theme(legend.title = element_text(size=7,face="bold", color=color.axis.title)) + 
  
  #Format facet labels
  theme(strip.text.x = element_text(size = 8, face="bold"))+

  # Format title and axes labels these and tick marks
  theme(plot.title=element_text(color=color.title, size=18, vjust=0.5, hjust=0, face="bold")) +
  theme(axis.text.x=element_text(size=8,color=color.axis.text)) +
  theme(axis.text.y=element_text(size=8,color=color.axis.text)) +
  theme(axis.title.x=element_text(size=11,color=color.axis.title, vjust=-1, face="bold")) +
  theme(axis.title.y=element_text(size=11,color=color.axis.title, vjust=1.8, face="bold")) +

  #Format title and facet_wrap title
  theme(strip.text = element_text(size=6.5), plot.title = element_text(size = 14, face = "bold", colour = "black", vjust = 1, hjust=0.5))+
    
  # Plot margins
  theme(plot.margin = unit(c(.2, .2, .2, .2), "cm"))
}

#Import and format datasets
trump_data <- read.csv('illinois.csv', check.names=F)

agg<-aggregate(.~district+candidate, data=trump_data, mean)
agg$raceavg<-agg$namewhite
agg$numavg<-agg$votenum
agg$pctavg<-agg$votepct
cols=c("district", "raceavg", "numavg", "pctavg", "candidate")
agg<-agg[cols]

trump_all<-merge(trump_data, agg, by=c("district", "candidate"))   

trump_all$pctdiff<-(trump_all$votepct-trump_all$pctavg)*100
trump_all$racediff<-(trump_all$namewhite-trump_all$raceavg)*100

trump<-trump_all[which(trump_all$trump==1),]

#PART I
trump1<-ggplot(data=trump, aes(x=namewhite, y=votepct)) + 
  my_theme()+ theme(plot.margin = unit(c(.5, .7, .7, .3), "cm"))+  
  geom_point(size=1) +
  geom_smooth(method=lm)+
  labs(x="\nTrump Delegate's Perceived Whiteness",
  y="Trump Delegate Vote Percentage\n")+
  scale_x_continuous(limits=c(0,1), labels = percent_format())+
  scale_y_continuous(limits=c(0,.2), labels = percent_format())+
  ggtitle(expression(atop(bold("The Curious Case of The Illinois Trump Delegates (Part I)"), 
                          atop(italic("Concept Credit to David Wasserman & Dataset via Evan Soltas"), 
                               atop(italic("Visual created by: Alex Albright (thelittledataset.com & @AllbriteAllday)")),""))))

trump1
grid.text((paste("'Perceived whiteness' measure = % of individuals in the US who are non-Hispanic white among those who share a delegate's last name (according to the 2000 Census)")),
x = unit(0.515, "npc"), y = unit(.02, "npc"), just = c("center", "bottom"), 
gp = gpar(fontface = "italic", fontsize = 7, fontfamily="Georgia"))

                      
#PART II
trump2<-ggplot(data=trump, aes(x=racediff, y=pctdiff)) + 
  my_theme()+ theme(plot.margin = unit(c(.5, .5, .9, .35), "cm"))+  
  geom_hline(aes(yintercept=0), colour="grey45")+
  geom_vline(aes(xintercept=0), colour="grey45")+
  geom_point(size=1) +geom_smooth(method=lm)+
  scale_y_continuous(limits=c(-1.6,1.6), breaks=c(-1.6, -.8, 0, 0.8, 1.6))+
  labs(x="\nTrump Delegate's Perceived Whiteness Relative to the District",
    	y="Trump Delegate's Vote Percentage Relative to the District\n")+
  ggtitle(expression(atop(bold("The Curious Case of The Illinois Trump Delegates (Part II)"), 
                          atop(italic("Concept Credit to David Wasserman & Dataset via Evan Soltas"), 
                               atop(italic("Visual created by: Alex Albright (thelittledataset.com & @AllbriteAllday)")),""))))

trump2
grid.text((paste("X: Percentage point difference between a Trump delegate's perceived whiteness & the average Trump delegate's perceived whiteness in that district\nY: Percentage point difference between a Trump delegate's vote percentage & the average Trump delegate's vote percentage in that district")),
x = unit(0.53, "npc"), y = unit(0.01, "npc"), just = c("center", "bottom"), 
gp = gpar(fontface = "italic", fontsize = 7, fontfamily="Georgia"))
               
#PART III

#cut the 3 with too few dots
trump_all<-trump_all[trump_all$candidate!="Gilmore" & trump_all$candidate!="Huckabee" & trump_all$candidate!="Santorum",]

trumpall2<-ggplot(data=trump_all, aes(x=racediff, y=pctdiff)) + 
  my_theme()+ theme(plot.margin = unit(c(.5, .5, 1.3, .35), "cm"))+  
  facet_wrap(~candidate, ncol=3, scales="free")+
  geom_hline(aes(yintercept=0), colour="grey45")+
  geom_vline(aes(xintercept=0), colour="grey45")+
  geom_point(size=1) +geom_smooth(method=lm)+
  labs(x="\nDelegate's Perceived Whiteness Relative to the District",
  y="Delegate's Vote Percentage Relative to the District\n")+
  scale_y_continuous(limits=c(-1.6,1.6), breaks=c(-1.6, -.8, 0, 0.8, 1.6))+
  ggtitle(expression(atop(bold("The Curious Case of The Illinois Trump Delegates (Part III)"), 
                          atop(italic("Concept Credit to David Wasserman & Dataset via Evan Soltas"), 
                               atop(italic("Visual created by: Alex Albright (thelittledataset.com & @AllbriteAllday)")),""))))
               
trumpall2
grid.text((paste("Dropped Gilmore, Huckabee, and Santorum due to insufficient data\nX: Percentage point difference between a delegate's perceived whiteness & the delegate's average perceived whiteness in that district\nY: Percentage point difference between a delegate's vote percentage & the delegate's average vote percentage in that district")),
x = unit(0.53, "npc"), y = unit(0.01, "npc"), just = c("center", "bottom"), 
gp = gpar(fontface = "italic", fontsize = 7, fontfamily="Georgia"))


trumpk<-trump_all[which(trump_all$candidate=="Kasich"),]
fit2<-lm(pctdiff~racediff, data=trumpk)
summary(fit2)

trumpk<-trump_all[which(trump_all$candidate=="Trump"),]
fit2<-lm(pctdiff~racediff, data=trumpk)
summary(fit2)
