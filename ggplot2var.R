#Bivariate/Multivar graphs
#2 QL vars
#1 QL and 1 QNT
#2 QNTS
###+++++++++++++++++++Joint graphs
#Gender[F, M] and Smoker[Y, N] Join barplot for two QL vars
library(ggplot2)

dtTips=read.csv("tips.csv")

jtable=table(dtTips$sex, dtTips$smoker)
jtable
jtable=as.data.frame(jtable)
jtable

ggplot(jtable, aes(x=Var1, y=Freq, fill=Var2))+
  geom_col(position = position_dodge())+
  theme_classic()+
  theme(axis.title.x = element_text(),
        legend.title = element_text(color = 'blue'),
        plot.title = element_text(face = 'bold',
                                  hjust = .5, 
                                  color='darkgreen'),
        legend.position = "bottom")+
  ggtitle('Join bar graph of Gender and Smoking')+
  xlab('Gender')+
  guides(fill=guide_legend('Smoking'))

ggsave('jointBarGenderSmoke.pdf')


####+++++++++++++++++++Joint density 
##Gender[F, M]  tip for on QL and one QNT variables

g0=ggplot(dtTips, aes(x=tip, color=sex))
g0+geom_density()+
  theme_replace()+
  scale_color_manual(values = c('red', 'blue')) +
  xlim(-2,11)+
  theme(plot.title = element_text(face="bold",
                                  hjust = .5), 
        axis.title.x = element_text(),
        axis.title.y = element_text(), 
        legend.title = element_text(color='blue'),
        legend.position = "bottom")+
  ggtitle('Joint distribution of Tip amount across Gender')+
  xlab('Tib Amount')

ggsave('jointDensity.png')

#####+++++++++++++++++++Ridgeline plot
#smoker[y, n] tip 
#install.packages("ggridges", repos = "https://cloud.r-project.org")
library(ggridges)

ggplot(dtTips, aes(x=tip, y=sex, fill=sex))+
  geom_density_ridges(color=7, 
                      lwd=.5)+
  theme_gray()    +
  theme(plot.title = element_text(face = 'bold', 
                                  hjust = .5), 
        axis.title.x = element_text(),
        axis.title.y = element_text(), 
        legend.title = element_text(color='blue'),
        legend.position = "bottom")+
  ggtitle('Joint dist of Tip and Gender')+
  xlab('Tip Amount')

ggsave('jointDistTipsSmoker.png')


######+++++++++++++++++++Joint box plot
#Gender[y,N] tip


ggplot(dtTips, aes(x=tip, y=sex, fill=sex))+
  geom_boxplot(color=2, 
               alpha=0.3, 
               outlier.colour = 'blue', 
               linetype=2, 
               lwd=.6)+
  stat_boxplot(geom = 'errorbar', 
               width=.5)+
  theme_gray()+
  theme(plot.title = element_text(face = 'bold', 
                                  hjust = .5), 
        axis.title.x = element_text(),
        axis.title.y = element_text(), 
        legend.title = element_text(color='blue'), 
        legend.position = "bottom")+
  ggtitle('Joint dist of Tip and Gender')+
  xlab('Tip Amount')+
  ylab('Gender')+
  xlim(-1,11)
ggsave('jointboxplot.png')

######+++++++++++++++++++boxplot with points##Not part of ggplot

boxplot(dtTips$tip, col = 'white', horizontal = T)
stripchart(dtTips$tip, 
           method = 'jitter',
           pch=1, 
           col=4, 
           add = TRUE)

#help("stripchart")
######+++++++++++++++++++Joint boxplot
##Gender[y,n] tip

boxplot(tip~sex,
        data = dtTips,
        col='white',
        horizontal = T)
stripchart(tip~sex,
           data = dtTips,
           method = 'jitter', 
           pch=1, 
           col=2:4,
           add = TRUE)
ggsave('jointboxplotwithpoints.pdf')
####+++++++++++++++++++Beeswarm graph

install.packages('ggbeeswarm')

library(ggbeeswarm)
#smoker[y, n] tip

ggplot(dtTips, aes(x=smoker, y=tip, color=sex))+
  geom_beeswarm(cex=1)+
  theme(legend.position = "bottom", 
        legend.title = element_text(color='blue'))

ggsave("beeswarm.png")

#2 QNT vars: Scatter plot
g0=ggplot(dtTips, aes(x=total_bill, y=tip))
g0+geom_point()

#modifications inside geom_point
g0=ggplot(dtTips, aes(x=total_bill, y=tip))
g0+geom_point(color=1, shape=1, size=2)

names(dtTips)
#Modifications in aes
g0=ggplot(dtTips, aes(x=total_bill, y=tip, color=sex))
g0+geom_point()

#Modifications in both 
g0=ggplot(dtTips, aes(x=total_bill, y=tip, alpha=size))
g0+geom_point(color=4, size=2)

#Adding a vertical line and size
g0=ggplot(dtTips, aes(x=total_bill, y=tip,color=time, size=size))
g0+geom_point()+
  geom_vline(xintercept = 40, linetype='dashed')+
  geom_hline(yintercept = 5, linetype='dashed')

#Adding Facet:
g0=ggplot(dtTips, aes(x=total_bill, y=tip,color=time, size=size))
g0+geom_point()+
  facet_wrap(~day)

g0=ggplot(dtTips, aes(x=total_bill, y=tip, color=time, size=size))
g0+geom_point()+
  facet_wrap(~smoker)

##Adding Regression line
g0=ggplot(dtTips, aes(x=total_bill, y=tip))
g0+geom_smooth()

g0=ggplot(dtTips, aes(x=total_bill, y=tip, group=sex))
g0+geom_smooth()+
  geom_point()

names(dtTips)
#Adding more detials
g0=ggplot(dtTips, aes(x=total_bill, y=tip, color=sex))
g1=g0+geom_smooth(se=FALSE)+
  geom_point(mapping = aes(color=time))+
  theme_bw()+
  theme(axis.title.x = element_text(), 
        axis.title.y = element_text(), 
        plot.title = element_text(hjust = .5), 
        legend.title = element_blank())+
  ggtitle('Scatter plot total_bill/tip')+
  xlab('Total Bill in USD')+
  ylab('Tip Amount')+
  theme(legend.position = 'bottom')
g1

##Export as html and ggplotly
install.packages('plotly')
library(plotly)

g2=plotly::ggplotly(g1)
htmlwidgets::saveWidget(g2, 
                        file = 'scatter.html')
