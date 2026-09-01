#ggplot2 
#install.packages("ggplot2")
library(ggplot2)
dtTips=read.csv('tips.csv')
head(dtTips)

#univar gaphs
##++++++++++++++++++Pie chart
#Gender

fdtGender=table(dtTips$sex)
fdtGender

fdtGender=as.data.frame(fdtGender)

fdtGender
colnames(fdtGender)=c("Gender","Count")
fdtGender

##we use the FDT of Gender as input for ggplot

g0=ggplot(fdtGender, aes(x="", y=Count, fill=Gender))
g1=g0+geom_col()+
  coord_polar(theta = "y")+
  theme_void()+
  theme(plot.title = element_text(colour = "blue",
                                  size = 12, 
                                  face = "bold", 
                                  hjust = .5))+
  ggtitle('Gender Distribution of Customers')+
  geom_text(aes(label=Count), 
            position = position_stack(vjust = .5))+
  scale_fill_manual(values = c('#99FF33', '#BE2A3E'))+
  theme(legend.position = 'bottom')

ggsave('genderDist.png')

g1


###++++++++++++++++++++++Bar Chart

tGender=table(dtTips$sex)
tGender=as.data.frame(tGender)
colnames(tGender)=c('Gender', 'Count')

g0=ggplot(tGender, aes(x=Gender, y=Count, fill=Gender))
g0+geom_bar(stat='identity')+
  theme_classic()+
  theme(legend.position = '')+
  theme(axis.title.x = element_text(),
        axis.title.y = element_text(),
        plot.title = element_text(face = 'bold', hjust=.5))+
  ggtitle('Customers Gender Distribution')+
  geom_text(aes(label=Count), vjust=2)+
  scale_fill_manual(values=c('#FF9933', '#0000CC'))
ggsave('genderBar.pdf')


###++++++++++++++++++++++++++Histogram

g0=ggplot(dtTips, aes(x=tip))
g0+geom_histogram(bins = 10, fill='#99FFFF', colour=4)+
  theme_classic()+
  theme(plot.title = element_text(face = 'bold',
                                  hjust = .5), 
        axis.title.x = element_text(), 
        axis.title.y = element_text())+
  ggtitle('Tip Distribution')+
  xlab('Tip Amount')+
  ylab('Frequency')+
  geom_vline(xintercept = mean(dtTips$tip),
             linetype='dashed',
             color='red', 
             size=1)+
  geom_vline(xintercept = median(dtTips$tip),
             linetype='dashed',
             color='blue', 
             size=1)
ggsave('tipDistHist.png')


###++++++++++++++++++++Density plot

g0=ggplot(dtTips, aes(x=tip))
g0+geom_density(color='red', size=.1)+
  theme_classic()+
  xlim(0,12)+
  theme(plot.title = element_text(face = 'bold',
                                  hjust = .5), 
        axis.title = element_text(), 
        axis.title.y = element_text())+
  ggtitle('Tip Distribution')+
  xlab('Tip Amount')+
  ylab('Density')+
  geom_vline(xintercept = 5,
             linetype='dashed',
             color='blue', 
             size=1)+
  geom_hline(yintercept = .3,
             linetype='dashed',
             color='blue', 
             size=1)
ggsave('tipDistHist.png')


##ECDF 
g0=ggplot(dtTips, aes(x=tip))
g1=g0+stat_ecdf(geom = "step", 
                col="red")+
  theme_classic()+
  theme(plot.title = element_text(face = 'bold',
                                  hjust = .5), 
        axis.title = element_text(), 
        axis.title.y = element_text())+
  ggtitle('Tip C. Distribution')+
  xlab('Tip Amount')+
  ylab('C. Density')+
  geom_vline(xintercept = 5,
             linetype='dashed',
             color='blue', 
             size=1)+
  geom_hline(yintercept = .92,
             linetype='dashed',
             color='blue', 
             size=1)


g1

ecdf(dtTips$tip)(5)
###+++++++++++++++++++Box Plot

g0=ggplot(dtTips, aes(y='',x=tip))
g0+geom_boxplot(fill=5, 
                color=6, 
                alpha=0.3, 
                outlier.colour = 'blue', 
                linetype=2, 
                lwd=.6)+
  theme_classic()+
  theme(axis.title.x = element_text(), 
        plot.title = element_text(face = 'bold',
                                  hjust = .5, 
                                  color='darkgreen'))+
  ggtitle('Box Plot of the Tip')+
  xlab('Tip Amount')
ggsave('boxplotTip.png')