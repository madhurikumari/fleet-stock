
library(ggplot2)
library(RColorBrewer)
# Fleet Stock

setwd("/Users/mkumari/Downloads")

#function to plot stacked bar charts
stacked_bar <- function(file,left){
  #left=TRUE means y-axis shows up on left side. Else on right.
  #bottom=TRUE displays x-axis on bottom. Else is blank.
  df_Melted <- melt(file, id.var = "Type")
  colourCount = length(unique(df_Melted$variable))
  p <- ggplot(df_Melted, aes(width=0.95, x = Type, y = value, fill = forcats::fct_rev(variable))) + 
    geom_bar(stat = "identity", position="fill") +
    theme_bw()+ 
    scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(colourCount))+
    guides(colour = guide_legend(nrow = 1))+
    {if(left==TRUE)scale_y_continuous(labels = function(x) paste0(x*100, "%"))}+
    {if(left==FALSE)scale_y_continuous(labels = function(x) paste0(x*100, "%"), position="right")}+
    theme( 
      plot.title = element_text(hjust = 0.5, size=15), 
      axis.title.x=element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size=13),
      legend.title = element_blank(),
      legend.text = element_text(size=15),
      legend.position = "none")
  return(p)
}


df <- read.csv("MK_FromMarshall - FleetStockBAU2030.csv", header = TRUE)
df2 <- read.csv("MK_FromMarshall - FleetStockBAU2050.csv", header = TRUE)
df3 <- read.csv("MK_FromMarshall - FleetStockZEV2030.csv", header = TRUE)
df4 <- read.csv("MK_FromMarshall - FleetStockZEV2050.csv", header = TRUE)
df5 <- read.csv("MK_FromMarshall - FleetStockZEV+B2030.csv", header = TRUE)
df6 <- read.csv("MK_FromMarshall - FleetStockZEV+B2050.csv", header = TRUE)


p1 <- stacked_bar(df, TRUE) + labs(title = "BAU 2030") +theme(axis.text.x = element_blank())
p2 <- stacked_bar(df2, FALSE)+  labs(title = "BAU 2050")+theme(axis.text.x = element_blank())
p3 <- stacked_bar(df3, TRUE) + labs(title = " ZEV 2030")+theme(axis.text.x = element_blank())
p4 <- stacked_bar(df4, FALSE) +   labs(title = "ZEV 2050")+theme(axis.text.x = element_blank())
p5 <- stacked_bar(df5, TRUE) +   labs(title = " ZEV + B 2030") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=15))
p6 <- stacked_bar(df6, FALSE) +   labs(title = "ZEV + B 2050")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=15))


p_final <- ggarrange(p1, p2, p3, p4,p5,p6, ncol=2, nrow=3, common.legend = TRUE, legend="right",
                     heights = c(1,1,1.55), widths = c(1.1,1))
annotate_figure(p_final, top=text_grob(label="Fleet Stock", hjust=1, size=20),
                left=text_grob("Percentage Fleet Stock (%)", hjust=0.5, size=15,rot = 90))

