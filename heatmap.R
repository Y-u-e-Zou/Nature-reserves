#============================================================================================

#α各样点分属谱系_保护区内外
library(picante) 
otuZY_1 <- read.csv("α多样性各样点分属_1.9.2.3_9.4谱系.csv", header=T, row.names = 1)

meta1 <- read.table("3.2.1.5_sample-metadata_simple_next_filter_R.txt", header=T, 
                    sep="\t", quote = "", row.names=1, comment.char="",stringsAsFactors = FALSE)
otuZY_1

#删除meta1的Disturbance
meta1 <- meta1[,-which(c(names(meta1) == "Disturbance"))]
meta1

otu_without_near <- rownames(otuZY_1)
otu_without_near
meta1
meta2 <- subset(meta1, rownames(meta1) %in% otu_without_near)
meta2

#删除植被类型
meta2 <- meta2[,-which(colnames(meta2) == "vegetation_type")]


meta <- meta2
otuZY_1 <- data.frame(otuZY_1)

str(meta)
str(otuZY_1)

library(dplyr)
#tb_meta <- meta[,-which(names(meta) == "Province")]    #删除type列
tb_meta <- meta


library(tidyverse)
library(forcats)

#############################################
tb_meta[c("Climate",
          "Nature_Reserve", 
          "Year","Vegetation_type_summarize","Weather","Area"
          #,"Disturbance"
)] <- lapply(tb_meta[c("Climate",
                       "Nature_Reserve",
                       "Year","Vegetation_type_summarize","Weather","Area"
                       #,"Disturbance"
)], factor)
str(tb_meta)
tb_meta$Climate <- as.numeric(tb_meta$Climate)
tb_meta$Year <- as.numeric(tb_meta$Year)
tb_meta$Nature_Reserve <- as.numeric(tb_meta$Nature_Reserve)
tb_meta$Vegetation_type_summarize <- as.numeric(tb_meta$Vegetation_type_summarize)
tb_meta$Weather <- as.numeric(tb_meta$Weather)
tb_meta$Area <- as.numeric(tb_meta$Area)
#tb_meta$Disturbance <- as.numeric(tb_meta$Disturbance)
tb_meta
##############################################

str(tb_meta)


library(tidyverse)
library(RColorBrewer)
library(ggtext)
library(magrittr)
library(reshape)
library(psych)


table1 <- tb_meta[order(rownames(tb_meta)),]
table1
table2 <- otuZY_1[order(rownames(otuZY_1)),]
table2

pp <- corr.test(table1,table2,method="spearman",adjust = "fdr")

df <- melt(pp$r) %>% mutate(pvalue=melt(pp$p)[,3],
                            p_signif=symnum(pvalue, corr = FALSE, na = FALSE,  
                                            cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                                            symbols = c("***", "**", "*", "", " "))) %>% 
  set_colnames(c("env","genus","r","p","p_signif")) %>% 
  mutate(p=round(p,digits = 2)) %>% 
  unite(.,col="p_value",p:p_signif,sep="",remove = T,na.rm = F)


Genus_in_near <- ggplot()+
  geom_tile(data= df %>% filter(!str_detect(p_value, "\\*")),
            aes(genus,env,fill=r),alpha = 0.5)+
  geom_text(data= df %>% filter(!str_detect(p_value, "\\*")),
            aes(genus,env,col=r,fill=r,label=p_value),
            size=3,color="grey70",hjust=0.5,vjust=0.7)+
  geom_tile(data= df %>% filter(str_detect(p_value, "\\*")),
            aes(genus,env,col=r,fill=r),alpha=1)+
  geom_text(data= df %>% filter(str_detect(p_value, "\\*")),
            aes(genus,env,col=r,fill=r,label=p_value),
            size=3,color="black",hjust=0.5,vjust=0.7)+
  scale_color_gradientn(colours = rev(RColorBrewer::brewer.pal(11,"RdBu")))+
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11,"RdBu")))+
  scale_y_discrete(expand=c(0,0),position = 'right')+
  scale_x_discrete(expand=c(0,0)) +
  theme(axis.text.x=element_text(angle =90,hjust =1,vjust =0.5,
                                 color="black",face = "bold",size = 10),
        axis.text.y=element_text(color="black",face = "bold",size =10),
        axis.ticks= element_blank(),
        panel.spacing.y = unit(0,"cm"),
        plot.background = element_blank(),
        panel.background = element_blank())+
  scale_size(range=c(1,10),guide=NULL)+
  guides(color=guide_colorbar(direction="vertical",reverse=F,barwidth=unit(.3,"cm"),
                              barheight=unit(6,"cm")))
x <- Genus_in_near + theme(
  legend.position.inside=c(1.05, 1),
  legend.justification=c(0, 1))

pdf("α多样性各样点分属谱系_保护区内外.pdf", width = 5.0, height = 6.5)
x
dev.off()

#============================================================================================

#α各样点分属谱系_仅保护区内
library(picante) 
otuZY_1 <- read.csv("α多样性各样点分属_1.9.2.3_9.4谱系.csv", header=T, row.names = 1)

meta1 <- read.table("3.2.1.5_sample-metadata_simple_next_filter_R.txt", header=T, 
                    sep="\t", quote = "", row.names=1, comment.char="",stringsAsFactors = FALSE)
otuZY_1

#去掉Nature_Reserve列
meta1 <- meta1[,-which(c(names(meta1) == "Nature_Reserve"))]
meta1


otu_without_near <- rownames(otuZY_1)
otu_without_near
meta1
meta2 <- subset(meta1, rownames(meta1) %in% otu_without_near)   #筛选meta1的行名为otu_without_near的行
meta2

#删除植被类型
meta2 <- meta2[,-which(colnames(meta2) == "vegetation_type")]

#去掉Disturbance列中含NA的行
meta2 <- na.omit(meta2)                     ##去掉Disturbance列中含NA的行
meta2

meta2_without_Nature <- rownames(meta2)
meta2_without_Nature

#筛选otuZY_1的行名为meta2_without_Nature的行
otuZY_1 <- subset(otuZY_1, rownames(otuZY_1) %in% meta2_without_Nature)
otuZY_1


meta <- meta2
otuZY_1 <- data.frame(otuZY_1)

str(meta)
str(otuZY_1)

library(dplyr)
tb_meta <- meta


library(tidyverse)
library(forcats)

#############################################
tb_meta[c("Climate",
          #"Nature_Reserve", 
          "Year","Vegetation_type_summarize","Weather","Area"
          ,"Disturbance"
)] <- lapply(tb_meta[c("Climate",
                       #"Nature_Reserve",
                       "Year","Vegetation_type_summarize","Weather","Area"
                       ,"Disturbance"
)], factor)

str(tb_meta)
tb_meta$Climate <- as.numeric(tb_meta$Climate)
tb_meta$Year <- as.numeric(tb_meta$Year)
#tb_meta$Nature_Reserve <- as.numeric(tb_meta$Nature_Reserve)
tb_meta$Vegetation_type_summarize <- as.numeric(tb_meta$Vegetation_type_summarize)
tb_meta$Weather <- as.numeric(tb_meta$Weather)
tb_meta$Area <- as.numeric(tb_meta$Area)
tb_meta$Disturbance <- as.numeric(tb_meta$Disturbance)
tb_meta
##############################################

str(tb_meta)


library(tidyverse)
library(RColorBrewer)
library(ggtext)
library(magrittr)
library(reshape)
library(psych)


table1 <- tb_meta[order(rownames(tb_meta)),]
table1
table2 <- otuZY_1[order(rownames(otuZY_1)),]
table2

pp <- corr.test(table1,table2,method="spearman",adjust = "fdr")

df <- melt(pp$r) %>% mutate(pvalue=melt(pp$p)[,3],
                            p_signif=symnum(pvalue, corr = FALSE, na = FALSE,  
                                            cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                                            symbols = c("***", "**", "*", "", " "))) %>% 
  set_colnames(c("env","genus","r","p","p_signif")) %>% 
  mutate(p=round(p,digits = 2)) %>% 
  unite(.,col="p_value",p:p_signif,sep="",remove = T,na.rm = F)


Genus_in_near <- ggplot()+
  geom_tile(data= df %>% filter(!str_detect(p_value, "\\*")),
            aes(genus,env,fill=r),alpha = 0.5)+
  geom_text(data= df %>% filter(!str_detect(p_value, "\\*")),
            aes(genus,env,col=r,fill=r,label=p_value),
            size=3,color="grey70",hjust=0.5,vjust=0.7)+
  geom_tile(data= df %>% filter(str_detect(p_value, "\\*")),
            aes(genus,env,col=r,fill=r),alpha=1)+
  geom_text(data= df %>% filter(str_detect(p_value, "\\*")),
            aes(genus,env,col=r,fill=r,label=p_value),
            size=3,color="black",hjust=0.5,vjust=0.7)+
  #  labs(x = NULL,y = NULL,color=NULL,fill=NULL)+
  scale_color_gradientn(colours = rev(RColorBrewer::brewer.pal(11,"RdBu")))+
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11,"RdBu")))+
  scale_y_discrete(expand=c(0,0),position = 'right')+
  scale_x_discrete(expand=c(0,0)) +
  theme(axis.text.x=element_text(angle =90,hjust =1,vjust =0.5,
                                 color="black",face = "bold",size = 10),
        axis.text.y=element_text(color="black",face = "bold",size =10),
        axis.ticks= element_blank(),
        panel.spacing.y = unit(0,"cm"),
        plot.background = element_blank(),
        panel.background = element_blank())+
  scale_size(range=c(1,10),guide=NULL)+
  guides(color=guide_colorbar(direction="vertical",reverse=F,barwidth=unit(.3,"cm"),
                              barheight=unit(6,"cm")))
x <- Genus_in_near + theme(
  legend.position.inside=c(1.05, 1),
  legend.justification=c(0, 1))

pdf("α多样性各样点分属谱系_仅保护区内.pdf", width = 5.0, height = 6.5)
x
dev.off()



#==========================================================================================================================================
#==========================================================================================================================================




#####

#α各样点分属各类多样性-保护区内外

otuZY_1 <- read.csv("α多样性各样点分属_1.9.2.3_9.4各类多样性.csv", header=T, row.names = 1)

meta1 <- read.table("3.2.1.5_sample-metadata_simple_next_filter_R.txt", header=T, 
                    sep="\t", quote = "", row.names=1, comment.char="",stringsAsFactors = FALSE)
otuZY_1

otuZY_1 <- otuZY_1[,-which(c(names(otuZY_1) == "Inverse.Simpson_Cav") | c(names(otuZY_1) == "Inverse.Simpson_Dic") | c(names(otuZY_1) == "Inverse.Simpson_Pol") | c(names(otuZY_1) == "Inverse.Simpson_Hag") | c(names(otuZY_1) == "Inverse.Simpson_Rap"))]
otuZY_1 <- otuZY_1[,-which(c(names(otuZY_1) == "Pielou_Cav" | names(otuZY_1) == "Pielou_Dic" | names(otuZY_1) == "Pielou_Pol" | names(otuZY_1) == "Pielou_Hag" | names(otuZY_1) == "Pielou_Rap"))]

otuZY_1

#删除meta1的Disturbance
meta1 <- meta1[,-which(c(names(meta1) == "Disturbance"))]
meta1


otu_without_near <- rownames(otuZY_1)
otu_without_near
meta1
meta2 <- subset(meta1, rownames(meta1) %in% otu_without_near)
meta2

#删除植被类型
meta2 <- meta2[,-which(colnames(meta2) == "vegetation_type")]

meta <- meta2
otuZY_1 <- data.frame(otuZY_1)

str(meta)
str(otuZY_1)



tb_meta <- meta




#############################################
tb_meta[c("Climate",
          "Nature_Reserve", 
          "Year","Vegetation_type_summarize","Weather","Area"
          #,"Disturbance"
)] <- lapply(tb_meta[c("Climate",
                       "Nature_Reserve",
                       "Year","Vegetation_type_summarize","Weather","Area"
                       #,"Disturbance"
)], factor)
str(tb_meta)
tb_meta$Climate <- as.numeric(tb_meta$Climate)
tb_meta$Year <- as.numeric(tb_meta$Year)
tb_meta$Nature_Reserve <- as.numeric(tb_meta$Nature_Reserve)
tb_meta$Vegetation_type_summarize <- as.numeric(tb_meta$Vegetation_type_summarize)
tb_meta$Weather <- as.numeric(tb_meta$Weather)
tb_meta$Area <- as.numeric(tb_meta$Area)
#tb_meta$Disturbance <- as.numeric(tb_meta$Disturbance)
tb_meta
##############################################

str(tb_meta)





table2 <- tb_meta[order(rownames(tb_meta)),]
table2
table1 <- otuZY_1[order(rownames(otuZY_1)),]
table1



library(psych)
pp <- corr.test(table1,table2,method="spearman",adjust = "fdr")



library(dplyr)
library(tidyverse)
library(magrittr)
library(reshape)
df <- melt(pp$r) %>% mutate(pvalue=melt(pp$p)[,3],
                            p_signif=symnum(pvalue, corr = FALSE, na = FALSE,  
                                            cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                                            symbols = c("***", "**", "*", "", " "))) %>% 
  set_colnames(c("env","genus","r","p","p_signif")) %>% 
  mutate(p=round(p,digits = 2)) %>%                              #小数点后2位
  unite(.,col="p_value",p:p_signif,sep="",remove = T,na.rm = F)


Genus_in_near <- ggplot()+                                                       ######可信的（p值小于0.05）不透明，不可信半透明；相关性越强颜色越深，红正相关，蓝负相关
  geom_tile(data= df %>% filter(!str_detect(p_value, "\\*")),
            aes(genus,env,fill=r),alpha = 0.5)+                 #填充颜色到差异不显著的方格
  geom_text(data= df %>% filter(!str_detect(p_value, "\\*")),
            aes(genus,env,col=r,fill=r,label=p_value),
            size=3,color="grey70",hjust=0.5,vjust=0.7)+         #填充置信p值数字到差异不显著的方格
  geom_tile(data= df %>% filter(str_detect(p_value, "\\*")),
            aes(genus,env
                ,col=r                    #右侧图例，以及差异显著方格的边框的颜色
                ,fill=r                   #填充r的颜色到差异显著的方格
            ),alpha=1)+               #差异显著的方格不透明
  geom_text(data= df %>% filter(str_detect(p_value, "\\*")),     #str_detect是看p_value是否符合"\\*"
            aes(genus,env,col=r,fill=r,label=p_value),
            size=3,color="black",hjust=0.5,vjust=0.7)+          #填充置信p值数字到差异显著的方格
  #  labs(x = NULL,y = NULL,color=NULL,fill=NULL)+                 #删除最右侧图例顶端的名字
  scale_color_gradientn(colours = rev(RColorBrewer::brewer.pal(11,"RdBu")))+     #右侧本来两个图例合并成一个
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11,"RdBu")))+      #给正相关标红
  scale_y_discrete(expand=c(0,0),position = 'right'
  )+                                            #控制y轴名称
  scale_x_discrete(expand=c(0,0)) +                      #给图拉宽一点（正值会让图变窄）
  theme(axis.text.x=element_text(angle =90, hjust =1,vjust =0.5,
                                 color="black",face = "bold",size = 10),    #x轴元素位置和加粗之类
        axis.text.y=element_text(color="black",face = "bold",size =10),     #y轴元素加粗之类
        axis.ticks= element_blank(),                  #x轴去掉下边本来有的刻录
        panel.spacing.y = unit(0,"cm"),               #y轴间距
        plot.background = element_blank(),
        panel.background = element_blank()            #整体变亮一点
  )+
  scale_size(range=c(1,10),guide=NULL)+            #数据点的大小范围
  guides(color=guide_colorbar(
    direction="vertical",                    #图例垂直显示
    reverse=F,                              #图例顺序
    barwidth=unit(.5,"cm"),                 #图例宽
    barheight=unit(11,"cm")                 #图例高
  ))
x <- Genus_in_near + theme(
  legend.position.inside=c(1.05, 1),
  legend.justification=c(0, 1))              #图例位置

pdf("α各样点分属各类多样性-保护区内外.pdf", width = 7, height = 18)
x
dev.off()
#========================================================================================================================
#========================================================================================================================





#α各样点分属各类多样性_仅保护区内
library(picante) 
otuZY_1 <- read.csv("α多样性各样点分属_1.9.2.3_9.4各类多样性.csv", header=T, row.names = 1)

meta1 <- read.table("3.2.1.5_sample-metadata_simple_next_filter_R.txt", header=T, 
                    sep="\t", quote = "", row.names=1, comment.char="",stringsAsFactors = FALSE)
otuZY_1


#去掉拥有Tnf和NaN数值的Pielou和Inverse.Simpson
otuZY_1 <- otuZY_1[,-which(c(names(otuZY_1) == "Inverse.Simpson_Cav") | c(names(otuZY_1) == "Inverse.Simpson_Dic") | c(names(otuZY_1) == "Inverse.Simpson_Pol") | c(names(otuZY_1) == "Inverse.Simpson_Hag") | c(names(otuZY_1) == "Inverse.Simpson_Rap"))]
otuZY_1 <- otuZY_1[,-which(c(names(otuZY_1) == "Pielou_Cav" | names(otuZY_1) == "Pielou_Dic" | names(otuZY_1) == "Pielou_Pol" | names(otuZY_1) == "Pielou_Hag" | names(otuZY_1) == "Pielou_Rap"))]

otuZY_1
meta1

#去掉Nature_Reserve列
meta1 <- meta1[,-which(c(names(meta1) == "Nature_Reserve"))]
meta1


otu_without_near <- rownames(otuZY_1)
otu_without_near
meta1
meta2 <- subset(meta1, rownames(meta1) %in% otu_without_near)   #筛选meta1的行名为otu_without_near的行
meta2

#删除植被类型
meta2 <- meta2[,-which(colnames(meta2) == "vegetation_type")]

#去掉Disturbance列中含NA的行
meta2 <- na.omit(meta2)                     ##去掉Disturbance列中含NA的行
meta2

meta2_without_Nature <- rownames(meta2)
meta2_without_Nature

#筛选otuZY_1的行名为meta2_without_Nature的行
otuZY_1 <- subset(otuZY_1, rownames(otuZY_1) %in% meta2_without_Nature)
otuZY_1

meta <- meta2
otuZY_1 <- data.frame(otuZY_1)

str(meta)
str(otuZY_1)

library(dplyr)
tb_meta <- meta


library(tidyverse)
library(forcats)

#############################################
tb_meta[c("Climate",
          #"Nature_Reserve", 
          "Year","Vegetation_type_summarize","Weather","Area","Disturbance")] <- lapply(tb_meta[c("Climate",
                                                                                                  #"Nature_Reserve", 
                                                                                                  "Year","Vegetation_type_summarize","Weather","Area","Disturbance")], factor)
str(tb_meta)
tb_meta$Climate <- as.numeric(tb_meta$Climate)
tb_meta$Year <- as.numeric(tb_meta$Year)
#tb_meta$Nature_Reserve <- as.numeric(tb_meta$Nature_Reserve)
tb_meta$Vegetation_type_summarize <- as.numeric(tb_meta$Vegetation_type_summarize)
tb_meta$Weather <- as.numeric(tb_meta$Weather)
tb_meta$Area <- as.numeric(tb_meta$Area)
tb_meta$Disturbance <- as.numeric(tb_meta$Disturbance)
tb_meta
##############################################

str(tb_meta)


library(tidyverse)
library(RColorBrewer)
library(ggtext)
library(magrittr)
library(reshape)
library(psych)


table2 <- tb_meta[order(rownames(tb_meta)),]
table2
table1 <- otuZY_1[order(rownames(otuZY_1)),]
table1



pp <- corr.test(table1,table2,method="spearman",adjust = "fdr")

df <- melt(pp$r) %>% mutate(pvalue=melt(pp$p)[,3],
                            p_signif=symnum(pvalue, corr = FALSE, na = FALSE,  
                                            cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                                            symbols = c("***", "**", "*", "", " "))) %>% 
  set_colnames(c("env","genus","r","p","p_signif")) %>% 
  mutate(p=round(p,digits = 2)) %>%                              #小数点后2位
  unite(.,col="p_value",p:p_signif,sep="",remove = T,na.rm = F)


Genus_in_near <- ggplot()+                                                       ######可信的（p值小于0.05）不透明，不可信半透明；相关性越强颜色越深，红正相关，蓝负相关
  geom_tile(data= df %>% filter(!str_detect(p_value, "\\*")),
            aes(genus,env,fill=r),alpha = 0.5)+                 #填充颜色到差异不显著的方格
  geom_text(data= df %>% filter(!str_detect(p_value, "\\*")),
            aes(genus,env,col=r,fill=r,label=p_value),
            size=3,color="grey70",hjust=0.5,vjust=0.7)+         #填充置信p值数字到差异不显著的方格
  geom_tile(data= df %>% filter(str_detect(p_value, "\\*")),
            aes(genus,env
                ,col=r                    #右侧图例，以及差异显著方格的边框的颜色
                ,fill=r                   #填充r的颜色到差异显著的方格
            ),alpha=1)+               #差异显著的方格不透明
  geom_text(data= df %>% filter(str_detect(p_value, "\\*")),     #str_detect是看p_value是否符合"\\*"
            aes(genus,env,col=r,fill=r,label=p_value),
            size=3,color="black",hjust=0.5,vjust=0.7)+          #填充置信p值数字到差异显著的方格
  #  labs(x = NULL,y = NULL,color=NULL,fill=NULL)+                 #删除最右侧图例顶端的名字
  scale_color_gradientn(colours = rev(RColorBrewer::brewer.pal(11,"RdBu")))+     #右侧本来两个图例合并成一个
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11,"RdBu")))+      #给正相关标红
  scale_y_discrete(expand=c(0,0),position = 'right'
  )+                                            #控制y轴名称
  scale_x_discrete(expand=c(0,0)) +                      #给图拉宽一点（正值会让图变窄）
  theme(axis.text.x=element_text(angle =90, hjust =1,vjust =0.5,
                                 color="black",face = "bold",size = 10),    #x轴元素位置和加粗之类
        axis.text.y=element_text(color="black",face = "bold",size =10),     #y轴元素加粗之类
        axis.ticks= element_blank(),                  #x轴去掉下边本来有的刻录
        panel.spacing.y = unit(0,"cm"),               #y轴间距
        plot.background = element_blank(),
        panel.background = element_blank()            #整体变亮一点
  )+
  scale_size(range=c(1,10),guide=NULL)+            #数据点的大小范围
  guides(color=guide_colorbar(
    direction="vertical",                    #图例垂直显示
    reverse=F,                              #图例顺序
    barwidth=unit(.5,"cm"),                 #图例宽
    barheight=unit(11,"cm")                 #图例高
  ))
x <- Genus_in_near + theme(
  legend.position.inside=c(1.05, 1),
  legend.justification=c(0, 1))              #图例位置

pdf("α多样性各样点分属各类多样性_仅保护区内.pdf", width = 7, height = 18)
x
dev.off()

#============================================================================================
#============================================================================================
#1.7.3.1保护区内α多样性和环境_1.7.2
library(picante) 
otuZY_1 <- read.csv("α多样性计算结果谱系-ZY_1.7.2.csv", header=T, row.names = 1)
meta1 <- read.table("3.2.1.5_sample-metadata_simple_next_filter_R.txt", header=T, 
                    sep="\t", quote = "", row.names=1, comment.char="",stringsAsFactors = FALSE)
#去掉Nature_Reserve列
meta1 <- meta1[,-which(c(names(meta1) == "Nature_Reserve"))]
meta1


otu_without_near <- rownames(otuZY_1)
otu_without_near
meta1
meta2 <- subset(meta1, rownames(meta1) %in% otu_without_near)   #筛选meta1的行名为otu_without_near的行
meta2

#删除植被类型
meta2 <- meta2[,-which(colnames(meta2) == "vegetation_type")]

#去掉Disturbance列中含NA的行
meta2 <- na.omit(meta2)                     ##去掉Disturbance列中含NA的行
meta2

meta2_without_Nature <- rownames(meta2)
meta2_without_Nature

#筛选otuZY_1的行名为meta2_without_Nature的行
otuZY_1 <- subset(otuZY_1, rownames(otuZY_1) %in% meta2_without_Nature)
otuZY_1


meta <- meta2
otuZY_1 <- data.frame(otuZY_1)

str(meta)
str(otuZY_1)

library(tidyverse)
library(forcats)


tb_meta <- meta





#############################################
tb_meta[c("Climate",
          #"Nature_Reserve", 
          "Year","Vegetation_type_summarize","Weather","Area","Disturbance")] <- lapply(tb_meta[c("Climate",
                                                                                                  #"Nature_Reserve", 
                                                                                                  "Year","Vegetation_type_summarize","Weather","Area","Disturbance")], factor)
str(tb_meta)
tb_meta$Climate <- as.numeric(tb_meta$Climate)
tb_meta$Year <- as.numeric(tb_meta$Year)
#tb_meta$Nature_Reserve <- as.numeric(tb_meta$Nature_Reserve)
tb_meta$Vegetation_type_summarize <- as.numeric(tb_meta$Vegetation_type_summarize)
tb_meta$Weather <- as.numeric(tb_meta$Weather)
tb_meta$Area <- as.numeric(tb_meta$Area)
tb_meta$Disturbance <- as.numeric(tb_meta$Disturbance)
tb_meta
##############################################




str(tb_meta)



library(tidyverse)
library(RColorBrewer)
library(ggtext)
library(magrittr)
library(reshape)
library(psych)






table1 <- tb_meta[order(rownames(tb_meta)),]
table1
table2 <- otuZY_1[order(rownames(otuZY_1)),]
table2




pp <- corr.test(table1,table2,method="spearman",adjust = "fdr")

df <- melt(pp$r) %>% mutate(pvalue=melt(pp$p)[,3],
                            p_signif=symnum(pvalue, corr = FALSE, na = FALSE,  
                                            cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                                            symbols = c("***", "**", "*", "", " "))) %>% 
  set_colnames(c("env","genus","r","p","p_signif")) %>% 
  mutate(p=round(p,digits = 2)) %>% 
  unite(.,col="p_value",p:p_signif,sep="",remove = T,na.rm = F)


p_nei<-ggplot()+
  geom_tile(data= df %>% filter(!str_detect(p_value, "\\*")),
            aes(genus,env,fill=r),alpha = 0.5)+
  geom_text(data= df %>% filter(!str_detect(p_value, "\\*")),
            aes(genus,env,col=r,fill=r,label=p_value),
            size=3,color="grey70",hjust=0.5,vjust=0.7)+
  geom_tile(data= df %>% filter(str_detect(p_value, "\\*")),
            aes(genus,env,col=r,fill=r),alpha=1)+
  geom_text(data= df %>% filter(str_detect(p_value, "\\*")),
            aes(genus,env,col=r,fill=r,label=p_value),
            size=3,color="black",hjust=0.5,vjust=0.7)+
  labs(x = NULL,y = NULL,color=NULL,fill=NULL)+
  scale_color_gradientn(colours = rev(RColorBrewer::brewer.pal(11,"RdBu")))+
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11,"RdBu")))+
  scale_y_discrete(expand=c(0,0),position = 'right')+
  scale_x_discrete(expand=c(0,0)) +
  theme(axis.text.x=element_text(angle =90,hjust =1,vjust =0.5,
                                 color="black",face = "bold",size = 10),
        axis.text.y=element_text(color="black",face = "bold",size =10),
        axis.ticks= element_blank(),
        panel.spacing.y = unit(0,"cm"),
        plot.background = element_blank(),
        panel.background = element_blank())+
  scale_size(range=c(1,10),guide=NULL)+
  guides(color=guide_colorbar(direction="vertical",reverse=F,barwidth=unit(.5,"cm"),
                              barheight=unit(11,"cm")))


pdf("保护区内α多样性和环境_1.7.2.pdf", width = 5, height = 5.5)
p_nei
dev.off()
#============================================================================================
#========================================================================================================================





#各物种clones多样性_仅保护区内
library(picante) 
otuZY_1 <- read.table("1.7.1_clones_g-1_R.txt", header=T, 
                      sep="\t", quote = "", row.names=1, comment.char="",stringsAsFactors = FALSE)

meta1 <- read.table("3.2.1.5_sample-metadata_simple_next_filter_R.txt", header=T, 
                    sep="\t", quote = "", row.names=1, comment.char="",stringsAsFactors = FALSE)
otuZY_1 <- t(otuZY_1)



otuZY_1
meta1

#去掉Nature_Reserve列
meta1 <- meta1[,-which(c(names(meta1) == "Nature_Reserve"))]
meta1


otu_without_near <- rownames(otuZY_1)
otu_without_near
meta1
meta2 <- subset(meta1, rownames(meta1) %in% otu_without_near)   #筛选meta1的行名为otu_without_near的行
meta2

#删除植被类型
meta2 <- meta2[,-which(colnames(meta2) == "vegetation_type")]

#去掉Disturbance列中含NA的行

meta2 <- na.omit(meta2)                     ##去掉Disturbance列中含NA的行
meta2

meta2_without_Nature <- rownames(meta2)
meta2_without_Nature

#筛选otuZY_1的行名为meta2_without_Nature的行
otuZY_1 <- subset(otuZY_1, rownames(otuZY_1) %in% meta2_without_Nature)
otuZY_1

meta <- meta2
otuZY_1 <- data.frame(otuZY_1)
otuZY_1

#去除整列都是0的列
otuZY_1 <- otuZY_1[, colSums(otuZY_1 == 0) != nrow(otuZY_1)]
otuZY_1


str(meta)
str(otuZY_1)

library(dplyr)

tb_meta <- meta


library(tidyverse)
library(forcats)

#############################################
tb_meta[c("Climate",
          #"Nature_Reserve", 
          "Year","Vegetation_type_summarize","Weather","Area","Disturbance")] <- lapply(tb_meta[c("Climate",
                                                                                                  #"Nature_Reserve", 
                                                                                                  "Year","Vegetation_type_summarize","Weather","Area","Disturbance")], factor)
str(tb_meta)
tb_meta$Climate <- as.numeric(tb_meta$Climate)
tb_meta$Year <- as.numeric(tb_meta$Year)
#tb_meta$Nature_Reserve <- as.numeric(tb_meta$Nature_Reserve)
tb_meta$Vegetation_type_summarize <- as.numeric(tb_meta$Vegetation_type_summarize)
tb_meta$Weather <- as.numeric(tb_meta$Weather)
tb_meta$Area <- as.numeric(tb_meta$Area)
tb_meta$Disturbance <- as.numeric(tb_meta$Disturbance)
tb_meta
##############################################

str(tb_meta)


library(tidyverse)
library(RColorBrewer)
library(ggtext)
library(magrittr)
library(reshape)
library(psych)


table2 <- tb_meta[order(rownames(tb_meta)),]
table2
table1 <- otuZY_1[order(rownames(otuZY_1)),]
table1



pp <- corr.test(table1,table2,method="spearman",adjust = "fdr")

df <- melt(pp$r) %>% mutate(pvalue=melt(pp$p)[,3],
                            p_signif=symnum(pvalue, corr = FALSE, na = FALSE,  
                                            cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                                            symbols = c("***", "**", "*", "", " "))) %>% 
  set_colnames(c("env","genus","r","p","p_signif")) %>% 
  mutate(p=round(p,digits = 2)) %>%                              #小数点后2位
  unite(.,col="p_value",p:p_signif,sep="",remove = T,na.rm = F)


Genus_in_near <- ggplot()+                                                       ######可信的（p值小于0.05）不透明，不可信半透明；相关性越强颜色越深，红正相关，蓝负相关
  geom_tile(data= df %>% filter(!str_detect(p_value, "\\*")),
            aes(genus,env,fill=r),alpha = 0.5)+                 #填充颜色到差异不显著的方格
  geom_text(data= df %>% filter(!str_detect(p_value, "\\*")),
            aes(genus,env,col=r,fill=r,label=p_value),
            size=3,color="grey70",hjust=0.5,vjust=0.7)+         #填充置信p值数字到差异不显著的方格
  geom_tile(data= df %>% filter(str_detect(p_value, "\\*")),
            aes(genus,env
                ,col=r                    #右侧图例，以及差异显著方格的边框的颜色
                ,fill=r                   #填充r的颜色到差异显著的方格
            ),alpha=1)+               #差异显著的方格不透明
  geom_text(data= df %>% filter(str_detect(p_value, "\\*")),     #str_detect是看p_value是否符合"\\*"
            aes(genus,env,col=r,fill=r,label=p_value),
            size=3,color="black",hjust=0.5,vjust=0.7)+          #填充置信p值数字到差异显著的方格
  #  labs(x = NULL,y = NULL,color=NULL,fill=NULL)+                 #删除最右侧图例顶端的名字
  scale_color_gradientn(colours = rev(RColorBrewer::brewer.pal(11,"RdBu")))+     #右侧本来两个图例合并成一个
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11,"RdBu")))+      #给正相关标红
  scale_y_discrete(expand=c(0,0),position = 'right'
  )+                                            #控制y轴名称
  scale_x_discrete(expand=c(0,0)) +                      #给图拉宽一点（正值会让图变窄）
  theme(axis.text.x=element_text(angle =90, hjust =1,vjust =0.5,
                                 color="black",face = "bold",size = 10),    #x轴元素位置和加粗之类
        axis.text.y=element_text(color="black",face = "bold",size =10),     #y轴元素加粗之类
        axis.ticks= element_blank(),                  #x轴去掉下边本来有的刻录
        panel.spacing.y = unit(0,"cm"),               #y轴间距
        plot.background = element_blank(),
        panel.background = element_blank()            #整体变亮一点
  )+
  scale_size(range=c(1,10),guide=NULL)+            #数据点的大小范围
  guides(color=guide_colorbar(
    direction="vertical",                    #图例垂直显示
    reverse=F,                              #图例顺序
    barwidth=unit(.5,"cm"),                 #图例宽
    barheight=unit(11,"cm")                 #图例高
  ))
x <- Genus_in_near + theme(
  legend.position.inside=c(1.05, 1),
  legend.justification=c(0, 1))              #图例位置

pdf("各物种clones多样性_仅保护区内.pdf", width = 9, height = 11)
x
dev.off()

#============================================================================================
