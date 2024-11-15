#============================================================================================
#所有保护区内外α多样性和环境-经纬度和气候带和保护区
library(picante) 
otuZY_1 <- read.csv("Table. S5.csv", header=T, row.names = 1)
meta1 <- read.table("Table. S6.txt", header=T, 
                    sep="\t", quote = "", row.names=1, comment.char="",stringsAsFactors = FALSE)
#去掉非经纬度、气候带和保护区列
meta1 <- meta1[,-which(c(names(meta1) == "Elevation"))]

meta1 <- meta1[,-which(c(names(meta1) == "Temperature"))]

meta1 <- meta1[,-which(c(names(meta1) == "Vegetation_type_summarize"))]

meta1 <- meta1[,-which(c(names(meta1) == "ph"))]

meta1 <- meta1[,-which(c(names(meta1) == "Weather"))]

meta1 <- meta1[,-which(c(names(meta1) == "Area"))]

meta1 <- meta1[,-which(c(names(meta1) == "Disturbance"))]

meta1 <- meta1[,-which(c(names(meta1) == "Year"))]

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
#meta777 <- meta1[!grepl("特定字符", meta1$Disturbance)]    #去掉Disturbance列中含特定字符的行
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
tb_meta[c("Climate"#,
          #"Nature_Reserve"
          #, 
          #"Year","Vegetation_type_summarize","Weather","Area","Disturbance"
)] <- lapply(tb_meta[c("Climate"#,
                       #"Nature_Reserve", 
                       #"Year","Vegetation_type_summarize","Weather","Area","Disturbance"
)], factor)
str(tb_meta)
tb_meta$Climate <- as.numeric(tb_meta$Climate)
#tb_meta$Year <- as.numeric(tb_meta$Year)
#tb_meta$Nature_Reserve <- as.numeric(tb_meta$Nature_Reserve)
#tb_meta$Vegetation_type_summarize <- as.numeric(tb_meta$Vegetation_type_summarize)
#tb_meta$Weather <- as.numeric(tb_meta$Weather)
#tb_meta$Area <- as.numeric(tb_meta$Area)
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


#table1 <- read_tsv("varechem.tsv")
#table2 <- read_tsv("varespec.tsv") %>% select(1:20)



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


pdf("所有保护区内外α多样性和环境-经纬度和气候带.pdf", width = 4.0, height = 2.7)
p_nei
dev.off()
#============================================================================================

#============================================================================================
#仅保护区内α多样性和环境-经纬度和气候带和保护区
library(picante) 
otuZY_1 <- read.csv("Table. S5.csv", header=T, row.names = 1)
meta1 <- read.table("Table. S6.txt", header=T, 
                    sep="\t", quote = "", row.names=1, comment.char="",stringsAsFactors = FALSE)
#去掉非经纬度、气候带列
meta1 <- meta1[,-which(c(names(meta1) == "Elevation"))]

meta1 <- meta1[,-which(c(names(meta1) == "Temperature"))]

meta1 <- meta1[,-which(c(names(meta1) == "Vegetation_type_summarize"))]

meta1 <- meta1[,-which(c(names(meta1) == "ph"))]

meta1 <- meta1[,-which(c(names(meta1) == "Weather"))]

meta1 <- meta1[,-which(c(names(meta1) == "Area"))]

meta1 <- meta1[,-which(c(names(meta1) == "Disturbance"))]

meta1 <- meta1[,-which(c(names(meta1) == "Year"))]

meta1 <- meta1[,-which(c(names(meta1) == "Nature_Reserve"))]

meta1

#去除非保护区的行
otuZY_1 <- otuZY_1[-which(c(rownames(otuZY_1) == "K2018") | rownames(otuZY_1) == "Z2018" | rownames(otuZY_1) == "K2019" | rownames(otuZY_1) == "Z2019" | rownames(otuZY_1) == "HBA" | rownames(otuZY_1) == "HBI"),]
otuZY_1

otu_without_near <- rownames(otuZY_1)
otu_without_near

meta1
meta2 <- subset(meta1, rownames(meta1) %in% otu_without_near)   #筛选meta1的行名为otu_without_near的行
meta2

#删除植被类型
meta2 <- meta2[,-which(colnames(meta2) == "vegetation_type")]

#去掉Disturbance列中含NA的行
#meta777 <- meta1[!grepl("特定字符", meta1$Disturbance)]    #去掉Disturbance列中含特定字符的行
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
tb_meta[c("Climate"#,
          #"Nature_Reserve", 
          #"Year","Vegetation_type_summarize","Weather","Area","Disturbance"
)] <- lapply(tb_meta[c("Climate"#,
                       #"Nature_Reserve"
                       #, 
                       #"Year","Vegetation_type_summarize","Weather","Area","Disturbance"
)], factor)
str(tb_meta)
tb_meta$Climate <- as.numeric(tb_meta$Climate)
#tb_meta$Year <- as.numeric(tb_meta$Year)
#tb_meta$Nature_Reserve <- as.numeric(tb_meta$Nature_Reserve)
#tb_meta$Vegetation_type_summarize <- as.numeric(tb_meta$Vegetation_type_summarize)
#tb_meta$Weather <- as.numeric(tb_meta$Weather)
#tb_meta$Area <- as.numeric(tb_meta$Area)
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


#table1 <- read_tsv("varechem.tsv")
#table2 <- read_tsv("varespec.tsv") %>% select(1:20)



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


pdf("仅保护区内α多样性和环境-经纬度和气候带.pdf", width = 4, height = 2.7)
p_nei
dev.off()
#============================================================================================
#============================================================================================
#属-所有保护区内外α多样性和环境-经纬度和气候带
library(picante) 
otuZY_1 <- read.csv("Table. S4.csv", header=T, row.names = 1)
meta1 <- read.table("Table. S6.txt", header=T, 
                    sep="\t", quote = "", row.names=1, comment.char="",stringsAsFactors = FALSE)

#去掉拥有Tnf和NaN数值的Pielou和Inverse.Simpson
otuZY_1 <- otuZY_1[,-which(c(names(otuZY_1) == "Inverse.Simpson_Cav") | c(names(otuZY_1) == "Inverse.Simpson_Dic") | c(names(otuZY_1) == "Inverse.Simpson_Pol") | c(names(otuZY_1) == "Inverse.Simpson_Hag") | c(names(otuZY_1) == "Inverse.Simpson_Rap"))]
otuZY_1 <- otuZY_1[,-which(c(names(otuZY_1) == "Pielou_Cav" | names(otuZY_1) == "Pielou_Dic" | names(otuZY_1) == "Pielou_Pol" | names(otuZY_1) == "Pielou_Hag" | names(otuZY_1) == "Pielou_Rap"))]

otuZY_1
#去掉非经纬度、气候带和保护区列
meta1 <- meta1[,-which(c(names(meta1) == "Elevation"))]

meta1 <- meta1[,-which(c(names(meta1) == "Temperature"))]

meta1 <- meta1[,-which(c(names(meta1) == "Vegetation_type_summarize"))]

meta1 <- meta1[,-which(c(names(meta1) == "ph"))]

meta1 <- meta1[,-which(c(names(meta1) == "Weather"))]

meta1 <- meta1[,-which(c(names(meta1) == "Area"))]

meta1 <- meta1[,-which(c(names(meta1) == "Disturbance"))]

meta1 <- meta1[,-which(c(names(meta1) == "Year"))]

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
#meta777 <- meta1[!grepl("特定字符", meta1$Disturbance)]    #去掉Disturbance列中含特定字符的行
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
tb_meta[c("Climate"#,
          #"Nature_Reserve"
          #, 
          #"Year","Vegetation_type_summarize","Weather","Area","Disturbance"
)] <- lapply(tb_meta[c("Climate"#,
                       #"Nature_Reserve", 
                       #"Year","Vegetation_type_summarize","Weather","Area","Disturbance"
)], factor)
str(tb_meta)
tb_meta$Climate <- as.numeric(tb_meta$Climate)
#tb_meta$Year <- as.numeric(tb_meta$Year)
#tb_meta$Nature_Reserve <- as.numeric(tb_meta$Nature_Reserve)
#tb_meta$Vegetation_type_summarize <- as.numeric(tb_meta$Vegetation_type_summarize)
#tb_meta$Weather <- as.numeric(tb_meta$Weather)
#tb_meta$Area <- as.numeric(tb_meta$Area)
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


#table1 <- read_tsv("varechem.tsv")
#table2 <- read_tsv("varespec.tsv") %>% select(1:20)



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


pdf("属-所有保护区内外α多样性和环境-经纬度和气候带.pdf", width = 3.6, height = 14.5)
p_nei
dev.off()
#============================================================================================


#============================================================================================
#属-仅保护区内α多样性和环境-经纬度和气候带
library(picante) 
otuZY_1 <- read.csv("Table. S4.csv", header=T, row.names = 1)
meta1 <- read.table("Table. S6.txt", header=T, 
                    sep="\t", quote = "", row.names=1, comment.char="",stringsAsFactors = FALSE)

#去掉拥有Tnf和NaN数值的Pielou和Inverse.Simpson
otuZY_1 <- otuZY_1[,-which(c(names(otuZY_1) == "Inverse.Simpson_Cav") | c(names(otuZY_1) == "Inverse.Simpson_Dic") | c(names(otuZY_1) == "Inverse.Simpson_Pol") | c(names(otuZY_1) == "Inverse.Simpson_Hag") | c(names(otuZY_1) == "Inverse.Simpson_Rap"))]
otuZY_1 <- otuZY_1[,-which(c(names(otuZY_1) == "Pielou_Cav" | names(otuZY_1) == "Pielou_Dic" | names(otuZY_1) == "Pielou_Pol" | names(otuZY_1) == "Pielou_Hag" | names(otuZY_1) == "Pielou_Rap"))]

otuZY_1
#去掉非经纬度、气候带列
meta1 <- meta1[,-which(c(names(meta1) == "Elevation"))]

meta1 <- meta1[,-which(c(names(meta1) == "Temperature"))]

meta1 <- meta1[,-which(c(names(meta1) == "Vegetation_type_summarize"))]

meta1 <- meta1[,-which(c(names(meta1) == "ph"))]

meta1 <- meta1[,-which(c(names(meta1) == "Weather"))]

meta1 <- meta1[,-which(c(names(meta1) == "Area"))]

meta1 <- meta1[,-which(c(names(meta1) == "Disturbance"))]

meta1 <- meta1[,-which(c(names(meta1) == "Year"))]

meta1 <- meta1[,-which(c(names(meta1) == "Nature_Reserve"))]

meta1

#去除非保护区的行
otuZY_1 <- otuZY_1[-which(c(rownames(otuZY_1) == "HBA") | rownames(otuZY_1) == "HBI" | rownames(otuZY_1) == "K2018" | rownames(otuZY_1) == "K2019" | rownames(otuZY_1) == "Z2018" | rownames(otuZY_1) == "Z2019"),]
otuZY_1

otu_without_near <- rownames(otuZY_1)
otu_without_near

meta1
meta2 <- subset(meta1, rownames(meta1) %in% otu_without_near)   #筛选meta1的行名为otu_without_near的行
meta2

#删除植被类型
meta2 <- meta2[,-which(colnames(meta2) == "vegetation_type")]

#去掉Disturbance列中含NA的行
#meta777 <- meta1[!grepl("特定字符", meta1$Disturbance)]    #去掉Disturbance列中含特定字符的行
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
tb_meta[c("Climate"#,
          #"Nature_Reserve", 
          #"Year","Vegetation_type_summarize","Weather","Area","Disturbance"
)] <- lapply(tb_meta[c("Climate"#,
                       #"Nature_Reserve"
                       #, 
                       #"Year","Vegetation_type_summarize","Weather","Area","Disturbance"
)], factor)
str(tb_meta)
tb_meta$Climate <- as.numeric(tb_meta$Climate)
#tb_meta$Year <- as.numeric(tb_meta$Year)
#tb_meta$Nature_Reserve <- as.numeric(tb_meta$Nature_Reserve)
#tb_meta$Vegetation_type_summarize <- as.numeric(tb_meta$Vegetation_type_summarize)
#tb_meta$Weather <- as.numeric(tb_meta$Weather)
#tb_meta$Area <- as.numeric(tb_meta$Area)
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


#table1 <- read_tsv("varechem.tsv")
#table2 <- read_tsv("varespec.tsv") %>% select(1:20)



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


pdf("属-仅保护区内α多样性和环境-经纬度和气候带.pdf", width = 3.6, height = 14.5)
p_nei
dev.off()
#============================================================================================

#============================================================================================
#18+19长白山保护区内外α多样性和环境_1.7.2
library(picante) 
otuZY_1 <- read.csv("Table. S5.csv", header=T, row.names = 1)
meta1 <- read.table("Table. S6.txt", header=T, 
                    sep="\t", quote = "", row.names=1, comment.char="",stringsAsFactors = FALSE)
#去掉Disturbance列
meta1 <- meta1[,-which(c(names(meta1) == "Disturbance"))]
meta1

meta1 <- meta1[,-which(c(names(meta1) == "Climate"))]
meta1

meta1 <- meta1[,-which(c(names(meta1) == "Latitude"))]
meta1

meta1 <- meta1[,-which(c(names(meta1) == "Longitude"))]
meta1

meta1 <- meta1[,-which(c(names(meta1) == "Nature_Reserve"))]
meta1

#去除非2018长白的行
otuZY_1 <- otuZY_1[-which(c(rownames(otuZY_1) != "Y2019") & rownames(otuZY_1) != "K2019" & rownames(otuZY_1) != "H2019" & rownames(otuZY_1) != "Z2019" & rownames(otuZY_1) != "Y2018" & rownames(otuZY_1) != "K2018" & rownames(otuZY_1) != "H2018" & rownames(otuZY_1) != "Z2018"),]
otuZY_1


otu_without_near <- rownames(otuZY_1)
otu_without_near
meta1
meta2 <- subset(meta1, rownames(meta1) %in% otu_without_near)   #筛选meta1的行名为otu_without_near的行
meta2



#删除植被类型
meta2 <- meta2[,-which(colnames(meta2) == "vegetation_type")]

#去掉Disturbance列中含NA的行
#meta777 <- meta1[!grepl("特定字符", meta1$Disturbance)]    #去掉Disturbance列中含特定字符的行
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
tb_meta[c(#"Climate",
  #"Nature_Reserve", 
  "Year","Vegetation_type_summarize","Weather","Area"
  #,"Disturbance"
)] <- lapply(tb_meta[c(#"Climate",
  #"Nature_Reserve", 
  "Year","Vegetation_type_summarize","Weather","Area"
  #,"Disturbance"
)], factor)
str(tb_meta)
#tb_meta$Climate <- as.numeric(tb_meta$Climate)
tb_meta$Year <- as.numeric(tb_meta$Year)
#tb_meta$Nature_Reserve <- as.numeric(tb_meta$Nature_Reserve)
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


#table1 <- read_tsv("varechem.tsv")
#table2 <- read_tsv("varespec.tsv") %>% select(1:20)



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
                              barheight=unit(2,"cm")))


pdf("18+19长白山保护区内外α多样性和环境_1.7.2.pdf", width = 5.2, height = 4.2)
p_nei
dev.off()
#============================================================================================




#============================================================================================
#18+19长白山仅保护区内α多样性和环境_1.7.2
library(picante) 
otuZY_1 <- read.csv("Table. S5.csv", header=T, row.names = 1)
meta1 <- read.table("Table. S6.txt", header=T, 
                    sep="\t", quote = "", row.names=1, comment.char="",stringsAsFactors = FALSE)
#去掉Nature_Reserve列
meta1 <- meta1[,-which(c(names(meta1) == "Nature_Reserve"))]
meta1

meta1 <- meta1[,-which(c(names(meta1) == "Climate"))]
meta1

meta1 <- meta1[,-which(c(names(meta1) == "Latitude"))]
meta1

meta1 <- meta1[,-which(c(names(meta1) == "Longitude"))]
meta1

#去除非2018长白的行
otuZY_1 <- otuZY_1[-which(c(rownames(otuZY_1) != "Y2019") & rownames(otuZY_1) != "H2019" & rownames(otuZY_1) != "Y2018" & rownames(otuZY_1) != "H2018"),]
otuZY_1


otu_without_near <- rownames(otuZY_1)
otu_without_near
meta1
meta2 <- subset(meta1, rownames(meta1) %in% otu_without_near)   #筛选meta1的行名为otu_without_near的行
meta2



#删除植被类型
meta2 <- meta2[,-which(colnames(meta2) == "vegetation_type")]

#去掉Disturbance列中含NA的行
#meta777 <- meta1[!grepl("特定字符", meta1$Disturbance)]    #去掉Disturbance列中含特定字符的行
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
tb_meta[c(#"Climate",
  #"Nature_Reserve", 
  "Year",
  "Vegetation_type_summarize","Weather","Area"
  ,"Disturbance"
)] <- lapply(tb_meta[c(#"Climate",
  #"Nature_Reserve", 
  "Year",
  "Vegetation_type_summarize","Weather","Area"
  ,"Disturbance"
)], factor)
str(tb_meta)
#tb_meta$Climate <- as.numeric(tb_meta$Climate)
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


#table1 <- read_tsv("varechem.tsv")
#table2 <- read_tsv("varespec.tsv") %>% select(1:20)



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
                              barheight=unit(4,"cm")))


pdf("18+19长白山仅保护区内α多样性和环境_1.7.2.pdf", width = 5.2, height = 4.5)
p_nei
dev.off()
#============================================================================================
#============================================================================================
#湖北仅保护区内α多样性和环境_1.7.2
library(picante) 
otuZY_1 <- read.csv("Table. S5.csv", header=T, row.names = 1)
meta1 <- read.table("Table. S6.txt", header=T, 
                    sep="\t", quote = "", row.names=1, comment.char="",stringsAsFactors = FALSE)
#去掉Nature_Reserve列
meta1 <- meta1[,-which(c(names(meta1) == "Nature_Reserve"))]
meta1

#去掉Climate列
meta1 <- meta1[,-which(c(names(meta1) == "Climate"))]
meta1

#去掉Year列
meta1 <- meta1[,-which(c(names(meta1) == "Year"))]
meta1

meta1 <- meta1[,-which(c(names(meta1) == "Latitude"))]
meta1

meta1 <- meta1[,-which(c(names(meta1) == "Longitude"))]
meta1

#去除非2018长白的行
otuZY_1 <- otuZY_1[-which(c(rownames(otuZY_1) != "BF") & rownames(otuZY_1) != "BN" & rownames(otuZY_1) != "BC" & rownames(otuZY_1) != "BH" & rownames(otuZY_1) != "BZ"),]
otuZY_1


otu_without_near <- rownames(otuZY_1)
otu_without_near
meta1
meta2 <- subset(meta1, rownames(meta1) %in% otu_without_near)   #筛选meta1的行名为otu_without_near的行
meta2



#删除植被类型
meta2 <- meta2[,-which(colnames(meta2) == "vegetation_type")]

#去掉Disturbance列中含NA的行
#meta777 <- meta1[!grepl("特定字符", meta1$Disturbance)]    #去掉Disturbance列中含特定字符的行
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
tb_meta[c(#"Climate",
  #"Nature_Reserve", 
  #"Year",
  "Vegetation_type_summarize","Weather","Area"
  ,"Disturbance"
)] <- lapply(tb_meta[c(#"Climate",
  #"Nature_Reserve", 
  #"Year",
  "Vegetation_type_summarize","Weather","Area"
  ,"Disturbance"
)], factor)
str(tb_meta)
#tb_meta$Climate <- as.numeric(tb_meta$Climate)
#tb_meta$Year <- as.numeric(tb_meta$Year)
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


#table1 <- read_tsv("varechem.tsv")
#table2 <- read_tsv("varespec.tsv") %>% select(1:20)



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
                              barheight=unit(4,"cm")))


pdf("湖北仅保护区内α多样性和环境_1.7.2.pdf", width = 5.2, height = 4.0)
p_nei
dev.off()
#============================================================================================
#============================================================================================
#海南保护区内外α多样性和环境_1.7.2
library(picante) 
otuZY_1 <- read.csv("Table. S5.csv", header=T, row.names = 1)
meta1 <- read.table("Table. S6.txt", header=T, 
                    sep="\t", quote = "", row.names=1, comment.char="",stringsAsFactors = FALSE)
#去掉Disturbance列
meta1 <- meta1[,-which(c(names(meta1) == "Disturbance"))]
meta1

#去掉Climate列
meta1 <- meta1[,-which(c(names(meta1) == "Climate"))]
meta1

#去掉Year列
meta1 <- meta1[,-which(c(names(meta1) == "Year"))]
meta1

meta1 <- meta1[,-which(c(names(meta1) == "Latitude"))]
meta1

meta1 <- meta1[,-which(c(names(meta1) == "Longitude"))]
meta1

meta1 <- meta1[,-which(c(names(meta1) == "Nature_Reserve"))]
meta1

#去除非2018长白的行
otuZY_1 <- otuZY_1[-which(c(rownames(otuZY_1) != "HY") & rownames(otuZY_1) != "HBA" & rownames(otuZY_1) != "HBI" & rownames(otuZY_1) != "HS" & rownames(otuZY_1) != "HN" & rownames(otuZY_1) != "HC" & rownames(otuZY_1) != "HH"),]
otuZY_1


otu_without_near <- rownames(otuZY_1)
otu_without_near
meta1
meta2 <- subset(meta1, rownames(meta1) %in% otu_without_near)   #筛选meta1的行名为otu_without_near的行
meta2



#删除植被类型
meta2 <- meta2[,-which(colnames(meta2) == "vegetation_type")]

#去掉Disturbance列中含NA的行
#meta777 <- meta1[!grepl("特定字符", meta1$Disturbance)]    #去掉Disturbance列中含特定字符的行
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
tb_meta[c(
  #"Climate",
  #"Nature_Reserve", 
  #"Year",
  "Vegetation_type_summarize","Weather","Area"
  #,"Disturbance"
)] <- lapply(tb_meta[c(
  #"Climate",
  #"Nature_Reserve", 
  #"Year",
  "Vegetation_type_summarize","Weather","Area"
  #,"Disturbance"
)], factor)
str(tb_meta)
#tb_meta$Climate <- as.numeric(tb_meta$Climate)
#tb_meta$Year <- as.numeric(tb_meta$Year)
#tb_meta$Nature_Reserve <- as.numeric(tb_meta$Nature_Reserve)
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


#table1 <- read_tsv("varechem.tsv")
#table2 <- read_tsv("varespec.tsv") %>% select(1:20)



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
                              barheight=unit(2,"cm")))


pdf("海南保护区内外α多样性和环境_1.7.2.pdf", width = 5.2, height = 3.6)
p_nei
dev.off()
#============================================================================================

#============================================================================================
#海南仅保护区内α多样性和环境_1.7.2
library(picante) 
otuZY_1 <- read.csv("Table. S5.csv", header=T, row.names = 1)
meta1 <- read.table("Table. S6.txt", header=T, 
                    sep="\t", quote = "", row.names=1, comment.char="",stringsAsFactors = FALSE)
#去掉Nature_Reserve列
meta1 <- meta1[,-which(c(names(meta1) == "Nature_Reserve"))]
meta1

#去掉Climate列
meta1 <- meta1[,-which(c(names(meta1) == "Climate"))]
meta1

#去掉Year列
meta1 <- meta1[,-which(c(names(meta1) == "Year"))]
meta1

meta1 <- meta1[,-which(c(names(meta1) == "Latitude"))]
meta1

meta1 <- meta1[,-which(c(names(meta1) == "Longitude"))]
meta1

#去除非2018长白的行
otuZY_1 <- otuZY_1[-which(c(rownames(otuZY_1) != "HY") & rownames(otuZY_1) != "HS" & rownames(otuZY_1) != "HN" & rownames(otuZY_1) != "HC" & rownames(otuZY_1) != "HH"),]
otuZY_1


otu_without_near <- rownames(otuZY_1)
otu_without_near
meta1
meta2 <- subset(meta1, rownames(meta1) %in% otu_without_near)   #筛选meta1的行名为otu_without_near的行
meta2



#删除植被类型
meta2 <- meta2[,-which(colnames(meta2) == "vegetation_type")]

#去掉Disturbance列中含NA的行
#meta777 <- meta1[!grepl("特定字符", meta1$Disturbance)]    #去掉Disturbance列中含特定字符的行
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
tb_meta[c(#"Climate",
  #"Nature_Reserve", 
  #"Year",
  "Vegetation_type_summarize","Weather","Area"
  ,"Disturbance"
)] <- lapply(tb_meta[c(#"Climate",
  #"Nature_Reserve", 
  #"Year",
  "Vegetation_type_summarize","Weather","Area"
  ,"Disturbance"
)], factor)
str(tb_meta)
#tb_meta$Climate <- as.numeric(tb_meta$Climate)
#tb_meta$Year <- as.numeric(tb_meta$Year)
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


#table1 <- read_tsv("varechem.tsv")
#table2 <- read_tsv("varespec.tsv") %>% select(1:20)



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
                              barheight=unit(4,"cm")))


pdf("海南仅保护区内α多样性和环境_1.7.2.pdf", width = 5.2, height = 4.0)
p_nei
dev.off()
#============================================================================================
#============================================================================================
#属-18+19长白山保护区内外α多样性和环境_1.7.2
library(picante) 
otuZY_1 <- read.csv("Table. S4.csv", header=T, row.names = 1)
meta1 <- read.table("Table. S6.txt", header=T, 
                    sep="\t", quote = "", row.names=1, comment.char="",stringsAsFactors = FALSE)

#去掉拥有Tnf和NaN数值的Pielou和Inverse.Simpson
otuZY_1 <- otuZY_1[,-which(c(names(otuZY_1) == "Inverse.Simpson_Cav") | c(names(otuZY_1) == "Inverse.Simpson_Dic") | c(names(otuZY_1) == "Inverse.Simpson_Pol") | c(names(otuZY_1) == "Inverse.Simpson_Hag") | c(names(otuZY_1) == "Inverse.Simpson_Rap"))]
otuZY_1 <- otuZY_1[,-which(c(names(otuZY_1) == "Pielou_Cav" | names(otuZY_1) == "Pielou_Dic" | names(otuZY_1) == "Pielou_Pol" | names(otuZY_1) == "Pielou_Hag" | names(otuZY_1) == "Pielou_Rap"))]

otuZY_1
#去掉Disturbance列
meta1 <- meta1[,-which(c(names(meta1) == "Disturbance"))]
meta1

meta1 <- meta1[,-which(c(names(meta1) == "Climate"))]
meta1

meta1 <- meta1[,-which(c(names(meta1) == "Latitude"))]
meta1

meta1 <- meta1[,-which(c(names(meta1) == "Longitude"))]
meta1

meta1 <- meta1[,-which(c(names(meta1) == "Nature_Reserve"))]
meta1

#去除非2018长白的行
otuZY_1 <- otuZY_1[-which(c(rownames(otuZY_1) != "Y2019") & rownames(otuZY_1) != "K2019" & rownames(otuZY_1) != "H2019" & rownames(otuZY_1) != "Z2019" & rownames(otuZY_1) != "Y2018" & rownames(otuZY_1) != "K2018" & rownames(otuZY_1) != "H2018" & rownames(otuZY_1) != "Z2018"),]
otuZY_1


otu_without_near <- rownames(otuZY_1)
otu_without_near
meta1
meta2 <- subset(meta1, rownames(meta1) %in% otu_without_near)   #筛选meta1的行名为otu_without_near的行
meta2



#删除植被类型
meta2 <- meta2[,-which(colnames(meta2) == "vegetation_type")]

#去掉Disturbance列中含NA的行
#meta777 <- meta1[!grepl("特定字符", meta1$Disturbance)]    #去掉Disturbance列中含特定字符的行
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
tb_meta[c(#"Climate",
  #"Nature_Reserve", 
  "Year","Vegetation_type_summarize","Weather","Area"
  #,"Disturbance"
)] <- lapply(tb_meta[c(#"Climate",
  #"Nature_Reserve", 
  "Year","Vegetation_type_summarize","Weather","Area"
  #,"Disturbance"
)], factor)
str(tb_meta)
#tb_meta$Climate <- as.numeric(tb_meta$Climate)
tb_meta$Year <- as.numeric(tb_meta$Year)
#tb_meta$Nature_Reserve <- as.numeric(tb_meta$Nature_Reserve)
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


#table1 <- read_tsv("varechem.tsv")
#table2 <- read_tsv("varespec.tsv") %>% select(1:20)



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


pdf("属-18+19长白山保护区内外α多样性和环境_1.7.2.pdf", width = 5, height = 15.5)
p_nei
dev.off()
#============================================================================================




#============================================================================================
#属-18+19长白山仅保护区内α多样性和环境_1.7.2
library(picante) 
otuZY_1 <- read.csv("Table. S4.csv", header=T, row.names = 1)
meta1 <- read.table("Table. S6.txt", header=T, 
                    sep="\t", quote = "", row.names=1, comment.char="",stringsAsFactors = FALSE)

#去掉拥有Tnf和NaN数值的Pielou和Inverse.Simpson
otuZY_1 <- otuZY_1[,-which(c(names(otuZY_1) == "Inverse.Simpson_Cav") | c(names(otuZY_1) == "Inverse.Simpson_Dic") | c(names(otuZY_1) == "Inverse.Simpson_Pol") | c(names(otuZY_1) == "Inverse.Simpson_Hag") | c(names(otuZY_1) == "Inverse.Simpson_Rap"))]
otuZY_1 <- otuZY_1[,-which(c(names(otuZY_1) == "Pielou_Cav" | names(otuZY_1) == "Pielou_Dic" | names(otuZY_1) == "Pielou_Pol" | names(otuZY_1) == "Pielou_Hag" | names(otuZY_1) == "Pielou_Rap"))]

otuZY_1
#去掉Nature_Reserve列
meta1 <- meta1[,-which(c(names(meta1) == "Nature_Reserve"))]
meta1

meta1 <- meta1[,-which(c(names(meta1) == "Climate"))]
meta1

meta1 <- meta1[,-which(c(names(meta1) == "Latitude"))]
meta1

meta1 <- meta1[,-which(c(names(meta1) == "Longitude"))]
meta1

#去除非2018长白的行
otuZY_1 <- otuZY_1[-which(c(rownames(otuZY_1) != "Y2019") & rownames(otuZY_1) != "H2019" & rownames(otuZY_1) != "Y2018" & rownames(otuZY_1) != "H2018"),]
otuZY_1


otu_without_near <- rownames(otuZY_1)
otu_without_near
meta1
meta2 <- subset(meta1, rownames(meta1) %in% otu_without_near)   #筛选meta1的行名为otu_without_near的行
meta2



#删除植被类型
meta2 <- meta2[,-which(colnames(meta2) == "vegetation_type")]

#去掉Disturbance列中含NA的行
#meta777 <- meta1[!grepl("特定字符", meta1$Disturbance)]    #去掉Disturbance列中含特定字符的行
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
tb_meta[c(#"Climate",
  #"Nature_Reserve", 
  "Year",
  "Vegetation_type_summarize","Weather","Area"
  ,"Disturbance"
)] <- lapply(tb_meta[c(#"Climate",
  #"Nature_Reserve", 
  "Year",
  "Vegetation_type_summarize","Weather","Area"
  ,"Disturbance"
)], factor)
str(tb_meta)
#tb_meta$Climate <- as.numeric(tb_meta$Climate)
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


#table1 <- read_tsv("varechem.tsv")
#table2 <- read_tsv("varespec.tsv") %>% select(1:20)



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


pdf("属-18+19长白山仅保护区内α多样性和环境_1.7.2.pdf", width = 5, height = 15.5)
p_nei
dev.off()
#============================================================================================
#============================================================================================
#属-湖北仅保护区内α多样性和环境_1.7.2
library(picante) 
otuZY_1 <- read.csv("Table. S4.csv", header=T, row.names = 1)
meta1 <- read.table("Table. S6.txt", header=T, 
                    sep="\t", quote = "", row.names=1, comment.char="",stringsAsFactors = FALSE)

#去掉拥有Tnf和NaN数值的Pielou和Inverse.Simpson
otuZY_1 <- otuZY_1[,-which(c(names(otuZY_1) == "Inverse.Simpson_Cav") | c(names(otuZY_1) == "Inverse.Simpson_Dic") | c(names(otuZY_1) == "Inverse.Simpson_Pol") | c(names(otuZY_1) == "Inverse.Simpson_Hag") | c(names(otuZY_1) == "Inverse.Simpson_Rap"))]
otuZY_1 <- otuZY_1[,-which(c(names(otuZY_1) == "Pielou_Cav" | names(otuZY_1) == "Pielou_Dic" | names(otuZY_1) == "Pielou_Pol" | names(otuZY_1) == "Pielou_Hag" | names(otuZY_1) == "Pielou_Rap"))]

otuZY_1
#去掉Nature_Reserve列
meta1 <- meta1[,-which(c(names(meta1) == "Nature_Reserve"))]
meta1

#去掉Climate列
meta1 <- meta1[,-which(c(names(meta1) == "Climate"))]
meta1

#去掉Year列
meta1 <- meta1[,-which(c(names(meta1) == "Year"))]
meta1

meta1 <- meta1[,-which(c(names(meta1) == "Latitude"))]
meta1

meta1 <- meta1[,-which(c(names(meta1) == "Longitude"))]
meta1

#去除非2018长白的行
otuZY_1 <- otuZY_1[-which(c(rownames(otuZY_1) != "BF") & rownames(otuZY_1) != "BN" & rownames(otuZY_1) != "BC" & rownames(otuZY_1) != "BH" & rownames(otuZY_1) != "BZ"),]
otuZY_1


otu_without_near <- rownames(otuZY_1)
otu_without_near
meta1
meta2 <- subset(meta1, rownames(meta1) %in% otu_without_near)   #筛选meta1的行名为otu_without_near的行
meta2



#删除植被类型
meta2 <- meta2[,-which(colnames(meta2) == "vegetation_type")]

#去掉Disturbance列中含NA的行
#meta777 <- meta1[!grepl("特定字符", meta1$Disturbance)]    #去掉Disturbance列中含特定字符的行
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
tb_meta[c(#"Climate",
  #"Nature_Reserve", 
  #"Year",
  "Vegetation_type_summarize","Weather","Area"
  ,"Disturbance"
)] <- lapply(tb_meta[c(#"Climate",
  #"Nature_Reserve", 
  #"Year",
  "Vegetation_type_summarize","Weather","Area"
  ,"Disturbance"
)], factor)
str(tb_meta)
#tb_meta$Climate <- as.numeric(tb_meta$Climate)
#tb_meta$Year <- as.numeric(tb_meta$Year)
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


#table1 <- read_tsv("varechem.tsv")
#table2 <- read_tsv("varespec.tsv") %>% select(1:20)



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


pdf("属-湖北仅保护区内α多样性和环境_1.7.2.pdf", width = 5, height = 15.5)
p_nei
dev.off()
#============================================================================================
#============================================================================================
#属-海南保护区内外α多样性和环境_1.7.2
library(picante) 
otuZY_1 <- read.csv("Table. S4.csv", header=T, row.names = 1)
meta1 <- read.table("Table. S6.txt", header=T, 
                    sep="\t", quote = "", row.names=1, comment.char="",stringsAsFactors = FALSE)

#去掉拥有Tnf和NaN数值的Pielou和Inverse.Simpson
otuZY_1 <- otuZY_1[,-which(c(names(otuZY_1) == "Inverse.Simpson_Cav") | c(names(otuZY_1) == "Inverse.Simpson_Dic") | c(names(otuZY_1) == "Inverse.Simpson_Pol") | c(names(otuZY_1) == "Inverse.Simpson_Hag") | c(names(otuZY_1) == "Inverse.Simpson_Rap"))]
otuZY_1 <- otuZY_1[,-which(c(names(otuZY_1) == "Pielou_Cav" | names(otuZY_1) == "Pielou_Dic" | names(otuZY_1) == "Pielou_Pol" | names(otuZY_1) == "Pielou_Hag" | names(otuZY_1) == "Pielou_Rap"))]

otuZY_1

#去掉Disturbance列
meta1 <- meta1[,-which(c(names(meta1) == "Disturbance"))]
meta1

#去掉Climate列
meta1 <- meta1[,-which(c(names(meta1) == "Climate"))]
meta1

#去掉Year列
meta1 <- meta1[,-which(c(names(meta1) == "Year"))]
meta1

meta1 <- meta1[,-which(c(names(meta1) == "Latitude"))]
meta1

meta1 <- meta1[,-which(c(names(meta1) == "Longitude"))]
meta1

meta1 <- meta1[,-which(c(names(meta1) == "Nature_Reserve"))]
meta1

#去除非2018长白的行
otuZY_1 <- otuZY_1[-which(c(rownames(otuZY_1) != "HY") & rownames(otuZY_1) != "HBA" & rownames(otuZY_1) != "HBI" & rownames(otuZY_1) != "HS" & rownames(otuZY_1) != "HN" & rownames(otuZY_1) != "HC" & rownames(otuZY_1) != "HH"),]
otuZY_1


otu_without_near <- rownames(otuZY_1)
otu_without_near
meta1
meta2 <- subset(meta1, rownames(meta1) %in% otu_without_near)   #筛选meta1的行名为otu_without_near的行
meta2



#删除植被类型
meta2 <- meta2[,-which(colnames(meta2) == "vegetation_type")]

#去掉Disturbance列中含NA的行
#meta777 <- meta1[!grepl("特定字符", meta1$Disturbance)]    #去掉Disturbance列中含特定字符的行
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
tb_meta[c(
  #"Climate",
  #"Nature_Reserve", 
  #"Year",
  "Vegetation_type_summarize","Weather","Area"
  #,"Disturbance"
)] <- lapply(tb_meta[c(
  #"Climate",
  #"Nature_Reserve", 
  #"Year",
  "Vegetation_type_summarize","Weather","Area"
  #,"Disturbance"
)], factor)
str(tb_meta)
#tb_meta$Climate <- as.numeric(tb_meta$Climate)
#tb_meta$Year <- as.numeric(tb_meta$Year)
#tb_meta$Nature_Reserve <- as.numeric(tb_meta$Nature_Reserve)
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


#table1 <- read_tsv("varechem.tsv")
#table2 <- read_tsv("varespec.tsv") %>% select(1:20)



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


pdf("属-海南保护区内外α多样性和环境_1.7.2.pdf", width = 5, height = 15.5)
p_nei
dev.off()
#============================================================================================

#============================================================================================
#属-海南仅保护区内α多样性和环境_1.7.2
library(picante) 
otuZY_1 <- read.csv("Table. S4.csv", header=T, row.names = 1)
meta1 <- read.table("Table. S6.txt", header=T, 
                    sep="\t", quote = "", row.names=1, comment.char="",stringsAsFactors = FALSE)

#去掉拥有Tnf和NaN数值的Pielou和Inverse.Simpson
otuZY_1 <- otuZY_1[,-which(c(names(otuZY_1) == "Inverse.Simpson_Cav") | c(names(otuZY_1) == "Inverse.Simpson_Dic") | c(names(otuZY_1) == "Inverse.Simpson_Pol") | c(names(otuZY_1) == "Inverse.Simpson_Hag") | c(names(otuZY_1) == "Inverse.Simpson_Rap"))]
otuZY_1 <- otuZY_1[,-which(c(names(otuZY_1) == "Pielou_Cav" | names(otuZY_1) == "Pielou_Dic" | names(otuZY_1) == "Pielou_Pol" | names(otuZY_1) == "Pielou_Hag" | names(otuZY_1) == "Pielou_Rap"))]

otuZY_1
#去掉Nature_Reserve列
meta1 <- meta1[,-which(c(names(meta1) == "Nature_Reserve"))]
meta1

#去掉Climate列
meta1 <- meta1[,-which(c(names(meta1) == "Climate"))]
meta1

#去掉Year列
meta1 <- meta1[,-which(c(names(meta1) == "Year"))]
meta1

meta1 <- meta1[,-which(c(names(meta1) == "Latitude"))]
meta1

meta1 <- meta1[,-which(c(names(meta1) == "Longitude"))]
meta1

#去除非2018长白的行
otuZY_1 <- otuZY_1[-which(c(rownames(otuZY_1) != "HY") & rownames(otuZY_1) != "HS" & rownames(otuZY_1) != "HN" & rownames(otuZY_1) != "HC" & rownames(otuZY_1) != "HH"),]
otuZY_1


otu_without_near <- rownames(otuZY_1)
otu_without_near
meta1
meta2 <- subset(meta1, rownames(meta1) %in% otu_without_near)   #筛选meta1的行名为otu_without_near的行
meta2



#删除植被类型
meta2 <- meta2[,-which(colnames(meta2) == "vegetation_type")]

#去掉Disturbance列中含NA的行
#meta777 <- meta1[!grepl("特定字符", meta1$Disturbance)]    #去掉Disturbance列中含特定字符的行
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
tb_meta[c(#"Climate",
  #"Nature_Reserve", 
  #"Year",
  "Vegetation_type_summarize","Weather","Area"
  ,"Disturbance"
)] <- lapply(tb_meta[c(#"Climate",
  #"Nature_Reserve", 
  #"Year",
  "Vegetation_type_summarize","Weather","Area"
  ,"Disturbance"
)], factor)
str(tb_meta)
#tb_meta$Climate <- as.numeric(tb_meta$Climate)
#tb_meta$Year <- as.numeric(tb_meta$Year)
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


#table1 <- read_tsv("varechem.tsv")
#table2 <- read_tsv("varespec.tsv") %>% select(1:20)



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


pdf("属-海南仅保护区内α多样性和环境_1.7.2.pdf", width = 5, height = 15.5)
p_nei
dev.off()
#============================================================================================
