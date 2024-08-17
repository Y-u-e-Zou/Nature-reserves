#=============================================================================================================================
#吉林2018内外科

pdf('Jilin_2018_in_near-Province.pdf', width = 15, height = 8)   #建立pdf




library(grid)
circle_size = unit(1, 'snpc')    #规定圈图单位是像素（grid包的功能）


group2 <- read.delim('3.2.3.3_sample-metadata_simple_next_mix_in_near_RENAME_R.txt', sep = '\t', stringsAsFactors = FALSE)

Jilin_group2 <- subset(group2, SampleID %in% c("Within_Changbai_Mountain_Nature_Reserve_2018", "Near_Changbai_Mountain_Nature_Reserve_2018"))
Jilin_group2

group <- Jilin_group2[order(Jilin_group2$"Province", Jilin_group2$"SampleID"), ]                         #先对"Province"这个列排序，在这基础上对"SampleID"的列排序后，结果用来给group来排序--影响最终图上右侧分组的顺序



all_group <- unique(group$"Province")                                                #提取group的"Province"列并除去重复后命名为all_group
all_group
group$"Province" <- factor(group$"Province", levels = all_group)
group$"Province"
all_sample <- group$"SampleID"                                                     #提取group的"SampleID"列并命名为all_sample
all_sample


otu_table2 <- read.delim('1.9.2.3_clones_mix_in_near_clones_g-1_RENAME_R.txt', sep = '\t')

Jilin_otu_table1 <- otu_table2[-which(c(otu_table2$Near_Changbai_Mountain_Nature_Reserve_2018 == 0.0 & otu_table2$Within_Changbai_Mountain_Nature_Reserve_2018 == 0.0)),]       ##取2018长白拥有clone的物种
Jilin_otu_table1

Jilin_otu_table2 <- Jilin_otu_table1[,-which(c(colnames(Jilin_otu_table1) != "OTUID" & colnames(Jilin_otu_table1) != "Near_Changbai_Mountain_Nature_Reserve_2018" & colnames(Jilin_otu_table1) != "Within_Changbai_Mountain_Nature_Reserve_2018"))]  ##取2018长白的地点
Jilin_otu_table2

Jilin_otu_table2$OTUID


taxonomy2 <- read.delim('4.1_taxonomy_KPCOFGS_R.txt', sep = '\t', stringsAsFactors = FALSE)    #或者read.delim(file.choose(), sep = '\t', stringsAsFactors = FALSE)



Jilin2018_taxonomy2 <- subset(taxonomy2, OTUID %in% c(Jilin_otu_table2$OTUID))          #选取Jilin2018特有的行
Jilin2018_taxonomy2
taxonomy <- Jilin2018_taxonomy2[order(Jilin2018_taxonomy2$"Family", Jilin2018_taxonomy2$"OTUID"), ]            #先对"Family"这个列排序，在这基础上对"OTUID"的列排序后，结果用来给taxonomy来排序--影响最终图上左侧分组的顺序

tax_phylum <- unique(taxonomy$"Family")                                          #提取taxonomy的"Family"列并除去重复后命名为tax_phylum
tax_phylum
taxonomy$"Family" <- factor(taxonomy$"Family", levels = tax_phylum)
taxonomy$"Family"
all_otu <- taxonomy$"OTUID"                                                      #提取taxonomy的"OTUID"列并命名为all_otu
all_otu
taxonomy$"OTUID" <- factor(taxonomy$"OTUID", levels = all_otu)
taxonomy$"OTUID"






otu_table <- merge(taxonomy, Jilin_otu_table2, by = "OTUID")                            #把taxonomy合并到otu_table里，以"OTUID"为对照
otu_table <- otu_table[order(otu_table$"Family", otu_table$"OTUID"), ]           #先对"Family"这个列排序，在这基础上对"OTUID"的列排序后，结果用来给otu_table来排序
otu_table
rownames(otu_table) <- otu_table$"OTUID"                                         #提取"OTUID"作为行
rownames(otu_table)
otu_table <- otu_table[all_sample]
otu_table



all_ID <- c(all_otu, all_sample)                                                 #罗列all_otu和all_sample（"OTUID"+"SampleID"）并命名all_ID
all_ID
accum_otu <- rowSums(otu_table)                                                  #提取otu_table每一行的和（每个"OTUID"的和）并命名accum_otu
accum_otu
accum_sample <- colSums(otu_table)                                               #提取otu_table每一列的和（每个"SampleID"的和）并命名accum_sample
accum_sample
all_ID_xlim <- cbind(rep(0, length(all_ID)),data.frame(c(accum_otu, accum_sample)))  #把all_ID数量的0作为列，和数据框（两列，第一列为accum_otu+accum_sample的首行"OTUID"+"SampleID"的合集，第二列为accum_otu+accum_sample的第二行数字"OTUID"+"SampleID"的和）的列一起合并成为all_ID_xlim
all_ID_xlim


library(reshape2)


otu_table$"OTUID" <- all_otu                                                     #otu_table的"OTUID"那一列设定为all_otu
otu_table$"OTUID"
plot_data <- melt(otu_table, id = "OTUID")                                       #将宽格式转化为长格式并命名为plot_data，plot_data的第一列为"OTUID"
plot_data
colnames(plot_data)[2] <- "SampleID"                                               #将plot_data的第二列题头改为"SampleID"
plot_data
plot_data$"OTUID" <- factor(plot_data$"OTUID", levels = all_otu)                 #设置plot_data的"OTUID"列的排序顺序是all_otu的顺序
plot_data$"SampleID" <- factor(plot_data$"SampleID", levels = all_sample)            #设置plot_data的"SampleID"列的排序顺序是all_sample的顺序
plot_data <- plot_data[order(plot_data$"OTUID", plot_data$"SampleID"), ]           #先升序排序plot_data的"OTUID"列，这基础上再升序"SampleID"列
plot_data
plot_data <- plot_data[c(2, 1, 3, 3)]                                            #排序plot_data，让原本第二列"SampleID"变第一列，原第一列"OTUID"变第二列，并且复制第3列并生成与3列相同的第4列
plot_data


color_otu <- c('#C0E2FD', '#FEC0C1', '#CDC6FF', '#FDC0F7', '#F3D8F1', 
               '#D6EBBF', '#E1CAF7', '#BFDCE2', '#F8F0BE')            #颜色设置  OTUID-2018吉林9个 湖北8个 海南6个

color_sample <- c('#BEEFBF', '#F8C9C8')                    # SampleID-2018吉林2个 湖北3个 海南4个

color_phylum <- c('#C0E2D2', '#E9BFC0', '#E3E3E3')                    # Family-2018吉林3个 湖北同1个 海南同3个独自1个

color_group <- c('#BFBFBF')                     # Province-2018吉林1个 湖北同1 海南同2

names(color_otu) <- all_otu                                                      #设置color_otu的名字对应all_otu里面的元素
color_otu
names(color_sample) <- all_sample                                                #设置color_sample的名字对应all_sample里面的元素
color_sample


library(circlize)

gap_size <- c(rep(3, length(all_otu) - 1), 6, rep(3, length(all_sample) - 1), 6) #设置圈图中样品间距，all_otu里的OTU间距和all_sample里的样品间距均为3，all_otu和all_sample两组间距为6。（得和下边统一）
gap_size
circos.par(cell.padding = c(0, 0, 0, 0), start.degree = 270, gap.degree = gap_size)  #设置圈图布局。是从逆时针270°开始
circos.initialize(sectors = factor(all_ID, levels = all_ID), xlim = all_ID_xlim) #设置圆内的因子为factor因子变量，x轴是all_ID_xlim


circos.trackPlotRegion(
  ylim = c(0, 1), track.height = 0.03, bg.border = NA,
  panel.fun = function(x, y) {
    sector.index = get.cell.meta.data('sector.index')
    xlim = get.cell.meta.data('xlim')
    ylim = get.cell.meta.data('ylim')
  } )                                                                            #第一圈（最外圈）基本设定，设置'sector.index'为sector扇区名字，xlim为x轴，ylim为y轴

for (i in 1:length(tax_phylum)) {
  tax_OTU <- {subset(taxonomy, Family == tax_phylum[i])}$"OTUID"
  highlight.sector(tax_OTU, track.index = 1, col = color_phylum[i], 
                   text = tax_phylum[i], cex = 0.5, text.col = 'black', niceFacing = FALSE)
}                                                                                #第一圈的设置变量i是1到tax_phylum的总长，选择taxonomy中符合Family == tax_phylum[i]的行里的"OTUID"列，这里Family不能加引号。#在其中的环段里显示文字，内容是所有tax_phylum的内容,字号0.5，黑色，在环段中心


for (i in 1:length(all_group)) {
  group_sample <- {subset(group, Province == all_group[i])}$"SampleID"
  highlight.sector(group_sample, track.index = 1, col = color_group[i], 
                   text = all_group[i], cex = 0.7, text.col = 'black', niceFacing = FALSE)
}                                                                                #第一圈的设置变量i是1到all_group的总长，选择group中符合Province == all_group[i]的行里的"SampleID"列，这里Province不能加引号。#在其中的环段里显示文字，内容是所有all_group的内容,字号0.7，黑色，顺着环段排布



circos.trackPlotRegion(
  ylim = c(0, 1), track.height = 0.05, bg.border = NA,
  panel.fun = function(x, y) {
    sector.index = get.cell.meta.data('sector.index')
    xlim = get.cell.meta.data('xlim')
    ylim = get.cell.meta.data('ylim')
  } )                                                                            #第二圈（百分比度数圈）设定开始，xlim是之前就指定的all_ID_xlim

circos.track(
  track.index = 2, bg.border = NA,
  panel.fun = function(x, y) {
    xlim = get.cell.meta.data('xlim')
    ylim = get.cell.meta.data('ylim')
    sector.name = get.cell.meta.data('sector.index')
    xplot = get.cell.meta.data('xplot')
    
    by = ifelse(abs(xplot[2] - xplot[1]) > 30, 0.25, 1)
    for (p in c(0, seq(by, 1, by = by))) circos.text(p*(xlim[2] - xlim[1]) + xlim[1], 
                                                     mean(ylim) + 0.4, paste0(p*100, '%'), 
                                                     cex = 0.4, adj = c(0.5, 0), 
                                                     niceFacing = FALSE)
    
    circos.lines(xlim, c(mean(ylim), mean(ylim)), lty = 3)
  } )                                                                            #第二圈circos.track设定百分比图，如果2的度数减1的度数的绝对值大于30，输出0.25，不大于30输出1。#p在0到by到1之间，设置标签文字



circos.trackPlotRegion(
  ylim = c(0, 1), track.height = 0.03, bg.col = c(color_otu, color_sample), bg.border = NA, track.margin = c(0, 0.01),
  panel.fun = function(x, y) {
    xlim = get.cell.meta.data('xlim')
    sector.name = get.cell.meta.data('sector.index')
    circos.axis(h = 'top', labels.cex = 0.4, major.tick.length = 0.4, 
                labels.niceFacing = FALSE)
    circos.text(mean(xlim), 0.2, sector.name, cex = 0.4, niceFacing = FALSE, 
                adj = c(0.5, 0))
  } )                                                                            #第三圈。在第三圈顶部画了比例尺


circos.trackPlotRegion(ylim = c(0, 1), track.height = 0.03, track.margin = c(0, 0.01))  #第四圈，track.margin是单元格/环段和环段间空白间距


for (i in seq_len(nrow(plot_data))) {
  circos.link(
    plot_data[i,2], c(accum_otu[plot_data[i,2]], accum_otu[plot_data[i,2]] - plot_data[i,4]),
    plot_data[i,1], c(accum_sample[plot_data[i,1]], accum_sample[plot_data[i,1]] - plot_data[i,3]),
    col = paste0(color_otu[plot_data[i,2]], '70'), border = NA )
  
  circos.rect(accum_otu[plot_data[i,2]], 0, accum_otu[plot_data[i,2]] - plot_data[i,4], 1, 
              sector.index = plot_data[i,2], col = color_sample[plot_data[i,1]], border = NA)
  circos.rect(accum_sample[plot_data[i,1]], 0, accum_sample[plot_data[i,1]] - plot_data[i,3], 1, 
              sector.index = plot_data[i,1], col = color_otu[plot_data[i,2]], border = NA)
  
  accum_otu[plot_data[i,2]] = accum_otu[plot_data[i,2]] - plot_data[i,4]
  accum_sample[plot_data[i,1]] = accum_sample[plot_data[i,1]] - plot_data[i,3]
}                                                                                #圈图最内部连线



library(ComplexHeatmap) 

otu_legend <- Legend(
  at = all_otu, labels = taxonomy$"OTUID", labels_gp = gpar(fontsize = 8),
  grid_height = unit(0.5, 'cm'), grid_width = unit(0.5, 'cm'), type = 'points', 
  pch = NA, background = color_otu)                                              #添加图例

pushViewport(viewport(x = 0.80, y = 0.5))
grid.draw(otu_legend)                                                            #添加图例注解
upViewport()


dev.off()                                                                        #关闭绘图设备
circos.clear()                                                                   #关闭圈图制作并清零面板




#=============================================================================================================================
#吉林2019内外科

pdf('Jilin_2019_in_near-Province.pdf', width = 15, height = 8)   #建立pdf




library(grid)
circle_size = unit(1, 'snpc')    #规定圈图单位是像素（grid包的功能）


group2 <- read.delim('3.2.3.3_sample-metadata_simple_next_mix_in_near_RENAME_R.txt', sep = '\t', stringsAsFactors = FALSE)

Jilin_group2 <- subset(group2, SampleID %in% c("Within_Changbai_Mountain_Nature_Reserve_2019", "Near_Changbai_Mountain_Nature_Reserve_2019"))
Jilin_group2

group <- Jilin_group2[order(Jilin_group2$"Province", Jilin_group2$"SampleID"), ]                         #先对"Province"这个列排序，在这基础上对"SampleID"的列排序后，结果用来给group来排序--影响最终图上右侧分组的顺序



all_group <- unique(group$"Province")                                                #提取group的"Province"列并除去重复后命名为all_group
all_group
group$"Province" <- factor(group$"Province", levels = all_group)
group$"Province"
all_sample <- group$"SampleID"                                                     #提取group的"SampleID"列并命名为all_sample
all_sample


otu_table2 <- read.delim('1.9.2.3_clones_mix_in_near_clones_g-1_RENAME_R.txt', sep = '\t')

Jilin_otu_table1 <- otu_table2[-which(c(otu_table2$Near_Changbai_Mountain_Nature_Reserve_2019 == 0.0 & otu_table2$Within_Changbai_Mountain_Nature_Reserve_2019 == 0.0)),]       ##取2019长白拥有clone的物种
Jilin_otu_table1

Jilin_otu_table2 <- Jilin_otu_table1[,-which(c(colnames(Jilin_otu_table1) != "OTUID" & colnames(Jilin_otu_table1) != "Near_Changbai_Mountain_Nature_Reserve_2019" & colnames(Jilin_otu_table1) != "Within_Changbai_Mountain_Nature_Reserve_2019"))]  ##取2019长白的地点
Jilin_otu_table2

Jilin_otu_table2$OTUID


taxonomy2 <- read.delim('4.1_taxonomy_KPCOFGS_R.txt', sep = '\t', stringsAsFactors = FALSE)    #或者read.delim(file.choose(), sep = '\t', stringsAsFactors = FALSE)



Jilin2019_taxonomy2 <- subset(taxonomy2, OTUID %in% c(Jilin_otu_table2$OTUID))          #选取Jilin2019特有的行
Jilin2019_taxonomy2
taxonomy <- Jilin2019_taxonomy2[order(Jilin2019_taxonomy2$"Family", Jilin2019_taxonomy2$"OTUID"), ]            #先对"Family"这个列排序，在这基础上对"OTUID"的列排序后，结果用来给taxonomy来排序--影响最终图上左侧分组的顺序

tax_phylum <- unique(taxonomy$"Family")                                          #提取taxonomy的"Family"列并除去重复后命名为tax_phylum
tax_phylum
taxonomy$"Family" <- factor(taxonomy$"Family", levels = tax_phylum)
taxonomy$"Family"
all_otu <- taxonomy$"OTUID"                                                      #提取taxonomy的"OTUID"列并命名为all_otu
all_otu
taxonomy$"OTUID" <- factor(taxonomy$"OTUID", levels = all_otu)
taxonomy$"OTUID"






otu_table <- merge(taxonomy, Jilin_otu_table2, by = "OTUID")                            #把taxonomy合并到otu_table里，以"OTUID"为对照
otu_table <- otu_table[order(otu_table$"Family", otu_table$"OTUID"), ]           #先对"Family"这个列排序，在这基础上对"OTUID"的列排序后，结果用来给otu_table来排序
otu_table
rownames(otu_table) <- otu_table$"OTUID"                                         #提取"OTUID"作为行
rownames(otu_table)
otu_table <- otu_table[all_sample]
otu_table



all_ID <- c(all_otu, all_sample)                                                 #罗列all_otu和all_sample（"OTUID"+"SampleID"）并命名all_ID
all_ID
accum_otu <- rowSums(otu_table)                                                  #提取otu_table每一行的和（每个"OTUID"的和）并命名accum_otu
accum_otu
accum_sample <- colSums(otu_table)                                               #提取otu_table每一列的和（每个"SampleID"的和）并命名accum_sample
accum_sample
all_ID_xlim <- cbind(rep(0, length(all_ID)),data.frame(c(accum_otu, accum_sample)))  #把all_ID数量的0作为列，和数据框（两列，第一列为accum_otu+accum_sample的首行"OTUID"+"SampleID"的合集，第二列为accum_otu+accum_sample的第二行数字"OTUID"+"SampleID"的和）的列一起合并成为all_ID_xlim
all_ID_xlim


library(reshape2)


otu_table$"OTUID" <- all_otu                                                     #otu_table的"OTUID"那一列设定为all_otu
otu_table$"OTUID"
plot_data <- melt(otu_table, id = "OTUID")                                       #将宽格式转化为长格式并命名为plot_data，plot_data的第一列为"OTUID"
plot_data
colnames(plot_data)[2] <- "SampleID"                                               #将plot_data的第二列题头改为"SampleID"
plot_data
plot_data$"OTUID" <- factor(plot_data$"OTUID", levels = all_otu)                 #设置plot_data的"OTUID"列的排序顺序是all_otu的顺序
plot_data$"SampleID" <- factor(plot_data$"SampleID", levels = all_sample)            #设置plot_data的"SampleID"列的排序顺序是all_sample的顺序
plot_data <- plot_data[order(plot_data$"OTUID", plot_data$"SampleID"), ]           #先升序排序plot_data的"OTUID"列，这基础上再升序"SampleID"列
plot_data
plot_data <- plot_data[c(2, 1, 3, 3)]                                            #排序plot_data，让原本第二列"SampleID"变第一列，原第一列"OTUID"变第二列，并且复制第3列并生成与3列相同的第4列
plot_data




color_otu <- c('#C0E2FD', '#DEECF6', '#CDC6FF', '#AFC8E2', '#E1CAF7',
               '#E2F2CD', '#B6DAA7', '#F9D5D5')            #颜色设置  OTUID-2019吉林9个 湖北8个 海南6个

color_sample <- c('#BEEFBF', '#F8C9C8')                    # SampleID-2019吉林2个 湖北3个 海南4个

color_phylum <- c('#C0E2D2', '#E9BFC0', '#E3E3E3')                    # Family-2019吉林3个 湖北同1个 海南同3个独自1个

color_group <- c('#E8E0EF')                     # Province-2019吉林1个 湖北同1 海南同2

names(color_otu) <- all_otu                                                      #设置color_otu的名字对应all_otu里面的元素
color_otu
names(color_sample) <- all_sample                                                #设置color_sample的名字对应all_sample里面的元素
color_sample


library(circlize)

gap_size <- c(rep(3, length(all_otu) - 1), 6, rep(3, length(all_sample) - 1), 6) #设置圈图中样品间距，all_otu里的OTU间距和all_sample里的样品间距均为3，all_otu和all_sample两组间距为6。（得和下边统一）
gap_size
circos.par(cell.padding = c(0, 0, 0, 0), start.degree = 270, gap.degree = gap_size)  #设置圈图布局。是从逆时针270°开始
circos.initialize(sectors = factor(all_ID, levels = all_ID), xlim = all_ID_xlim) #设置圆内的因子为factor因子变量，x轴是all_ID_xlim


circos.trackPlotRegion(
  ylim = c(0, 1), track.height = 0.03, bg.border = NA,
  panel.fun = function(x, y) {
    sector.index = get.cell.meta.data('sector.index')
    xlim = get.cell.meta.data('xlim')
    ylim = get.cell.meta.data('ylim')
  } )                                                                            #第一圈（最外圈）基本设定，设置'sector.index'为sector扇区名字，xlim为x轴，ylim为y轴

for (i in 1:length(tax_phylum)) {
  tax_OTU <- {subset(taxonomy, Family == tax_phylum[i])}$"OTUID"
  highlight.sector(tax_OTU, track.index = 1, col = color_phylum[i], 
                   text = tax_phylum[i], cex = 0.5, text.col = 'black', niceFacing = FALSE)
}                                                                                #第一圈的设置变量i是1到tax_phylum的总长，选择taxonomy中符合Family == tax_phylum[i]的行里的"OTUID"列，这里Family不能加引号。#在其中的环段里显示文字，内容是所有tax_phylum的内容,字号0.5，黑色，在环段中心


for (i in 1:length(all_group)) {
  group_sample <- {subset(group, Province == all_group[i])}$"SampleID"
  highlight.sector(group_sample, track.index = 1, col = color_group[i], 
                   text = all_group[i], cex = 0.7, text.col = 'black', niceFacing = FALSE)
}                                                                                #第一圈的设置变量i是1到all_group的总长，选择group中符合Province == all_group[i]的行里的"SampleID"列，这里Province不能加引号。#在其中的环段里显示文字，内容是所有all_group的内容,字号0.7，黑色，顺着环段排布



circos.trackPlotRegion(
  ylim = c(0, 1), track.height = 0.05, bg.border = NA,
  panel.fun = function(x, y) {
    sector.index = get.cell.meta.data('sector.index')
    xlim = get.cell.meta.data('xlim')
    ylim = get.cell.meta.data('ylim')
  } )                                                                            #第二圈（百分比度数圈）设定开始，xlim是之前就指定的all_ID_xlim

circos.track(
  track.index = 2, bg.border = NA,
  panel.fun = function(x, y) {
    xlim = get.cell.meta.data('xlim')
    ylim = get.cell.meta.data('ylim')
    sector.name = get.cell.meta.data('sector.index')
    xplot = get.cell.meta.data('xplot')
    
    by = ifelse(abs(xplot[2] - xplot[1]) > 30, 0.25, 1)
    for (p in c(0, seq(by, 1, by = by))) circos.text(p*(xlim[2] - xlim[1]) + xlim[1], 
                                                     mean(ylim) + 0.4, paste0(p*100, '%'), 
                                                     cex = 0.4, adj = c(0.5, 0), 
                                                     niceFacing = FALSE)
    
    circos.lines(xlim, c(mean(ylim), mean(ylim)), lty = 3)
  } )                                                                            #第二圈circos.track设定百分比图，如果2的度数减1的度数的绝对值大于30，输出0.25，不大于30输出1。#p在0到by到1之间，设置标签文字



circos.trackPlotRegion(
  ylim = c(0, 1), track.height = 0.03, bg.col = c(color_otu, color_sample), bg.border = NA, track.margin = c(0, 0.01),
  panel.fun = function(x, y) {
    xlim = get.cell.meta.data('xlim')
    sector.name = get.cell.meta.data('sector.index')
    circos.axis(h = 'top', labels.cex = 0.4, major.tick.length = 0.4, 
                labels.niceFacing = FALSE)
    circos.text(mean(xlim), 0.2, sector.name, cex = 0.4, niceFacing = FALSE, 
                adj = c(0.5, 0))
  } )                                                                            #第三圈。在第三圈顶部画了比例尺


circos.trackPlotRegion(ylim = c(0, 1), track.height = 0.03, track.margin = c(0, 0.01))  #第四圈，track.margin是单元格/环段和环段间空白间距


for (i in seq_len(nrow(plot_data))) {
  circos.link(
    plot_data[i,2], c(accum_otu[plot_data[i,2]], accum_otu[plot_data[i,2]] - plot_data[i,4]),
    plot_data[i,1], c(accum_sample[plot_data[i,1]], accum_sample[plot_data[i,1]] - plot_data[i,3]),
    col = paste0(color_otu[plot_data[i,2]], '70'), border = NA )
  
  circos.rect(accum_otu[plot_data[i,2]], 0, accum_otu[plot_data[i,2]] - plot_data[i,4], 1, 
              sector.index = plot_data[i,2], col = color_sample[plot_data[i,1]], border = NA)
  circos.rect(accum_sample[plot_data[i,1]], 0, accum_sample[plot_data[i,1]] - plot_data[i,3], 1, 
              sector.index = plot_data[i,1], col = color_otu[plot_data[i,2]], border = NA)
  
  accum_otu[plot_data[i,2]] = accum_otu[plot_data[i,2]] - plot_data[i,4]
  accum_sample[plot_data[i,1]] = accum_sample[plot_data[i,1]] - plot_data[i,3]
}                                                                                #圈图最内部连线



library(ComplexHeatmap) 

otu_legend <- Legend(
  at = all_otu, labels = taxonomy$"OTUID", labels_gp = gpar(fontsize = 8),
  grid_height = unit(0.5, 'cm'), grid_width = unit(0.5, 'cm'), type = 'points', 
  pch = NA, background = color_otu)                                              #添加图例

pushViewport(viewport(x = 0.80, y = 0.5))
grid.draw(otu_legend)                                                            #添加图例注解
upViewport()


dev.off()                                                                        #关闭绘图设备
circos.clear()                                                                   #关闭圈图制作并清零面板




#=============================================================================================================================

#湖北内外科

pdf('Hubei_in_near-Province.pdf', width = 15, height = 8)   #建立pdf




library(grid)
circle_size = unit(1, 'snpc')    #规定圈图单位是像素（grid包的功能）


group2 <- read.delim('3.2.3.3_sample-metadata_simple_next_mix_in_near_RENAME_R.txt', sep = '\t', stringsAsFactors = FALSE)

Hubei_group2 <- subset(group2, SampleID %in% c("Within_Shennongjia_Nature_Reserve"))
Hubei_group2

group <- Hubei_group2[order(Hubei_group2$"Province", Hubei_group2$"SampleID"), ]                         #先对"Province"这个列排序，在这基础上对"SampleID"的列排序后，结果用来给group来排序--影响最终图上右侧分组的顺序



all_group <- unique(group$"Province")                                                #提取group的"Province"列并除去重复后命名为all_group
all_group
group$"Province" <- factor(group$"Province", levels = all_group)
group$"Province"
all_sample <- group$"SampleID"                                                     #提取group的"SampleID"列并命名为all_sample
all_sample


otu_table2 <- read.delim('1.9.2.3_clones_mix_in_near_clones_g-1_RENAME_R.txt', sep = '\t')

Hubei_otu_table1 <- otu_table2[-which(c(otu_table2$Within_Shennongjia_Nature_Reserve == 0.0)),]       ##取Hubei拥有clone的物种
Hubei_otu_table1

Hubei_otu_table2 <- Hubei_otu_table1[,-which(c(colnames(Hubei_otu_table1) != "OTUID" & colnames(Hubei_otu_table1) != "Within_Shennongjia_Nature_Reserve"))]  ##取Hubei的地点
Hubei_otu_table2

Hubei_otu_table2$OTUID


taxonomy2 <- read.delim('4.1_taxonomy_KPCOFGS_R.txt', sep = '\t', stringsAsFactors = FALSE)    #或者read.delim(file.choose(), sep = '\t', stringsAsFactors = FALSE)



Hubei_taxonomy2 <- subset(taxonomy2, OTUID %in% c(Hubei_otu_table2$OTUID))          #选取Hubei特有的行
Hubei_taxonomy2
taxonomy <- Hubei_taxonomy2[order(Hubei_taxonomy2$"Family", Hubei_taxonomy2$"OTUID"), ]            #先对"Family"这个列排序，在这基础上对"OTUID"的列排序后，结果用来给taxonomy来排序--影响最终图上左侧分组的顺序

tax_phylum <- unique(taxonomy$"Family")                                          #提取taxonomy的"Family"列并除去重复后命名为tax_phylum
tax_phylum
taxonomy$"Family" <- factor(taxonomy$"Family", levels = tax_phylum)
taxonomy$"Family"
all_otu <- taxonomy$"OTUID"                                                      #提取taxonomy的"OTUID"列并命名为all_otu
all_otu
taxonomy$"OTUID" <- factor(taxonomy$"OTUID", levels = all_otu)
taxonomy$"OTUID"






otu_table <- merge(taxonomy, Hubei_otu_table2, by = "OTUID")                            #把taxonomy合并到otu_table里，以"OTUID"为对照
otu_table <- otu_table[order(otu_table$"Family", otu_table$"OTUID"), ]           #先对"Family"这个列排序，在这基础上对"OTUID"的列排序后，结果用来给otu_table来排序
otu_table
rownames(otu_table) <- otu_table$"OTUID"                                         #提取"OTUID"作为行
rownames(otu_table)
otu_table <- otu_table[all_sample]
otu_table



all_ID <- c(all_otu, all_sample)                                                 #罗列all_otu和all_sample（"OTUID"+"SampleID"）并命名all_ID
all_ID
accum_otu <- rowSums(otu_table)                                                  #提取otu_table每一行的和（每个"OTUID"的和）并命名accum_otu
accum_otu
accum_sample <- colSums(otu_table)                                               #提取otu_table每一列的和（每个"SampleID"的和）并命名accum_sample
accum_sample
all_ID_xlim <- cbind(rep(0, length(all_ID)),data.frame(c(accum_otu, accum_sample)))  #把all_ID数量的0作为列，和数据框（两列，第一列为accum_otu+accum_sample的首行"OTUID"+"SampleID"的合集，第二列为accum_otu+accum_sample的第二行数字"OTUID"+"SampleID"的和）的列一起合并成为all_ID_xlim
all_ID_xlim


library(reshape2)


otu_table$"OTUID" <- all_otu                                                     #otu_table的"OTUID"那一列设定为all_otu
otu_table$"OTUID"
plot_data <- melt(otu_table, id = "OTUID")                                       #将宽格式转化为长格式并命名为plot_data，plot_data的第一列为"OTUID"
plot_data
colnames(plot_data)[2] <- "SampleID"                                               #将plot_data的第二列题头改为"SampleID"
plot_data
plot_data$"OTUID" <- factor(plot_data$"OTUID", levels = all_otu)                 #设置plot_data的"OTUID"列的排序顺序是all_otu的顺序
plot_data$"SampleID" <- factor(plot_data$"SampleID", levels = all_sample)            #设置plot_data的"SampleID"列的排序顺序是all_sample的顺序
plot_data <- plot_data[order(plot_data$"OTUID", plot_data$"SampleID"), ]           #先升序排序plot_data的"OTUID"列，这基础上再升序"SampleID"列
plot_data
plot_data <- plot_data[c(2, 1, 3, 3)]                                            #排序plot_data，让原本第二列"SampleID"变第一列，原第一列"OTUID"变第二列，并且复制第3列并生成与3列相同的第4列
plot_data




color_otu <- c('#FDDED7', '#F5BE8F', '#C1E0DB', '#CCD376', '#A28CC2', '#8498AB', '#5CB0C3',
               '#EF98A1')            #颜色设置  OTUID-2018吉林9个 湖北8个 海南6个

color_sample <- c('#BEEFBF')                    # SampleID-2018吉林2个 湖北1个 海南4个

color_phylum <- c('#E9BFC0')                    # Family-2018吉林3个 湖北同1个 海南同3个独自1个

color_group <- c('#C2B1D7')                     # Province-2018吉林1个 湖北1 海南同2

names(color_otu) <- all_otu                                                      #设置color_otu的名字对应all_otu里面的元素
color_otu
names(color_sample) <- all_sample                                                #设置color_sample的名字对应all_sample里面的元素
color_sample


library(circlize)

gap_size <- c(rep(3, length(all_otu) - 1), 6, rep(3, length(all_sample) - 1), 6) #设置圈图中样品间距，all_otu里的OTU间距和all_sample里的样品间距均为3，all_otu和all_sample两组间距为6。（得和下边统一）
gap_size
circos.par(cell.padding = c(0, 0, 0, 0), start.degree = 270, gap.degree = gap_size)  #设置圈图布局。是从逆时针270°开始
circos.initialize(sectors = factor(all_ID, levels = all_ID), xlim = all_ID_xlim) #设置圆内的因子为factor因子变量，x轴是all_ID_xlim


circos.trackPlotRegion(
  ylim = c(0, 1), track.height = 0.03, bg.border = NA,
  panel.fun = function(x, y) {
    sector.index = get.cell.meta.data('sector.index')
    xlim = get.cell.meta.data('xlim')
    ylim = get.cell.meta.data('ylim')
  } )                                                                            #第一圈（最外圈）基本设定，设置'sector.index'为sector扇区名字，xlim为x轴，ylim为y轴

for (i in 1:length(tax_phylum)) {
  tax_OTU <- {subset(taxonomy, Family == tax_phylum[i])}$"OTUID"
  highlight.sector(tax_OTU, track.index = 1, col = color_phylum[i], 
                   text = tax_phylum[i], cex = 0.5, text.col = 'black', niceFacing = FALSE)
}                                                                                #第一圈的设置变量i是1到tax_phylum的总长，选择taxonomy中符合Family == tax_phylum[i]的行里的"OTUID"列，这里Family不能加引号。#在其中的环段里显示文字，内容是所有tax_phylum的内容,字号0.5，黑色，在环段中心


for (i in 1:length(all_group)) {
  group_sample <- {subset(group, Province == all_group[i])}$"SampleID"
  highlight.sector(group_sample, track.index = 1, col = color_group[i], 
                   text = all_group[i], cex = 0.7, text.col = 'black', niceFacing = FALSE)
}                                                                                #第一圈的设置变量i是1到all_group的总长，选择group中符合Province == all_group[i]的行里的"SampleID"列，这里Province不能加引号。#在其中的环段里显示文字，内容是所有all_group的内容,字号0.7，黑色，顺着环段排布



circos.trackPlotRegion(
  ylim = c(0, 1), track.height = 0.05, bg.border = NA,
  panel.fun = function(x, y) {
    sector.index = get.cell.meta.data('sector.index')
    xlim = get.cell.meta.data('xlim')
    ylim = get.cell.meta.data('ylim')
  } )                                                                            #第二圈（百分比度数圈）设定开始，xlim是之前就指定的all_ID_xlim

circos.track(
  track.index = 2, bg.border = NA,
  panel.fun = function(x, y) {
    xlim = get.cell.meta.data('xlim')
    ylim = get.cell.meta.data('ylim')
    sector.name = get.cell.meta.data('sector.index')
    xplot = get.cell.meta.data('xplot')
    
    by = ifelse(abs(xplot[2] - xplot[1]) > 30, 0.25, 1)
    for (p in c(0, seq(by, 1, by = by))) circos.text(p*(xlim[2] - xlim[1]) + xlim[1], 
                                                     mean(ylim) + 0.4, paste0(p*100, '%'), 
                                                     cex = 0.4, adj = c(0.5, 0), 
                                                     niceFacing = FALSE)
    
    circos.lines(xlim, c(mean(ylim), mean(ylim)), lty = 3)
  } )                                                                            #第二圈circos.track设定百分比图，如果2的度数减1的度数的绝对值大于30，输出0.25，不大于30输出1。#p在0到by到1之间，设置标签文字



circos.trackPlotRegion(
  ylim = c(0, 1), track.height = 0.03, bg.col = c(color_otu, color_sample), bg.border = NA, track.margin = c(0, 0.01),
  panel.fun = function(x, y) {
    xlim = get.cell.meta.data('xlim')
    sector.name = get.cell.meta.data('sector.index')
    circos.axis(h = 'top', labels.cex = 0.4, major.tick.length = 0.4, 
                labels.niceFacing = FALSE)
    circos.text(mean(xlim), 0.2, sector.name, cex = 0.4, niceFacing = FALSE, 
                adj = c(0.5, 0))
  } )                                                                            #第三圈。在第三圈顶部画了比例尺


circos.trackPlotRegion(ylim = c(0, 1), track.height = 0.03, track.margin = c(0, 0.01))  #第四圈，track.margin是单元格/环段和环段间空白间距


for (i in seq_len(nrow(plot_data))) {
  circos.link(
    plot_data[i,2], c(accum_otu[plot_data[i,2]], accum_otu[plot_data[i,2]] - plot_data[i,4]),
    plot_data[i,1], c(accum_sample[plot_data[i,1]], accum_sample[plot_data[i,1]] - plot_data[i,3]),
    col = paste0(color_otu[plot_data[i,2]], '70'), border = NA )
  
  circos.rect(accum_otu[plot_data[i,2]], 0, accum_otu[plot_data[i,2]] - plot_data[i,4], 1, 
              sector.index = plot_data[i,2], col = color_sample[plot_data[i,1]], border = NA)
  circos.rect(accum_sample[plot_data[i,1]], 0, accum_sample[plot_data[i,1]] - plot_data[i,3], 1, 
              sector.index = plot_data[i,1], col = color_otu[plot_data[i,2]], border = NA)
  
  accum_otu[plot_data[i,2]] = accum_otu[plot_data[i,2]] - plot_data[i,4]
  accum_sample[plot_data[i,1]] = accum_sample[plot_data[i,1]] - plot_data[i,3]
}                                                                                #圈图最内部连线



library(ComplexHeatmap) 

otu_legend <- Legend(
  at = all_otu, labels = taxonomy$"OTUID", labels_gp = gpar(fontsize = 8),
  grid_height = unit(0.5, 'cm'), grid_width = unit(0.5, 'cm'), type = 'points', 
  pch = NA, background = color_otu)                                              #添加图例

pushViewport(viewport(x = 0.80, y = 0.5))
grid.draw(otu_legend)                                                            #添加图例注解
upViewport()


dev.off()                                                                        #关闭绘图设备
circos.clear()                                                                   #关闭圈图制作并清零面板




#=============================================================================================================================
#海南内外科

pdf('Hainan_in_near-Province.pdf', width = 15, height = 8)   #建立pdf




library(grid)
circle_size = unit(1, 'snpc')    #规定圈图单位是像素（grid包的功能）


group2 <- read.delim('3.2.3.3_sample-metadata_simple_next_mix_in_near_RENAME_R.txt', sep = '\t', stringsAsFactors = FALSE)

Hainan_group2 <- subset(group2, SampleID %in% c("Within_Jianfengling_Nature_Reserve", "Near_Jianfengling_Nature_Reserve"))
Hainan_group2

group <- Hainan_group2[order(Hainan_group2$"Province", Hainan_group2$"SampleID"), ]                         #先对"Province"这个列排序，在这基础上对"SampleID"的列排序后，结果用来给group来排序--影响最终图上右侧分组的顺序



all_group <- unique(group$"Province")                                                #提取group的"Province"列并除去重复后命名为all_group
all_group
group$"Province" <- factor(group$"Province", levels = all_group)
group$"Province"
all_sample <- group$"SampleID"                                                     #提取group的"SampleID"列并命名为all_sample
all_sample


otu_table2 <- read.delim('1.9.2.3_clones_mix_in_near_clones_g-1_RENAME_R.txt', sep = '\t')

Hainan_otu_table1 <- otu_table2[-which(c(otu_table2$Near_Jianfengling_Nature_Reserve == 0.0 & otu_table2$Within_Jianfengling_Nature_Reserve == 0.0)),]       ##取长白拥有clone的物种
Hainan_otu_table1

Hainan_otu_table2 <- Hainan_otu_table1[,-which(c(colnames(Hainan_otu_table1) != "OTUID" & colnames(Hainan_otu_table1) != "Near_Jianfengling_Nature_Reserve" & colnames(Hainan_otu_table1) != "Within_Jianfengling_Nature_Reserve"))]  ##取长白的地点
Hainan_otu_table2

Hainan_otu_table2$OTUID


taxonomy2 <- read.delim('4.1_taxonomy_KPCOFGS_R.txt', sep = '\t', stringsAsFactors = FALSE)    #或者read.delim(file.choose(), sep = '\t', stringsAsFactors = FALSE)



Hainan_taxonomy2 <- subset(taxonomy2, OTUID %in% c(Hainan_otu_table2$OTUID))          #选取Hainan特有的行
Hainan_taxonomy2
taxonomy <- Hainan_taxonomy2[order(Hainan_taxonomy2$"Family", Hainan_taxonomy2$"OTUID"), ]            #先对"Family"这个列排序，在这基础上对"OTUID"的列排序后，结果用来给taxonomy来排序--影响最终图上左侧分组的顺序

tax_phylum <- unique(taxonomy$"Family")                                          #提取taxonomy的"Family"列并除去重复后命名为tax_phylum
tax_phylum
taxonomy$"Family" <- factor(taxonomy$"Family", levels = tax_phylum)
taxonomy$"Family"
all_otu <- taxonomy$"OTUID"                                                      #提取taxonomy的"OTUID"列并命名为all_otu
all_otu
taxonomy$"OTUID" <- factor(taxonomy$"OTUID", levels = all_otu)
taxonomy$"OTUID"






otu_table <- merge(taxonomy, Hainan_otu_table2, by = "OTUID")                            #把taxonomy合并到otu_table里，以"OTUID"为对照
otu_table <- otu_table[order(otu_table$"Family", otu_table$"OTUID"), ]           #先对"Family"这个列排序，在这基础上对"OTUID"的列排序后，结果用来给otu_table来排序
otu_table
rownames(otu_table) <- otu_table$"OTUID"                                         #提取"OTUID"作为行
rownames(otu_table)
otu_table <- otu_table[all_sample]
otu_table



all_ID <- c(all_otu, all_sample)                                                 #罗列all_otu和all_sample（"OTUID"+"SampleID"）并命名all_ID
all_ID
accum_otu <- rowSums(otu_table)                                                  #提取otu_table每一行的和（每个"OTUID"的和）并命名accum_otu
accum_otu
accum_sample <- colSums(otu_table)                                               #提取otu_table每一列的和（每个"SampleID"的和）并命名accum_sample
accum_sample
all_ID_xlim <- cbind(rep(0, length(all_ID)),data.frame(c(accum_otu, accum_sample)))  #把all_ID数量的0作为列，和数据框（两列，第一列为accum_otu+accum_sample的首行"OTUID"+"SampleID"的合集，第二列为accum_otu+accum_sample的第二行数字"OTUID"+"SampleID"的和）的列一起合并成为all_ID_xlim
all_ID_xlim


library(reshape2)


otu_table$"OTUID" <- all_otu                                                     #otu_table的"OTUID"那一列设定为all_otu
otu_table$"OTUID"
plot_data <- melt(otu_table, id = "OTUID")                                       #将宽格式转化为长格式并命名为plot_data，plot_data的第一列为"OTUID"
plot_data
colnames(plot_data)[2] <- "SampleID"                                               #将plot_data的第二列题头改为"SampleID"
plot_data
plot_data$"OTUID" <- factor(plot_data$"OTUID", levels = all_otu)                 #设置plot_data的"OTUID"列的排序顺序是all_otu的顺序
plot_data$"SampleID" <- factor(plot_data$"SampleID", levels = all_sample)            #设置plot_data的"SampleID"列的排序顺序是all_sample的顺序
plot_data <- plot_data[order(plot_data$"OTUID", plot_data$"SampleID"), ]           #先升序排序plot_data的"OTUID"列，这基础上再升序"SampleID"列
plot_data
plot_data <- plot_data[c(2, 1, 3, 3)]                                            #排序plot_data，让原本第二列"SampleID"变第一列，原第一列"OTUID"变第二列，并且复制第3列并生成与3列相同的第4列
plot_data




color_otu <- c('#A5D1B0', '#CE8A8D', '#FFF7C1', '#E0F3FF', '#ADD3F4', 
               '#F7C9CF')            #颜色设置  OTUID-吉林9个 湖北8个 海南6个

color_sample <- c('#BEEFBF', '#F8C9C8')                    # SampleID-吉林2个 湖北3个 海南4个

color_phylum <- c('#C0E2D2', '#E9BFC0', '#E3E3E3')                    # Family-吉林3个 湖北同1个 海南同3个独自1个

color_group <- c('#FCCB8E')                     # Province-吉林1个 湖北同1 海南同2

names(color_otu) <- all_otu                                                      #设置color_otu的名字对应all_otu里面的元素
color_otu
names(color_sample) <- all_sample                                                #设置color_sample的名字对应all_sample里面的元素
color_sample


library(circlize)

gap_size <- c(rep(3, length(all_otu) - 1), 6, rep(3, length(all_sample) - 1), 6) #设置圈图中样品间距，all_otu里的OTU间距和all_sample里的样品间距均为3，all_otu和all_sample两组间距为6。（得和下边统一）
gap_size
circos.par(cell.padding = c(0, 0, 0, 0), start.degree = 270, gap.degree = gap_size)  #设置圈图布局。是从逆时针270°开始
circos.initialize(sectors = factor(all_ID, levels = all_ID), xlim = all_ID_xlim) #设置圆内的因子为factor因子变量，x轴是all_ID_xlim


circos.trackPlotRegion(
  ylim = c(0, 1), track.height = 0.03, bg.border = NA,
  panel.fun = function(x, y) {
    sector.index = get.cell.meta.data('sector.index')
    xlim = get.cell.meta.data('xlim')
    ylim = get.cell.meta.data('ylim')
  } )                                                                            #第一圈（最外圈）基本设定，设置'sector.index'为sector扇区名字，xlim为x轴，ylim为y轴

for (i in 1:length(tax_phylum)) {
  tax_OTU <- {subset(taxonomy, Family == tax_phylum[i])}$"OTUID"
  highlight.sector(tax_OTU, track.index = 1, col = color_phylum[i], 
                   text = tax_phylum[i], cex = 0.5, text.col = 'black', niceFacing = FALSE)
}                                                                                #第一圈的设置变量i是1到tax_phylum的总长，选择taxonomy中符合Family == tax_phylum[i]的行里的"OTUID"列，这里Family不能加引号。#在其中的环段里显示文字，内容是所有tax_phylum的内容,字号0.5，黑色，在环段中心


for (i in 1:length(all_group)) {
  group_sample <- {subset(group, Province == all_group[i])}$"SampleID"
  highlight.sector(group_sample, track.index = 1, col = color_group[i], 
                   text = all_group[i], cex = 0.7, text.col = 'black', niceFacing = FALSE)
}                                                                                #第一圈的设置变量i是1到all_group的总长，选择group中符合Province == all_group[i]的行里的"SampleID"列，这里Province不能加引号。#在其中的环段里显示文字，内容是所有all_group的内容,字号0.7，黑色，顺着环段排布



circos.trackPlotRegion(
  ylim = c(0, 1), track.height = 0.05, bg.border = NA,
  panel.fun = function(x, y) {
    sector.index = get.cell.meta.data('sector.index')
    xlim = get.cell.meta.data('xlim')
    ylim = get.cell.meta.data('ylim')
  } )                                                                            #第二圈（百分比度数圈）设定开始，xlim是之前就指定的all_ID_xlim

circos.track(
  track.index = 2, bg.border = NA,
  panel.fun = function(x, y) {
    xlim = get.cell.meta.data('xlim')
    ylim = get.cell.meta.data('ylim')
    sector.name = get.cell.meta.data('sector.index')
    xplot = get.cell.meta.data('xplot')
    
    by = ifelse(abs(xplot[2] - xplot[1]) > 30, 0.25, 1)
    for (p in c(0, seq(by, 1, by = by))) circos.text(p*(xlim[2] - xlim[1]) + xlim[1], 
                                                     mean(ylim) + 0.4, paste0(p*100, '%'), 
                                                     cex = 0.4, adj = c(0.5, 0), 
                                                     niceFacing = FALSE)
    
    circos.lines(xlim, c(mean(ylim), mean(ylim)), lty = 3)
  } )                                                                            #第二圈circos.track设定百分比图，如果2的度数减1的度数的绝对值大于30，输出0.25，不大于30输出1。#p在0到by到1之间，设置标签文字



circos.trackPlotRegion(
  ylim = c(0, 1), track.height = 0.03, bg.col = c(color_otu, color_sample), bg.border = NA, track.margin = c(0, 0.01),
  panel.fun = function(x, y) {
    xlim = get.cell.meta.data('xlim')
    sector.name = get.cell.meta.data('sector.index')
    circos.axis(h = 'top', labels.cex = 0.4, major.tick.length = 0.4, 
                labels.niceFacing = FALSE)
    circos.text(mean(xlim), 0.2, sector.name, cex = 0.4, niceFacing = FALSE, 
                adj = c(0.5, 0))
  } )                                                                            #第三圈。在第三圈顶部画了比例尺


circos.trackPlotRegion(ylim = c(0, 1), track.height = 0.03, track.margin = c(0, 0.01))  #第四圈，track.margin是单元格/环段和环段间空白间距


for (i in seq_len(nrow(plot_data))) {
  circos.link(
    plot_data[i,2], c(accum_otu[plot_data[i,2]], accum_otu[plot_data[i,2]] - plot_data[i,4]),
    plot_data[i,1], c(accum_sample[plot_data[i,1]], accum_sample[plot_data[i,1]] - plot_data[i,3]),
    col = paste0(color_otu[plot_data[i,2]], '70'), border = NA )
  
  circos.rect(accum_otu[plot_data[i,2]], 0, accum_otu[plot_data[i,2]] - plot_data[i,4], 1, 
              sector.index = plot_data[i,2], col = color_sample[plot_data[i,1]], border = NA)
  circos.rect(accum_sample[plot_data[i,1]], 0, accum_sample[plot_data[i,1]] - plot_data[i,3], 1, 
              sector.index = plot_data[i,1], col = color_otu[plot_data[i,2]], border = NA)
  
  accum_otu[plot_data[i,2]] = accum_otu[plot_data[i,2]] - plot_data[i,4]
  accum_sample[plot_data[i,1]] = accum_sample[plot_data[i,1]] - plot_data[i,3]
}                                                                                #圈图最内部连线



library(ComplexHeatmap) 

otu_legend <- Legend(
  at = all_otu, labels = taxonomy$"OTUID", labels_gp = gpar(fontsize = 8),
  grid_height = unit(0.5, 'cm'), grid_width = unit(0.5, 'cm'), type = 'points', 
  pch = NA, background = color_otu)                                              #添加图例

pushViewport(viewport(x = 0.80, y = 0.5))
grid.draw(otu_legend)                                                            #添加图例注解
upViewport()


dev.off()                                                                        #关闭绘图设备
circos.clear()                                                                   #关闭圈图制作并清零面板
