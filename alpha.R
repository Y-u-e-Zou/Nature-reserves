#===========================================================================================================
#各样点所有α多样性
#α多样性三地保护区内外Cavenderia clones_g-1比较
library(vegan)
library(picante)  
#读入抽平后的otu表
otuZY <- read.table("Table S1.txt", header=T, sep="\t", quote = "", row.names=1, comment.char="",stringsAsFactors = FALSE)
otuZY
otuZY_X10 <- otuZY * 10
otuZY_X10

#去除非Cavenderia的行
otuZY_X10_Cav <- otuZY_X10[-which(c(rownames(otuZY_X10) != "Cavenderia_bifurcatimacrosorum") & rownames(otuZY_X10) != "Cavenderia_fasciculaticomplexispora" & rownames(otuZY_X10) != "Cavenderia_inordinata" & rownames(otuZY_X10) != "Cavenderia_varicelloides"),]
otuZY_X10_Cav


#将otu数据转置
otuZY_1 <- t(otuZY_X10_Cav)
otuZY_1
# otu <- otuZY_1 * 100    #将otu表整体乘100，以便计算多样性
otu <- otuZY_1

otu

library(ape) #读取发育树用的包
tree <- read.tree("Tree S1.nwk")


alpha_diversity <- function(x, tree = NULL) {
  
  observed_species_Cav <- estimateR(x)[1, ]   #observed_species指数是物种丰富度Richness指数，这里也可以用 observed_species <- rowSums(x > 0) 代替（即原本的 observed_species <- rowSums(otu > 0)）
  Chao1_Cav <- estimateR(x)[2, ]  #Chao1和ACE是群落丰富度Community richness的指数，是样本中物种种类数目。 #Chao1衡量物种丰富度，值越高代表群落物种越丰富
  ACE_Cav <- estimateR(x)[4, ]  #ACE是利用稀有物种估算物种多样性的指数，值越高代表群落物种种类越丰富，是估计群落中含OTU数目的指数
  Shannon_Cav <- diversity(x, index = 'shannon',base = 2) #Shannon和Simpson是群落多样性Community diversity的指数,样本中各个种的相对密度，即群落中个体分配上的均匀度Evenness #Shannon是估算样本中多样性的指数,值越大群落多样性越高 
  #Shannon指数通常用2、e作为指数，这里是2作为指数，如果用e作为指数就要变成 Shannon <- diversity(x, index = 'shannon', base = exp(1))
  #或者试试 Shannon <- diversity(Aspe,index="shannon")
  Simpson_Cav <- diversity(x, index = 'simpson')   #Simpson是估算样本中多样性的指数，Gini-Simpson指数随着丰富度的增加而增加；经典Simpson则是指数值越大，群落多样性越低 
  #注意，这里是常用的Simpson指数Gini-Simpson指数，如果是经典Simpson指数则需要在下边再加一个为 simpson_index <- 1 - Simpson，或者 simpson_index <- diversity(Aspe,index="inv")
  goods_Coverage_Cav <- 1 - rowSums(x == 1) / rowSums(x)  #Coverage：是指各样品文库的覆盖率，其数值越高，样本中序列没有被测出的概率越低。该指数反映了测序结果是否代表样本的真实情况。
  
  Shannon.Wiener_Cav <- diversity(x, index = "shannon")    #Shannon-Wiener指数计算
  Inverse.Simpson_Cav <- diversity(x, index = "inv")    #Inverse Simpson指数计算 另一种经典Simpson指数
  Specnumber_Cav <- specnumber(x)     #物种累计数（物种丰富度）计算，也可以是丰富度richness，即群落中丰度大于0的otu数量之和 richness <- herb.mat="">0)
  Pielou_Cav <- Shannon.Wiener_Cav/log(Specnumber_Cav)  #Pielou均匀度指数计算（Shannon比上log(specnumber)）
  
  simpson_index_Cav <- 1 - Simpson_Cav  #Simpson’s Index of Diversity
  
  #保留四位小数
  Shannon_Cav <- sprintf("%0.4f", Shannon_Cav)
  Simpson_Cav <- sprintf("%0.4f", Simpson_Cav)
  goods_Coverage_Cav <- sprintf("%0.4f", goods_Coverage_Cav)
  
  Shannon.Wiener_Cav <- sprintf("%0.4f", Shannon.Wiener_Cav)
  simpson_index_Cav <- sprintf("%0.4f", simpson_index_Cav)  
  Inverse.Simpson_Cav <- sprintf("%0.4f", Inverse.Simpson_Cav)
  Pielou_Cav <- sprintf("%0.4f", Pielou_Cav)
  
  if (!is.null(tree)) {
    PD_whole_tree_Cav <- pd(x, tree, include.root = TRUE)[1]     #这里是计算了谱系多样性（PD），include.root = FALSE不计算单一物种，include.root = TRUE能计算群落里只有一个物种时，但需要有根树
    names(PD_whole_tree_Cav) <- 'PD_whole_tree_Cav'
    result <- cbind(PD_whole_tree_Cav)
    
    result <- data.frame(
      PD_whole_tree_Cav,
      observed_species_Cav, Chao1_Cav, Shannon_Cav, Simpson_Cav, Shannon.Wiener_Cav, Inverse.Simpson_Cav, Specnumber_Cav, Pielou_Cav, simpson_index_Cav
    )
  }
  result
}

#需要计算谱系多样性时，需要指定进化树文件
alpha_PD_Cav <- alpha_diversity (otu, tree)
alpha_PD_Cav







#α多样性三地保护区内外Dictyostelium clones_g-1比较
library(ggThemeAssist)
library(vegan)
library(picante)  
#读入抽平后的otu表
otuZY <- read.table("Table S1.txt", header=T, sep="\t", quote = "", row.names=1, comment.char="",stringsAsFactors = FALSE)
otuZY
otuZY_X10 <- otuZY * 10
otuZY_X10

#去除非Dictyostelium的行
library(dplyr)
otuZY_X10$type <- rownames(otuZY_X10)      #在otuZY_X10里加一个列type，内容是行名
#dplyr::filter(otuZY_X10, !grepl('Dictyostelium', type))   #删除行名含Dictyostelium的行
otuZY_X10_Dic <- dplyr::filter(otuZY_X10, grepl('Dictyostelium', type))   #筛选出行名含Dictyostelium的行
otuZY_X10_Dic <- otuZY_X10_Dic[,-which(names(otuZY_X10_Dic) == "type")]    #删除type列
otuZY_X10_Dic
#将otu数据转置
otuZY_1 <- t(otuZY_X10_Dic)
otuZY_1
# otu <- otuZY_1 * 100    #将otu表整体乘100，以便计算多样性
otu <- otuZY_1

otu

#如果需要计算谱系多样性，则再加载一个进化树文件otutree.tre： tree <- read.tree(file.choose())

library(ape) #读取发育树用的包
tree <- read.tree("Tree S1.nwk")


alpha_diversity <- function(x, tree = NULL) {
  
  observed_species_Dic <- estimateR(x)[1, ]   #observed_species指数是物种丰富度Richness指数，这里也可以用 observed_species <- rowSums(x > 0) 代替（即原本的 observed_species <- rowSums(otu > 0)）
  Chao1_Dic <- estimateR(x)[2, ]  #Chao1和ACE是群落丰富度Community richness的指数，是样本中物种种类数目。 #Chao1衡量物种丰富度，值越高代表群落物种越丰富
  ACE_Dic <- estimateR(x)[4, ]  #ACE是利用稀有物种估算物种多样性的指数，值越高代表群落物种种类越丰富，是估计群落中含OTU数目的指数
  Shannon_Dic <- diversity(x, index = 'shannon',base = 2) #Shannon和Simpson是群落多样性Community diversity的指数,样本中各个种的相对密度，即群落中个体分配上的均匀度Evenness #Shannon是估算样本中多样性的指数,值越大群落多样性越高 
  #Shannon指数通常用2、e作为指数，这里是2作为指数，如果用e作为指数就要变成 Shannon <- diversity(x, index = 'shannon', base = exp(1))
  #或者试试 Shannon <- diversity(Aspe,index="shannon")
  Simpson_Dic <- diversity(x, index = 'simpson')   #Simpson是估算样本中多样性的指数，Gini-Simpson指数随着丰富度的增加而增加；经典Simpson则是指数值越大，群落多样性越低 
  #注意，这里是常用的Simpson指数Gini-Simpson指数，如果是经典Simpson指数则需要在下边再加一个为 simpson_index <- 1 - Simpson，或者 simpson_index <- diversity(Aspe,index="inv")
  goods_Coverage_Dic <- 1 - rowSums(x == 1) / rowSums(x)  #Coverage：是指各样品文库的覆盖率，其数值越高，样本中序列没有被测出的概率越低。该指数反映了测序结果是否代表样本的真实情况。
  
  Shannon.Wiener_Dic <- diversity(x, index = "shannon")    #Shannon-Wiener指数计算
  Inverse.Simpson_Dic <- diversity(x, index = "inv")    #Inverse Simpson指数计算 另一种经典Simpson指数
  Specnumber_Dic <- specnumber(x)     #物种累计数（物种丰富度）计算，也可以是丰富度richness，即群落中丰度大于0的otu数量之和 richness <- herb.mat="">0)
  Pielou_Dic <- Shannon.Wiener_Dic/log(Specnumber_Dic)  #Pielou均匀度指数计算（Shannon比上log(specnumber)）
  
  simpson_index_Dic <- 1 - Simpson_Dic  #Simpson’s Index of Diversity
  
  #保留四位小数
  Shannon_Dic <- sprintf("%0.4f", Shannon_Dic)
  Simpson_Dic <- sprintf("%0.4f", Simpson_Dic)
  goods_Coverage_Dic <- sprintf("%0.4f", goods_Coverage_Dic)
  
  Shannon.Wiener_Dic <- sprintf("%0.4f", Shannon.Wiener_Dic)
  simpson_index_Dic <- sprintf("%0.4f", simpson_index_Dic)  
  Inverse.Simpson_Dic <- sprintf("%0.4f", Inverse.Simpson_Dic)
  Pielou_Dic <- sprintf("%0.4f", Pielou_Dic)
  
  if (!is.null(tree)) {
    PD_whole_tree_Dic <- pd(x, tree, include.root = TRUE)[1]     #这里是计算了谱系多样性（PD），include.root = FALSE不计算单一物种，include.root = TRUE能计算群落里只有一个物种时，但需要有根树
    names(PD_whole_tree_Dic) <- 'PD_whole_tree_Dic'
    result <- cbind(PD_whole_tree_Dic)
    
    result <- data.frame(
      PD_whole_tree_Dic,
      observed_species_Dic, Chao1_Dic, Shannon_Dic, Simpson_Dic, Shannon.Wiener_Dic, Inverse.Simpson_Dic, Specnumber_Dic, Pielou_Dic, simpson_index_Dic
    )
  }
  result
}

#需要计算谱系多样性时，需要指定进化树文件
alpha_PD_Dic <- alpha_diversity (otu, tree)
alpha_PD_Dic







#α多样性三地保护区内外Polysphondylium clones_g-1比较
library(vegan)
library(picante)  
#读入抽平后的otu表
otuZY <- read.table("Table S1.txt", header=T, sep="\t", quote = "", row.names=1, comment.char="",stringsAsFactors = FALSE)
otuZY
otuZY_X10 <- otuZY * 10
otuZY_X10

#去除非Polysphondylium的行
library(dplyr)
otuZY_X10$type <- rownames(otuZY_X10)      #在otuZY_X10里加一个列type，内容是行名
#dplyr::filter(otuZY_X10, !grepl('Polysphondylium', type))   #删除行名含Polysphondylium的行
otuZY_X10_Pol <- dplyr::filter(otuZY_X10, grepl('Polysphondylium', type))   #筛选出行名含Polysphondylium的行
otuZY_X10_Pol <- otuZY_X10_Pol[,-which(names(otuZY_X10_Pol) == "type")]    #删除type列
otuZY_X10_Pol
#将otu数据转置
otuZY_1 <- t(otuZY_X10_Pol)
otuZY_1
# otu <- otuZY_1 * 100    #将otu表整体乘100，以便计算多样性
otu <- otuZY_1

otu

#如果需要计算谱系多样性，则再加载一个进化树文件otutree.tre： tree <- read.tree(file.choose())

library(ape) #读取发育树用的包
tree <- read.tree("Tree S1.nwk")


alpha_diversity <- function(x, tree = NULL) {
  
  observed_species_Pol <- estimateR(x)[1, ]   #observed_species指数是物种丰富度Richness指数，这里也可以用 observed_species <- rowSums(x > 0) 代替（即原本的 observed_species <- rowSums(otu > 0)）
  Chao1_Pol <- estimateR(x)[2, ]  #Chao1和ACE是群落丰富度Community richness的指数，是样本中物种种类数目。 #Chao1衡量物种丰富度，值越高代表群落物种越丰富
  ACE_Pol <- estimateR(x)[4, ]  #ACE是利用稀有物种估算物种多样性的指数，值越高代表群落物种种类越丰富，是估计群落中含OTU数目的指数
  Shannon_Pol <- diversity(x, index = 'shannon',base = 2) #Shannon和Simpson是群落多样性Community diversity的指数,样本中各个种的相对密度，即群落中个体分配上的均匀度Evenness #Shannon是估算样本中多样性的指数,值越大群落多样性越高 
  #Shannon指数通常用2、e作为指数，这里是2作为指数，如果用e作为指数就要变成 Shannon <- diversity(x, index = 'shannon', base = exp(1))
  #或者试试 Shannon <- diversity(Aspe,index="shannon")
  Simpson_Pol <- diversity(x, index = 'simpson')   #Simpson是估算样本中多样性的指数，Gini-Simpson指数随着丰富度的增加而增加；经典Simpson则是指数值越大，群落多样性越低 
  #注意，这里是常用的Simpson指数Gini-Simpson指数，如果是经典Simpson指数则需要在下边再加一个为 simpson_index <- 1 - Simpson，或者 simpson_index <- diversity(Aspe,index="inv")
  goods_Coverage_Pol <- 1 - rowSums(x == 1) / rowSums(x)  #Coverage：是指各样品文库的覆盖率，其数值越高，样本中序列没有被测出的概率越低。该指数反映了测序结果是否代表样本的真实情况。
  
  Shannon.Wiener_Pol <- diversity(x, index = "shannon")    #Shannon-Wiener指数计算
  Inverse.Simpson_Pol <- diversity(x, index = "inv")    #Inverse Simpson指数计算 另一种经典Simpson指数
  Specnumber_Pol <- specnumber(x)     #物种累计数（物种丰富度）计算，也可以是丰富度richness，即群落中丰度大于0的otu数量之和 richness <- herb.mat="">0)
  Pielou_Pol <- Shannon.Wiener_Pol/log(Specnumber_Pol)  #Pielou均匀度指数计算（Shannon比上log(specnumber)）
  
  simpson_index_Pol <- 1 - Simpson_Pol  #Simpson’s Index of Diversity
  
  #保留四位小数
  Shannon_Pol <- sprintf("%0.4f", Shannon_Pol)
  Simpson_Pol <- sprintf("%0.4f", Simpson_Pol)
  goods_Coverage_Pol <- sprintf("%0.4f", goods_Coverage_Pol)
  
  Shannon.Wiener_Pol <- sprintf("%0.4f", Shannon.Wiener_Pol)
  simpson_index_Pol <- sprintf("%0.4f", simpson_index_Pol)  
  Inverse.Simpson_Pol <- sprintf("%0.4f", Inverse.Simpson_Pol)
  Pielou_Pol <- sprintf("%0.4f", Pielou_Pol)
  
  if (!is.null(tree)) {
    PD_whole_tree_Pol <- pd(x, tree, include.root = TRUE)[1]     #这里是计算了谱系多样性（PD），include.root = FALSE不计算单一物种，include.root = TRUE能计算群落里只有一个物种时，但需要有根树
    names(PD_whole_tree_Pol) <- 'PD_whole_tree_Pol'
    result <- cbind(PD_whole_tree_Pol)
    
    result <- data.frame(
      PD_whole_tree_Pol,
      observed_species_Pol, Chao1_Pol, Shannon_Pol, Simpson_Pol, Shannon.Wiener_Pol, Inverse.Simpson_Pol, Specnumber_Pol, Pielou_Pol, simpson_index_Pol
    )
  }
  result
}

#需要计算谱系多样性时，需要指定进化树文件
alpha_PD_Pol <- alpha_diversity (otu, tree)
alpha_PD_Pol







#α多样性三地保护区内外Hagiwaraea clones_g-1比较
library(vegan)
library(picante)  
#读入抽平后的otu表
otuZY <- read.table("Table S1.txt", header=T, sep="\t", quote = "", row.names=1, comment.char="",stringsAsFactors = FALSE)
otuZY
otuZY_X10 <- otuZY * 10
otuZY_X10

#去除非Hagiwaraea的行
library(dplyr)
otuZY_X10$type <- rownames(otuZY_X10)      #在otuZY_X10里加一个列type，内容是行名
#dplyr::filter(otuZY_X10, !grepl('Hagiwaraea', type))   #删除行名含Hagiwaraea的行
otuZY_X10_Hag <- dplyr::filter(otuZY_X10, grepl('Hagiwaraea', type))   #筛选出行名含Hagiwaraea的行
otuZY_X10_Hag <- otuZY_X10_Hag[,-which(names(otuZY_X10_Hag) == "type")]    #删除type列
otuZY_X10_Hag
#将otu数据转置
otuZY_1 <- t(otuZY_X10_Hag)
otuZY_1
# otu <- otuZY_1 * 100    #将otu表整体乘100，以便计算多样性
otu <- otuZY_1

otu

#如果需要计算谱系多样性，则再加载一个进化树文件otutree.tre： tree <- read.tree(file.choose())

library(ape) #读取发育树用的包
tree <- read.tree("Tree S1.nwk")


alpha_diversity <- function(x, tree = NULL) {
  
  observed_species_Hag <- estimateR(x)[1, ]   #observed_species指数是物种丰富度Richness指数，这里也可以用 observed_species <- rowSums(x > 0) 代替（即原本的 observed_species <- rowSums(otu > 0)）
  Chao1_Hag <- estimateR(x)[2, ]  #Chao1和ACE是群落丰富度Community richness的指数，是样本中物种种类数目。 #Chao1衡量物种丰富度，值越高代表群落物种越丰富
  ACE_Hag <- estimateR(x)[4, ]  #ACE是利用稀有物种估算物种多样性的指数，值越高代表群落物种种类越丰富，是估计群落中含OTU数目的指数
  Shannon_Hag <- diversity(x, index = 'shannon',base = 2) #Shannon和Simpson是群落多样性Community diversity的指数,样本中各个种的相对密度，即群落中个体分配上的均匀度Evenness #Shannon是估算样本中多样性的指数,值越大群落多样性越高 
  #Shannon指数通常用2、e作为指数，这里是2作为指数，如果用e作为指数就要变成 Shannon <- diversity(x, index = 'shannon', base = exp(1))
  #或者试试 Shannon <- diversity(Aspe,index="shannon")
  Simpson_Hag <- diversity(x, index = 'simpson')   #Simpson是估算样本中多样性的指数，Gini-Simpson指数随着丰富度的增加而增加；经典Simpson则是指数值越大，群落多样性越低 
  #注意，这里是常用的Simpson指数Gini-Simpson指数，如果是经典Simpson指数则需要在下边再加一个为 simpson_index <- 1 - Simpson，或者 simpson_index <- diversity(Aspe,index="inv")
  goods_Coverage_Hag <- 1 - rowSums(x == 1) / rowSums(x)  #Coverage：是指各样品文库的覆盖率，其数值越高，样本中序列没有被测出的概率越低。该指数反映了测序结果是否代表样本的真实情况。
  
  Shannon.Wiener_Hag <- diversity(x, index = "shannon")    #Shannon-Wiener指数计算
  Inverse.Simpson_Hag <- diversity(x, index = "inv")    #Inverse Simpson指数计算 另一种经典Simpson指数
  Specnumber_Hag <- specnumber(x)     #物种累计数（物种丰富度）计算，也可以是丰富度richness，即群落中丰度大于0的otu数量之和 richness <- herb.mat="">0)
  Pielou_Hag <- Shannon.Wiener_Hag/log(Specnumber_Hag)  #Pielou均匀度指数计算（Shannon比上log(specnumber)）
  
  simpson_index_Hag <- 1 - Simpson_Hag  #Simpson’s Index of Diversity
  
  #保留四位小数
  Shannon_Hag <- sprintf("%0.4f", Shannon_Hag)
  Simpson_Hag <- sprintf("%0.4f", Simpson_Hag)
  goods_Coverage_Hag <- sprintf("%0.4f", goods_Coverage_Hag)
  
  Shannon.Wiener_Hag <- sprintf("%0.4f", Shannon.Wiener_Hag)
  simpson_index_Hag <- sprintf("%0.4f", simpson_index_Hag)  
  Inverse.Simpson_Hag <- sprintf("%0.4f", Inverse.Simpson_Hag)
  Pielou_Hag <- sprintf("%0.4f", Pielou_Hag)
  
  if (!is.null(tree)) {
    PD_whole_tree_Hag <- pd(x, tree, include.root = TRUE)[1]     #这里是计算了谱系多样性（PD），include.root = FALSE不计算单一物种，include.root = TRUE能计算群落里只有一个物种时，但需要有根树
    names(PD_whole_tree_Hag) <- 'PD_whole_tree_Hag'
    result <- cbind(PD_whole_tree_Hag)
    
    result <- data.frame(
      PD_whole_tree_Hag,
      observed_species_Hag, Chao1_Hag, Shannon_Hag, Simpson_Hag, Shannon.Wiener_Hag, Inverse.Simpson_Hag, Specnumber_Hag, Pielou_Hag, simpson_index_Hag
    )
  }
  result
}

#需要计算谱系多样性时，需要指定进化树文件
alpha_PD_Hag <- alpha_diversity (otu, tree)
alpha_PD_Hag
#做谱系多样性时，则输出







#α多样性三地保护区内外Raperostelium clones_g-1比较
library(vegan)
library(picante)  
#读入抽平后的otu表
#otuZY_1 <- read.csv("1.9.2.4_clones_mix_in_near_clones_g-1_RENAME_x10for_diversity_R.csv", header=T, row.names = 1) #或者读xls格式otu <- read.delim(file.choose(), row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE) 
#或者读取txt格式的表 otu <- read.table(file.choose(), header=T, sep="\t", quote = "", row.names=1, comment.char="",stringsAsFactors = FALSE)
otuZY <- read.table("Table S1.txt", header=T, sep="\t", quote = "", row.names=1, comment.char="",stringsAsFactors = FALSE)
otuZY
otuZY_X10 <- otuZY * 10
otuZY_X10

#去除非Raperostelium的行
library(dplyr)
otuZY_X10$type <- rownames(otuZY_X10)      #在otuZY_X10里加一个列type，内容是行名
#dplyr::filter(otuZY_X10, !grepl('Raperostelium', type))   #删除行名含Raperostelium的行
otuZY_X10_Rap <- dplyr::filter(otuZY_X10, grepl('Raperostelium', type))   #筛选出行名含Raperostelium的行
otuZY_X10_Rap <- otuZY_X10_Rap[,-which(names(otuZY_X10_Rap) == "type")]    #删除type列
otuZY_X10_Rap
#将otu数据转置
otuZY_1 <- t(otuZY_X10_Rap)
otuZY_1
# otu <- otuZY_1 * 100    #将otu表整体乘100，以便计算多样性
otu <- otuZY_1

otu

#如果需要计算谱系多样性，则再加载一个进化树文件otutree.tre： tree <- read.tree(file.choose())

library(ape) #读取发育树用的包
tree <- read.tree("Tree S1.nwk")


alpha_diversity <- function(x, tree = NULL) {
  
  observed_species_Rap <- estimateR(x)[1, ]   #observed_species指数是物种丰富度Richness指数，这里也可以用 observed_species <- rowSums(x > 0) 代替（即原本的 observed_species <- rowSums(otu > 0)）
  Chao1_Rap <- estimateR(x)[2, ]  #Chao1和ACE是群落丰富度Community richness的指数，是样本中物种种类数目。 #Chao1衡量物种丰富度，值越高代表群落物种越丰富
  ACE_Rap <- estimateR(x)[4, ]  #ACE是利用稀有物种估算物种多样性的指数，值越高代表群落物种种类越丰富，是估计群落中含OTU数目的指数
  Shannon_Rap <- diversity(x, index = 'shannon',base = 2) #Shannon和Simpson是群落多样性Community diversity的指数,样本中各个种的相对密度，即群落中个体分配上的均匀度Evenness #Shannon是估算样本中多样性的指数,值越大群落多样性越高 
  #Shannon指数通常用2、e作为指数，这里是2作为指数，如果用e作为指数就要变成 Shannon <- diversity(x, index = 'shannon', base = exp(1))
  #或者试试 Shannon <- diversity(Aspe,index="shannon")
  Simpson_Rap <- diversity(x, index = 'simpson')   #Simpson是估算样本中多样性的指数，Gini-Simpson指数随着丰富度的增加而增加；经典Simpson则是指数值越大，群落多样性越低 
  #注意，这里是常用的Simpson指数Gini-Simpson指数，如果是经典Simpson指数则需要在下边再加一个为 simpson_index <- 1 - Simpson，或者 simpson_index <- diversity(Aspe,index="inv")
  goods_Coverage_Rap <- 1 - rowSums(x == 1) / rowSums(x)  #Coverage：是指各样品文库的覆盖率，其数值越高，样本中序列没有被测出的概率越低。该指数反映了测序结果是否代表样本的真实情况。
  
  Shannon.Wiener_Rap <- diversity(x, index = "shannon")    #Shannon-Wiener指数计算
  Inverse.Simpson_Rap <- diversity(x, index = "inv")    #Inverse Simpson指数计算 另一种经典Simpson指数
  Specnumber_Rap <- specnumber(x)     #物种累计数（物种丰富度）计算，也可以是丰富度richness，即群落中丰度大于0的otu数量之和 richness <- herb.mat="">0)
  Pielou_Rap <- Shannon.Wiener_Rap/log(Specnumber_Rap)  #Pielou均匀度指数计算（Shannon比上log(specnumber)）
  
  simpson_index_Rap <- 1 - Simpson_Rap  #Simpson’s Index of Diversity
  
  #保留四位小数
  Shannon_Rap <- sprintf("%0.4f", Shannon_Rap)
  Simpson_Rap <- sprintf("%0.4f", Simpson_Rap)
  goods_Coverage_Rap <- sprintf("%0.4f", goods_Coverage_Rap)
  
  Shannon.Wiener_Rap <- sprintf("%0.4f", Shannon.Wiener_Rap)
  simpson_index_Rap <- sprintf("%0.4f", simpson_index_Rap)  
  Inverse.Simpson_Rap <- sprintf("%0.4f", Inverse.Simpson_Rap)
  Pielou_Rap <- sprintf("%0.4f", Pielou_Rap)
  
  if (!is.null(tree)) {
    PD_whole_tree_Rap <- pd(x, tree, include.root = TRUE)[1]     #这里是计算了谱系多样性（PD），include.root = FALSE不计算单一物种，include.root = TRUE能计算群落里只有一个物种时，但需要有根树
    names(PD_whole_tree_Rap) <- 'PD_whole_tree_Rap'
    result <- cbind(PD_whole_tree_Rap)
    
    result <- data.frame(
      PD_whole_tree_Rap,
      observed_species_Rap, Chao1_Rap, Shannon_Rap, Simpson_Rap, Shannon.Wiener_Rap, Inverse.Simpson_Rap, Specnumber_Rap, Pielou_Rap, simpson_index_Rap
    )
  }
  result
}

#需要计算谱系多样性时，需要指定进化树文件
alpha_PD_Rap <- alpha_diversity (otu, tree)
alpha_PD_Rap
#做谱系多样性时，则输出


alpha_PD_Cav$Type <- rownames(alpha_PD_Cav)
alpha_PD_Dic$Type <- rownames(alpha_PD_Dic)
alpha_PD_Pol$Type <- rownames(alpha_PD_Pol)
alpha_PD_Hag$Type <- rownames(alpha_PD_Hag)
alpha_PD_Rap$Type <- rownames(alpha_PD_Rap)


#合并多表并作图
alpha_PD_merge_CD <- merge(alpha_PD_Cav, alpha_PD_Dic, by = 'Type', all = FALSE)

alpha_PD_merge_PH <- merge(alpha_PD_Pol, alpha_PD_Hag, by = 'Type', all = FALSE) 

alpha_PD_merge_CDPH  <- merge(alpha_PD_merge_CD, alpha_PD_merge_PH, by = 'Type', all = FALSE) 

alpha_PD_merge_all<- merge(alpha_PD_merge_CDPH, alpha_PD_Rap, by = 'Type', all = FALSE)

rownames(alpha_PD_merge_all) <- alpha_PD_merge_all$Type
alpha_PD_merge_all
alpha_PD_merge_all <- alpha_PD_merge_all[,-which(names(alpha_PD_merge_all) == "Type")]    #删除type列
write.csv(alpha_PD_merge_all,"Table S4.csv", quote = FALSE) #1.7.1


#===========================================================================================================
#==========================================================================================================================
library(vegan)
library(picante)  
#读入抽平后的otu表
#otuZY_1 <- read.csv("1.7.2_clones_g-1X10_R.csv", header=T, row.names = 1) #或者读xls格式otu <- read.delim(file.choose(), row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE) 
#或者读取txt格式的表 otu <- read.table(file.choose(), header=T, sep="\t", quote = "", row.names=1, comment.char="",stringsAsFactors = FALSE)
#将otu数据转置
otuZY <- read.table("Table S1.txt", header=T, sep="\t", quote = "", row.names=1, comment.char="",stringsAsFactors = FALSE)
otuZY
otuZY_X10 <- otuZY * 10
otuZY_X10
#将otu数据转置
otuZY_1 <- t(otuZY_X10)
otuZY_1

# otu <- otuZY_1 * 100    #将otu表整体乘100，以便计算多样性
otu <- otuZY_1

otu

#如果需要计算谱系多样性，则再加载一个进化树文件otutree.tre： tree <- read.tree(file.choose())

library(ape) #读取发育树用的包
tree <- read.tree(file.choose())


alpha_diversity <- function(x, tree = NULL) {
  observed_species <- estimateR(x)[1, ]   #observed_species指数是物种丰富度Richness指数，这里也可以用 observed_species <- rowSums(x > 0) 代替（即原本的 observed_species <- rowSums(otu > 0)）
  Chao1 <- estimateR(x)[2, ]  #Chao1和ACE是群落丰富度Community richness的指数，是样本中物种种类数目。 #Chao1衡量物种丰富度，值越高代表群落物种越丰富
  ACE <- estimateR(x)[4, ]  #ACE是利用稀有物种估算物种多样性的指数，值越高代表群落物种种类越丰富，是估计群落中含OTU数目的指数
  Shannon <- diversity(x, index = 'shannon',base = 2) #Shannon和Simpson是群落多样性Community diversity的指数,样本中各个种的相对密度，即群落中个体分配上的均匀度Evenness #Shannon是估算样本中多样性的指数,值越大群落多样性越高 
  #Shannon指数通常用2、e作为指数，这里是2作为指数，如果用e作为指数就要变成 Shannon <- diversity(x, index = 'shannon', base = exp(1))
  #或者试试 Shannon <- diversity(Aspe,index="shannon")
  Simpson <- diversity(x, index = 'simpson')   #Simpson是估算样本中多样性的指数，Gini-Simpson指数随着丰富度的增加而增加；经典Simpson则是指数值越大，群落多样性越低 
  #注意，这里是常用的Simpson指数Gini-Simpson指数，如果是经典Simpson指数则需要在下边再加一个为 simpson_index <- 1 - Simpson，或者 simpson_index <- diversity(Aspe,index="inv")
  goods_Coverage <- 1 - rowSums(x == 1) / rowSums(x)  #Coverage：是指各样品文库的覆盖率，其数值越高，样本中序列没有被测出的概率越低。该指数反映了测序结果是否代表样本的真实情况。
  
  Shannon.Wiener <- diversity(x, index = "shannon")    #Shannon-Wiener指数计算
  Inverse.Simpson <- diversity(x, index = "inv")    #Inverse Simpson指数计算 另一种经典Simpson指数
  Specnumber <- specnumber(x)     #物种累计数（物种丰富度）计算，也可以是丰富度richness，即群落中丰度大于0的otu数量之和 richness <- herb.mat="">0)
  Pielou <- Shannon.Wiener/log(Specnumber)  #Pielou均匀度指数计算（Shannon比上log(specnumber)）
  
  simpson_index <- 1 - Simpson  #Simpson’s Index of Diversity
  
  #保留四位小数
  Shannon <- sprintf("%0.4f", Shannon)
  Simpson <- sprintf("%0.4f", Simpson)
  goods_Coverage <- sprintf("%0.4f", goods_Coverage)
  
  Shannon.Wiener <- sprintf("%0.4f", Shannon.Wiener)
  simpson_index <- sprintf("%0.4f", simpson_index)  
  Inverse.Simpson <- sprintf("%0.4f", Inverse.Simpson)
  Pielou <- sprintf("%0.4f", Pielou)
  
  result <- data.frame(observed_species, Chao1, Shannon, Simpson, Shannon.Wiener)
  
  
  
  
  
  if (!is.null(tree)) {
    PD_whole_tree <- pd(x, tree, include.root = TRUE)[1]     #这里是计算了谱系多样性（PD），include.root = FALSE不计算单一物种，include.root = TRUE能计算群落里只有一个物种时，但需要有根树
    names(PD_whole_tree) <- 'PD_whole_tree'
    result <- cbind(result, PD_whole_tree)
    
    result <- data.frame(observed_species,Chao1, Shannon, Simpson, 
                         PD_whole_tree, 
                         Shannon.Wiener)
  }
  
  
  result
}

#如果不需要计算谱系多样性
alpha <- alpha_diversity (otu)
alpha

write.csv(alpha,"α多样性计算结果-ZY_1.7.2.csv", quote = FALSE)  #或者 write.table(alpha,"多样性计算结果.csv", quote = FALSE)

#需要计算谱系多样性时，需要指定进化树文件
alpha1 <- alpha_diversity (otu, tree)
alpha1
#做谱系多样性时，则输出
write.csv(alpha1,"Table S5.csv", quote = FALSE)


#========================================================================================================================
#α多样性三地保护区内外clones_g-1比较
library(vegan)
library(picante)  
#读入抽平后的otu表
otuZY <- read.table("Table S2.txt", header=T, sep="\t", quote = "", row.names=1, comment.char="",stringsAsFactors = FALSE)
otuZY
otuZY_X10 <- otuZY * 10
otuZY_X10
#将otu数据转置
otuZY_1 <- t(otuZY_X10)
otuZY_1

otu <- otuZY_1

otu



library(ape) #读取发育树用的包
tree <- read.tree("Tree S1.nwk")


alpha_diversity <- function(x, tree = NULL) {
  observed_species <- estimateR(x)[1, ]   #observed_species指数是物种丰富度Richness指数，这里也可以用 observed_species <- rowSums(x > 0) 代替（即原本的 observed_species <- rowSums(otu > 0)）,去掉[1, ]则会出现observe,ACE,Chao1等值
  Chao1 <- estimateR(x)[2, ]  #Chao1和ACE是群落丰富度Community richness的指数，是样本中物种种类数目。 #Chao1衡量物种丰富度，值越高代表群落物种越丰富
  ACE <- estimateR(x)[4, ]  #ACE是利用稀有物种估算物种多样性的指数，值越高代表群落物种种类越丰富，是估计群落中含OTU数目的指数
  Shannon <- diversity(x, index = 'shannon',base = 2) #Shannon和Simpson是群落多样性Community diversity的指数,样本中各个种的相对密度，即群落中个体分配上的均匀度Evenness #Shannon是估算样本中多样性的指数,值越大群落多样性越高 
  #Shannon指数通常用2、e作为指数，这里是2作为指数，如果用e作为指数就要变成 Shannon <- diversity(x, index = 'shannon', base = exp(1))
  #或者试试 Shannon <- diversity(Aspe,index="shannon")
  Simpson <- diversity(x, index = 'simpson')   #Simpson是估算样本中多样性的指数，Gini-Simpson指数随着丰富度的增加而增加；经典Simpson则是指数值越大，群落多样性越低 
  #注意，这里是常用的Simpson指数Gini-Simpson指数，如果是经典Simpson指数则需要在下边再加一个为 simpson_index <- 1 - Simpson，或者 simpson_index <- diversity(Aspe,index="inv")
  goods_Coverage <- 1 - rowSums(x == 1) / rowSums(x)  #Coverage：是指各样品文库的覆盖率，其数值越高，样本中序列没有被测出的概率越低。该指数反映了测序结果是否代表样本的真实情况。
  
  Shannon.Wiener <- diversity(x, index = "shannon")    #Shannon-Wiener指数计算
  Inverse.Simpson <- diversity(x, index = "inv")    #Inverse Simpson指数计算 另一种经典Simpson指数
  Specnumber <- specnumber(x)     #物种累计数（物种丰富度）计算，也可以是丰富度richness，即群落中丰度大于0的otu数量之和 richness <- herb.mat="">0)
  Pielou <- Shannon.Wiener/log(Specnumber)  #Pielou均匀度指数计算（Shannon比上log(specnumber)）
  simpson_index <- 1 - Simpson  #Simpson’s Index of Diversity
  
  #保留四位小数
  Shannon <- sprintf("%0.4f", Shannon)
  Simpson <- sprintf("%0.4f", Simpson)
  goods_Coverage <- sprintf("%0.4f", goods_Coverage)
  
  Shannon.Wiener <- sprintf("%0.4f", Shannon.Wiener)
  simpson_index <- sprintf("%0.4f", simpson_index)  
  Inverse.Simpson <- sprintf("%0.4f", Inverse.Simpson)
  Pielou <- sprintf("%0.4f", Pielou)
  
  result <- data.frame(observed_species, Chao1, Shannon, Simpson, Shannon.Wiener, Inverse.Simpson, Specnumber, simpson_index
                       , Pielou
  )
  
  
  
  
  
  if (!is.null(tree)) {
    PD_whole_tree <- pd(x, tree, include.root = TRUE)[1]     #这里是计算了谱系多样性（PD），include.root = FALSE不计算单一物种，include.root = TRUE能计算群落里只有一个物种时，但需要有根树
    names(PD_whole_tree) <- 'PD_whole_tree'
    result <- cbind(result, PD_whole_tree)
    
    result <- data.frame(observed_species,Chao1, Shannon, Simpson, 
                         PD_whole_tree, 
                         Shannon.Wiener, Inverse.Simpson, Specnumber, simpson_index
                         , Pielou
    )
  }
  
  
  result
}



#需要计算谱系多样性时，需要指定进化树文件
alpha_PD <- alpha_diversity (otu, tree)
alpha_PD
#做谱系多样性时，则输出
write.csv(alpha_PD,"Table S3.csv", quote = FALSE)


#做图
alpha_PD <- read.csv("Table S3.csv", header=T, row.names = 1)



#加了湖北

alpha_PD2 <- alpha_PD
alpha_PD2$Names <- rownames(alpha_PD2)
alpha_PD2

Names_alpha_PD2 <- alpha_PD2$Names
Names_alpha_PD2

Nature_reserve <- data.frame(Nature_reserve = c("Changbai_Mountain_Nature_Reserve_2018", "Changbai_Mountain_Nature_Reserve_2018", 
                                                "Changbai_Mountain_Nature_Reserve_2019", "Changbai_Mountain_Nature_Reserve_2019", 
                                                "Jianfengling_Nature_Reserve", "Jianfengling_Nature_Reserve", 
                                                "Shennongjia_Nature_Reserve"), 
                             Names = c(Names_alpha_PD2))
Nature_reserve

alpha_PD3 <- merge(alpha_PD2, Nature_reserve, by = "Names")
alpha_PD3

library(ggpubr)
#棒棒糖-谱系
p2_0_Hubei <- ggdotchart(alpha_PD3, x = "Names", y = "PD_whole_tree",
                         group = "Nature_reserve",
                         color = "Nature_reserve",                                # 按照cyl填充颜色
                         palette = c('#ABC6E4', '#C39398', '#FCDABA', '#A7D2BA'), # 修改颜色
                         sorting = "ascending",                        # 升序降序
                         add = "segments",                             # 添加棒子
                         add.params = list(color = '#D0CADE', size = 1.5),  #棒子参数
                         ggtheme = theme_pubr(),                        # 改变主题
                         xlab="",
                         shape = 16,                                    #糖形状
                         dot.size = 7                                  #糖大小
)
p2_0_Hubei
p2_Hubei <- p2_0_Hubei + theme(axis.text.x = element_text(
  #  family = "Times",
  family = "TT Times New Roman", 
  size = 8,angle = 45,vjust = 1)) +labs(x = NULL)+
  theme(legend.direction = "vertical")+                                     #将标签横过来
  theme(legend.text = element_text(size = 8,
                                   family = "TT Times New Roman" 
                                   #family = "Times"
  ), legend.title = element_text(size = 8,
                                 family = "TT Times New Roman"
                                 #family = "Times"
  ))
p2_Hubei 

Nature_reserve_palette = c('#ABC6E4', '#C39398', '#FCDABA', '#A7D2BA')

library(forcats)
#柱状-Inv.Simpson
p1_0_Hubei <- alpha_PD3 %>%
  mutate(Names = fct_reorder(Names, Nature_reserve)) %>%
  ggplot(
    mapping = aes(x = Names, y = Inverse.Simpson),scale = Nature_reserve)+
  geom_bar(stat = "boxplot",
           aes(fill = Nature_reserve))+
  scale_fill_manual(values = Nature_reserve_palette)+
  theme_classic()+
  xlab("")+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top")    # 将图例放置在底部
#  coord_flip()   #方向转为垂直
p1_0_Hubei
p1_Hubei <- p1_0_Hubei + theme(axis.text.x = element_text(family = "Times",size = 8,angle = 45,colour = "black"), 
                               panel.background = element_rect(fill = NA)) +labs(x = NULL)+
  theme(legend.direction = "vertical")+                                     #将标签横过来
  theme(legend.text = element_text(size = 8,
                                   family = "Times"), legend.title = element_text(size = 8,
                                                                                  family = "Times"))

p1_Hubei

library(cowplot)

p3_Hubei <- plot_grid(p1_Hubei,p2_Hubei)
p3_Hubei

pdf("p3_Hubei.pdf", width = 13, height = 13)
p3_Hubei
dev.off()




#===========================================================================================================
