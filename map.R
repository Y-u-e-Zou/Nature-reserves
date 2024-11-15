#=================================================================================================
#地图
library(sf)
# 获取中国地图数据（来源：https://datav.aliyun.com/portal/school/atlas/area_selector#&lat=31.769817845138945&lng=104.29901249999999&zoom=4）
China_map <- st_read("中华人民共和国.json")



library(tidyverse)
#提取三省数据
Jilin_map <- China_map %>%
  filter(name == "吉林省")
Hubei_map <- China_map %>%
  filter(name == "湖北省")
Hainan_map <- China_map %>%
  filter(name == "海南省")
JiHuHai_map <- China_map %>%
  filter(name %in% c("吉林省", "湖北省", "海南省"))



# 绘制球体地图


stptp<- read.table("Table S6.xls", header=T, sep="\t", comment.char="", stringsAsFactors = F)
scatter_df_tro <- st_as_sf(stptp,coords = c("Longitude", "Latitude"),crs = 4326)


library(ggplot2)
library(ggspatial)
ggplot() + 
  geom_sf(data = China_map, fill="NA", size=0.05, color="#010101") + 
  geom_sf(data = Jilin_map, fill="NA", size=0.5, color="#F0A73A") + 
  geom_sf(data = Hubei_map, fill="NA", size=0.5, color="#3ABF99") + 
  geom_sf(data = Hainan_map, fill="NA", size=0.5, color="#2C91E0") + 
  geom_sf(data = scatter_df_tro, aes(fill = stptp$"SampleID"),shape = 21, colour = '#383838', stroke = 0.005, size = 2.5)+    #colour是圈边缘色stroke是圈边厚度size是圈大小
  coord_sf(crs = "+proj=laea +lat_0=40 +lon_0=104")+
  scale_fill_manual(values = c('#C6DCB9', '#B6E2DC', '#B8FABF', '#98F4E0', '#8DECF5', 
                               '#8FDBF3', '#A2C4F1', '#CEBAF0', '#C6C3E1', '#EEC2E5', 
                               '#FFCFD1', '#FBE3CD', '#F9F9CA', '#FAA09C', '#F9B29C', 
                               '#F9C89B', '#FBDF9D', '#E9E4AF', '#C6DC89', '#B6E2DC'))+         #圈内填色
  annotation_scale(location = "bl") +                                  #bl是bottom底部left左侧，就是比例尺位置
  #  annotation_north_arrow(location = "tl", which_north = "false",
  #                         style = north_arrow_nautical)+        #指北针
  theme_linedraw()+
  theme(text = element_text( size = 12, face = "bold"),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "#808080"),         #经纬线
        axis.line = element_line(linewidth = 1, colour = "#010101"),     #地图外边框
        panel.ontop = FALSE
  )
ggsave("china_map2.pdf", device = pdf, width = 10, height = 10, dpi = 300)
#=================================================================================================
library(sf)
# 获取中国地图数据（来源：https://datav.aliyun.com/portal/school/atlas/area_selector#&lat=31.769817845138945&lng=104.29901249999999&zoom=4）
China_map <- st_read("中华人民共和国.json")



library(tidyverse)
#提取三省数据
Jilin_map <- China_map %>%
  filter(name == "吉林省")
Hubei_map <- China_map %>%
  filter(name == "湖北省")
Hainan_map <- China_map %>%
  filter(name == "海南省")
JiHuHai_map <- China_map %>%
  filter(name %in% c("吉林省", "湖北省", "海南省"))

# 绘制平面地图
#ggplot() +
#  geom_sf(data = China_map, fill = "#C6C2DF", color = "#338FEB") +
#  theme_minimal() +
#  labs(title = "中国地图")

# 保存地图
#ggsave("china_map.pdf", device = pdf, width = 10, height = 6, dpi = 300)

# 绘制球体地图


stptp<- read.table("Table S6.txt", header=T, sep="\t", comment.char="", stringsAsFactors = F)
Changbai <- c("Y2018", "K2018", "H2018", "Z2018", "Y2019", "K2019", "H2019", "Z2019")
Shennongjia <- c("BF", "BN", "BC", "BH", "BZ")
Jianfengling <- c("HY", "HBA", "HBI", "HS", "HN", "HC", "HH")

stptp_Changbai <- stptp[stptp$SampleID %in% Changbai, ]
stptp_Shennongjia <- stptp[stptp$SampleID %in% Shennongjia, ]
stptp_Jianfengling <- stptp[stptp$SampleID %in% Jianfengling, ]

scatter_df_tro_Changbai <- st_as_sf(stptp_Changbai,coords = c("Longitude", "Latitude"),crs = 4326)
scatter_df_tro_Shennongjia <- st_as_sf(stptp_Shennongjia,coords = c("Longitude", "Latitude"),crs = 4326)
scatter_df_tro_Jianfengling <- st_as_sf(stptp_Jianfengling,coords = c("Longitude", "Latitude"),crs = 4326)


library(ggplot2)
library(ggspatial)
library(ggforce)
ggplot() + 
  #geom_sf(data = China_map, fill="NA", size=0.05, color="#010101") + 
  geom_sf(data = Jilin_map, fill="NA", size=0.5, color="#F0A73A") + 
  #geom_sf(data = Hubei_map, fill="NA", size=0.5, color="#3ABF99") + 
  #geom_sf(data = Hainan_map, fill="NA", size=0.5, color="#2C91E0") + 
  geom_sf(data = scatter_df_tro_Changbai, aes(fill = stptp_Changbai$"SampleID"),shape = 21, colour = '#383838', stroke = 0.005, size = 7)+    #colour是圈边缘色stroke是圈边厚度size是圈大小
  coord_sf(crs = "+proj=laea +lat_0=40 +lon_0=104")+
  scale_fill_manual(values = c(#'#C6DCB9', '#B6E2DC', '#B8FABF', '#98F4E0', '#8DECF5'
    #, '#CEBAF0', '#C6C3E1', '#EEC2E5', '#FFCFD1', '#FBE3CD', '#F9F9CA', '#FAA09C', 
    '#8FDBF3', '#A2C4F1', '#F9B29C', '#F9C89B', '#FBDF9D', '#E9E4AF', '#C6DC89', '#B6E2DC'
  ))+         #圈内填色
  annotation_scale(location = "bl") +                                  #bl是bottom底部left左侧，就是比例尺位置
  #  annotation_north_arrow(location = "tl", which_north = "false",
  #                         style = north_arrow_nautical)+        #指北针
  theme_linedraw()+
  theme(text = element_text( size = 12, face = "bold"),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "#808080"),         #经纬线
        axis.line = element_line(linewidth = 1, colour = "#010101"),     #地图外边框
        panel.ontop = FALSE
  ) +
  coord_sf(xlim = c(127.7, 128.5),  # 设置x轴的显示范围
           ylim = c(42, 42.5),       # 设置y轴的显示范围
           expand = FALSE)
ggsave("china_Changbai.pdf", device = pdf, width = 10, height = 10, dpi = 300)

ggplot() + 
  #geom_sf(data = China_map, fill="NA", size=0.05, color="#010101") + 
  #geom_sf(data = Jilin_map, fill="NA", size=0.5, color="#F0A73A") + 
  geom_sf(data = Hubei_map, fill="NA", size=0.5, color="#3ABF99") + 
  #geom_sf(data = Hainan_map, fill="NA", size=0.5, color="#2C91E0") + 
  geom_sf(data = scatter_df_tro_Shennongjia, aes(fill = stptp_Shennongjia$"SampleID"),shape = 21, colour = '#383838', stroke = 0.005, size = 7)+    #colour是圈边缘色stroke是圈边厚度size是圈大小
  coord_sf(crs = "+proj=laea +lat_0=40 +lon_0=104")+
  scale_fill_manual(values = c('#C6DCB9', '#B6E2DC', '#B8FABF', '#98F4E0', '#8DECF5'
                               #, '#CEBAF0', '#C6C3E1', '#EEC2E5', '#FFCFD1', '#FBE3CD', '#F9F9CA', '#FAA09C'
                               #, '#8FDBF3', '#A2C4F1', '#F9B29C', '#F9C89B', '#FBDF9D', '#E9E4AF', '#C6DC89', '#B6E2DC'
  ))+         #圈内填色
  annotation_scale(location = "bl") +                                  #bl是bottom底部left左侧，就是比例尺位置
  #  annotation_north_arrow(location = "tl", which_north = "false",
  #                         style = north_arrow_nautical)+        #指北针
  theme_linedraw()+
  theme(text = element_text( size = 12, face = "bold"),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "#808080"),         #经纬线
        axis.line = element_line(linewidth = 1, colour = "#010101"),     #地图外边框
        panel.ontop = FALSE
  ) +
  coord_sf(xlim = c(109.5, 111.1),  # 设置x轴的显示范围
           ylim = c(31, 32),       # 设置y轴的显示范围
           expand = FALSE)
ggsave("china_Shennongjia.pdf", device = pdf, width = 10, height = 10, dpi = 300)

ggplot() + 
  #geom_sf(data = China_map, fill="NA", size=0.05, color="#010101") + 
  #geom_sf(data = Jilin_map, fill="NA", size=0.5, color="#F0A73A") + 
  #geom_sf(data = Hubei_map, fill="NA", size=0.5, color="#3ABF99") + 
  geom_sf(data = Hainan_map, fill="NA", size=0.5, color="#2C91E0") + 
  geom_sf(data = scatter_df_tro_Jianfengling, aes(fill = stptp_Jianfengling$"SampleID"),shape = 21, colour = '#383838', stroke = 0.005, size = 7)+    #colour是圈边缘色stroke是圈边厚度size是圈大小
  coord_sf(crs = "+proj=laea +lat_0=40 +lon_0=104")+
  scale_fill_manual(values = c(#'#C6DCB9', '#B6E2DC', '#B8FABF', '#98F4E0', '#8DECF5', 
    '#CEBAF0', '#C6C3E1', '#EEC2E5', '#FFCFD1', '#FBE3CD', '#F9F9CA', '#FAA09C'
    #, '#8FDBF3', '#A2C4F1', '#F9B29C', '#F9C89B', '#FBDF9D', '#E9E4AF', '#C6DC89', '#B6E2DC'
  ))+         #圈内填色
  annotation_scale(location = "bl") +                                  #bl是bottom底部left左侧，就是比例尺位置
  #  annotation_north_arrow(location = "tl", which_north = "false",
  #                         style = north_arrow_nautical)+        #指北针
  theme_linedraw()+
  theme(text = element_text( size = 12, face = "bold"),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "#808080"),         #经纬线
        axis.line = element_line(linewidth = 1, colour = "#010101"),     #地图外边框
        panel.ontop = FALSE
  ) +
  coord_sf(xlim = c(108.6, 109),  # 设置x轴的显示范围
           ylim = c(18.55, 18.85),       # 设置y轴的显示范围
           expand = FALSE)
#coord_sf(xlim = c(108.5, 109.5), ylim = c(18, 19))
ggsave("china_Jianfengling.pdf", device = pdf, width = 10, height = 10, dpi = 300)
