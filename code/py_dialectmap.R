setwd("C:/Users/JohnCarlyle/Documents/ProtoYue")

rm(list = ls())
library(ggplot2)
library(ggmap)
library(raster)
library(gridExtra)
library(ggrepel)
library(RColorBrewer)
library(scales)

CHN_adm0 <- getData('GADM', country='CHN', level=0)
CHN_adm1 <- getData('GADM', country='CHN', level=1)
CHN_adm2 <- getData('GADM', country='CHN', level=2)
CHN_adm3 <- getData('GADM', country='CHN', level=3)
HK0 <- getData('GADM', country='HK', level=0)
HK1 <- getData('GADM', country='HK', level=1)

# colors
reds <- brewer.pal(7, "OrRd")
blues <- brewer.pal(3, "PuBu")


####################################################
# Add additional data
# China
# Dialect
CHN_adm3$dialect <- rep(NA, nrow(CHN_adm3))
# Color
CHN_adm3$color <- rep(NA, nrow(CHN_adm3))

# Hong Kong
# Dialect
HK1$dialect <- rep(NA, nrow(HK1))
# Color
HK1$color <- rep(NA, nrow(HK1))

#Northern Delta Dialects
#Zhaoqing Sanyi
b_zs <- c("Gaoyao", "Zhaoqing", "Sihui", "Shunde", 
          "Gaoming", "Futian", "Nanhai")
CHN_adm3$dialect[CHN_adm3$NAME_3 %in% b_zs] <- "Zhaoqing Sanyi (Z-S)"
CHN_adm3$color[CHN_adm3$NAME_3 %in% b_zs] <- reds[2] 

#Inland
b_ndi <- c("Binyang", "Nanning", "Guangning", "Fengkai", 
           "Teng", "Huaiji", "Deqing", "He", "Lianshan", 
           "Lianzhou", "Yangshan", "Lianshan Zhuang and Yao", 
           "Liannan Yao", "Heng", "Yongning")
CHN_adm3$dialect[CHN_adm3$NAME_3 %in% b_ndi | 
                   CHN_adm3$NAME_2 == "Yunfu"] <- "N. Delta Inland (Inld)"
CHN_adm3$color[CHN_adm3$NAME_3 %in% b_ndi & 
                 CHN_adm3$NAME_2 == "Yunfu"] <- reds[5]

#S. Delta
#Zhongshan
b_zs <- c("Zhongshan", "Zhuhai")
CHN_adm3$dialect[CHN_adm3$NAME_3 %in% b_zs & 
                   CHN_adm3$NAME_1 == "Guangdong"] <- "Zhongshan (ZS)"
CHN_adm3$color[CHN_adm3$NAME_3 %in% b_zs & 
                 CHN_adm3$NAME_1 == "Guangdong"] <- reds[1]


#Guanlian
b_gl <- c("Dongguan", "Qinzhou", "Beihai")
CHN_adm3$dialect[CHN_adm3$NAME_2 %in% b_gl] <- "Guanlian (G-L)"
CHN_adm3$color[CHN_adm3$NAME_2 %in% b_gl] <- reds[2]

#Guanlian Inland for HK, NOTE: this uses admin1 for HK and his less columns
b_g_lhk <- c("North", "Sai Kung", "Sha Tin", "Tai Po", "Islands", 
             "Kwai Tsing", "Tsuen Wan", "Tuen Mun", "Yuen Long")
HK1$dialect[HK1$NAME_1 %in% b_g_lhk] <- "Guanlian Inland (G-L Inld)"
HK1$color[HK1$NAME_1 %in% b_g_lhk] <- reds[3]

#Guanlian Inland
b_sdi <- c("Huazhou", "Wuchuan", "Cenxi", "Lianjiang", "Shaoguan", 
           "Ruyuan Yao", "Lechang")
CHN_adm3$dialect[CHN_adm3$NAME_3 %in% b_sdi | CHN_adm3$NAME_2 == "Yulin"] <- 
  "Guanlian Inland (G-L Inld)"
CHN_adm3$color[CHN_adm3$NAME_3 %in% b_sdi | CHN_adm3$NAME_2 == "Yulin"] <- reds[3]

#Guangfu
#Guangfu Central
b_gfc <- c("Bao'an", "Shenzhen", "Sanshui")
CHN_adm3$dialect[CHN_adm3$NAME_3 %in% b_gfc | CHN_adm3$NAME_2 == "Guangzhou"] <- 
  "Guangfu (GF)"
CHN_adm3$color[CHN_adm3$NAME_3 %in% b_gfc | CHN_adm3$NAME_2 == "Guangzhou"] <- reds[3]

#Guangfu HK, see above, reverse of GL Inld for HK
b_gfhk <- c("North", "Sai Kung", "Sha Tin", "Tai Po", "Islands", 
            "Kwai Tsing", "Tsuen Wan", "Tuen Mun", "Yuen Long")
HK1$dialect[!(HK1$NAME_1 %in% b_gfhk)] <- 
  "Guangfu (GF)"
HK1$color[HK1$NAME_1 %in% b_gfhk] <- reds[7]

#GF Inland
b_gfi <- c("Maoming", "Gaozhou", "Xinyi", "Dianbai", "Cangwu", "Guiping", "Gui", 
           "Yingde", "Renhua", "Fogang", "Qujiang", "Wengyuan", 
           "Qingxin", "Qingcheng", "Wuzhou", "Pingnan")
CHN_adm3$dialect[CHN_adm3$NAME_3 %in% b_gfi] <- 
  "Guangfu Inland (GF Inld)"
CHN_adm3$color[CHN_adm3$NAME_3 %in% b_gfi] <- reds[6]

########################################################
#NOTE These are in a separate branch from the rest
#Wuyi Liangyang
#Wuyi
b_xe <- c("Taishan", "Enping", "Xinhui", "Doumen", "Jiangmen")
CHN_adm3$dialect[CHN_adm3$NAME_3 %in% b_xe] <- "Xin En (X-E)"
CHN_adm3$color[CHN_adm3$NAME_3 %in% b_xe] <- blues[3]

# Kai He 
b_kh <- c("Heshan", "Kaiping")
CHN_adm3$dialect[CHN_adm3$NAME_3 %in% b_kh & CHN_adm3$NAME_1 == "Guangdong"] <- 
  "Kai He (K-H)"
CHN_adm3$color[CHN_adm3$NAME_3 %in% b_kh & CHN_adm3$NAME_1 == "Guangdong"] <- 
  blues[2]

#Liangyang
CHN_adm3$dialect[CHN_adm3$NAME_2 == "Yangjiang"] <- "Liangyang (LY)"
CHN_adm3$color[CHN_adm3$NAME_2 == "Yangjiang"] <- blues[1]




######################################
# Data viz

# fortify data
# China
library(maptools)

# China outline
CHN_adm0 <- fortify(CHN_adm0)

# Province outline
CHN_adm1_f <- fortify(CHN_adm1[CHN_adm1@data$NAME_1 %in% 
                                 c("Guangdong", "Guangxi"),])
CHN_adm1@data$id <- as.character(CHN_adm1@data$ID_1)
CHN_adm1_f <- plyr::join(CHN_adm1_f, 
                         CHN_adm1@data[CHN_adm1@data$NAME_1 %in% 
                                         c("Guangdong", "Guangxi"),], by="id")

# China
CHN_adm3_f <- fortify(CHN_adm3[CHN_adm3@data$NAME_1 %in% 
                                 c("Guangdong", "Guangxi"),])
CHN_adm3@data$id <- as.character(CHN_adm3@data$ID_3)
CHN_adm3_f <- plyr::join(CHN_adm3_f, 
                         CHN_adm3@data[CHN_adm3@data$NAME_1 %in% 
                                         c("Guangdong", "Guangxi"),], by="id")
# Hong Kong
HK1_f <- fortify(HK1)
HK1@data$id <- as.character(HK1@data$ID_1)
HK1_f <- plyr::join(HK1_f, HK1@data, by="id")

# create dark and light
dialect <- unique(c(unique(CHN_adm3_f$dialect), 
                    unique(HK1$dialect)))
dialect_ab <- c(NA, 1:10)
shades <- ifelse(dialect %in% c("Xin En (X-E)", 
                                "Kai He (K-H)", 
                                "Liangyang (LY)"),
                 "dark", "light")
shades[is.na(dialect)] <- NA
meta_data <- data.frame(dialect, dialect_ab, shades)
meta_data <- meta_data[order(meta_data$shades),]
meta_data$fill_c <- c(hue_pal(l = 50)(15)[c(5, 8, 11)],
                      #"lightsalmon",
                      hue_pal(l = 75)(22)[c(1,17)], "coral2", "darkorange", #18
                      hue_pal(l = 85, c = 80)(22)[c(2, 20, 4)], NA)

show_col(hue_pal(l = 75)(22))

CHN_adm3_f <- plyr::join(CHN_adm3_f, meta_data, by="dialect")
HK1_f <- plyr::join(HK1_f, meta_data, by="dialect")

# province names
p_names <- aggregate(cbind(long, lat) ~ NAME_1, data = CHN_adm1_f, 
                     FUN=function(x)mean(range(x)))
p_names[1,2] <- 115.6
p_names[1,3] <- 23.64497
# cities
cities <- data.frame(city = c("Nanning", "Guangzhou", "Hong Kong"),
                     lat = c(22.8170, 23.1322, 22.3964),
                     long = c(108.3665, 113.2665, 114.1095))

china_cnames <- plyr::join(data.frame(aggregate(dialect_ab ~ NAME_3, 
                                                data = CHN_adm3_f, FUN=head, 1),
                                      aggregate(shades ~ NAME_3, 
                                                data = CHN_adm3_f, FUN=head, 1)), 
                           aggregate(cbind(long, lat) ~ NAME_3, data = CHN_adm3_f, 
                                     FUN=function(x)mean(range(x))))
HK_cnames <- plyr::join(data.frame(aggregate(dialect_ab ~ NAME_1, 
                                             data = HK1_f, FUN=head, 1),
                                   aggregate(shades ~ NAME_1, 
                                             data = HK1_f, FUN=head, 1)),
                        aggregate(cbind(long, lat) ~ NAME_1, 
                                  data = HK1_f, 
                                  FUN=function(x)mean(range(x))))

CHN_adm3_f$dialect <- factor(CHN_adm3_f$dialect, levels = meta_data$dialect)
HK1_f$dialect <- factor(HK1_f$dialect, levels = meta_data$dialect)


p1<-ggplot() +
  geom_polygon(data = CHN_adm0,
               aes(x = long, y = lat, group = group), color = "black", 
               size = 0.25, fill = NA) +
  geom_polygon(data = CHN_adm3_f[!is.na(CHN_adm3_f$dialect),],
               aes(x = long, y = lat, group = group, 
                   color = shades,
                   fill = dialect),
               size = 0.1, na.rm = TRUE) +
  geom_polygon(data = HK1_f,
               aes(x = long, y = lat, group = group, 
                   color = shades,
                   fill = dialect), size = 0.1, 
               na.rm = TRUE) +
  geom_polygon(data = CHN_adm1_f,
               aes(x = long, y = lat, group = group), color = "black", 
               size = 0.25, fill = NA) +
  geom_text(data = p_names, aes(x = long, y = lat, label = NAME_1),
            size = 5, fontface = 'bold') +
  geom_point(data = cities, aes(x = long, y = lat), size=2) + 
  geom_label(data = cities, aes(x = long, y = lat + 0.16, label = city), size=3, fontface = 'bold', fill='white')+
  # geom_text_repel(data = cities, aes(x = long, y = lat, label = city),
  #                 size = 3, force = 3, box.padding = unit(0.35, "lines")) +
  #geom_text(data = china_cnames,
  #          aes(x = long, y = lat, label = dialect_ab)) + 
  # geom_text(data = HK_cnames,
  #          aes(x = long, y = lat, label = dialect_ab)) +
  scale_fill_manual(values = meta_data$fill_c[1:10], na.value = NA,
                    name = "Branch") + 
  scale_color_manual(values = c("black", "black"), na.value = NA) + 
  theme_bw() + theme(legend.position = "bottom") +
  scale_x_continuous(breaks = NULL, name = NULL) +
  scale_y_continuous(breaks = NULL, name = NULL) +
  coord_cartesian(xlim = c(min(CHN_adm3_f$long)+0.5,
                           max(CHN_adm3_f$long)+0.1), 
                  ylim = c(min(CHN_adm3_f$lat)+0.01,
                           max(CHN_adm3_f$lat)+0.01)) +
  guides(colour=FALSE)

p1

ggsave("test_shading.pdf", width = 12, height = 8)

pol<-data.frame(xmin=104.5,xmax=118 ,ymin=19.9 ,ymax=26.5)

#Inset
p2 <- ggplot()+
  geom_polygon(data=CHN_adm0, aes(long,lat,group=group),colour="grey10",fill=NA)+
  coord_equal()+theme_bw()+labs(x=NULL,y=NULL)+
  geom_rect(data = pol, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha=0, colour="red", size = 1, linetype=1)+
  # geom_polygon(data=CHN_adm1[CHN_adm1@data$NAME_1 %in% c("Guangdong", "Guangxi"),], 
  #              aes(x=long, y=lat, group=group), colour = "grey10", fill = "grey10") +
  theme_bw() + theme(legend.position = "bottom") +
  scale_x_continuous(breaks = NULL, name = NULL) + 
  scale_y_continuous(breaks = NULL, name = NULL)
p2

# png(file='yue-dialect-map.png', width=1200, height=800)
pdf(file='yue-dialect-map.pdf', width=12, height=8)
grid.newpage()
v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2<-viewport(width = 0.2, height = 0.4, x = 0.86, y = 0.25) #plot area for the inset map
print(p1,vp=v1) 
print(p2,vp=v2)
dev.off()
