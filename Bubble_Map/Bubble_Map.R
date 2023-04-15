library(sf)
library(dplyr)
library(readxl)
library(ggmap)
library(ggrepel)

data <- read_excel("D:/CityMap/Munchen/company_hub/code/company.xlsx", sheet = "fulllist")
data_sf <- st_as_sf(data, coords = c("lng", "lat"), crs = 4326)
data_sf_manufacture <- data %>% filter(Category %in% c("Manufacture", "Property"))

bbx <- st_bbox(data_sf)
mymap_halifax <- get_stamenmap(bbox = c(left = 11.27, bottom = 48.02, right = 11.8, top =48.28644), 
                               zoom=12, maptype = "toner-lite")


theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Noto-Sans", color = "#22211d"),
      # remove all axes
      axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      
      # background colors
      plot.background = element_rect(fill = "white",
                                     color = NA),
      panel.background = element_rect(fill = "white",
                                      color = NA),
      legend.background = element_rect(fill = "white",
                                       color = NA),
      # legend
      legend.position = c(.95, .2),
      legend.text = element_text( size = 25),
      legend.title  = element_text( size = 30),
      legend.spacing.y = unit(0, 'cm'),
      legend.spacing.x = unit(0, 'cm'),
      # title
      plot.title = element_text(size = 50, hjust = 0.5,
                                color = "#4e4d47"
      ),
      plot.subtitle = element_text(size = 30, hjust = 0.5,
                                   color = "#4e4d47",
                                   
                                   debug = F),
      # captions
      plot.caption = element_text(size = 25,
                                  hjust = .5,
                                  margin = margin(t = 0.2,
                                                  b = 0,
                                                  unit = "cm"),
                                  color = "#939184")
    )
}

library (showtext)
# in map template above we use font Noto Sans
font_add_google(name = "Noto Sans", family = "Noto-Sans") 
# use showtext to render the plot
showtext_auto()

ggmap(mymap_halifax) +
  geom_point(data = data_sf_manufacture,aes(x = lng, y = lat, size = Scale),color="red",alpha=.6)+
  geom_point(data = subset(data_sf_manufacture, Scale==7),aes(x = lng, y = lat,size = Scale),
             color="yellow", show.legend = F, stroke = 1, shape=21)+
  scale_size_continuous(range = c(2, 6), labels = c("<= 10M", "10-50M","50-100M", "100-500M","500M-1B", "1-10B",">10B"))+
  labs(title="TOP COMPANIES IN MANUFACTURING",
       size = "Revenue ($)",
       caption ="Data Source: ZoomInfo|city of Munchen Department of Labor and Economic Development")+
  geom_label_repel(data=subset(data_sf_manufacture,Scale==7),
                   aes(x = lng, y = lat,label=name),
                   size = 8,
                   family = "Noto Sans",
                   segment.curvature = -0.1,
                   segment.ncp = 3,
                   segment.angle = 20)+
  theme_map()


ggsave(
  filename = "D:/CityMap/Munchen/company_hub/manufacture.png",
  width = 5, height = 4, dpi = 500,
  device = "png"
)
