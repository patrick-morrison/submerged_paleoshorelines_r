library(readxl)
library(ggpubr)
library(raster)
library(marmap)
library(tidyverse)

land <- raster("rottnest_shelf.tif")
dims <- as.integer(min(dim(land)[1:2] / c(2000, 3000)))
dims <- ifelse(dims>=1, dims, 1) 
land <- land %>% aggregate(dims, progress='text') %>% rasterToPoints() %>% as_tibble()
colnames(land) <- c('x', 'y', 'z')

grant2012 <- read_excel('grant2012.xls', sheet = "(4) RSL", range = "E9:J1210")[-1,]

sealevel <- grant2012 %>% 
  select(
    age = Age...1,
    smooth = RSL_smooth
  ) %>% mutate_all(as.double) %>% filter(age<=125)

ci <- grant2012 %>% select(
  ci_age = Age...4,
  upper = `RSL_95%upper`,
  lower = `RSL_95%lower`
) %>% mutate_all(as.double) %>% filter(ci_age<=125)

years <- seq(0,
             125,
             by = .1) #in ka

sea_spline <- with(sealevel, smooth.spline(age, smooth, all.knots=TRUE))

predictions <- predict(sea_spline, years) %>% as_tibble() %>% dplyr::select('age' = x, "smooth"=y)

simulate <- function (row, z) {
  z-predictions$smooth[row]
}

for (time in 1:nrow(predictions)) {
  
  map <- ggplot(land, aes(x,y, fill=simulate(time, z))) +
    geom_raster() + scale_fill_etopo(guide = FALSE) +
    coord_equal() +
    labs(title=paste("Sea Level at", predictions$age[time]*1000, "BP"),
         x=NULL, y=NULL) +
    theme_minimal()
  
  graph <- ggplot() +
    geom_ribbon(data=ci, aes(x = ci_age, ymin=lower, ymax=upper),
                fill='lightgrey') +
    geom_line(data=predictions, aes(age,smooth),
              colour="blue4", size=1) + 
    geom_point(data=subset(predictions, age==predictions$age[time]),
               aes(age,smooth), size=3) +
    scale_x_reverse(n.breaks=12, expand = c(0, 0)) +
    scale_y_continuous(n.breaks=6, position = "right") +
    labs(title="", subtitle = "",
         x='Thousands of years ago',
         y='Sea level below present (m)')+
    theme_minimal() +
    geom_vline(xintercept=c(predictions$age[time]),
               linetype="dotted", size=.3)
  
  ggarrange(map, graph, 
            heights = c(2, 1),
            ncol = 1, nrow = 2) %>% return()
  
  ggsave(paste0(nrow(predictions)-time+1, "_rottnest_shelf.png"),
         height=9,
         width=9)
}
