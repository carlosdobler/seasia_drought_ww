
# source("~/00-mount.R")

library(tidyverse)
library(lubridate)
library(tictoc)
library(furrr)
library(stars)
library(units)

# sf_use_s2(F)

options(future.fork.enable = T)



# LAND MASK

c(st_point(c(87, -15)),
  st_point(c(149, 29))) %>%
  st_bbox() %>%
  st_set_crs(4326) -> box_reference

box_reference %>%
  st_as_stars(dx = 0.05, dy = 0.05, values = -9999) -> rast_reference_0.05

box_reference %>%
  st_as_stars(dx = 0.2, dy = 0.2, values = -9999) -> rast_reference_0.2

"~/bucket_mine/misc_data/ne_50m_land/ne_50m_land.shp" %>%
  st_read(quiet = T) %>%
  mutate(a = 1) %>%
  select(a) %>%
  st_rasterize(rast_reference_0.05) -> land

land %>%
  st_warp(rast_reference_0.2, use_gdal = T, method = "mode") %>%
  suppressWarnings() %>% 
  setNames("a") %>%
  mutate(a = ifelse(a == -9999, NA, 1)) -> land

land %>%
  st_set_dimensions(c(1,2), names = c("lon", "lat")) -> land



"~/bucket_mine/results/global_spei_ww/new_derived/" %>% 
  list.files(full.names = T) %>% 
  .[str_detect(., "SEA")] %>% 
  .[str_detect(., "REMO")] %>% 
  .[str_detect(., "spei-12")] %>% 
  .[str_detect(., "probability")] %>% 
  .[str_detect(., "D3")] %>% 
  .[1] %>% 
  read_ncdf %>% 
  st_dimensions() -> foo

land %>% 
  st_warp(foo) -> land





mods <- c("HadGEM2-ES", "MPI-ESM-LR", "NorESM1-M")
wls <- c("1.0C", "2.0C", "3.0C")




# EXTREME DROUGHTS

map(wls, function(wl){
  
  "~/bucket_mine/results/global_spei_ww/new_derived/" %>% 
    list.files(full.names = T) %>% 
    .[str_detect(., "SEA")] %>% 
    .[str_detect(., "REMO")] %>% 
    .[str_detect(., "spei-12")] %>% 
    .[str_detect(., "probability")] %>% 
    .[str_detect(., "D3")] %>% 
    .[str_detect(., wl)] %>% 
    map(read_ncdf) -> l_s
  
  l_s %>% 
    map(function(s){
      
      # % more likely
      s %>% 
        mutate(prob = (prob - (1-pnorm(1.6))) * 100) -> s
      
      s[is.na(land)] <- NA
      
      return(s)
      
      # Xx
      # s %>% 
      #   mutate(prob = prob/(1-pnorm(1.6))) #%>% mapview::mapview()
      
      # 1 in X
      # baseline = 1 in 18
      # s %>% 
      #   mutate(prob = ifelse(prob == 0, 0.000001, prob)) %>% 
      #   mutate(prob = 1/prob) #%>% mapview::mapview(at = c(seq(0,5), Inf))
      
    }) -> l_s_delta
  
  l_s_delta %>% 
    do.call(c, .) %>% 
    merge() %>% 
    st_apply(c(1,2), mean) %>% 
    setNames(wl)
  
}) -> l_s_delta

l_s_delta %>% 
  do.call(c, .) %>% 
  merge(name = "wl") -> tb_1

write_stars(tb_1, "~/bucket_mine/results/seasia_drought_ww/freq_extreme_drought.tif")
  
tb_1 %>% 
  as_tibble() %>% 
  mutate(X = ifelse(X > 40, 40, X)) %>% 
  
  ggplot(aes(lon, lat, fill = X)) +
  geom_raster() +
  facet_wrap(~wl, nrow = 1) +
  colorspace::scale_fill_continuous_sequential("Plasma",
                                               rev = F,
                                               na.value = "transparent",
                                               name = " ",
                                               breaks = c(0,20,40),
                                               labels = c("0%", "20%", "+40%")) +
  coord_equal() +
  labs(#title = "How more likely will extreme droughts be in SE Asia?",
       title = "Change in likelihood of extreme drought (5% chance event)",
       subtitle = "Baseline = 1971-2000 (0.5C)",
       caption = "Data: RCM REMO2015 ensemble") +
  theme(axis.title = element_blank(),
        legend.position="bottom") +
  guides(fill = guide_colourbar(barwidth = 12, barheight = 0.8))






# 


map(wls, function(wl){
  
  "~/bucket_mine/results/global_spei_ww/new_derived/" %>% 
    list.files(full.names = T) %>% 
    .[str_detect(., "SEA")] %>% 
    .[str_detect(., "REMO")] %>% 
    .[str_detect(., "spei-12")] %>% 
    .[str_detect(., "mean")] %>%
    .[str_detect(., wl)] %>% 
    map(read_ncdf) -> l_s
  
  l_s %>% 
    map(function(s){
      
      s %>% 
        mutate(prob = (pnorm(prob)-0.5)*100) -> s
      
      s[is.na(land)] <- NA
      
      return(s)
      
    }) -> l_s_delta
  
  l_s_delta %>% 
    do.call(c, .) %>% 
    merge() %>% 
    st_apply(c(1,2), mean) %>% 
    setNames(wl)
  
}) -> l_s_delta

l_s_delta %>% 
  do.call(c, .) %>% 
  merge(name = "wl") -> tb_2

write_stars(tb_2, "~/bucket_mine/results/seasia_drought_ww/hydro_conditions.tif")


tb_2 %>% 
  as_tibble() %>% 
  mutate(X = case_when(X > 40 ~ 40,
                       X < -40 ~ -40,
                       TRUE ~ X)) %>% 
  ggplot(aes(lon, lat, fill = X)) +
  geom_raster() +
  facet_wrap(~wl, nrow = 1) +
  colorspace::scale_fill_continuous_divergingx("Spectral",
                                               rev = F,
                                               na.value = "transparent",
                                               name = " ",
                                               breaks = c(-40, -20, 0, 20, 40),
                                               labels = c("+40%\ndrier", "20%\ndrier", "no\nchange", "20%\nwetter", "+40%\nwetter")
                                              ) +
  coord_equal() +
  labs(title = "Change in likelihood of experiencing a *drier* or *wetter* year",
       subtitle = "Baseline = 1971-2000 (0.5C)",
       caption = "Data: RCM REMO2015 ensemble") +
  theme(axis.title = element_blank(),
        legend.position="bottom") +
  guides(fill = guide_colourbar(barwidth = 12, barheight = 0.8))


