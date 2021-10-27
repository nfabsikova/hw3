library(sf)
library(tidyverse)
library(janitor)
library(raster)

# check out the layers
st_layers("data/countries/gadm36_CZE.gpkg")

# read in country outlines
sk <- st_read("data/countries/gadm36_SVK.gpkg",
              layer='gadm36_SVK_0')
cz <- st_read("data/countries/gadm36_CZE.gpkg", 
              layer='gadm36_CZE_0')

# check the outlines
sk %>% st_geometry() %>% plot()
cz %>% st_geometry() %>% plot()

# join cz and sk
czsk <- rbind(cz, sk)
czsk %>% st_geometry() %>% plot()

# load cities
cities <- st_read("data/cities/World_Cities.shp") %>% 
  clean_names() %>%
  filter(cntry_name %in% c("Slovakia", "Czech Republic"))

# plot cities
cities %>% st_geometry() %>% plot()

# load climate data
ssp1 <- brick("data/clim/wc2.1_2.5m_tmax_BCC-CSM2-MR_ssp126_2081-2100.tif")
ssp5 <- brick("data/clim/wc2.1_2.5m_tmax_BCC-CSM2-MR_ssp585_2081-2100.tif")

# crop rasters
ssp1_czsk <- ssp1 %>% 
  crop(., czsk) %>% 
  mask(., czsk)

ssp5_czsk <- ssp5 %>% 
  crop(., czsk) %>% 
  mask(., czsk)

# calculate difference between ssp1 and ssp5
ssp_diff <- ssp5_czsk - ssp1_czsk

# rename layers
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

names(ssp_diff) <- months
plot(ssp_diff)

# extract temperatures for cities
city_ssp_diff <- raster::extract(ssp_diff, cities) %>%
  as_tibble() %>% 
  add_column(name = cities$city_name, .before = 'Jan')

# make tidy
city_diff_tidy <- city_ssp_diff %>% 
  pivot_longer(.,
               -name,
               names_to = 'month',
               values_to = 'temp_diff') %>%
  mutate(month = factor(month, levels = months))

# make faceted histogram
ggplot(city_diff_tidy, aes(x=temp_diff, na.rm = TRUE)) +
  geom_histogram(color="black", binwidth = 1) +
  labs(title= "Histogram of temperature differnce between ssp1 and ssp5 in major CZSK cities",
       x = "Temperature difference",
       y = "Frequency") + 
  facet_grid(month ~ .) +
  theme(plot.title = element_text(hjust=0.5)) +
  theme_minimal()

# make barplots for cities
ggplot(city_diff_tidy, aes(x=month, y=temp_diff)) +
  geom_bar(stat='identity') +
  facet_grid(name ~ .)

# it's working

# i would put some other kind of plot in here
  