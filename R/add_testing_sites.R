

add_ts_ped_map <- function()
{

library("magick")
#read map
map <- image_read(paste0("//c19links/COVID-19/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/jtf_figs/active_ped_map/active_ped_map_",Sys.Date(),".png"))%>%
                    image_scale("5000")
#read test sites
t_sites <- image_read("C:/Users/allison.plaxco/Desktop/testing sites 2.png")%>%
                image_scale("5000")

#put the test sites on top of the map
try_map <- image_composite(map, t_sites)

#save the combined image
magick::image_write(try_map, path = paste0("//c19links/COVID-19/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/jtf_figs/active_ped_map/active_pd_map_TS_",Sys.Date(),".png"))

}



add_ts_test_map <- function()
{

library("magick")
#read map
map <- image_read(paste0("//c19links/COVID-19/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/jtf_figs/test_map/test_map_",Sys.Date(),".png"))%>%
                image_scale("5000")
#read test sites
t_sites <- image_read("C:/Users/allison.plaxco/Desktop/testing sites 2.png")%>%
                                      image_scale("5000")

#put the test sites on top of the map
try_map <- image_composite(map, t_sites)

#save the combined image
magick::image_write(try_map, path = paste0("//c19links/COVID-19/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/jtf_figs/test_map/test_map_TS_",Sys.Date(),".png"))

}




add_ts_active_map <- function()
{

library("magick")
#read map
map <- image_read(paste0("//c19links/COVID-19/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/jtf_figs/active_case_map/active_case_map_",Sys.Date(),".png"))%>%
  image_scale("5000")
#read test sites
t_sites <- image_read("C:/Users/allison.plaxco/Desktop/testing sites 2.png")%>%
  image_scale("5000")

#put the test sites on top of the map
try_map <- image_composite(map, t_sites)

#save the combined image
magick::image_write(try_map, path = paste0("//c19links/COVID-19/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/jtf_figs/active_case_map/active_case_map_TS_",Sys.Date(),".png"))

}

