#' Add testing sites to peds map
#'
#' @return A saved png object for peds
#'
#' @export
add_ts_ped_map <- function(date = NULL)
{

  date <- date_inv(date)

library("magick")
#read map
map <- image_read(paste0("//c19links/COVID-19/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/jtf_figs/active_ped_map/active_ped_map_",date,".png"))%>%
                    image_scale("5000")
#read test sites
#t_sites <- image_read("C:/Users/allison.plaxco/Desktop/testing sites 2.png")%>%
#                image_scale("5000")

#put the test sites on top of the map
#try_map <- image_composite(map, t_sites)

#save the combined image
#magick::image_write(try_map, path = paste0("//c19links/COVID-19/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/jtf_figs/active_ped_map/active_pd_map_TS_",date,".png"))

}


#' Add testing sites to test map
#'
#' @return A saved png object for tests
#'
#' @export
add_ts_test_map <- function(date = NULL)
{

  date <- date_inv(date)

library("magick")
#read map
map <- image_read(paste0("//c19links/COVID-19/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/jtf_figs/test_map/test_map_",date,".png"))%>%
                image_scale("5000")
#read test sites
#t_sites <- image_read("C:/Users/allison.plaxco/Desktop/testing sites 2.png")%>%
#                                      image_scale("5000")

#put the test sites on top of the map
#try_map <- image_composite(map, t_sites)

#save the combined image
#magick::image_write(try_map, path = paste0("//c19links/COVID-19/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/jtf_figs/test_map/test_map_TS_",date,".png"))

}



#' Add testing sites to grant test map
#'
#' @return A saved png object for tests
#'
#' @export
add_ts_grant_test_map <- function(date = NULL)
{

  date <- date_inv(date)

  library("magick")
  #read map
  map <- image_read(paste0("//c19links/COVID-19/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/jtf_figs/test_map/grant_test_map_",date,".png"))%>%
    image_scale("5000")
  #read test sites
  #t_sites <- image_read("C:/Users/allison.plaxco/Desktop/testing sites 2.png")%>%
  #  image_scale("5000")

  #put the test sites on top of the map
  #try_map <- image_composite(map, t_sites)

  #save the combined image
  #magick::image_write(try_map, path = paste0("//c19links/COVID-19/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/jtf_figs/test_map/grant_test_map_TS_",date,".png"))

}



#' Add testing sites to active map
#'
#' @return A saved png object for active
#'
#' @export
add_ts_active_map <- function(date = NULL)
{
  date <- date_inv(date)

library("magick")
#read map
map <- image_read(paste0("//c19links/COVID-19/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/jtf_figs/active_case_map/active_case_map_",date,".png"))%>%
  image_scale("5000")
#read test sites
# t_sites <- image_read("C:/Users/allison.plaxco/Desktop/testing sites 2.png")%>%
#   image_scale("5000")

#put the test sites on top of the map
# try_map <- image_composite(map, t_sites)
#
# #save the combined image
# magick::image_write(try_map, path = paste0("//c19links/COVID-19/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/jtf_figs/active_case_map/active_case_map_TS_",date,".png"))

}


#' Add testing sites to grant active map
#'
#' @return A saved png object for active
#'
#' @export
add_ts_grant_active_map <- function(date = NULL)
{
  date <- date_inv(date)

  library("magick")
  #read map
  map <- image_read(paste0("//c19links/COVID-19/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/jtf_figs/active_case_map/grant_active_case_map_",date,".png"))%>%
    image_scale("5000")
  #read test sites
  # t_sites <- image_read("C:/Users/allison.plaxco/Desktop/testing sites 2.png")%>%
  #   image_scale("5000")
  #
  # #put the test sites on top of the map
  # try_map <- image_composite(map, t_sites)
  #
  # #save the combined image
  # magick::image_write(try_map, path = paste0("//c19links/COVID-19/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/jtf_figs/active_case_map/grant_active_case_map_TS_",date,".png"))

}

