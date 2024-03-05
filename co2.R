install.packages("C:\\Users\\sarat\\Downloads\\exactextractr_0.10.0.tar.gz", repos = NULL, type = "source")


libs<-c(
  "tidyverse",
  "geodata",
  "terra",
  "exactextractr",
  "sf","classInt"
)

installed_libs<-libs %in% rownames(
  installed.packages()
)

if(any(installed_libs==F)){
  install.packages(
    libs[!installed_libs],
    dependencies = T
    
  )
}
invisible(
  lapply(
    libs,library,
    character.only=T
  )
)
url<-"https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2020_GLOBE_R2023A_4326_30ss/V1-0/GHS_POP_E2020_GLOBE_R2023A_4326_30ss_V1_0.zip"
file_name<-basename(url)

# Function to download file if it does not exist
download_if_not_exists <- function(url, destfile) {
  if (!file.exists(destfile)) {
    download.file(url = url, destfile = destfile)
  } else {
    message(paste("File", destfile, "already exists. Skipping download."))
  }
}

# Download Population Data
url <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2020_GLOBE_R2023A_4326_30ss/V1-0/GHS_POP_E2020_GLOBE_R2023A_4326_30ss_V1_0.zip"
file_name <- basename(url)

download_if_not_exists(url, file_name)
unzip(file_name)

raster_name <- gsub(".zip", ".tif", file_name)
pop <- terra::rast(raster_name)

country<-geodata::gadm(
  country = "LK",
  level = 2,
  path = getwd()
)|>
  sf::st_as_sf()

country$population<-exactextractr::exact_extract(
  pop,
  country,
  "sum"
)

u<-"https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v80_FT2022_GHG/CO2/TOTALS/emi_nc/v8.0_FT2022_GHG_CO2_2022_TOTALS_emi_nc.zip"
download.file(
  url = u,
  path=getwd(),
  destfile = basename(u)
)
unzip(basename(u))
co2 <- terra::rast("v8.0_FT2022_GHG_CO2_2022_TOTALS_emi.nc")

country$sum_co2<-exactextractr::exact_extract(
  co2,
  country,
  "sum"
)
country$co2_pc<-country$sum_co2/country$population

theme_for_the_win<-function(){
  theme_void()+
    theme(
      legend.position = "top",
      legend.title = element_text(
        size = 9,colour = "grey20"
      ),
      legend.text = element_text(
        size = 7,colour = "grey20"
      ),
      plot.margin = unit(
       c(
         t=1,r=0,
         b=0,l=0
       ),"lines"
      )
    )
}

cols<-hcl.colors(
  5,"Inferno",
  rev=T
)

pal<-colorRampPalette(
  cols
)(64)

breaks<-classInt::classIntervals(
  country$co2_pc,
  n=6,
  style="equal"
)$brks

map <- ggplot() +
  geom_sf(
    data = country,
    aes(fill = co2_pc),
    color = "white",
    size = 0.15
  ) +
  scale_fill_gradientn(
    name = "",  # Remove legend title here
    colors = pal,
    breaks = round(breaks, 3),
    labels = round(breaks, 3),
    na.value = "white"
  ) +
  guides(
    fill = guide_colorbar(
      title = "Tonnes Per Capita",
      direction = "horizontal",
      barwidth = 12,
      barheight = 0.5
    )
  ) +
  theme_for_the_win() +
  theme(
    legend.position = "top",
    legend.title = element_text(margin = margin(r = 20)),  # Adjust the margin to move title left
    legend.box.margin = margin(0, 0, 0, 10)
  )

ggsave(
  "de_lvl2_co2.png",
  map,
  width = 6,
  height = 8,
  units = "in",
  bg = "white"
)
















