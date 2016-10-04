
library(pacman)
p_load(plyr, dplyr, leaflet, stringi, htmltools, RColorBrewer, rvest)

# Parse and read storm track data.
html <- read_html('http://weather.unisys.com/hurricane/atlantic/2016/index.php')
links <- html_attr(html_nodes(html, "a"), "href")
links <- links[grep('track.dat', links)]

track <- select.list(links, title="Select storm:", graphics = FALSE)
#track <- "HERMINE/track.dat"

url <- paste("http://weather.unisys.com/hurricane/atlantic/2016", track, sep="/")

storm <- readLines(url)

# Storm scale
ss <-  c("Tropical Depression", "Tropical Storm", "Hurricane-1", "Hurricane-2", "Hurricane-3", "Hurricane-4", "Hurricane-5")
pal <- colorRampPalette(c("blue", "green", "yellow", "orange", "red", "darkred", "black"))(length(ss))

storm_dat <- read.table(textConnection(gsub("TROPICAL ", "TROPICAL_", storm[3:length(storm)])), 
                        header=TRUE, stringsAsFactors=FALSE) %>%
  mutate(STAT = stri_trans_totitle(gsub("_", " ", STAT))) %>%
  setNames(tolower(names(.))) %>%
  # Make colnames pretty.
  rename(advisory=adv, wind_speed=wind, pressure=pr, status=stat) %>%
  # Set color levels for status.
  mutate(color = as.character(factor(status, levels = ss, labels = pal))) %>%
  # Set date to proper date/time.
  mutate(date = strftime(strptime(time, format="%m/%d/%H"), '%m/%d %Hh')) %>%
  # Separate complete and intermediate advisories (assuming they come in pairs - TODO)
  mutate(advisory = gsub("A", "", advisory)) %>%
  # Make windspeeds useful for point sizes
  mutate(wind_speed = as.integer(wind_speed)) %>%
  mutate(wind_speed = ifelse(is.na(wind_speed), mean(wind_speed, na.rm = T), wind_speed)) %>%
  mutate(opacity = 0.5)

# Lighten past position colors.
storm_dat$opacity[strptime(storm_dat$time, format="%m/%d/%H") <= Sys.time()] <- 0.1

# Make storm dataframe for mapping.
storm <- plyr::ddply(storm_dat, "advisory", head, 1) %>%
  arrange(date) %>%
  mutate(time = as.POSIXct(gsub("Z", "", time), format='%m/%d/%H', tz="UTC")) %>%
  mutate(localtime = as.POSIXct(format(time, tz=Sys.timezone(), usetz = TRUE))) %>%
  mutate(date = paste(weekdays(localtime, abbreviate = TRUE), format(localtime, "%H:%M"), " "))

# Create leaflet map.
leaflet(storm) %>%
  addTiles() %>%
  addWMSTiles(
    "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
    layers = "nexrad-n0r-900913",
    options = WMSTileOptions(format = "image/png", transparent = TRUE)
  ) %>%
  addPolylines(~lon, ~lat, color = 'grey', weight=3) %>%
  addCircleMarkers(~lon, ~lat, radius = ~wind_speed / 3, stroke = TRUE, color = 'grey', 
                   opacity = 1, weight = 2, fill = true, fillColor = ~color, 
                   fillOpacity = ~opacity,
                   popup = ~sprintf("<b>Advisory forecast %s (%s)</b><hr noshade size='1'/>
                                    Local time: %s<br/>
                                    Position: %3.2f, %3.2f<br/>
                                    Strength: <strong>%s</strong><br/>
                                    Wind: %s (knots)<br/>Pressure: %s",
                                    htmlEscape(advisory), htmlEscape(date), htmlEscape(format(localtime, "%b %d %H:%M")),
                                    htmlEscape(lon), htmlEscape(lat),
                                    htmlEscape(status), htmlEscape(wind_speed), htmlEscape(pressure))
  ) %>%
  addCircleMarkers(data = storm[storm$wind_speed == 0,], ~lon, ~lat, radius = 10, 
                   stroke = TRUE, opacity = ~opacity, weight = 1, color = ~color) %>%
  addLegend("bottomright", colors = pal, labels = ss) %>%
  html_print