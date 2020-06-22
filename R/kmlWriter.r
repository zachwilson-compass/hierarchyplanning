#' Write KML Boundary File
#'
#' This function allows you to get the boundary areas of a categorized group of Lat/Long points
#' @param g Vector of categories / Groups
#' @param la The latitude of the points
#' @param lo The longitude of the points
#' @param sheetname The sheetname specified in the KML File
#' @param filename The file name and path for the KML File
#' @keywords boundary
#' @export
#' @examples
#' @getBoudnaryArea()

kmlWriter <- function(g, la, lo, sheetname, filename) {

  dat <- data.frame(KML_Group = g, Latitude = la, Longitude = lo)

  # Boilerplat Text for KML File
  frame_1 <- '<?xml version="1.0" encoding="UTF-8"?>
<kml xmlns="http://www.opengis.net/kml/2.2">
  <Document>
    <name>'
  frame_2 <- '</name>
    <Style id="Mapit">
      <LineStyle>
        <width>1.5</width>
      </LineStyle>
      <PolyStyle>
        <color>7dff0000</color>
      </PolyStyle>
    </Style>'
  polygon_1 <- '<Placemark>
      <name>'
  polygon_2  <- '</name>
      <styleUrl>#shp2kml</styleUrl>
      <Polygon>
        <altitudeMode>relativeToGround</altitudeMode>
        <outerBoundaryIs>
          <LinearRing>
            <coordinates> '
  polygon_3 <- '</coordinates>
          </LinearRing>
        </outerBoundaryIs>
      </Polygon>
    </Placemark>'
  frame_3 <- '  </Document>
</kml>'

  file_output <- c(paste(frame_1,sheetname,frame_2, sep=''))

  current_group =''

  for ( row in 1:nrow(dat)) {

    # Checks if the grouping has chagned
    if (dat$KML_Group[row] != current_group) {

      if(current_group != '') {
        file_output <- c(file_output, polygon_3)
      }

      file_output <- c(file_output, paste(polygon_1, as.character(dat$KML_Group[row]),polygon_2,sep=''))
      # Sets new current_group
      current_group <- dat$KML_Group[row]
    }

    # Creates entry for single polygon
    file_output <- c(file_output, paste(dat$Latitude[row],',',dat$Longitude[row],',0.0', sep=''))

  }

  file_output <- c(file_output, polygon_3, frame_3)

  fileConn<-file(paste(filename,'.kml', sep=''))
  writeLines(file_output, fileConn)
  close(fileConn)

}
