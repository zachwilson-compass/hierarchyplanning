#' Get Boundary Area
#'
#' This function allows you to get the boundary areas of a categorized group of Lat/Long points
#' @param category Vector of categories
#' @param lat The latitude of the points
#' @param lng The longitude of the points
#' @keywords boundary
#' @export
#' @examples
#' @getBoudnaryArea()


getBoundaryArea <- function(category, lat, lng){

  # Required Library
  library(dplyr)

  # Creates Initial Data Frame
  dat <- data.frame(category = category, Latitude = x, Longitude = y )

  # Unique list of categories
  categories <- dat %>%
    select(category) %>%
    unique()

  first <- TRUE

  # Loops through each category and finds boundary points
  for (cat in categories$category){

    # Subsets data to just the current category
    category_data <- dat %>%
      filter(category == cat) %>%
      row_to_column("point_id")

    # Determines Convex Hull and Gives the points a sequence ID
    boundary_points <- data.frame(point_id = chull(category_data$Latitude, category_data$Longitude))
    boundary_points$point_order <- seq.int(nrow(boundary_points))

    # Adds routing into original data frame ane removes NAs
    polygon <- dat %>%
      left_join(boundary_points) %>%
      na.omit() %>%
      arrange(point_order)

    # Adds first point onto the end (Used for KML Polygons)
    polygon <- polygon %>%
      bind_rows [route[1,]]


    if (first == TRUE){
      polygons <- polygon
      first <- FALSE
    } else {
      polygons <- bind_rows(polygons, polygon)
    }

  }

  return(polygons)

}
