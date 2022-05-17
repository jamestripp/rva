##################################################
# Download VA api data
# Exploration of the VA API v2
##################################################
# NA
##################################################
# Author: Dr James Tripp
## Copyright: 2022
## License: NA
# Version: 0.0.1
# Maintainer: James Tripp
## Email: james.tripp@warwick.ac.uk
# Status: Under development
##################################################

# following guidelines here: https://httr.r-lib.org/articles/api-packages.html

library(httr)
library(jsonlite)

# Details of the search API are at
#  https://developers.vam.ac.uk/guide/v2/search/introduction.html#searching-top
# Details on the page argument is here
#  https://developers.vam.ac.uk/guide/v2/results/introduction.html
search_api <- function(queries, n_pages = 1, page_size = 100, verbose = FALSE) {
  query_api <- function(page) {
    queries$page <- page
    base_url <- "https://api.vam.ac.uk/v2/objects/search"
    resp <- GET(url = base_url, query = queries)
    resp
  }
  for (page in seq(1, n_pages)) {
    message("Downloading page ", page)
    resp <- query_api(page)
    message(paste("Response code:", resp$status_code))
    if (http_type(resp) != "application/json") {
      stop("API did not return json", call. = FALSE)
    }
    this_data <- fromJSON(rawToChar(resp$content), simplifyDataFrame = TRUE)
    if (class(this_data$records) != "data.frame") {
      message("No dataframe in response")
      return(list(
        info = data$info,
        records = records,
        clusters = data$clusters
      ))
    }
    these_records <- flatten(this_data$records, recursive = TRUE)
    if (page > 1) {
      records <- rbind(records, these_records)
    } else {
      records <- these_records
    }
  }
  if (verbose) {
    message(paste("Found", this_data$info$record_count, "records and", this_data$info$image_count, "images."))
    message(paste("There are", this_data$info$pages, "pages of results. You have requested", n_pages, "page(s)."))
    message(paste("We have a total of", nrow(records), "records."))
  }
  return(list(
    info = data$info,
    records = records,
    clusters = data$clusters
  ))
}

get_images <- function(folder_name, records){
  full_folder_name <- paste0(sub('\\..*', '', folder_name), format(Sys.time(),'_%Y%m%d_%H%M%S'))
  dir.create(full_folder_name)
  urls <- unlist(na.omit(data$records["_images._iiif_presentation_url"]))
  for (i in seq(1, length(urls))){
    # read in json manifest
    this_url <- urls[i]
    resp <- GET(url = this_url)
    if (http_type(resp) != "application/json") {
      message("API did not return json")
    }
    this_manifest <- fromJSON(rawToChar(resp$content))
    # download image into folder
    image_url <- this_manifest$sequences$canvases[[1]]$images[[1]]$resource$`@id`
    id <- strsplit(this_url, "/")[[1]][5]
    download.file(image_url, destfile = paste(full_folder_name, "/", id, ".jpg", sep=""), mode = 'wb')
  }
}

queries <- list(q = "queen")
data <- search_api(queries, n_pages = 1, verbose = TRUE)
get_images("Queen", data$records)

data$records
