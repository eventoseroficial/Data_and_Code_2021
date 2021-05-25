# https://www.garrickadenbuie.com/blog/sharing-xaringan-slides/#get-your-slides-online

# IT DOES NOT WORK ON UBUNTU (CHROME PROBLEM?)

#' Screenshot Your Title Slide for Share Image
#'
#' Takes a screenshot of your title slide for sharing on Twitter
#' (and other social media sites).
#'
#' @param slides_rmd Your slides file
#' @param path Path to new share image
screenshot_share_image <- function(
  slides_rmd,
  path_image = "share-card.png"
) {
  if (!requireNamespace("webshot2", quietly = TRUE)) {
    stop(
      "`webshot2` is required: ", 
      'remotes::install_github("rstudio/webshot2")'
    )
  }
  
  webshot2::rmdshot(
    doc = slides_rmd,
    file = path_image,
    vheight = 600,
    vwidth = 600 * 191 / 100,
    rmd_args = list(
      output_options = list(
        nature = list(ratio = "191:100"),
        self_contained = TRUE
      )
    )
  )
  
  path_image
}

screenshot_share_image("Slides.Rmd")
