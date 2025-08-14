#' Save Raster as 8-bit Indexed PNG
#'
#' This function saves a raster object as an 8-bit indexed color PNG file
#' using the magick package while preserving the original colors.
#'
#' @param raster_obj A raster object to be saved
#' @param output_path Character string specifying the output PNG file path
#' @param color_palette Vector of colors to use for the raster (hex colors)
#' @param width Numeric, width of the output image in specified units (default: 25)
#' @param height Numeric, height of the output image in specified units (default: 20)
#' @param resolution Numeric, resolution in DPI (default: 300)
#' @param units Character, units for width and height ("cm", "in", "px") (default: "cm")
#' @param margins Numeric vector of length 4, margins in the order c(bottom, left, top, right) (default: c(0, 0, 0, 0))
#' @param background Character, background color (default: "transparent")
#' @param colorspace Character, colorspace for quantization ("sRGB" or "rgb") (default: "sRGB")
#' @param max_colors Numeric, maximum number of colors for indexed palette (default: 256)
#' @param show_legend Logical, whether to show the raster legend (default: FALSE)
#' @param axes Logical, whether to show axes (default: FALSE)
#' @param box Logical, whether to show a box around the plot (default: FALSE)
#' @param cleanup_temp Logical, whether to remove temporary files (default: TRUE)
#' @param verbose Logical, whether to print progress messages (default: FALSE)
#'
#' @return Invisibly returns the magick image object of the final indexed PNG
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' save_indexed_png(my_raster, "output.png", my_colors)
#' 
#' # With custom dimensions and margins
#' save_indexed_png(my_raster, "output.png", my_colors, 
#'                  width = 30, height = 25, margins = c(1, 1, 1, 1))
#' 
#' # High resolution output
#' save_indexed_png(my_raster, "output.png", my_colors, 
#'                  resolution = 600, units = "in", width = 8, height = 6)
#' }
#'
#' @export
save_indexed_png <- function(raster_obj, 
                           output_path, 
                           color_palette,
                           width = 25, 
                           height = 20, 
                           resolution = 300,
                           units = "cm",
                           margins = c(0, 0, 0, 0),
                           background = "transparent",
                           colorspace = "sRGB",
                           max_colors = 256,
                           show_legend = FALSE,
                           axes = FALSE,
                           box = FALSE,
                           cleanup_temp = TRUE,
                           verbose = FALSE) {
  
  # Load required libraries
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Package 'magick' is required but not installed.")
  }
  if (!requireNamespace("raster", quietly = TRUE)) {
    stop("Package 'raster' is required but not installed.")
  }
  
  library(magick)
  library(raster)
  library(grDevices)
  
  if (verbose) cat("Creating temporary PNG...\n")
  
  # Create temporary PNG file
  temp_png <- tempfile(fileext = ".png")
  
  # Create the standard PNG first
  png(temp_png, 
      width = width, 
      height = height, 
      res = resolution, 
      bg = background, 
      units = units)
  
  # Set margins
  par(mar = margins)
  
  # Plot the raster
  plot(raster_obj, 
       col = color_palette, 
       legend = show_legend, 
       axes = axes, 
       box = box)
  
  dev.off()
  
  if (verbose) cat("Converting to indexed PNG...\n")
  
  # Read the temporary PNG with magick
  img <- image_read(temp_png)
  
  # Convert to 8-bit indexed color PNG while preserving colors
  img_indexed <- img %>%
    image_quantize(max = max_colors, 
                   colorspace = colorspace, 
                   dither = FALSE) %>%
    image_strip()  # Remove unnecessary metadata
  
  # Write the indexed PNG
  image_write(img_indexed, output_path, format = "png", depth = 8)
  
  if (verbose) cat(paste("Saved indexed PNG to:", output_path, "\n"))
  
  # Clean up temporary file
  if (cleanup_temp) {
    unlink(temp_png)
    if (verbose) cat("Cleaned up temporary files.\n")
  } else {
    if (verbose) cat(paste("Temporary file saved at:", temp_png, "\n"))
  }
  
  # Return the magick image object invisibly
  invisible(img_indexed)
}

# Helper function to inspect the saved PNG properties
inspect_indexed_png <- function(png_path) {
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Package 'magick' is required but not installed.")
  }
  if (!requireNamespace("png", quietly = TRUE)) {
    stop("Package 'png' is required but not installed.")
  }
  
  library(magick)
  library(png)
  
  cat("=== PNG Inspection Results ===\n")
  
  # Method 1: Using magick
  img <- image_read(png_path)
  info <- image_info(img)
  
  cat("Magick Information:\n")
  cat(paste("  Dimensions:", info$width, "x", info$height, "\n"))
  cat(paste("  Color space:", info$colorspace, "\n"))
  cat(paste("  Depth:", info$depth, "bit\n"))
  cat(paste("  Format:", info$format, "\n"))
  
  # Method 2: Using png package for detailed info
  png_info <- readPNG(png_path, info = TRUE)
  info_attr <- attr(png_info, "info")
  
  color_types <- c("0" = "Grayscale", 
                   "2" = "RGB", 
                   "3" = "Indexed", 
                   "4" = "Grayscale + Alpha", 
                   "6" = "RGBA")
  
  cat("\nDetailed PNG Information:\n")
  cat(paste("  Color Type:", color_types[as.character(info_attr$color.type)], 
            "(", info_attr$color.type, ")\n"))
  cat(paste("  Is Indexed:", info_attr$color.type == 3, "\n"))
  cat(paste("  Bit Depth:", info_attr$bit.depth, "\n"))
  
  return(invisible(list(magick_info = info, png_info = info_attr)))
}

