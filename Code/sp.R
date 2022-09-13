sp <- function (raw_values, title = "Syuzhet Plot", legend_pos = "top", 
                lps = 10, window = 0.1, plot = TRUE) 
{
  wdw <- round(length(raw_values) * window)
  rolled <- rescale(zoo::rollmean(raw_values, k = wdw, fill = 0))
  half <- round(wdw/2)
  rolled[1:half] <- NA
  end <- length(rolled) - half
  rolled[end:length(rolled)] <- NA
  trans <- get_dct_transform(raw_values, low_pass_size = lps, 
                             x_reverse_len = length(raw_values), scale_range = T)
  x <- 1:length(raw_values)
  y <- raw_values
  raw_lo <- stats::loess(y ~ x, span = 0.5)
  low_line <- rescale(stats::predict(raw_lo))
  
  # This line of code has been moved
  normed_trans <- get_dct_transform(raw_values, scale_range = T, 
                                    low_pass_size = 5)
  
  # Wrap the plots in an if statement where the TRUE condition returns the outputs
  if(plot == TRUE){
    graphics::par(mfrow = c(2, 1))
    graphics::plot(low_line, type = "l", ylim = c(-1, 1), main = title, 
                   xlab = "Full Narrative Time", ylab = "Scaled Sentiment", 
                   col = "blue", lty = 2)
    graphics::lines(rolled, col = "grey", lty = 2)
    graphics::lines(trans, col = "red")
    graphics::abline(h = 0, lty = 3)
    graphics::legend(legend_pos, c("Loess Smooth", "Rolling Mean", 
                                   "Syuzhet DCT"), lty = 1, lwd = 1, col = c("blue", "grey", 
                                                                             "red"), bty = "n", cex = 0.75)
    graphics::plot(normed_trans, type = "l", ylim = c(-1, 1), 
                   main = "Simplified Macro Shape", xlab = "Normalized Narrative Time", 
                   ylab = "Scaled Sentiment", col = "red")
    graphics::par(mfrow = c(1, 1))
  }
  
  # Collect and return data as a list of data frames
  # Data frame of smoothers
  df_smoothers <- cbind.data.frame(x, y, rolled, trans, low_line)
  names(df_smoothers) <- c("Sentence", "Sentiment", "Rolling Mean", "DCT", "Loess")
  # Data frame of simplified macro shape
  df_normed <- cbind.data.frame(t = 1:100, normed_trans)
  names(df_normed) <- c("Normalised_time", "Simplified")
  # Return the list of data frames
  res <- list(df_smoothers, df_normed)
  return(res)
}
