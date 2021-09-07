### get data
get_data <- function() {
  part2_data <- read.csv("Data/framingham.csv")
  contains_na <- apply(part2_data, MARGIN = 1, function(x) any(is.na(x)))
  part2_data[!contains_na,]
}
part2_data <- get_data()


### model
logis_step_full <- glm(TenYearCHD ~ ., data = part2_data, family = binomial) %>%
  step(., direction = "backward", trace = F, k = log(dim(part2_data)[1]))
chd_rate <- mean(part2_data$TenYearCHD)
chd_predict_threshold <- quantile(logis_step_full$fitted.values, 
                                  probs = 1 - mean(part2_data$TenYearCHD))
chd_plot_threshold <- quantile(logis_step_full$fitted.values, probs = 1 - 0.01)



### functions

# function for custom
theme_custom <- function() {
  theme(
    axis.title   = element_text(size = 12), 
    title        = element_text(size = 14),
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 10),
    axis.text.x  = element_text(size = 10),
    axis.text.y  = element_text(size = 10),
    plot.caption = element_text(size = 10))
}

# function for gauge plot
# reference: 
# https://stackoverflow.com/questions/50042214/fill-a-polygon-with-gradient-scale-in-r
gg.gauge <- function(pos) {
  pos <- pos/chd_plot_threshold * 100
  breaks = c(0, chd_predict_threshold / chd_plot_threshold * 90, 90, 100)
  require(ggplot2)
  get.poly <- function(a, b, r1 = 0.5, r2 = 1.0) {
    if (b > 101) {
      th.start <- pi * (1 - 99 / 100)
      th.end   <- pi * (1 - 101 / 100)
      th       <- seq(th.start, th.end, length = 1000)
      x        <- r1 * cos(th)
      xend     <- r2 * cos(th)
      y        <- r1 * sin(th)
      yend     <- r2 * sin(th)
      return(data.frame(x, y, xend, yend))
    }
    th.start <- pi * (1 - a / 100)
    th.end   <- pi * (1 - b / 100)
    th       <- seq(th.start, th.end, length = 1000)
    x        <- r1 * cos(th)
    xend     <- r2 * cos(th)
    y        <- r1 * sin(th)
    yend     <- r2 * sin(th)
    return(data.frame(x, y, xend, yend))
  }
  
  options(digits = 3)
  ggplot() + 
    geom_segment(data = get.poly(breaks[1],breaks[4]), 
                 aes(x = x, y = y, xend = xend, yend = yend, color = xend)) +
    scale_color_gradientn(colors = colorRampPalette(c("#86c3e9", "#f18e8f", "#ee797a"))(3)) +
    geom_segment(data = get.poly(pos - 1, pos + 1, 0.2), 
                 aes(x = x, y  =y, xend = xend, yend = yend)) +
    geom_text(data=as.data.frame(breaks), size = 5, fontface = "bold", vjust = 0,
              aes(x = 0.8 * cos(pi * (1 - breaks / 100)),  y = -0.1), 
              label = c('Low', '', '', "High")) +
    labs(title = "Your Risk for Heart Disease",
         caption = paste0("Your Risk is: ", 
                          formatC(pos * chd_plot_threshold/
                                    (100 * chd_predict_threshold), 
                                  digits = 3), "!")) + 
    coord_fixed() +
    theme_bw() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          legend.position = "none",
          plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
          plot.caption = element_text(size = 16, hjust = 0.5, color = "red",
                                      face = "bold"))
}

# function for getting risk probability
get_risk <- function(Sex, Age, sysBP, glucose, cigsPerDay, smoker){
  if (Sex == "male") { Sex = 1 }
  else { Sex = 0 }
  
  if (smoker == "No") { cigsPerDay = 0 }
  
  new <- data.frame(male = Sex, age = Age, cigsPerDay = cigsPerDay,
                    sysBP = sysBP, glucose = glucose)
  logit_predict <- predict(logis_step_full, newdata = new)
  odds_predict <- exp(logit_predict)
  p_predict <- odds_predict / (1 + odds_predict)
  
  p_predict
}