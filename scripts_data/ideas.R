p <- ggplot(poland, aes(x=as.Date(date), y=onedose),xmin=) +
  geom_line(size=1,color="red") + scale_x_date(date_breaks = "1 month",
                                               date_minor_breaks = "1 week", date_labels = "%B") + zew_plotstyle() +
  geom_vline(aes(xintercept = as.integer(as.POSIXct("2021-07-01"))),
             col = "red")

p