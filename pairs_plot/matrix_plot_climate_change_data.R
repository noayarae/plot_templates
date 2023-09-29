
data <- read.csv("E:/osu_account_google_drive/invest/Paper/Paper_1/matrix_plots/climate_change.csv") # 
head(data)

hist(data$slope_Q)
plot(density(data$slope_Q))

a2 = data[, c(1:3)]
g_runs = data[, 4]
unique(g_runs)

#It builds a function called "panel.pearson"
panel.cor <- function(x, y, cex.cor = 0.8, method = "pearson", ...) {
  options(warn = -1)                   # Turn of warnings (e.g. tied ranks)
  usr <- par("usr"); on.exit(par(usr)) # Saves current "usr" and resets on exit
  par(usr = c(0, 1, 0, 1))             # Set plot size to 1 x 1
  r <- cor(x, y, method = method, use = "pair")               # correlation coef
  p <- cor.test(x, y, method = method)$p.val                  # p-value
  n <- sum(complete.cases(x, y))                              # How many data pairs
  txt <- format(r, digits = 3)                                # Format r-value
  txt1 <- format(p, digits = 3)                                 # Format p-value
  txt2 <- paste0("r= ", txt, '\n', "p= ", txt1, '\n', 'n= ', n) # Make panel text
  text(0.5, 0.5, txt2, cex = cex.cor, ...)                      # Place panel text
  options(warn = 0)                                             # Reset warning
}

g_runs = data[, 4]                    # This gets 'character'
#g_runs = data["model"]                # This does not work because it is 'List'
#g_runs = data[c("model")]             # This does not work because it is 'List'
#g_runs_ch <- unlist(g_runs)          # Convert list to 'character'

marker_set <- c("CNRM_CM5_RCP45"=1, "CNRM_CM5_RCP85"=22)
color_set <- c("CNRM_CM5_RCP45"='blue', "CNRM_CM5_RCP85"='red')
bg_set <- c("CNRM_CM5_RCP45"='cyan', "CNRM_CM5_RCP85"='orange')

pairs(a2, 
      main="scatter plot matrix", 
      pch = marker_set[g_runs],
      col = color_set[g_runs],
      bg = bg_set[g_runs],
      upper.panel = panel.cor,
      gap=0.5,                      # Space between plot boundaries
      cex=1.5, 
      lwd = 0.1, 
      labels = c("ET_trend", "Q_trend", "Sed_trend")
)


type1 = data[, 4]          # This gets character
type2 = data["model"]      # This gets list
type3 = data[c("model")]   # Get a 'list'
print(typeof(type1)) 
print(typeof(type2))
print(typeof(type3))












