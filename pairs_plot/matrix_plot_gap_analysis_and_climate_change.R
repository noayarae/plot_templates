install.packages("Hmisc")
install.packages("psych")
install.packages("car")
library(Hmisc)
library(psych)
library(car)

#data <- read.csv("G:/work/CHIN/research/plots/plots_in_R/tradeoff1.csv")
#data <- read.csv("D:/thesis/plots/tradeoff1.csv") # Read CSV dataset and assign to "data"
data <- read.csv("E:/osu_account_google_drive/invest/Paper/Paper_1/matrix_plots/wetland_optimization.csv") # Read CSV dataset and assign to "data"

head(data)
hist(data$wet_area)
plot(density(data$wet_area))

a2 = data[, c(1:5)]
g_runs = data[, 6]
unique(g_runs)

cor(a2) # Correlation matrix

#panel.pearson <- function(x,y,...){
#  horizontal <- (par("usr")[1]+par("usr")[2])/2;
#  vertical <- (par("usr")[3]+par("usr")[4])/2;
#  text(horizontal, vertical, format(abs(cor(x,y)),digits = 2))
#}
#pairs(a2, main="scatter plot matrix", pch=21, bg=c("red","green","blue")[unclass(data$case)],
#      upper.panel = panel.pearson, labels = c("Pot-Wet","PFR","HI"))

#It builds a function called "panel.pearson" (line 26-127)
panel.pearson <- function(x,y,digits=2,cex.cor,...){
  usr <- par("usr");on.exit(par(usr))
  par(usr=c(0,1,0,1))
  
  ####### Filter "run1-noGAP" data
  z = data[,c(6)]
  xyz <- data.frame(x,y,z) # put together x,y,z in one table
  xx <- which(xyz$z =="run1-noGAP") # filtering data
  yy <- xyz[xx,]
  x1 <- yy[1]
  y1 <- yy[2]
  #print(yy)
  #correlation coeff for "run1-noGAP" data
  r1 <- cor(x1,y1)
  #print (r1)
  txt <- format(c(r1, 0.123456789), digits = digits)[1]
  txt <- paste(intToUtf8(120),"  Run1-noGAP  r = ",txt,sep = "")
  text(0.5, 0.9, txt) #position of text
  
  # Filter run1-wGAP data
  xyz <- data.frame(x,y,z) # put together x,y,z in one table
  xx <- which(xyz$z =="run1-wGAP") # filtering data
  yy <- xyz[xx,]
  x2 <- yy[1]
  y2 <- yy[2]
  #print(yy)
  #correlation coeff for "run1-wGAP" data
  r2 <- cor(x2,y2)
  #print (r2)
  txt <- format(c(r2, 0.123456789), digits = digits)[1]
  txt <- paste(intToUtf8(9633),"  Run1-wGAP  r = ",txt,sep = "")
  text(0.5, 0.75, txt) #position of text
  
  ####### Filter run2-noGAP data
  z = data[,c(6)]
  xyz <- data.frame(x,y,z) # put together x,y,z in one table
  xx <- which(xyz$z =="run2-noGAP") # filtering data
  yy <- xyz[xx,]
  x3 <- yy[1]
  y3 <- yy[2]
  #print(yy)
  #correlation coeff for "run2-noGAP" data
  r3 <- cor(x3,y3)
  #print (r3)
  txt <- format(c(r3, 0.123456789), digits = digits)[1]
  txt <- paste(intToUtf8(43),"  Run2-noGAP  r = ",txt,sep = "")
  text(0.5, 0.6, txt) #position of text
  
  # Filter run2-wGAP data
  xyz <- data.frame(x,y,z) # put together x,y,z in one table
  xx <- which(xyz$z =="run2-wGAP") # filtering data
  yy <- xyz[xx,]
  x4 <- yy[1]
  y4 <- yy[2]
  #print(yy)
  #correlation coeff for "run2-wGAP" data
  r4 <- cor(x4,y4)
  #print (r4)
  txt <- format(c(r4, 0.123456789), digits = digits)[1]
  txt <- paste(intToUtf8(111),"  Run2-wGAP  r = ",txt,sep = "")
  text(0.5, 0.45, txt) #position of text
  
  ####### Filter run3-noGAP data
  z = data[,c(6)]
  xyz <- data.frame(x,y,z) # put together x,y,z in one table
  xx <- which(xyz$z =="run3-noGAP") # filtering data
  yy <- xyz[xx,]
  x5 <- yy[1]
  y5 <- yy[2]
  #print(yy)
  #correlation coeff for "run3-noGAP" data
  r5 <- cor(x5,y5)
  #print (r5)
  txt <- format(c(r5, 0.123456789), digits = digits)[1]
  txt <- paste(intToUtf8(42),"  Run3-noGAP  r = ",txt,sep = "")
  text(0.5, 0.3, txt) #position of text
  
  # Filter run3-wGAP data
  xyz <- data.frame(x,y,z) # put together x,y,z in one table
  xx <- which(xyz$z =="run3-wGAP") # filtering data
  yy <- xyz[xx,]
  x6 <- yy[1]
  y6 <- yy[2]
  #print(yy)
  #correlation coeff for "run3-wGAP" data
  r6 <- cor(x6,y6)
  #print (r6)
  txt <- format(c(r6, 0.123456789), digits = digits)[1]
  txt <- paste(intToUtf8(9651),"  Run3-wGAP  r = ",txt,sep = "")
  text(0.5, 0.15, txt) #position of text
  
  
  ########## p-value calculaion
  ### p <- cor.test(x,y)$p.value
  ### #pt <- c(0.20,0.6,0.01,0.11,0.009,0.098,0.036,0.162, 0.119,0.04)
  ### #p <- t(pt)
  ### #print (p)
  ### txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  ### txt2 <- paste("p=",txt2,sep = "")
  ### if(p<0.01) txt2 <- paste("p=","<0.01",sep = "")
  ### text(0.5, 0.4, txt2) #position of text
}

marker_set <- c("run1-noGAP"=1, "run1-wGAP"=2,  "run2-noGAP"=18, "run2-wGAP"=22,  "run3-noGAP"=23, "run3-wGAP"=24)
color_set <- c("run1-noGAP"='blue', "run1-wGAP"='cyan',  "run2-noGAP"='red', "run2-wGAP"='orange',  "run3-noGAP"='black', "run3-wGAP"='#009933')
pairs(a2, 
      main="scatter plot matrix", 
      #pch=c(4,4,3,1,8,2)[unclass(data$case)], 
      pch = marker_set[g_runs],
      col = color_set[g_runs],
      #bg=c("black","red","green3","blue","magenta","cyan")[unclass(data$case)],
      upper.panel = panel.pearson, 
      gap=0.5,                      # Space between plot boundaries
      cex=1.5, 
      lwd = 0.1, 
      labels = c("Pot. Wetland Area", "Peak Flow Reduction", "Sediment Reduction","Nitrate Reduction","Habitat Index")
)



# ==============================================================================

#aa1 <- read.csv("D:/work/research_t/ClimateChangeDB/tables/001_CNRM-CM5_RCP45_Ceap2_tables/trend_all_m01_CNRM_CM5_RCP45.csv") # Read CSV dataset and assign to "data"
#aa1 <- read.csv("D:/work/research_t/ClimateChangeDB/tables/002_CNRM-CM5_RCP85_Ceap2_tables/trend_all_m01_CNRM_CM5_RCP85.csv") # Read CSV dataset and assign to "data"
aa1 <- read.csv("D:/work/research_t/ClimateChangeDB/tables/002_CNRM-CM5_RCP85_Ceap2_tables/trend_CNRM_CM5.csv") 
aa2 <- aa1[c("slope_ET","slope_Q","slope_Sed")]

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

pairs(aa2, 
      main = 'Model: CNRM-CM5_RCP45_Ceap2',
      col = "blue",
      pch = 21,
      labels = c("ET-trend", "Q-trend", "Sed-trend"),
      lower.panel = panel.cor, 
      cex.cor = 2
)

# ----------- Plot trend_CNRM_CM5 with colors -----------------------
t_model <- aa1[c("model")]
t_model <- unlist(t_model)  # Convert 'List' to 'Character'

marker_palette <- c('CNRM_CM5_RCP45'=1, 'CNRM_CM5_RCP85'=22)
color_palette <- c("CNRM_CM5_RCP45" = "black", "CNRM_CM5_RCP85" = "red") 
bg_palette <- c("CNRM_CM5_RCP45" = "green", "CNRM_CM5_RCP85" = "cyan") 
pairs(aa2,
      #pch = 22,
      pch = marker_palette[t_model],
      col = color_palette[t_model],
      bg = bg_palette[t_model]
)

# ----------- Plot trend_CNRM_CM5 with r, p values -----------------------
# Default method ("pearson")
pairs(aa2, 
      main = 'Model: CNRM-CM5_RCP45_Ceap2',
      col = "blue",
      pch = 21,
      labels = c("ET-trend", "Q-trend", "Sed-trend"),
      lower.panel = panel.cor, 
      cex.cor = 2
      )


# -----------------------------------------------------------------------------





























