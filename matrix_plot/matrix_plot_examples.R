

install.packages("Hmisc")
install.packages("psych")
install.packages("car")
library(Hmisc)
library(psych)
library(car)


### ------------------ Pair plot segmenting by groups --------------------------
set.seed(525354)                       # Set seed for reproducibility
N <- 100                              # Sample size of 1000
x1 <- rnorm(N)                         # Create variable
x2 <- x1 + rnorm(N, 0, 3)              # Create correlated variable
x3 <- 2 * x1 - x2 + rnorm(N, 0, 2)     # Create another correlated variable
data <- data.frame(x1, x2, x3)         # Combine all variables to data.frame

pairs(data)                            # Apply pairs function Example 1

pairs(~ x1 + x2 + x3, data = data)     # Produces same plot as in Example 1

pairs(~ x1 + x3, data = data)          # Leave out one variable

pairs(data[ , 1:3],
      col = "red",                                         # Change color
      pch = 18,                                            # Change shape of points
      labels = c("var1", "var2", "var3"),                  # Change labels of diagonal
      main = "This is a nice pairs plot in R")             # Add a main title

# Grouping
group <- NA
group[data$x1 < - 0.5] <- 1
group[data$x1 >= - 0.5 & data$x1 <= 0.5] <- 2
group[data$x1 > 0.5] <- 3

pairs(data[ , 1:3],
      col = c("red", "cornflowerblue", "black")[group],   # Change color by group
      pch = c(8, 18, 1)[group],                            # Change points by group
      labels = c("var1", "var2", "var3"),                  # Change labels by group
      main = "This is an even nicer pairs plot in R")      # Set main title


### --------------  Pair plot including mean and median values -----------------
a <- matrix(rnorm(5000, 10, 1) + rgamma(5000, 1, 2), 10, 5) # Create a matrix of 5x10
var_mean <- apply(a, 2, mean)
var_median <- apply(a, 2, median)
# Method 1
pairs(a)

# Method 2
# Ref.: https://stackoverflow.com/questions/65947719/how-to-add-the-point-plot-for-the-scatterplot-using-pairs-function-in-r
da = rbind(a,var_mean,var_median)
pairs(da,col = c(rep("black",nrow(a)),"blue","red"),
      pch= c(rep(5,nrow(a)),3,3),
      cex = c(rep(1.5,nrow(a)),1,1)
)


### ------------------ Several functions for pairs plot -----------------------
# Numeric variables
df <- iris[1:4]
# method 1
pairs(df)

# Method 2. Equivalent to:
pairs(~ Sepal.Length + Sepal.Width +
        Petal.Length + Petal.Width, data = df)
# Method 3. Equivalent to:
with(df, pairs(~ Sepal.Length + Sepal.Width +
                 Petal.Length + Petal.Width))
# Method 4. Equivalent to:
plot(df)


# ------------------ Pair plot with custom colors (Iris data) ------------------
df <- iris[1:4] # Iris data
# Groups
species <- iris[, 5]

marker_palette <- c('versicolor'=1, 'virginica'=22, 'setosa'=25)
color_palette <- c("versicolor" = "black", "virginica" = "red", "setosa" = "blue") # marker-edge color
bg_palette <- c("versicolor" = "green", "virginica" = "#ffcc80", "setosa" = "cyan") # 'bg' works only on pch>20
pairs(df,
      #pch = 22,
      pch = marker_palette[species],
      col = color_palette[species],
      bg = bg_palette[species]
)

# ------------------ Pair plot with custom colors (Random data) -----------------
set.seed(123)
data <- data.frame(
  Class = rep(letters[1:3], each = 20),
  Var1 = rnorm(60),
  Var2 = rnorm(60),
  Var3 = rnorm(60)
)

# Define colors and markers for each class
colors <- c("a" = "red", "b" = "blue", "c" = "green")
markers <- c("a" = 1, "b" = 2, "c" = 3)

# Create a pair matrix plot with different colors and markers
pairs(data[, -1], 
      col = colors[data$Class], 
      pch = markers[data$Class])


# ====================== Matrix plot GAP analysis =============================

library(Hmisc)
library(psych)
library(car)
#data <- read.csv("G:/work/CHIN/research/plots/plots_in_R/tradeoff1.csv")
#data <- read.csv("D:/thesis/plots/tradeoff1.csv") # Read CSV dataset and assign to "data"
data <- read.csv("E:/osu_account_google_drive/invest/Paper/Paper_1/matrix_plots/tradeoff1.csv") # Read CSV dataset and assign to "data"

head(data)
hist(data$pwa)
plot(density(data$pwa))

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

pairs(a2, 
      main="scatter plot matrix",
      #pch=c(4,0,3,1,8,2)[unclass(data$case)], 
      pch = marker_set[g_runs],
      col = color_set[g_runs],
      #bg=c("black","red","green3","blue","magenta","cyan")[unclass(data$case)],
      upper.panel = NULL, 
      gap=0.1, 
      cex=0.5, 
      lwd = 0.1, 
      labels = c("Pot. Wetland Area", "Peak Flow Reduction", "Sediment Reduction","Nitrate Reduction","Habitat Index")
      )

pairs(a2, 
      main="scatter plot matrix", 
      #pch=c(4,0,3,1,8,2)[unclass(data$case)], 
      pch = marker_set[g_runs],
      col = color_set[g_runs],
      #bg=c("black","red","green3","blue","magenta","cyan")[unclass(data$case)],
      lower.panel = NULL, 
      gap=0.1, 
      cex=0.5, 
      lwd = 0.1, 
      labels = c("Pot. Wetland Area", "Peak Flow Reduction", "Sediment Reduction","Nitrate Reduction","Habitat Index")
      )











