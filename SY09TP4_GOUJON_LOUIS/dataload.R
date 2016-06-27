# Paul GOUJON & Stéphane LOUIS
# UTC - SY09 - TP4

# Data loading function

# little script to load data, because we are lazy


data1 = read.table("./donnees-tp4/Synth1-1000.txt");
data2 = read.table("./donnees-tp4/Synth2-1000.txt");
data3 = read.table("./donnees-tp4/Synth3-1000.txt");
data4 = read.csv("./donnees-tp4/Pima.csv", header=T);
data5 = read.csv("./donnees-tp4/bcw.csv", header=T);

data = list(data1, data2, data3, data4, data5);
names(data) = c("Synth1", "Synth2", "Synth3", "Pima", "BCW");

source("separ1.R");
source("separ2.R");

