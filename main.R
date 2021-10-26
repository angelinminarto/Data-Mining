
#input data ke dalam r 
 cuisine <- read.csv("data/chefmozcuisine.csv")
 geoplace <- read.csv("data/geoplaces2.csv")
 rating <- read.csv("data/rating_final.csv")
 
 View(cuisine)
 View(geoplace)
 View(rating)
 
 
 #1. Data Visualization dalam bentuk piechart
 
 #1.a
 #menampung data baru utk menggabungkan data
 cuisine_distribution <- merge(cuisine, geoplace, 
                               by = "placeID")
 
 #utk select data apa yg akan kita pake
 #$ utk access nama kolom
 #tampungan baru dari data yang kita mau ambil
 data_a <- table(cuisine_distribution$Rcuisine)
 data_a <- data_a[data_a > 5] #data a adalah data penampung
 
 View(data_a)
 
 #untuk menvisualisasikan data dg menggunakan pie
 pie(data_a, main = "Cuisines Distribution") #utk tulis judul menggunakan syntax main
 
 #1.b
 #untuk menvisualisasikan data dg menggunakan histogram
#pertama buat data baru lagi
 data_h <- table(cuisine_distribution$placeID)
 
 #utk menvisualisasikan data dg menggunakan histogram dg syarat
 #membuat tampungan baru dari data yg kita mau ambil
    
    #data_h <- data_h[data_h > 1]
 View (data_h)
 
 #utk menvisualisasikan data dg menggunakan histogram jika tanpa syarat
 #xlab utk tulis keterangan data menyamping
 #ylab utk tulis keterangan data ke atas
 #col utk warna
 hist(data_h, 
      main = "Cuisines Count Frequency based on Restaurant", 
      xlab = "Cuisine Count", col = c("lightblue", "pink", "red")) #kalau mau lebih dari 1
 
 hist(data_h, 
      main = "Cuisines Count Frequency based on Restaurant", 
      xlab = "Cuisine Count", col = c("lightblue")) #kalau 1 warna
 
 #1.c
 #barplot
 average <- merge(rating, geoplace, by = "placeID")
 
 average["avg_rate"] <- (average$food_rating + 
                        average$rating + average$service_rating) / 3
 
 #memilih data yg sesuai seperti if
 average <- average[average$avg_rate > 1.2, ]
 
 #bersihin data/menyatukan data yg terpisah-pisah
 average$state <- tolower(average$state)
 average$state <- ifelse(average$state == "s.l.p", "slp",
                    ifelse(average$state == "san luis potosi", "slp",
                      ifelse(average$state == "san luis potos", "slp",
                            average$state)))
 View(average)
 
 
 data_b <- table(average$avg_rate, average$state)
 View (data_b)
 
 #beside supaya datanya menyamping
 barplot(data_b, beside = TRUE, 
         col = c("red", "blue", "pink"), 
         xlab = "State", 
         main = "Average Ratings Distribution based on The State of Restaurant")
 
 legend("top", legend = rownames(data_b), 
        fill = c("red", "blue", "pink"), 
        cex = 0.8)
 
 #2. Frequent Pattern Analysis
 
 #Data Processing
 preprocess_data <- cuisine_distribution[cuisine_distribution$franchise == "f", ]
 preprocess_data <- cuisine_distribution[cuisine_distribution$other_services == "none", ]
 preprocess_data <- cuisine_distribution[cuisine_distribution$country != "?", ] #tdk ambil yg tanda tanya
 preprocess_data$Rcuisine <- gsub("_", " ", preprocess_data$Rcuisine)
 
 
 #Data Transformation
 #apriori utk memisahkan data
 apriori_data <- split(preprocess_data$Rcuisine, 
                  preprocess_data$placeID)
 
 #Data Mining (utk mengambil datanya)
 install.packages("arules") #download arules
 
 library(arules)
 frequent_cuisine <- apriori(apriori_data, 
                             parameter = list(support = 0.008, 
                                                      target = "frequent itemsets"))
 
 
 inspect(frequent_cuisine)
 
 #association rules
 aso_rule <- ruleInduction(frequent_cuisine, confidence = 0.8)
 
 inspect(aso_rule)