########## Basic ##########

### 1-1
main = function(){
  print(5+9)
}
main()


### 1-2
main = function(){
  print(15^9)
}
main()


### 1-3
main = function(){
  print(100/7)
}
main()


### 1-4
main = function(){
  a = 9
  b = 7
  c = 4
  print((a+b)^c)
}
main()


### 1-5
main = function(){
  tmp = "Hello world!"
  print(tmp)
}
main()


### 1-6
main = function(){
  tmp1 = "Hello"
  tmp2 = "world!"
  print(paste(tmp1, tmp2))
}
main()


### 1-7
main = function(){
  tmp1 = "Hello"
  tmp2 = "world!"
  print(paste(tmp1, tmp2, sep = "-"))
}
main()


### 1-8
main = function(){
  tmp = 1:10
  print(paste(tmp, collapse = ","))
}
main()


### 1-9
main = function(){
  a = 5
  b = 3
  tmp = sprintf("a+b=%d", a + b)
  cat(tmp)
}
main()


### 1-10
main = function(){
  a = c(38, 14, 25, 62, 8, 71, 64, 29, 92)
  tmp = sprintf("Mean of a is %.2f\nSD of a is %.2f", mean(a), sd(a))
  cat(tmp)
}
main()



########## Function ##########

### 2-1
main = function(){
  vector_sum = function(vec){
    return(sum(vec))
  }
  print(vector_sum(1:10))
}
main()


### 2-2
main = function(){
  MorSD = function(vec, indicator){
    tmp = NULL
    if(indicator == "mean"){
      tmp = mean(vec)
    }else if(indicator == "sd"){
      tmp = sd(vec)
    }else{
      stop("invalid indicator: 'mean' or 'sd' is availavle.")
    }
    return(tmp)
  }
  print(MorSD(1:10, "mean"))
  print(MorSD(1:10, "sd"))
}
main()


### 2-3
main = function(){
  each_row_max = function(mat){
    for(i in 1:nrow(mat)){
      tmp1 = mat[i,]
      tmp2 = sprintf("row: %d max: %d loc: %s\n",
                   i, max(mat[i,]), paste(which(tmp1 == max(tmp1)), collapse = ", "))
      cat(tmp2)
    }
  }
  each_row_max(matrix(c(3, 7, 8, 5, 2, 8, 6, 4, 4), 3, 3))
}
main()


### 2-4
main = function(){
  random_vector = function(){
    ordered_vector = NULL
    count = 0
    trigger = 1
    set.seed(12345)
    while(trigger == 1){
      tmp1 = rnorm(1)
      if(abs(tmp1) <= 2){
        count = count + 1
      }
      tmp2 = sum(ordered_vector <= tmp1)
      ordered_vector = append(ordered_vector, tmp1, after = tmp2)
      if(count == 10000){
        trigger = 0
      }
    }
    return(ordered_vector)
  }
  random_vector()
}
main()

# main2 = function(){
#   random_vector = function(){
#     ordered_vector = NULL
#     count = 0
#     trigger = 1
#     set.seed(12345)
#     while(trigger == 1){
#       tmp1 = rnorm(1)
#       if(abs(tmp1) <= 2){
#         count = count + 1
#       }else if(abs(tmp1) > 3){
#         # stop("Error: generated value is over 3 or -3.")
#       }
#       tmp2 = c(ordered_vector, tmp1)
#       ordered_vector = sort(tmp2)
#       if(count == 10000){
#         trigger = 0
#       }
#     }
#     return(ordered_vector)
#   }
#   random_vector()
# }
# system.time(main())
# system.time(main2()) # main2 is slower than main


########## Vector ##########

### 3-1
main = function(){
  return(seq(0, 100, by = 5))
}
main()


### 3-2
main = function(){
  user = paste0("user", rep(1:5, 5))
  item = paste0("item", rep(LETTERS[1:5], each = 5))
  tmp = paste(user, item, sep = "/")
  return(matrix(tmp, 5, 5))
}
main()


### 3-3
main = function(){
  tmp = rnorm(100)
  sum(tmp[tmp > 0])
}
main()


### 3-4
main = function(){
  tmp = sample(1:100, 100, replace = T)
  print(tmp[tmp%%5 == 0])
}
main()


### 3-5
main = function(){
  tmp = sample(1:100, 100, replace = T)
  tmp[(tmp%%2 == 0 & tmp%%3 == 0) | (tmp%%5 == 0)] = NA
}
main()



########## Matrix ##########

### 4-1
main = function(){
  A = matrix(0, 10, 10)
  for(i in 1:10000){
    tmp = rmultinom(2, 1, rep(0.1, 10))
    row.loc = which(tmp[,1] == 1)
    col.loc = which(tmp[,2] == 1)
    A[row.loc, col.loc] = A[row.loc, col.loc] + 1
  }
  return(A)
}
main()


### 4-2
main = function(){
  mat = matrix(1:100, 10, 10, byrow = T)
  for(i in seq(2, 10, by = 2)){
    mat[,i] = mat[,i][order(mat[,i], decreasing = T)]
  }
  return(mat)
}
main()


### 4-3
main = function(){
  mat_original = matrix(rnorm(100), 10, 10)
  tmp = svd(mat_original)
  mat_recover = tmp$u %*% diag(tmp$d) %*% t(tmp$v)
  identical(round(mat_original, 5), round(mat_recover, 5))
}
main()


### 4-4
main = function(){
  array_function = function(){
    arr = array(rnorm(18), dim = c(3, 3, 2))
    return(list(trans = t(arr[,,1]), inverse = solve(arr[,,2])))
  }
  array_function()
}
main()


### 4-5
main = function(){
  tmp = vector("list", 3)
  names(tmp) = c("original", "mean", "max")
  tmp$original = rnorm(10)
  tmp$mean = mean(tmp[[1]])
  tmp$max = max(tmp[[1]])
  cat("this is vector:\n")
  print(tmp[[1]])
  cat("\nthis is list:\n")
  print(tmp[1])
  cat("\nis this list?:\n")
  print(is.list(tmp[1]))
  cat("\nmean and max simultaneously:\n")
  print(tmp[1:2])
}
main()


########## Apply family ##########

### 5-1
main = function(){
  arr = array(rnorm(24), dim = c(4, 3, 2))
  apply(arr, c(1, 3), mean)
}
main()


### 5-2
main = function(){
  letters_list = vector("list", 10)
  for(i in 1:10){
    tmp = sample(1:10, 1)
    letters_list[[i]] = sample(c(letters, LETTERS), tmp, replace = T)
  }
  sapply(letters_list, length)
}
main()


### 5-3
main = function(){
  letters_list = vector("list", 10)
  for(i in 1:10){
    tmp = sample(1:10, 1)
    letters_list[[i]] = sample(c(letters, LETTERS), tmp, replace = T)
  }
  lapply(letters_list, function(x) x[x %in% letters])
}
main()


### 5-4
main = function(){
  tmp1 = rnorm(100)
  tmp2 = rep(0, 100)
  mapply(max, tmp1, tmp2)
}
main()


########## Data frame ##########

### 6-1
main = function(){
  df = data.frame(product = c("apple", "apple", "peach", "peach", "grape"),
                  price = c(150, 152, 156, 156, 155),
                  sales = c(23, 24, 28, 21, 27))
  df
}
main()


### 6-2
main = function(){
  data(iris)
  iris_mean = sapply(iris[1:4], mean)
  print(iris_mean)
  
  tmp = iris[iris$Sepal.Length >= 5.0 & iris$Sepal.Width >= 3.0,]
  iris_extract = table(tmp$Species)
  print(iris_extract)
  
  leaves = sample(1:20, nrow(iris), replace = T)
  iris_leaves = cbind(iris[1:4], leaves, iris[5])
  print(iris_leaves)
  
  newayame = data.frame(Sepal.Length = round(runif(50, 3.0, 6.0), 1),
                        Sepal.Width = round(runif(50, 2.0, 4.0), 1),
                        Petal.Length = round(runif(50, 3.0, 6.0), 1),
                        Petal.Width = round(runif(50, 1.0, 2.0), 1),
                        Species = rep("newayame", 50))
  iris_ayame = rbind(iris, newayame)
  print(iris_ayame)
}
main()


### 6-3
main = function(){
  require(data.table)
  require(lubridate)
  df = fread("https://igarashim.github.io/data/tweet.tsv",
             quote = "", encoding = "UTF-8", colClasses = c("tw_id" = "character"))
  newdf = df[df$fav != 0 | df$RT != 0,]
  fwrite(newdf, file = "hoge.tsv", sep = "\t")
  
  Sys.setlocale("LC_TIME", "C")
  df$time = as.POSIXct(df$time, format = "%a %b %d %H:%M:%S +0000 %Y")
  fav_max = which.max(df$fav)
  rt_max = which.max(df$RT)
  tmp = seconds_to_period(difftime(df$time[fav_max], df$time[rt_max], units = "secs"))
  tmp = sprintf("difference is %d days, %d hours, and %d minutes.", tmp@day, tmp@hour, tmp@minute)
  print(tmp)
  
  df$tw_id = as.numeric(df$tw_id)
  options(scipen = 100)
  print(df$tw_id[fav_max])
  print(df$tw_id[rt_max])
}
main()


########## Character handling ##########

### 7-1
main = function(){
  require(stringr)
  tmp = paste(letters, collapse = "")
  str_count(tmp)
}
main()


### 7-2
main = function(){
  require(stringr)
  tmp1 = paste(letters, collapse = ",")
  tmp2 = paste(LETTERS, collapse = ",")
  cat(paste(tmp1, tmp2, sep = "\n"))
}
main()


### 7-3
main = function(){
  require(stringr)
  require(data.table)
  df = fread("https://igarashim.github.io/data/tweet.tsv",
             quote = "", encoding = "UTF-8", colClasses = c("tw_id" = "character"))
  tmp = unname(sapply(df$text, str_split, pattern = " "))
  head(tmp)
}
main()


### 7-4
main = function(){
  require(stringr)
  require(data.table)
  df = fread("https://igarashim.github.io/data/tweet.tsv",
             quote = "", encoding = "UTF-8", colClasses = c("tw_id" = "character"))
  unname(sapply(df$text, str_count, pattern = "is"))
}
main()


### 7-5
main = function(){
  require(stringr)
  require(data.table)
  df = fread("https://igarashim.github.io/data/tweet.tsv",
             quote = "", encoding = "UTF-8", colClasses = c("tw_id" = "character"))
  tmp = unname(sapply(df$text, str_remove_all, pattern = "@[:alnum:]+"))
  print(df$text[2])
  print(tmp[2])
}
main()


### 7-6
main = function(){
  require(stringr)
  require(data.table)
  df = fread("https://igarashim.github.io/data/tweet.tsv",
             quote = "", encoding = "UTF-8", colClasses = c("tw_id" = "character"))
  tmp = unname(sapply(df$text, tolower))
  tmp = unname(sapply(tmp, str_remove_all, pattern = "@[:alnum:]+"))
  tmp = unname(sapply(tmp, str_remove_all, pattern = "https?://[[:graph:]]*"))
  tmp = unname(sapply(tmp, str_remove_all, pattern = "[[:punct:]]|[[:digit:]]"))
  tmp = unname(sapply(tmp, str_split, pattern = " "))
  tmp = sapply(tmp, function(x) x[x != ""])
  head(tmp)
}
main()



########## Figure ##########

### 8-1
main = function(){
  require(ggplot2)
  load("data.RData")
  df = df$`Problem8-1`
  
  plot(df$Year, df$Passengers, type = "l", xlab = "Year", ylab = "Passengers")
  
  ggplot(df, aes(x = Year, y = Passengers)) + geom_line()
}
main()


### 8-2
main = function(){
  require(ggplot2)
  load("data.RData")
  df = df$`Problem8-2`
  
  plot(df$x, df$y, type = "p", xlab = "x", ylab = "y", main = "Regression with confidence region", ylim = c(-5, 25))
  par(new = T)
  tmp = lm(y ~ x, data = df)
  tmp = predict(tmp, df, interval = "confidence", level = 0.95)
  plot(df$x, tmp[,"fit"], type = "l", xaxt = "n", yaxt = "n", ann = F, ylim = c(-5, 25))
  par(new=T)
  plot(df$x, tmp[,"lwr"], type = "l", lty = 2, xaxt = "n", yaxt = "n", ann = F, ylim = c(-5, 25))
  par(new = T)
  plot(df$x, tmp[,"upr"], type = "l",lty=2, xaxt = "n", yaxt = "n", ann = F, ylim = c(-5, 25))
  
  ggplot(df, aes(x, y)) + geom_point() + geom_smooth(method = lm) + ggtitle("Regression with confidence region")
}
main()


### 8-3
main = function(){
  require(ggplot2)
  load("data.RData")
  df = df$`Problem8-3`
  
  plot(1:4, df[df$Sex == "Male" & df$Age == "Child", "Survived"],
       type = "o", pch = 1, xaxt = "n", xlab = "Class", ylab = "Survived Rate", col = "Blue", ylim = c(0, 1))
  par(new = T)
  plot(1:4, df[df$Sex == "Male" & df$Age == "Adult", "Survived"],
       type = "o", pch = 2, xaxt = "n", yaxt = "n", xlab = "", ylab = "", col = "Blue", ylim = c(0, 1))
  par(new = T)
  plot(1:4, df[df$Sex == "Female" & df$Age == "Child", "Survived"],
       type = "o", pch = 1, xaxt = "n", yaxt = "n", xlab = "", ylab = "", col = "Red", ylim = c(0, 1))
  par(new = T)
  plot(1:4, df[df$Sex == "Female" & df$Age == "Adult", "Survived"],
       type = "o", pch = 2, xaxt = "n", yaxt = "n", xlab = "", ylab = "", col = "Red", ylim = c(0, 1))
  axis(side = 1, at = 1:4, labels = levels(df$Class))
  legend("topright",
         legend = c("Male / Child", "Male / Adult", "Female / Child", "Female / Adult"),
         lty = c(1, 1, 1, 1), pch = c(1, 2, 1, 2), col = c("Blue", "Blue", "Red", "Red"))
  
  levels(df$Sex) = c("Female", "Male")
  ggplot(df, aes(x = Class, y = Survived, group = interaction(Sex, Age), colour = Sex, shape = Age)) +
    geom_line() + geom_point(size = 3) + scale_colour_manual(values = c("#00BFC4", "#F8766D"))
}
main()


### 8-4
main = function(){
  require(ggplot2)
  load("data.RData")
  df = df$`Problem8-4`
  
  barplot(df$Sepal.Width[c(1, 3, 2)], xlab = "Species", ylab = "Sepal.Width", names.arg = df$Species[c(1, 3, 2)])
  
  ggplot(df, aes(x = Species,y = Sepal.Width)) +
    geom_bar(stat = "identity") +
    scale_x_discrete(limits = c("setosa", "virginica", "versicolor"))
}
main()


### 8-5
main = function(){
  require(ggplot2)
  require(reshape2)
  load("data.RData")
  df = df$`Problem8-4`
  
  par.old = par(no.readonly = T)
  par(mfrow = c(2, 2))
  for(i in 1:4){
    barplot(df[,i], xlab = "", ylab = "", main = names(df)[i], names.arg = df$Species)
  }
  par(par.old)
  
  ggplot(melt(df), aes(Species, value)) + 
    geom_bar(stat = "identity") + 
    facet_wrap(~ variable, ncol = 2, scale = "free") +
    ylab("")
}
main()


### 8-6
main = function(){
  require(ggplot2)
  require(reshape2)
  load("data.RData")
  df = df$`Problem8-6`
  
  hist(df$A, breaks = seq(0, 1, 0.05), main = "Histgram", xlab = "range", ylim = c(0, 40), col = "#F8766D50")
  hist(df$B, col = "#00BFC450", add = T)
  hist(df$C, col = "#7CAE0050", add = T)
  legend("topright", legend = c("A", "B", "C"), col = c("#F8766D", "#00BFC4", "#7CAE00"), pch = 16, title = "Types")
  
  ggplot(melt(df), aes(value, fill = L1)) + geom_histogram(alpha = 0.5, position = "identity") +
    xlab("range") + 
    ylab("Frequency") +
    scale_fill_discrete(name = "Types") +
    scale_x_continuous(limits = c(0, 1))
}
main()



########## Application ##########

### 9-1
main = function(){
  require(MASS)
  data(Boston)
  res1 = lm(medv ~ ., data = Boston)
  summary(res1)
  
  loglike = function(params, data, object, explanatory = NULL){
    if(is.null(explanatory)){
      k = names(data[-object])
    }else{
      k = length(explanatory)
    }
    
  }
}