create_logs <- function() {
  # 임의의 좌표값 생성
  x1 <- sample(1:100000, 1000, replace=T)
  x2 <- sample(1:100000, 1000, replace=T)
  y <- sample(1:100000, 1000, replace=T)
  # data.frame 생성
  logs <- data.frame(x1, x2, y)
  
  # x1의 값이 x2보다 큰 경우
  if (nrow(logs[logs$x1 > logs$x2, ]) != 0) {
    temp <- subset(logs, x1>x2, select=x1)
    logs[logs$x1 > logs$x2, "x1"] <- logs[logs$x1 > logs$x2, "x2"]
    logs[logs$x1 == logs$x2, "x2"] <- temp
  }
  
  # x1과 x2가 같은 경우
  if (nrow(logs[logs$x1 == logs$x2, ]) != 0) {
    temp <- subset(logs, x1==x2)
    for (r in temp) {
      if (r$x1 == 100000) {
        r$x1 <- r$x1 -1
      }
      else if (r$x2 == 1) {
        r$x2 <- r$x2 + 1
      }
      else {
        r$x2 <- r$x2 + 1
      }
    }
  }
  
  return (logs)
}

cross_logs <- function(d) {
  # x1의 최소와 x2의 최대 찾기
  x1_min <- min(d[, "x1"])
  x2_max <- max(d[, "x2"])
  max <- 0
  count <- 0
  for (i in c(x1_min:x2_max)) {
    count <- nrow(d[d$x1 <= i & d$x2 >= i, ])
    if (count > max) {
      max <- count
    }
  }
  cat("max =", max, "\n")
}

logs <- create_logs()

cross_logs(logs)
system.time(cross_logs(logs))
