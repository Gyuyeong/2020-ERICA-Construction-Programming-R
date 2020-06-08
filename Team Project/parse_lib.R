parse_string <- function(s) {
  s <- str_replace(s, "^\\.\\.\\.", "")
  s <- str_replace(s, "\\.\\.\\.$", "")
  s <- str_replace_all(s, "[\t\n]", "")
  s <- str_replace_all(s, "\\[.+\\]", "")
  s <- str_replace(s, "^\\s+", "")
  s <- str_replace(s, "[ ]+$", "")
  s <- str_replace(s, "\\(.+\\)", "")
  s <- str_replace_all(s, "▶", "")
  s <- str_replace_all(s, "◆", "")
  s <- str_replace_all(s, "◇", "")
  s <- str_replace_all(s, "▲", "")
  s <- str_replace_all(s, "■", "")
  s <- str_replace_all(s, "△", "")
  s <- str_replace_all(s, "지금 들어오시면 평생무료! 카톡방 입장하기!   무료방송 및 카톡방에서는 영상강의, 추천종목, 실시간 리딩등 다양한 서비스를 무료로 만날 수 있다.  지금까지 화려한 행보로 압도적인 수익률을 보여주어 왔던 탑TV, 카톡방에서의 추천주도 많은 투자자들이 관심을 갖는 이유이다.", "")
  s <- str_replace_all(s, "무료상담", "")
  s <- str_replace_all(s, "[0-9]+명 한정 무료입장! ->   무료로 운영되는 카톡방임에도 이렇게 추천주와 시황등의 내용을 알려준다는 것에 증권업계 관계자들이 놀라기도 하였다.", "")
  s <- str_replace_all(s, "타 유료서비스 못지않은 수익률을 안겨주고 있는 무료주식카톡방, 해당 카톡방은 [0-9]+명 한정으로만 무료로 운영된다고 하니 서둘러 참여해보자.  ", "")
  s <- str_replace_all(s, "수익인증의 주인공이 될 수 있습니다! 지금 바로 입장!   한국TV 주식카톡방은 무료로 입장이 가능하며 종목 추천 등의 서비스도 전액 무료로 제공받을 수 있다. 다만 입장 정원이 마감되면 입장이 불가하니 위기를 기회로 만들고 싶다면 주저하지 말고 한국TV 주식카톡방의 문을 두드려 보자.", "")
  s <- str_replace_all(s, "최정상급 전문가들이 직접 추천주와 리딩을 하기 때문에 이 종목만으로도 주식투자에 대한 고민은 사라질 수 있다.   지금 들어오시면 평생무료! 카톡방 입장하기!   지금까지 화려한 행보로 압도적인 수익률을 보여주어 왔던 탑TV, 카톡방에서의 추천주도 많은 투자자들이 관심을 갖는 이유이다", "")
  s <- str_replace_all(s, "100% 무료! 주식카톡방 입장하기    ‘평생무료’라는 모토로 무제한 무료 주식리딩을 하고 있는 주식카톡방. 선착순 정원마감이 될 수 있다고 하니 추천주를 확인하고 싶다면 지금 바로 참여해보자.  ", "")
  s <- str_replace_all(s, "[\U4E00-\U9FFF\U3000-\U303F]", "")
  s <- str_replace(s, "^\\s+", "")
  s <- str_replace(s, "[ ]+$", "")
  return (s)
}

parse_date <- function(s) {
  s <- substring(s, 1, 10)
  s <- str_replace_all(s, "\\.", "-")
  s <- as.character(s)
  return (s)
}

parse_price <- function(s) {
  s <- str_replace_all(s, ",", "")
  s <- as.integer(s)
  return (s)
}

