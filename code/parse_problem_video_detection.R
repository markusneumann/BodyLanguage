library(stringr)

#actual
logfiles <- list.files("../results/problem_files_2018/content/results2018/", pattern = ".txt$", full.names = T)

problems <- list()

for(l in logfiles){
  out <- readLines(l)
  
  out2 <- str_extract(out, "drop_count:-?[0-9]+")
  out2 <- out2[is.na(out2)==F]
  out3 <- str_extract(out2, "-?[0-9]+")
  out3 <- as.numeric(out3)
  #print(mean(out3))
  if(mean(out3)>5 & var(out3)<200){
    print(l)
    problems <- append(problems, l)
  }
}

#positive values seem to be a problem
#the problem video is 15
#everything else is <0
problems18 <- str_replace(problems, ".txt", ".wmv")
problems18 <- basename(problems18)
problems18 <- str_remove(problems18, ".wmv")
save(problems18, file = "../data/problem_videos_18.rdata")

#----
#2020

#actual
logfiles <- list.files("../results/problem_files_2020/content/results2020/", pattern = ".txt$", full.names = T)

problems <- list()

for(l in logfiles){
  out <- readLines(l)
  
  out2 <- str_extract(out, "drop_count:-?[0-9]+")
  out2 <- out2[is.na(out2)==F]
  out3 <- str_extract(out2, "-?[0-9]+")
  out3 <- as.numeric(out3)
  #print(mean(out3))
  if(mean(out3)>5 & var(out3)<200){
    print(l)
    problems <- append(problems, l)
  }
}

#positive values seem to be a problem
#the problem video is 15
#everything else is <0
problems20 <- str_replace(problems, ".txt", ".wmv")
problems20 <- basename(problems20)
problems20 <- str_remove(problems20, ".wmv")
save(problems20, file = "../data/problem_videos_20.rdata")
