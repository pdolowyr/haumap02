
# PackageCheck
# PD
# 2023-02-10
# installs and loads required packages and returns a list of their names

# https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
PackageCheck<-function(libs, repos) {
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need, repos = repos)
    lapply(need,require,character.only=TRUE)
  }
}
