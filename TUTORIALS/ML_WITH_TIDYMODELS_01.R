# MACHINE LEARNING WITH TIDYMODELS
# POSIT::CONF 2024 WORKSHOPS
# https://workshops.tidymodels.org/
# INTRODUCTION TO TIDYMODELS
# 1 - INTRODUCTION
# https://workshops.tidymodels.org/slides/intro-01-introduction.html#/title-slide

# 📦 INSTALL PACKAGES -----------------------------------------------------

pkgs <- 
  c("bonsai", "Cubist", "doParallel", "earth", "embed", "finetune", 
    "forested", "lightgbm", "lme4", "parallelly", "plumber", "probably", 
    "ranger", "rpart", "rpart.plot", "rules", "splines2", "stacks", 
    "text2vec", "textrecipes", "tidymodels", "vetiver")

install.packages(pkgs)
