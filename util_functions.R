clearConsole = clc = function(){
  # Clear console messages
  cat( "\014" )
}

clearPlots = function () {
  # Clear plots
  if( dev.cur() > 1 ) dev.off()
}

clearWorkspace = function () {
  # Clear global workspace
  rm( list = ls( envir = globalenv() ), envir = globalenv() )
}

clearAll = function(){
  # Clear console, plots, and workspace
  clearConsole()
  clearPlots()
  clearWorkspace()
}

library(skimr)
sink("outputlm.txt")
df_wider %>% skimr::skim()
sink()


options(max.print=100000) 
options(max.print=1000) 

options('tibble.print_max' = 10000)

getOption(tibble.print_max)

lm2_coeff_tb %>% print(n=Inf)
