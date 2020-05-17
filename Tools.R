#Função que recupera um percentual do dataset como amostra
sample_file <- function(file,percent,qtde_linha_arquivo){
  linhas_amostradas <- sample(1:qtde_linha_arquivo,round(percent*qtde_linha_arquivo))
  data <- fread(file,stringsAsFactors = F)
  return(data[linhas_amostradas])
}

# Função para os Density Plots
plot.scatter <- function(X,target,dt){ 
  ggplot(dt, aes_string(x = X, y = target)) + 
    geom_point(aes_string(colour = target), alpha = 0.1) + 
    scale_colour_gradient(low = "green", high = "blue") + 
    geom_smooth(method = "loess") + 
    theme(text = element_text(size = 20)) 
}