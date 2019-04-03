Limpando o banco
----------------

``` r
rm(list = ls())
```

Carregando o pacote tidyverse
-----------------------------

``` r
if(!require(tidyverse)) {
  install.packages("tidyverse", dependencies = T); 
  require(tidyverse)
}
```

    ## Loading required package: tidyverse

    ## -- Attaching packages ------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.1.0     v purrr   0.3.0
    ## v tibble  2.0.1     v dplyr   0.7.8
    ## v tidyr   0.8.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.3.0

    ## -- Conflicts ---------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
if(!require(knitr)) {
  install.packages("knitr", dependencies = T); 
  require(knitr)
}
```

    ## Loading required package: knitr

``` r
if(!require(rmarkdown)) {
  install.packages("rmarkdown", dependencies = T); 
  require(rmarkdown)
}
```

    ## Loading required package: rmarkdown

Definindo o set.seed (semente)
------------------------------

``` r
set.seed(12345)
```

Questão 1
---------

### Questão 1.1

``` r
funcaoq11 <- function(x){
  
  valor <- x^2 - 5*x + 6
  
  return(valor)
  
}


runif(n = 100, min = 0, max = 4) %>%
  as_tibble() %>%
  mutate(y = seq_along(map(.x = value, .f = funcaoq11))) %>% 
  ggplot() +
  geom_line(aes(x = y, y = value)) +
  theme_bw()
```

![](Script_Tarefa0_files/figure-markdown_github/funcaoq11-1.png)

### Questão 1.2

``` r
rnorm(n = 100) %>% 
  as_tibble %>%
  ggplot(aes(x = value)) +
  geom_histogram(aes(y = ..density..),
                 colour = "black",
                 fill = "white",
                 bins = 11) +
  theme_bw() +
  stat_function(fun = dnorm, 
                args = list(mean = 0, sd = 1),
                color = "darkred",
                size = 0.7) +
  labs(x = "Valor" , y = "Densidade")
```

![](Script_Tarefa0_files/figure-markdown_github/funcaoq12-1.png)

### Questão 1.3 :: D = 1

``` r
funcaoq13 <- function(x, a, b, D){
  
  valor1 <- 1/(1 + exp(-D * a * (x - b)))
  
  return(valor1)
  
}


runif(n = 100, min = 1, max = 20) %>%
  map_dbl(~ funcaoq13(x = ., a = 1.5, b = 1, D = 1)) %>%
  as_tibble %>%
  mutate(cont = seq_along(value)) %>%
  ggplot(data = .) +
  geom_line(aes(x = cont, y = value)) +
  theme_bw()
```

![](Script_Tarefa0_files/figure-markdown_github/funcaoq131-1.png)

### Questão 1.3 :: D = 1.7

``` r
runif(n = 100, min = 1, max = 20) %>%
  map_dbl(~ funcaoq13(x = ., a = 1.5, b = 1, D = 1.7)) %>%
  as_tibble %>%
  mutate(cont = seq_along(value)) %>%
  ggplot(data = .) +
  geom_line(aes(x = cont, y = value)) +
  theme_bw()
```

![](Script_Tarefa0_files/figure-markdown_github/funcaoq132-1.png)

### Questão 1.4

``` r
rnorm(n = 100) %>% 
  as_tibble %>%
  ggplot(aes(x = value)) +
  geom_histogram(aes(y = ..density..),
                 colour = "black",
                 fill = "white",
                 bins = 11) +
  theme_bw() +
  stat_function(fun = funcaoq13, 
                aes(colour = "bla"),
                args = list(a = 1.5, b = 1, D = 1.7),
                size = 1.2, 
                alpha = 0.9) + 
  stat_function(fun = dnorm, 
                aes(colour = "blabla"),
                size = 1.2,
                alpha = 0.9) + 
  scale_colour_manual(name = "", values = c("darkred", "darkblue"), 
                      breaks = c("bla", "blabla"), 
                      labels = c("Função 1.3 \n (D = 1,7)", "Densidade \n da N(0,1)")) + 
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        axis.title.y = element_text(colour = "black"),
        axis.title.x = element_text(colour = "black"),
        axis.text = element_text(colour = "black")) +
  labs(x = "Valor", y = "Densidade")
```

![](Script_Tarefa0_files/figure-markdown_github/funcaoq14-1.png)

### Questão 1.5

``` r
funcaoq15 <- function(x, a, b, c, D){
  
  valor3 <- (c + (1 - c))/(1 + exp(-D * a * (x - b)))
  
  return(valor3)
  
}


runif(n = 100, min = 0, max = 10) %>%
  map_dbl(~ funcaoq15(x = ., a = 1.5, b = 1, c = 0.2, D = 1.7)) %>%
  as_tibble %>%
  mutate(cont = seq_along(value)) %>%
  ggplot(data = .) +
  geom_line(aes(x = cont, y = value)) +
  theme_bw()
```

![](Script_Tarefa0_files/figure-markdown_github/funcaoq15-1.png)

### Questão 1.6 (Em progresso)

Questão 2
---------

### Questão 2.1

``` r
runif(n = 1000, min = 0, max = 1) %>%
  as_tibble() %>% 
  ggplot(aes(x = value)) +
  geom_histogram(aes(y = ..density..), 
                 colour = "black", 
                 bins = 12) +
  theme_bw() +
  labs(x = "Valor", y = "Densidade")
```

![](Script_Tarefa0_files/figure-markdown_github/funcaoq21-1.png)

### Questão 2.2

``` r
rbinom(n = 1000, size = 1, prob = 0.3) %>%
  as_tibble %>%
  summarise(media = mean(value), 
            variancia = var(value))
```

    ## # A tibble: 1 x 2
    ##   media variancia
    ##   <dbl>     <dbl>
    ## 1 0.289     0.206

### Questão 2.3

``` r
rbinom(n = 10, size = 1, prob = 0.5) %>%
  as_tibble %>%
  summarise(media = mean(value), 
            variancia = var(value))
```

    ## # A tibble: 1 x 2
    ##   media variancia
    ##   <dbl>     <dbl>
    ## 1   0.7     0.233

### Questão 2.4

``` r
rnorm(n = 1000) %>%
  as_tibble %>%
  summarise(media = mean(value), 
            variancia = var(value))
```

    ## # A tibble: 1 x 2
    ##     media variancia
    ##     <dbl>     <dbl>
    ## 1 -0.0313     0.981
