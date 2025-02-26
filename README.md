# sonata <a href="https://usaid-mozambique.github.io/sonata/"><a href="https://usaid-mozambique.github.io/sonata/"><img src="man/figures/logo.png" align="right" height="175" alt="sonata website" /></a>

Interagir com MozART 2.0

<!-- badges: start -->
[![R-CMD-check](https://github.com/usaid-mozambique/sonata/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/usaid-mozambique/sonata/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![sonata status badge](https://usaid-mozambique.r-universe.dev/badges/sonata)](https://usaid-mozambique.r-universe.dev/sonata)
[![:name status badge](https://usaid-mozambique.r-universe.dev/badges/:name)](https://usaid-mozambique.r-universe.dev/)
<!-- badges: end -->

## Resumo

O MozART 2.0 é um recurso rico em informações sobre serviços de cuidados e tratamento do HIV prestados em Moçambique. O `sonata` foi criado para facilitar o trabalho dos analistas na resposta a questões analíticas e na geração de quadros de dados agregados para utilização fora do MozART 2.0.  

## Instalação

`sonata` não está no CRAN e precisa ser instalado diretamente do rOpenSci usando o código abaixo.

``` r
    # instalar a partir de rOpenSci
      install.packages("sonata", repos = c('https://usaid-mozambique.r-universe.dev', 'https://cloud.r-project.org'))
    
    # carregar pacote
      library(sonata)
      
    # Listar funções do pacote
      ls("package::sonata")
    
```

---

*Disclaimer: The findings, interpretation, and conclusions expressed herein are those of the authors and do not necessarily reflect the views of United States Agency for International Development. All errors remain our own.*
