# Seminário-R
Dados e scripts para o seminário. Na pasta [data](https://github.com/HyperGryphon/Seminario-R/tree/main/data) estão os arquivos com os dados que usaremos no seminário. Na pasta [scripts](https://github.com/HyperGryphon/Seminario-R/tree/main/scripts) estão os códigos. Estos últimos serão atualizados às versões finais após o seminário.

## Requerimentos
* Instalar R baixando o arquivo [daqui](https://cloud.r-project.org/bin/windows/base/R-4.1.3-win.exe)
* Instalar Rtools baixando o arquivo [daqui](https://cloud.r-project.org/bin/windows/Rtools/rtools40.html)
* Instalar RStudio [aqui](https://download1.rstudio.org/desktop/windows/RStudio-2022.02.1-461.exe)
* Entrar na pasta [scripts](https://github.com/HyperGryphon/Seminario-R/tree/main/scripts), baixar ou copiar o script [check_packages.R](https://github.com/HyperGryphon/Seminario-R/blob/main/scripts/check_packages.R), e rodar ele. Para rodar cliquem ctrl+A e depois ctrl+Enter. Esse arquivo comprovará se os pacotes necessários estão instalados e se não instalará eles.

## Instalação de pacotes
Para instalar novos pacotes usem a função ``install.packages('nome-do-pacote')``. Atenção as maiúsculas nos nomes dos pacotes pois pode dar erro se não estiverem certos.

## Carregar pacotes
Para carregar pacotes usem a função ``library('nome-do-pacote')``. De novo, atenção às maiúsculas.

Também podem usar pacotes instalados sem precisar carregar eles usando a sintaxis ``nome-do-pacote::função-do-pacote``.

