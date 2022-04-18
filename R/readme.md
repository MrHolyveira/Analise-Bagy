# Solução em R
O projeto em R foi escrito com o auxilio do [Jupyter notebooks](https://jupyter.org/).


O arquivo original Jupyter é o [main.ipynb](./main.ipynb), que pode ser executado para guardar as variáveis contendo as respostas dos insights requisitados e gerar as imagens contendo os gráficos de visualização dos insights. Também foi gerado automaticamente um arquivo [main.r](./main.r) para fácil execução fora do Jupyter notebooks.
O arquivo [main.r](./main.r) foi gerado com o seguinte comando:
```console
jupyter nbconvert .\R\main.ipynb --to script
```

Dentro da pasta encontram-se os arquivos dos plots gerados em formato jpeg para os insights de 1 a 3, como também suas análises propostas intermediárias (1.1, 3.1, 3.2, etc).