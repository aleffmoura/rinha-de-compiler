# Tree-Walking F#

Implementação de Tree-Walking em F# para a [Rinha de Compiler]().

## Como executar

### Usando Docker

A imagem está configurada para executar acetar um parametro contendo o local de uma pasta e executar todos `*.rinha.json` e imprimir o resultado 
no console.
Caso local não exista ele pega os arquivos padrões dentro da imagem, localizados em /app/src_json

Exemplo:
```
docker build -t rinha-de-interpreter-fsharp .
docker run rinha-de-interpreter-fsharp "/var/rinha"
```