# Flujo legal de armas de fuego 


## ¿Qué es esto?



TODO: write readme











## Descarcar datos y limpiar base 

Para descargar y limpiar los datos necesitas clonar este repositorio, una vez clonado ingresar y ejecutar el script de descarga:

```sh
me@compu:~$  git clone git@github.com:ildanilo/armas-de-fuego-app.git

me@compu:~$ cd armas-de-fuego-app/

me@compu:~$ Rscript src/clean_data.R 

```
## Configurar un ambiente Python para habilitar R reticulate 

Francamente resultó muy difícil hacer treemaps en Plotly usando R, creo que es más sencillo hacerlos usando Python. Por este motivo terminé usando R reticulate para hacer esta visualización. 
Para que funcione localmente es necesario configurar un ambiente dentro de este repostitorio usando virtualenv después de haberlo clonado.

```sh
me@compu:~$ sudo pip install virtualenv

me@compu:~$ virtualenv env

```
Una vez preparado el ambiente virtual, es necesario instalar Plotly para Python, para eso emplar el archivo requirements.txt

```sh
env/bin/pip install -r requirements.txt
```


