# Flujo legal de armas de fuego 


## ¿Qué es esto?

Código para generar una Shiny app que permite visualizar datos sobre transferencias legales de armas en México. Los datos aquí presentados fueron generados a partir de la revisión de facturas de transferencias de armas de fuego y municiones de la Secretaría de la Defensa Nacional (SEDENA) a las autoridades estatales durante el periodo 2006-2018.

![Sankey](assets/sankey.png)

La documentación fue obtenida a través de la solicitud de información *#0000700176018*. Tanto la base de datos como la metodología con la que se construyó se encuentran disponibles para descarga [aquí](https://www.stopusarmstomexico.org/police-firearms-database).

![Treemap](assets/treemap.png)


## Preparar entorno

Para correr la app locamente se pueden emplear las recetas del makefile incluido en el repositorio. Primero es necesario descargar los datos, para ello ejecutar:

```sh
make getdata
```
También necesitas tener instalado [reticulate](https://rstudio.github.io/reticulate/), además un ambiente Python e instalar algunas dependencias. Para hacer esto usar:

```sh
make env
```
Finalmente para correr la app:

```sh
make run
```
