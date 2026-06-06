
# Buscador de mi blog

Esta app la hice a mano porque encontré demasiado engorroso agregar un motor de búsqueda a un blog Hugo. Como su nombre lo indica, es un buscador de publicaciones progrmado con R.

[Accede al buscador aquí.](https://bastianoleah.shinyapps.io/buscador/)

Es una [aplicación Shiny](https://bastianolea.rbind.io/tags/shiny) muy básica que lee el JSON de mi sitio personal, transforma el JSON a una tabla, y luego busca el texto insertado por la/el usuario/a y retorna HTML con los resultados.

La búsqueda se hace con el [algoritmo BM25 implementado en el paquete {rbm25}](https://davzim.github.io/rbm25/), que entrega un puntaje a cada texto en base a la relevancia con respecto al término de búsqueda. Para ello se requiere limpiar el texto previamente (pasar a minúsculas, eliminar puntuación, y [eliminar stopwords](https://github.com/quanteda/stopwords)).

La app [guarda un cache](https://bastianolea.rbind.io/blog/shiny_optimizar/) de los datos del sitio cada 6 horas para no sobrecargar el servidor.

## Actualizaciones
- 2026-06-06: algoritmo de búsqueda mejorado (BM25), que ordena resultados por relevancia y considerando también su fecha de publicación
- 2025-12-02: ahora los resultados muestran el texto de resumen de cada post

## Recursos
- https://aaronluna.dev/blog/add-search-to-static-site-lunrjs-hugo-vanillajs/