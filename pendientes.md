
estado actual:
- no está eliminando stopwords
- no tiene cache
- artículos del json hasta 10 mil palabras, total de aprox. 1 millón
- json viene en minúsculas

así como está ahora, no tiene cache, y carga más lento, pero busca mejor (1 millón de palabras aprox)

Si el contenido del sitio no esutviera en JSON, sino que se subiera a una base de datos...
pero eso implca un priceso de cargar el json, procesarlo y cargarlo a la base de datos
podría ser un cron o github action