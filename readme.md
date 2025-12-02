
# Buscador de mi blog

Esta app la hice a mano porque encontré demasiado engorroso agregar un motor de búsqueda a un blog Hugo. 

[Accede al buscador aquí.](https://bastianoleah.shinyapps.io/buscador/)

Es una [aplicación Shiny](https://bastianolea.rbind.io/tags/shiny) muy básica que lee el XML de mi sitio personal, transforma el XML a una tabla, y luego busca el texto insertado por la/el usuario/a y retorna HTML con los resultados.

La app [guarda un cache](https://bastianolea.rbind.io/blog/shiny_optimizar/) de los datos del sitio cada 1 hora para no sobrecargar el servidor.

## Actualizaciones
- 2025-12-02: ahora los resultados muestran el texto de resumen de cada post

## Recursos
- https://aaronluna.dev/blog/add-search-to-static-site-lunrjs-hugo-vanillajs/