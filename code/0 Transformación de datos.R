# Función que crea una base de datos (BD) y un libro de variables (LV)
# a partir de un archivo de SPSS (sav) que guarda una tabla de datos.
sav2BDyLV <- function(in.sav, # nombre archivo (entrada) sav, e.g. data.sav
                      out.BD, # nombre archivo (salida) BD, e.g. data.xlsx
                      out.LV  # nombre archivo (salida) LV, e.g. codebook.xlsx
) {
  # Revisa si los paquetes requeridos están instalados
  require(haven)
  require(codebook)
  require(webshot)
  require(openxlsx)
  # Revisa si PhantomJS está instalado en el computador
  if (!is_phantomjs_installed()) {
    stop("Instale PhantomJS. Ejecute 'webshot::install_phantomjs()'")}
  # Carga los datos
  datos <- haven::read_sav(file = in.sav)
  # Crea una base de datos
  openxlsx::write.xlsx(x = as.data.frame(datos),
                       file = out.BD,
                       sheetName = "Datos",
                       keepNA = TRUE)
  # Crea un libro de variables
  libro <- as.data.frame(codebook::codebook_table(datos))
  columnas.hoja.1 <- c("name", "label", "n_missing",
                       "complete_rate", "n_value_labels", "format.spss")
  columnas.hoja.2 <- c("name", "label", "value_labels")
  hoja.1 <- as.data.frame(libro[, columnas.hoja.1])
  hoja.2 <- as.data.frame(libro[, columnas.hoja.2])
  openxlsx::write.xlsx(x = list(hoja.1, hoja.2),
                       file = out.LV,
                       sheetName = c("Variables", "Categor?as"),
                       keepNA = TRUE)
}
#
# 
# Obtener un listado de archivos .sav de la carpeta
setwd("./data")
for (nombre.sav in list.files(pattern = ".sav")) {
  nombre.BD <- sub(pattern = ".sav$",
                   replacement = ".xlsx",
                   x = nombre.sav)
  nombre.LV <- sub(pattern = "^Base-de-Datos",
                   replacement = "Libro-de-Variables",
                   x = nombre.BD)
  sav2BDyLV(in.sav = nombre.sav,
            out.BD = nombre.BD,
            out.LV = nombre.LV)
}
setwd("..")