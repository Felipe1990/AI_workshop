library(lubridate)
library(dplyr)

incaut <- read.csv("data/incautacion_2019.csv", encoding = "UTF-8", stringsAsFactors = FALSE) %>%
  transmute(clase = X.U.FEFF.CLASE_BIEN, 
            depto = DEPTO_HECHO,
            mpio = MUNICIPIO_HECHO,
            fecha = mdy_hms(FECHA_HECHO),
            sitio = CLASE_SITIO, 
            dia = DIA_SEMANA,
            unidad = BIEN_UNIDAD_MEDIDA,
            cantidad = Droga.Incautada..Cantidad) %>%
  filter(mpio == "BOGOTÁ D.C. (CT)")


write.csv(incaut, "data/incautacion_bogota.csv", row.names = FALSE)
