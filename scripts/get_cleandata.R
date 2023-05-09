# vim set fileencoding=utf-8:

#---
# script name: get_cleandata.R
# purpose: obtains data for shiny app
# author: Daniel Mata <daniel.mata@flacso.edu.mx>
# date created: 18-12-2022
# license: (c) DM, 2022, GPL v3
#---

require(dplyr)
require(tidyr)
require(magrittr)
require(stringr)
require(priceR)

# make dirs and download data
if (dir.exists("data")){
  message("Data already downloaded")
  } else {
    # url
    firearms  <- "https://stopusarmstomexico.org/wp-content/uploads/2020/12/Armas_Policias_Mexico.xlsx"
    # download stuff
    dir.create("data/raw", recursive = TRUE)
    download.file(firearms,"data/raw/Armas_Policias_Mexico.xlsx")
    message("Ok!")
}

#--------- start processing -------------

# read
facturas <- readxl::read_excel(here::here("data/raw","Armas_Policias_Mexico.xlsx"))

# clean names and translate vars
facturas |>
       	janitor::clean_names() |>
	mutate(tipo_es=recode(tipo,
			      "Ametralladora"="Ametralladoras",
			      "Carabina"="Carabinas",
			      "Escopeta"="Escopetas",
			      "Escopeta de bomba"="Escopetas",
			      "Escopeta lanza granadas"="Lanzagranadas",
			      "Escopeta multilanzador"="Lanzagranadas",
			      "Fusil"="Rifles",
			      "Fusil de asalto"="Rifles",
			      "Fusil galil"="Rifles",
			      "Fusil sniper"="Rifles",
			      "Lanza granadas"="Lanzagranadas",
			      "Lanzador"="Lanzagranadas",
			      "Lanzador sencillo"="Lanzagranadas",
			      "Lanzador simple"="Lanzagranadas",
			      "Metralleta"="Ametralladoras",
			      "Multilanzador"="Lanzagranadas",
			      "Pistola"="Pistolas",
			      "Pistola ametralladora"="Pistolas ametralladoras",
			      "Pistola subametralladora"="Pistolas ametralladoras",
			      "Revolver"="Revólveres",
			      "Rifle"="Rifles",
			      "Rifle de asalto"="Rifles",
			      "Rifle tactico"="Rifles",
			      "Subametralladora"="Subametralladoras",
			      "Submetralleta"="Subametralladoras",
			      "testado"=NA_character_),
	       tipo_en=recode(tipo_es,
			      "Ametralladoras"="Machine gun",
			      "Carabina"="Carbine",
 			      "Escopetas"="Shotgun",
 			      "Lanzagranadas"="Grenade Launcher",
 			      "Pistolas"="Pistol",
 			      "Pistolas ametralladoras"="Machine pistol",
 			      "Revólveres"="Revolver",
 			      "Rifles"="Rifle",
 			      "Subametralladoras"="Sub-machine gun"),
	       pais_origen_empresa_en=recode(pais_origen_empresa,
					     "Alemania"="Germany",
					     "Austria"="Austria",
					     "Bélgica"="Belgium",
			     		     "Brasil"="Brazil",
			     		     "China"="China",
					     "España"="Spain",
					     "Estados Unidos"="United States",
					     "Estado Unidos"="United States",
			     		     "Israel"="Israel",
					     "Italia"="Italy",
			     		     "México"="Mexico",
		     			     "n.a."=NA_character_,
				     	     "República Checa"="Czech Republic",
			     		     "Suiza"="Switzerland",
			     		     "Turquía"="Turkey"))  |> 
  tidyr::unite('vendido_a_cliente', usuario_agencia_estatal:usuario_municipal,remove = FALSE) |>
	mutate(vendido_a_cliente=str_replace(vendido_a_cliente, "_NA|NA_", ""),
	       vendido_a_cliente=recode(vendido_a_cliente,
	                                "n.a."=NA_character_,
	                                "NA"=NA_character_,
	                                "subsria. de seg. publica y readaptacion social"="Subsecretaria de Seguridad Publica y Readaptacion Social",
	                                "Policias municipales"="Policía Municipal"),
	       vendido_a_cliente=replace_na(vendido_a_cliente,"No especificado"),
	       marca=ifelse(marca=="n.a.",NA_character_,marca),
	       pais_origen_empresa=ifelse(pais_origen_empresa=="n.a.","Sin dato",
	                                  ifelse(pais_origen_empresa=="Estado Unidos","Estados Unidos",pais_origen_empresa)),
	       calibre=ifelse(calibre=="NA","Sin dato",calibre),
	       pais_origen_empresa=replace_na(pais_origen_empresa,"Sin dato")
	       ) -> facturas

# fix some accents
facturas$vendido_a_cliente <- str_replace(facturas$vendido_a_cliente, "cion", "ción")
facturas$vendido_a_cliente <- str_replace(facturas$vendido_a_cliente, "calia", "calía")
facturas$vendido_a_cliente <- str_replace(facturas$vendido_a_cliente, "olicia", "olicía")
facturas$vendido_a_cliente <- str_replace(facturas$vendido_a_cliente, "ublica", "ública")
facturas$vendido_a_cliente <- str_replace(facturas$vendido_a_cliente, "ecretaria", "ecretaría")
facturas$vendido_a_cliente <- str_replace(facturas$vendido_a_cliente, "ecretaríado","ecretariado")
# fix typos
facturas$vendido_a_cliente <- str_replace(facturas$vendido_a_cliente, "_Policías municipales", " y Policía Municipal")
facturas$vendido_a_cliente <- str_replace(facturas$vendido_a_cliente, "Procuradoría|Procuradoria|Procuraduria", "Procuraduría")
facturas$vendido_a_cliente <- str_replace(facturas$vendido_a_cliente, "Jusiticia", "Justicia")
facturas$vendido_a_cliente <- str_replace(facturas$vendido_a_cliente, "Cuidadana","Ciudadana")
facturas$vendido_a_cliente <- str_replace(facturas$vendido_a_cliente, "Cuidadania","Ciudadana")
# set filter
armas_fuego <- c("Ametralladoras", "Carabinas","Escopetas", "Rifles", 
                 "Pistolas", "Pistolas ametralladoras","Revólveres", "Subametralladoras")

# filter data base on years and firearms types
facturas <- facturas |>
  filter(ano >= 2006 & ano <= 2018, # years between 2006 and 2018
         tipo_es %in% armas_fuego) # filter only firearms 

# dolar conversion, format dates and costs
facturas$fecha <- as.Date(facturas$fecha, tryFormats=c("%Y-%m-%d"))
facturas$costo_pesos_mex <- as.numeric(facturas$costo_pesos_mex)


# request exchange rates by day
exchange_dollar_rate <- historical_exchange_rates(from = "USD",to = "MXN",
						  start_date = min(facturas$fecha,na.rm=TRUE),
						  end_date = max(facturas$fecha,na.rm=TRUE))

# merge exchange rates and data by day
facturas <- left_join(facturas,exchange_dollar_rate, by=c("fecha"="date"))

# calculate costs in dolars
facturas$costo_usd <- facturas$costo_pesos_mex / facturas$one_USD_equivalent_to_x_MXN

# deflate prices in 2019 mexican pesos 
facturas$en_pesos_2019 <- adjust_for_inflation(facturas$costo_pesos_mex, facturas$ano, "MX", to_date = 2019)

# deflate prices in 2019 dolars
facturas$en_dolares_2019 <- adjust_for_inflation(facturas$costo_usd, facturas$ano, "US", to_date = 2019)

# arrange data
facturas |> 
	select(estado,
	       usuario_agencia_estatal,
	       usuario_municipal,
	       vendido_a_cliente,
	       fecha,
	       mes,
	       dia,
	       ano,
	       factura_no,
	       marca,
	       tipo_en,
	       tipo_es,
	       semi_auto_auto_n_a,
	       calibre,
	       pais_origen_empresa,
	       pais_origen_empresa_en,
	       no_piezas,
	       costo_pesos_mex,
	       costo_usd,
	       en_pesos_2019,
	       en_dolares_2019
	) -> facturas

# export data in csv file
write.csv(facturas,here::here("armas-de-fuego","compras_armas_final_web.csv"),
          fileEncoding = "UTF-8", row.names = FALSE)
