library(tidyverse)
library(data.table)

map <- tibble()
for(i in 1:8){

	filename <- paste0("data-raw/hp_", i, ".txt")
	df <- fread(filename) %>%
		dplyr::mutate(V1 = gsub(".*:","",V1) %>% as.numeric()) %>%
		dplyr::filter(!(V1 == 0 & V2 == 0 & V3 == 0)) %>%
		dplyr::mutate(movie = i) %>%
		# dplyr::group_by(V1, V2, V3, movie) %>%
		# dplyr::filter(row_number(V1) == 1) %>%
		# dplyr::arrange(V1 + V2 + V3) %>%
		data.table()

	map <- rbind(map, df)
}
hp.map <- map
save(map, file = "data/hp.map")
devtools::use_data(hp.map, internal = TRUE, overwrite = TRUE)
