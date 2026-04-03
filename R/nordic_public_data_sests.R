
#Tällä saat selettavaksi pohjoismaisen (ja baltian) julkisen hallinnon datat.
nordic_public <- pxweb_api_catalogue()
#Tämän avulla voit tehdä tilastokeskuksen datoista kysleyn, jota voi käyttää.
tilastokeskus <- pxweb_interactive("statfin.stat.fi")

#esimerkkitapaus alla
px_data <- 
  pxweb_get(url = "https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/tyonv/statfin_tyonv_pxt_12ti.px",
            query = pxweb_query_list)

# Convert to data.frame 
px_data_frame <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
dput(pxweb_query_list)
fst::write_fst(paskaa,here::here("data","raw","unemployment","tyottomat_ja_tyopaikat_by_ammatti.fst"))


px_data <- 
  pxweb_get(url = "https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/tyonv/statfin_tyonv_pxt_12tj.px",
            query = pxweb_query_list)

# Convert to data.frame 
px_data_frame <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")