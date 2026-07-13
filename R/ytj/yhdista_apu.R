# Turvallinen minimipäivämäärä: palauttaa NA:n tyhjälle vektorille sen sijaan
# että min() antaisi Inf + varoituksen. Käytetään lakkaamispäivän poiminnassa.
min_pvm <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) as.Date(NA) else min(x)
}

# Cramérin V efektikokona khiin neliö -testistä (ryhmäerojen voimakkuus).
cramers_v <- function(chi) {
  n <- sum(chi$observed)
  sqrt(as.numeric(chi$statistic) / (n * (min(dim(chi$observed)) - 1)))
}

# Rank-biserial-korrelaatio Mann–Whitneyn U-testistä (efektikoko).
rank_biserial <- function(x, y) {
  wt <- suppressWarnings(wilcox.test(x, y))
  U  <- as.numeric(wt$statistic)
  1 - 2 * U / (length(x) * length(y))
}

# Kokoaa survfit-olion siistiksi tibbleksi ggplotia varten ilman lisäriippuvuuksia
# (ei survminer). Palauttaa ajan, eloonjäämistodennäköisyyden ja luottamusvälin.
km_tidy <- function(fit) {
  if (is.null(fit$strata)) {
    tibble::tibble(aika = fit$time, surv = fit$surv,
                   lower = fit$lower, upper = fit$upper, ryhma = "Kaikki")
  } else {
    tibble::tibble(aika = fit$time, surv = fit$surv,
                   lower = fit$lower, upper = fit$upper,
                   ryhma = rep(names(fit$strata), fit$strata))
  }
}
