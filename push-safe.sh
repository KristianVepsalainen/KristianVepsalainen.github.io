#!/usr/bin/env bash
# =============================================================================
# push-safe.sh – vie kaikki muutokset (myös data/) GitHubiin varmasti läpi.
#
# Miksi aiemmat yritykset kaatuivat: 136 Mt hilma_index.qs2 on edelleen
# PUSHAAMATTOMASSA COMMITISSA. Tiedoston poistaminen levyltä ei poista sitä
# historiasta, ja GitHub tarkistaa jokaisen pushattavan commitin.
#
# Tämä skripti:
#   1. pilkkoo isot datatiedostot (R/hilma/hilma-migrate-split.R)
#   2. pysähtyy, jos työkansiossa on yhäkin yli 95 Mt tiedosto
#   3. purkaa pushaamattomat commitit (tiedostot jäävät levylle ennalleen)
#   4. commitoi muutokset alle 1,5 Gt erissä (GitHubin pushiraja on 2 Gt)
#   5. tarkistaa ennen jokaista pushia JOKAISEN pushattavan objektin koon
#      samalla tavalla kuin GitHub, ja pushaa vasta sitten
#
# Aja projektin juuresta:  bash push-safe.sh
# =============================================================================
set -euo pipefail

LIMIT_MB=95
BATCH_MB=1500
BRANCH=main

die() { echo "VIRHE: $*" >&2; exit 1; }

# --- 0. Oikea kansio ---------------------------------------------------------
git rev-parse --is-inside-work-tree >/dev/null 2>&1 \
  || die "Tämä ei ole git-kansio. Siirry projektin juureen (cd ~/R-koodi/KristianVepsäläinen)."
cd "$(git rev-parse --show-toplevel)"
echo "Projekti: $(pwd)"

git fetch origin
git rev-parse --verify "origin/$BRANCH" >/dev/null || die "origin/$BRANCH puuttuu."

# --- 1. Isot datatiedostot pilkotuiksi ----------------------------------------
if [ -f R/hilma/hilma-repair.R ]; then
  echo "== Tarkistetaan ja korjataan Hilma-tiedostot =="
  Rscript -e 'source(here::here("R", "hilma", "hilma-repair.R"))'
fi

# --- 2. Ei yhtään yli rajan olevaa tiedostoa työkansiossa ---------------------
echo "== Tarkistetaan tiedostokoot =="
# Vain tiedostot, jotka git oikeasti veisi (.gitignore huomioidaan, raw-kansiot ohitetaan)
BIG=""
while IFS= read -r -d '' f; do
  [ -f "$f" ] || continue
  [ "$(stat -c %s "$f")" -gt $((LIMIT_MB * 1000000)) ] && BIG+="$f"$'\n'
done < <(git ls-files -z --cached --others --exclude-standard)
if [ -n "$BIG" ]; then
  echo "Seuraavat tiedostot ovat yli ${LIMIT_MB} Mt:"
  printf '%s' "$BIG" | while IFS= read -r f; do du -h "$f"; done
  die "Pilko tai poista nämä ennen pushia (esim. tutkihankintojen raakatiedostot poistuvat, kun data-prep vaihe 6 on ajettu loppuun)."
fi

# --- 3. Pushaamattomat commitit pois historiasta (tiedostot säilyvät) ---------
AHEAD=$(git rev-list --count "origin/$BRANCH..HEAD")
echo "== Pushaamattomia committeja: $AHEAD – puretaan ne =="
git reset --soft "origin/$BRANCH"
git reset -q                       # kaikki muutokset takaisin "lisäämättömiksi"

# --- 4. Muutetut tiedostot listaksi -----------------------------------------
mapfile -d '' FILES < <(git ls-files -z --modified --deleted --others --exclude-standard | sort -zu)
[ "${#FILES[@]}" -gt 0 ] || { echo "Ei muutoksia. Valmis."; exit 0; }
echo "Muutettuja tiedostoja: ${#FILES[@]}"

# Tarkistus, joka vastaa GitHubin omaa: jokainen pushattava objekti alle rajan
check_outgoing() {
  local max
  max=$(git rev-list --objects "origin/$BRANCH..HEAD" \
        | git cat-file --batch-check='%(objecttype) %(objectsize) %(rest)' \
        | awk '$1=="blob" && $2 > m {m=$2; f=$0; sub(/^[^ ]+ [^ ]+ /, "", f)} END {printf "%d %s", m/1e6, f}')
  local mb=${max%% *}
  echo "   suurin pushattava tiedosto: ${max#* } (${mb} Mt)"
  [ "${mb:-0}" -lt "$LIMIT_MB" ] || die "Pushattava tiedosto on liian iso: ${max#* }"
}

push_batch() {
  local n=$1
  git commit -q -m "Data ja sisältö (erä $n)"
  check_outgoing
  git push origin "HEAD:$BRANCH"
  git fetch -q origin
}

# --- 5. Erissä committiin ja pushiin ------------------------------------------
batch=()
size=0
n=1
for f in "${FILES[@]}"; do
  s=0
  [ -f "$f" ] && s=$(( $(stat -c %s "$f") / 1000000 ))
  if [ ${#batch[@]} -gt 0 ] && [ $(( size + s )) -gt $BATCH_MB ]; then
    printf '%s\0' "${batch[@]}" | git add -A --pathspec-from-file=- --pathspec-file-nul
    echo "== Erä $n: ${#batch[@]} tiedostoa, ~${size} Mt =="
    push_batch $n
    n=$((n + 1)); batch=(); size=0
  fi
  batch+=("$f"); size=$((size + s))
done
if [ ${#batch[@]} -gt 0 ]; then
  printf '%s\0' "${batch[@]}" | git add -A --pathspec-from-file=- --pathspec-file-nul
  echo "== Erä $n: ${#batch[@]} tiedostoa, ~${size} Mt =="
  push_batch $n
fi

git status --short | head
echo "Valmis: kaikki muutokset ovat GitHubissa."
