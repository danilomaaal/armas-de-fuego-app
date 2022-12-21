#!/bin/bash
# vim set fileencoding=utf-8:

#---
# script name: download.sh
# purpose: download dataset
# author: Daniel Mata <daniel.mata@flacso.edu.mx>
# date created: 18-12-2022
# license: (c) DM, 2022, GPL v3
#---

#  ___________________________________
# <Be sure to run this from get_data.R>
#  -----------------------------------
#         \   ,__,
#          \  (oo)____
#             (__)    )\
#                ||--|| *

mkdir -p data/raw

if cd data/raw
then

  printf "Donwloading data...\n"

  wget "https://stopusarmstomexico.org/wp-content/uploads/2020/12/Armas_Policias_Mexico.xlsx"

  printf "OK!...\n"

else
  prinf "Sorry, something failed...\n"
fi
