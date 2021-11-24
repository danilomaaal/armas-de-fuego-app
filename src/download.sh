#!/bin/bash

# this script downloads data on mexican police firearms, be sure to execute it from Rscript

if cd data/raw

then

  printf "Donwloading data...\n"

  wget "https://stopusarmstomexico.org/wp-content/uploads/2020/12/Armas_Policias_Mexico.xlsx"

  printf "OK!...\n"

else
  prinf "Sorry somtheing failed... :( \n"
fi
