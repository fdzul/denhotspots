# Simulated geocoded dengue cases of the metropolitan area of Guadalajara

\*\*Simulated\*\* geocoded dengue cases (2019-2024) of Guadalajara,
Zapopan, San Pedro Tlaquepaque and Tonalá (Jalisco), with the columns of
the geocoded dataset of SINAVE. They are not real cases: they were made
to show the functions of the package (persistent hotspots, space-time
interaction, transmission chains, intensity of cases) without publishing
surveillance data.

## Usage

``` r
sim_dengue
```

## Format

A data frame with 5,502 rows (cases) and 15 columns:

- VEC_ID:

  identifier of the case (simulated).

- ANO:

  year of onset of symptoms.

- SEM:

  epidemiological week of onset of symptoms.

- FEC_INI_SIGNOS_SINT:

  date of onset of symptoms.

- IDE_EDA_ANO:

  age in years.

- IDE_SEX:

  sex (1 man, 2 woman).

- CVE_EDO_RES, DES_EDO_RES:

  state of residence (14, Jalisco).

- CVE_MPO_RES, DES_MPO_RES:

  municipality of residence.

- ESTATUS_CASO:

  status of the case (1 probable, 2 confirmed).

- DES_DIAG_FINAL:

  final diagnosis (dengue no grave, con signos de alarma, grave).

- DENGUE_SER_TRIPLEX:

  serotype (DENV-1 to DENV-4) of some confirmed cases.

- long, lat:

  coordinates of the residence (WGS84).

## Source

Simulated with \`data-raw/sim_dengue.R\`; population of Kontur
(popmex::hdx_pop2023) and localities of INEGI 2019 (rgeomex).

## Details

The cases combine three processes: background cases placed in the 400 m
H3 hexagons of Kontur 2023 (popmex) in proportion to their population;
six persistent foci active every year; and transmission chains
(secondary cases within 300 m and 3 to 18 days of a primary case). The
onset of symptoms follows a seasonal curve with the peak in the
epidemiological weeks 36 to 42. The script that makes them is
\`data-raw/sim_dengue.R\`.

## Examples

``` r
head(sim_dengue)
table(sim_dengue$ANO)
```
