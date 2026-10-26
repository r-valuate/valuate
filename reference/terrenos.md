# Conjunto de datos de terrenos

Este dataset contiene información sobre terrenos en venta, incluyendo el
precio, moneda, tipo de valor, superficie y situación jurídica.

## Usage

``` r
terrenos
```

## Format

Un data frame con las siguientes columnas:

- id:

  Identificador único del terreno.

- valor:

  Valor total del terreno en la moneda especificada.

- moneda:

  Moneda en la que está expresado el valor del terreno (por ejemplo,
  "Dólar (USD)").

- tipo_valor:

  Tipo de valor registrado (por ejemplo, "Oferta").

- superficie:

  Superficie total del terreno en metros cuadrados.

- situacion_juridica:

  Estado legal del terreno (por ejemplo, "Con Escritura", "Pre venta").

- valor_m2:

  Valor por metro cuadrado del terreno.

- geom:

  Coordenadas del terreno en el sistema de referencia utilizado.

## Source

Datos generados a partir de anuncios de terrenos en venta.
