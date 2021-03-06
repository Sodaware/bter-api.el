# bter-api

## bter.com has closed. this library is no longer functional

## Description

**beter-api** is an Emacs library for working with the Bter API. Bter.com was a
Bitcoin and crypto-currency exchange platform.

It's not a particularly useful extension on its own, but can be used to build
something more interesting.


## Library Functions

The Bter API contains both public and private endpoints. Public endpoints can be
accessed by any client, whereas private endpoints require an authorization key
and secret token.

Online documentation for the API can be found here:
https://bter.com/api


### API Functions

The API functions are mapped as follows:

API Method           | Local Function
---------------------|--------------------------------------
pairs                | bter-api-get-pairs
marketinfo           | bter-api-get-all-market-info
marketlist           | bter-api-get-all-market-details
tickers              | bter-api-get-tickers
ticker               | bter-api-get-ticker
depth                | bter-api-get-depth
trade                | bter-api-get-trades

The Emacs library also adds the following helper functions:

* `bter-api-valid-pair-p`
* `bter-api-get-market-info`
* `bter-api-get-market-details`


#### bter-api-get-pairs

Get a list of all trading pairs supported by the Bter platform. Returns a list
of strings containing pairs in the form of "<from>_<to>", such as
"btc_usd".

```el
(bter-api-get-pairs)
=> ("btc_usd" "btc_cny" *152 more*)
```

#### bter-api-valid-pair-p *pair*

Verify that PAIR is a valid trading pair.

```el
(bter-api-valid-pair-p "btc_usd")
=> t

(bter-api-valid-pair-p "inv_inv")
=> nil
```

#### bter-api-get-all-market-info

Get a list of all market fees, minimum amounts and decimal places. Returns a
list of alists containing market information.

Each item in the list contains the following keys:

| Key               | Description
|-------------------|-------------------------------------------
| `:pair`           | The pair identifier as a string
| `:decimal-places` | The number of decimal places in this pair's price
| `:min-amount`     | The minimal currency amount for this pair
| `:fee`            | The fee for trading this pair

```el
(bter-api-get-all-market-info)
=> (((:pair . "btc_usd")
     (:decimal-places . 3)
     (:min-amount . 0.0001)
     (:fee . 0.2))
     *more pairs*)
```

#### bter-api-get-market-info *market*

Get the market fees, minimum amounts and decimal places for MARKET. Returns an
alist containing details for *MARKET* in the same format as
`bter-api-get-all-market-info`.

```el
(bter-api-get-market-info "btc_usd")
=> ((:pair . "btc_usd")
    (:decimal-places . 3)
    (:min-amount . 0.0001)
    (:fee . 0.2))
```

#### bter-api-get-tickers

Get details for all tickers. Returns a list of alists, each containing the
following fields:

| Key               | Description
|-------------------|-------------------------------------------
| `:last`           | The last exchange value 
| `:high`           | The high value for this pair
| `:low`            | The low value for this pair
| `:average`        | The average value for this pair
| `:sell`           | The sell price for this pair
| `:buy`            | The buy price for this pair
| `:volume-from`    | The trade volume for the "FROM" currency
| `:volume-to`      | The trade volume for the "TO" currency

```el
(bter-api-get-tickers)
=> (((:last . 386)
     (:high . 414.001)
     (:low . 360)
     (:average . 392.842)
     (:sell . 393.88)
     (:buy . 369.011)
     (:volume-from . 1.1309)
     (:volume-to . 444.259))
    *more*)
```



## Licence

Copyright (C) 2014 Phil Newton

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
Street, Fifth Floor, Boston, MA 02110-1301, USA.
