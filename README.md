# bter-api

## Description

**beter-api** is an Emacs library for working with the Bter API. Bter.com is a
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
