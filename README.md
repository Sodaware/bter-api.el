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


### Public API

The API functions are mapped as follows:

API Method           | Local Function
---------------------|--------------------------------------
pairs                | bter-api-get-pairs
marketinfo           | bter-api-get-all-market-info
                     | bter-api-get-market-info
marketlist           | bter-api-get-all-market-details
                     | bter-api-get-market-details


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
