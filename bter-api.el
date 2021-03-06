;;; bter-api.el --- Library for working with the Bter.com API

;; Copyright (C) 2014 Phil Newton

;; Author: Phil Newton <phil@sodaware.net>
;; Version: 0.1.0
;; Package-Requires: ((json "1.2"))
;; Keywords: bter crypto bitcoin litecoin dogecoin

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; For more information on the API, see https://bter.com/api/

;;; Code:

;; Dependencies

(require 'json)
(require 'url-http)

(defvar url-http-end-of-headers)

;; Configuration

(defconst bter-api-endpoint "http://data.bter.com/api/1/")


;; Main query methods

(defun bter-api-get-pairs ()
  "Get a list of all trading pairs supported by the Bter platform."
  (append (bter-api--get "pairs") nil))

(defun bter-api-valid-pair-p (pair)
  "Verify that PAIR is a valid trading pair."
  (let ((pairs (bter-api-get-pairs)))
    (member-ignore-case pair pairs)))

(defun bter-api-get-all-market-info ()
  "Get a list of all market fees, minimum amounts and decimal places."
  (let* ((market-info (bter-api--get "marketinfo"))
         (pairs (assoc-default 'pairs market-info)))
    (mapcar 'bter-api--convert-market-info pairs)))

(defun bter-api-get-market-info (market)
  "Get the market fees, minimum amounts and decimal places for MARKET."
  (let ((market-info (bter-api-get-all-market-info)))
    (bter-api--find-market market market-info)))

(defun bter-api-get-all-market-details ()
  "Get detailed information about all markets."
  (let* ((response (bter-api--get "marketlist"))
         (markets (assoc-default 'data response)))
    (mapcar 'bter-api--convert-market-details markets)))

(defun bter-api-get-market-details (market)
  "Get the market details for MARKET."
  (let ((market-info (bter-api-get-all-market-details)))
    (bter-api--find-market market market-info)))

(defun bter-api-get-tickers ()
  "Get details for all tickers."
  (let* ((ticker-info (bter-api--get "tickers"))
         (result))
    (dolist (pair ticker-info result)
      (push (bter-api--convert-tickers pair) result))))

(defun bter-api-get-ticker (from to)
  "Get the ticker for the FROM/TO currency pair."
  (let* ((pair-name (downcase (format "%s_%s" from to)))
         (response (bter-api--get (concat "ticker/" pair-name))))
    (if (string= "true" (assoc-default 'result response))
        (bter-api--convert-ticker response from to)
      (error (assoc-default 'message response)))))

(defun bter-api-get-depth (from to)
  "Get the trading depth data for the FROM/TO currency pair."
  (let* ((pair-name (downcase (format "%s_%s" from to)))
         (response (bter-api--get (concat "depth/" pair-name))))
    (if (string= "true" (assoc-default 'result response))
        `((:asks . ,(mapcar 'bter-api--convert-depth (assoc-default 'asks response)))
          (:bids . ,(mapcar 'bter-api--convert-depth (assoc-default 'bids response))))
      (error (assoc-default 'message response)))))

(defun bter-api-get-trades (from to &optional since)
  "Get latest trades for FROM/TO currency pair, optionally limited to after SINCE."
  (let* ((pair-name (bter-api--get-pair-name from to))
         (uri (if (null since)
                  (format "trade/%s" pair-name)
                (format "trade/%s/%s" pair-name since)))
         (response (bter-api--get uri)))
    `((:elapsed . ,(bter-api--string-to-number (assoc-default 'elapsed response)))
      (:trades . ,(mapcar 'bter-api--convert-trade-data (assoc-default 'data response))))))


;; Internal helpers

(defun bter-api--get (path &optional query-vars)
  "Generate a uri using PATH and optional QUERY-VARS and retrieve the result."
  (bter-api--get-uri (bter-api--create-endpoint path query-vars)))

(defun bter-api--create-endpoint (path &optional query-vars)
  "Build an endpoint to the api using PATH and optional QUERY-VARS."
  (format "%s%s/%s"
          bter-api-endpoint
          path
          (bter-api--build-query query-vars)))

(defun bter-api--string-to-number (value)
  "Safely convert VALUE to a number.  Works if VALUE is a string or number."
  (if (stringp value)
      (string-to-number value)
    value))

(defun bter-api--build-query (query-vars)
  "Build a query string using QUERY-VARS.

QUERY-VARS should be a list of symbols and their corresponding
values.

For example (:key value :other-key value) will generate
the following string: key=value&other-key=value"
  (if (null query-vars)
      ""
    (progn (let (query-string)
             (dolist (var query-vars)
               (if (symbolp var)
                   (setq query-string (concat query-string (substring (symbol-name var) 1) "="))
                 (setq query-string (format "%s%s&" query-string var))))
             (concat "?" (substring query-string 0 -1))))))

(defun bter-api--get-uri (uri)
  "Fetch the contents of URI and return as JSON."
  (with-current-buffer (url-retrieve-synchronously uri)
    (goto-char (point-min))
    (goto-char url-http-end-of-headers)
    (prog1 (json-read)
      (kill-buffer))))

(defun bter-api--find-market (market market-data)
  "Find MARKET in MARKET-DATA."
  (let ((result))
    (dolist (m market-data)
      (when (string= market (assoc-default :pair m))
        (setq result m)))
    result))

(defun bter-api--get-pair-name (from to)
  "Create a pair name for the FROM/TO currency pair."
  (replace-regexp-in-string ":" "" (downcase (format "%s_%s" from to))))


;; Data conversion

(defun bter-api--convert-market-info (market)
  "Convert marketinfo json for MARKET into an assoc list."
  (let ((pair-name (caar market))
        (pair-data (cdar market)))
    `((:pair . ,(symbol-name pair-name))
      (:decimal-places . ,(assoc-default 'decimal_places pair-data))
      (:min-amount . ,(assoc-default 'min_amount pair-data))
      (:fee . ,(assoc-default 'fee pair-data)))))

(defun bter-api--convert-market-details (market)
  "Convert the json representation of MARKET into Lisp friendly assoc list."
  `((:number . ,(assoc-default 'no market))
    (:symbol . ,(assoc-default 'symbol market))
    (:name . ,(assoc-default 'name market))
    (:name-cn . ,(assoc-default 'name_cn market))
    (:pair . ,(assoc-default 'pair market))
    (:rate . ,(bter-api--string-to-number (assoc-default 'rate market)))
    (:volume-a . ,(assoc-default 'vol_a market))
    (:volume-b . ,(bter-api--string-to-number (assoc-default 'vol_b market)))
    (:currency-a . ,(assoc-default 'curr_a market))
    (:currency-b . ,(assoc-default 'curr_b market))
    (:currency-suffix . ,(assoc-default 'curr_suffix market))
    (:rate-percent . ,(bter-api--string-to-number (assoc-default 'rate_percent market)))
    (:trend . ,(assoc-default 'trend market))
    (:supply . ,(assoc-default 'supply market))
    (:market-cap . ,(bter-api--string-to-number (assoc-default 'marketcap market)))))

(defun bter-api--convert-tickers (ticker)
  "Convert tickers json info for TICKER into an assoc list."
  (let* ((pair-name (car ticker))
         (pair-data (cdr ticker))
         (symbols (split-string (symbol-name pair-name) "_"))
         (from-symbol (intern (concat "vol_" (car symbols))))
         (to-symbol (intern (concat "vol_" (cadr symbols)))))
    `((:pair . ,(symbol-name pair-name))
      (:last . ,(assoc-default 'last pair-data))
      (:high . ,(assoc-default 'high pair-data))
      (:low . ,(assoc-default 'low pair-data))
      (:average . ,(assoc-default 'avg pair-data))
      (:sell . ,(assoc-default 'sell pair-data))
      (:buy . ,(assoc-default 'buy pair-data))
      (:volume-from . ,(bter-api--string-to-number (assoc-default from-symbol pair-data)))
      (:volume-to . ,(bter-api--string-to-number (assoc-default to-symbol pair-data))))))

(defun bter-api--convert-ticker (ticker from to)
  "Convert json info for TICKER for FROM/TO pair into an assoc list."
  (let* ((from-symbol (intern (concat "vol_" from)))
         (to-symbol (intern (concat "vol_" to))))
    `((:last . ,(assoc-default 'last ticker))
      (:high . ,(assoc-default 'high ticker))
      (:low . ,(assoc-default 'low ticker))
      (:average . ,(assoc-default 'avg ticker))
      (:sell . ,(assoc-default 'sell ticker))
      (:buy . ,(assoc-default 'buy ticker))
      (:volume-from . ,(bter-api--string-to-number (assoc-default from-symbol ticker)))
      (:volume-to . ,(bter-api--string-to-number (assoc-default to-symbol ticker))))))

(defun bter-api--convert-depth (depth)
  "Convert json info for DEPTH data."
  `((:price . ,(elt depth 0))
    (:amount . ,(elt depth 1))))

(defun bter-api--convert-trade-data (trade-data)
  "Convert json info for TRADE-DATA."
  `((:date . ,(string-to-number (assoc-default 'date trade-data)))
    (:price . ,(assoc-default 'price trade-data))
    (:amount . ,(assoc-default 'amount trade-data))
    (:tid . ,(bter-api--string-to-number (assoc-default 'tid trade-data)))
    (:type . ,(bter-api--symbolify-trade-type (assoc-default 'type trade-data)))))

(defun bter-api--symbolify-trade-type (type)
  "Convert TYPE from a quoted value to a symbol."
  (cond ((equal 'sell type) :sell)
        ((string= "sell" type) :sell)
        ((equal 'buy type) :buy)
        ((string= "buy" type) :buy)))

(provide 'bter-api)
;;; bter-api.el ends here
