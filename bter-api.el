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
    (mapcar (lambda (data)
              (let ((pair-name (caar data))
                    (pair-data (cdar data)))
                `((,:pair . ,(symbol-name pair-name))
                  (,:decimal-places . ,(assoc-default 'decimal_places pair-data))
                  (,:min-amount . ,(assoc-default 'min_amount pair-data))
                  (,:fee . ,(assoc-default 'fee pair-data)))))
            pairs)))

(defun bter-api-get-market-info (market)
  "Get the market fees, minimum amounts and decimal places for MARKET."
  (let ((market-info (bter-api-get-all-market-info)))
    (bter-api--find-market market market-info)))


(defun bter-api-get-all-market-details ()
  "Get detailed information about all markets."
  (let* ((response (bter-api--get "marketlist"))
         (markets (assoc-default 'data response)))
    (mapcar (lambda (data)
              `((,:number . ,(assoc-default 'no data))
                (,:symbol . ,(assoc-default 'symbol data))
                (,:name . ,(assoc-default 'name data))
                (,:name-cn . ,(assoc-default 'name_cn data))
                (,:pair . ,(assoc-default 'pair data))
                (,:rate . ,(assoc-default 'rate data))
                (,:volume-a . ,(assoc-default 'vol_a data))
                (,:volume-b . ,(assoc-default 'vol_b data))
                (,:currency-a . ,(assoc-default 'curr_a data))
                (,:currency-b . ,(assoc-default 'curr_b data))
                (,:currency-suffix . ,(assoc-default 'curr_suffix data))
                (,:rate-percent . ,(assoc-default 'rate_percent data))
                (,:trend . ,(assoc-default 'trend data))
                (,:supply . ,(assoc-default 'supply data))
                (,:market-cap . ,(assoc-default 'marketcap data))))
            markets)))

(defun bter-api-get-market-details (market)
  "Get the market details for MARKET."
  )


;; Internal helpers

(defun bter-api--get (path &optional query-vars)
  "Generate a uri using PATH and optional QUERY-VARS and retrieve the result."
  (bter-api--get-uri (bter-api--create-endpoint path query-vars)))

(defun bter-api--create-endpoint (path &optional query-vars)
  "Build an endpoint to the api using PATH and QUERY-VARS."
  (format "%s%s/%s"
          bter-api-endpoint
          path
          (bter-api--build-query query-vars)))


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


(provide 'bter-api)
;;; bter-api.el ends here
