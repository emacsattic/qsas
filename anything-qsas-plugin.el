;;; anything-qsas-plugin.el --- Anything QSAS Plugin

;; Copyright (C) 2011  Tomohiro Matsuyama

;; Author: Tomohiro Matsuyama <tomo@cx4a.org>
;; Keywords: convenience

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

;; 

;;; Code:

(require 'cl)
(require 'qsas)

(defvar aqsas-timeout 0.3)
(defvar aqsas-quality 0.7)

(defun aqsas-candidate-transformer (candidates)
  (let ((candidates-with-score
         (qsas-realtime-abbrev-scoring candidates anything-pattern
                                       :limit anything-candidate-number-limit
                                       :timeout aqsas-timeout
                                       :quality aqsas-quality)))
    (mapcar 'car candidates-with-score)))

(defun anything-compile-source--qsas-plugin (source)
  `((match identity)
    (volatile)
    (candidate-transformer aqsas-candidate-transformer)
    ,@source))

(add-to-list 'anything-compile-source-functions 'anything-compile-source--qsas-plugin t)

(provide 'anything-qsas-plugin)
;;; anything-qsas-plugin.el ends here
