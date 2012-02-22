;;; qsas.el --- QuickSilver's Abbreviation Scoring

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

(defun qsas-count-matches-in-string (regexp string &optional start end)
  (setq start (or start 0)
        end   (or end (length string)))
  (loop for start = start then (1+ matched)
        for matched = (let ((case-fold-search nil))
                        (string-match regexp string start))
        while (and matched (< (1+ matched) end))
        count matched))

(defun qsas-make-abbrev-regexp (abbrev)
  (apply 'concat
         "^"
         (loop for char across (downcase abbrev) collect
               (format ".*?\\(%s\\)"
                       (regexp-quote (string char))))))

(defun qsas-abbrev-penalty (string skip-start skip-end)
  (let ((skipped (- skip-end skip-start)))
    (cond
     ((zerop skipped) 0)
     ((string-match "[ \\t\\r\\n_-]+$" (substring string skip-start skip-end))
      (let ((seps (- (match-end 0) (match-beginning 0))))
        (+ seps (* (- skipped seps) 0.15))))
     ((let ((case-fold-search nil))
        (eq (string-match "[[:upper:]]" string skip-end) skip-end))
      (let ((ups (let ((case-fold-search nil))
                   (qsas-count-matches-in-string
                    "[[:upper:]]" string skip-start skip-end))))
        (+ ups (* (- skipped ups) 0.15))))
     (t skipped))))

(defun qsas-abbrev-score-nocache (string abbrev)
  (cond
   ((zerop (length abbrev))             0.9)
   ((< (length string) (length abbrev)) 0.0)
   ((let ((case-fold-search t))
      (string-match (qsas-make-abbrev-regexp abbrev) string))
    (loop with groups = (cddr (match-data))
          while groups
          for prev    = 0 then end
          for start   = (pop groups)
          for end     = (pop groups)
          for matched = (- end start)
          for skipped = (- start prev)
          for penalty = (qsas-abbrev-penalty string prev start)
          sum (+ matched (- skipped penalty)) into point
          finally return
          (let* ((length (length string))
                 (rest (- length end)))
            (/ (+ point (* rest 0.9)) (float length)))))
   (t 0.0)))

(defvar qsas-abbrev-score-cache
  (make-hash-table :test 'equal :weakness t))

(defun qsas-abbrev-score (string abbrev)
  (let ((cache-key (cons string abbrev)))
    (or (gethash cache-key qsas-abbrev-score-cache)
        (puthash cache-key
                 (qsas-abbrev-score-nocache string abbrev)
                 qsas-abbrev-score-cache))))



;;; Realtime Abbreviation Scoring

(defun* qsas-add-to-list-as-sorted (list-var value &key (test '<) (key 'identity))
  (let ((list (symbol-value list-var)))
    (if (or (null list)
            (funcall test
                     (funcall key value)
                     (funcall key (car list))))
        (set list-var (cons value list))
      (while (and list
                  (cdr list)
                  (funcall test
                           (funcall key (cadr list))
                           (funcall key value)))
        (setq list (cdr list)))
      (setcdr list (cons value (cdr list))))))

(defsubst qsas-current-time-float ()
  (let ((time (current-time)))
    (+ (* (float (first time))
          (lsh 2 16))
       (float (second time))
       (/ (float (third time))
          1000000))))

(defmacro* qsas-with-stopwatch ((&optional (elapsed-name 'elapsed)) &body body)
  (declare (indent 1))
  (let ((start (gensym "START")))
    `(let ((,start (qsas-current-time-float)))
       (flet ((,elapsed-name ()
                 (- (qsas-current-time-float) ,start)))
         ,@body))))

(defmacro* qsas-with-timeout ((timeout &optional timeout-result (tick-name 'tick)) &body body)
  (declare (indent 1))
  (let ((elapsed (gensym "ELAPSED")))
    `(catch 'timeout
       (qsas-with-stopwatch (,elapsed)
         (flet ((,tick-name ()
                   (when (< ,timeout (,elapsed))
                     (throw 'timeout ,timeout-result))))
           ,@body)))))

(defun* qsas-realtime-abbrev-scoring (list abbrev &key limit timeout quality)
  (let (new-list)
    (qsas-with-timeout (timeout (nreverse new-list))
      (loop with length = 0
            for string in list
            for score = (qsas-abbrev-score string abbrev)
            if (>= score quality)
            do (qsas-add-to-list-as-sorted 'new-list (cons string score)
                                           :test '<
                                           :key 'cdr)
               (incf length)
            if (> length limit)
            do (pop new-list) (setq length limit)
            do (tick)
            finally return (nreverse new-list)))))

(provide 'qsas)
;;; qsas.el ends here
