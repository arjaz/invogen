(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload
   '(:alexandria :mito :cl-template)
   :silent t))

(defpackage #:invogen
  (:use :cl :alexandria))
(in-package #:invogen)

(mito:deftable entity ()
  ((id :col-type :text
       :primary-key t)
   (name :col-type :text)
   (alias :col-type :text)
   (address :col-type :text)
   (phone :col-type (or :text :null))
   (tax-id :col-type (or :text :null))
   (vat-id :col-type (or :text :null))
   (iban :col-type (or :text :null))
   (swift :col-type (or :text :null))
   (legal-form :col-type (or :text :null)))
  (:unique-keys alias))

(defun make-entity (id name alias address phone tax-id vat-id iban swift legal-form)
  (make-instance 'entity
                 :id id
                 :name name
                 :alias alias
                 :address address
                 :phone phone
                 :tax-id tax-id
                 :vat-id vat-id
                 :iban iban
                 :swift swift
                 :legal-form legal-form))

(mito:deftable invoice ()
  ((issuer :col-type entity)
   (payer :col-type entity)
   ;; REVIEW: replace with built-in created_at?
   (date :col-type :text)
   (due-date :col-type :text)))

(mito:deftable fee ()
  ((description :col-type :text)
   (quantity :col-type :integer)
   (price :col-type :integer)
   (invoice :col-type invoice)))

(defun make-fee (description quantity price &optional invoice)
  (make-instance 'fee
                 :description description
                 :quantity quantity
                 :price price
                 :invoice invoice))

(defun make-some-fees (make-fee-args-list)
  (mapcar (lambda (args) (apply #'make-fee args))
          make-fee-args-list))

(defun ensure-entity (e)
  (unless (mito:find-dao 'entity :id (entity-id e))
    (mito:insert-dao e)))

(defun ensure-database ()
  (mapcar #'mito:ensure-table-exists '(entity invoice fee)))

(defun drop-tables (tables)
  (mapcar #'mito:recreate-table tables))

;; TODO: move from latex to something simpler
(defun compile-tex (invoice-file)
  (uiop:run-program (concatenate 'string "pdflatex " invoice-file)
                    :force-shell t))

(defun compile-inv-template (template inv fees inv-id)
  (let ((issuer (invoice-issuer inv))
        (payer (invoice-payer inv)))
    (funcall
     (clt:compile-template template)
     (list :issuer issuer :payer payer :fees fees :inv inv :inv-id inv-id))))

(defun delete-files (files)
  (mapcar #'delete-file files))

(defparameter +template-tex+ (uiop:read-file-string "../template.tex")
  "Tex string to base the invoice on")
(defparameter +template-class+ (uiop:read-file-string "../template.cls")
  "Some styling for the template")

(defun compile-invoice-pdf (inv fees &optional (out-dir "invoices/"))
  (uiop:with-current-directory (out-dir)
    (format t (princ-to-string inv))
    (let* ((invoice-id (princ-to-string (mito:object-id inv)))
           (log-file (concatenate 'string "invoice-" invoice-id ".log"))
           (aux-file (concatenate 'string "invoice-" invoice-id ".aux"))
           (tex-file (concatenate 'string "invoice-" invoice-id ".tex")))
      (write-string-into-file +template-class+ "invoice.cls"
                              :if-exists :supersede)
      (write-string-into-file
       (compile-inv-template +template-tex+
                             inv
                             fees
                             invoice-id)
       tex-file
       :if-exists :supersede)
      (compile-tex tex-file)
      (delete-files (list log-file aux-file tex-file))
      (concatenate 'string out-dir "invoice-" invoice-id ".pdf"))))

(defun make-payment (issuer payer date due-date fees-args-list)
  (let* ((inv (make-instance 'invoice
                             :issuer issuer
                             :payer payer
                             :date date
                             :due-date due-date))
         (fees-args+invoice-list (mapcar (lambda (list)
                                           (append list (list inv)))
                                         fees-args-list))
         (fees (make-some-fees fees-args+invoice-list)))
    (list inv fees)))

(defun format-time (time)
  "Convert the unix epoch `TIME' to a DD-MM-YYYY string."
  (multiple-value-bind (_s _m _h day month year) (decode-universal-time time)
    (declare (ignore _s _m _h))
    (format nil "~2,'0d.~2,'0d.~d" day month year)))

(defun create-invoice (issuer-alias payer-alias fees-args days-to-pay
                       &optional (out-dir "invoices/"))
  (let* ((current-time (get-universal-time))
         (payment-time (+ current-time (* days-to-pay 60 60 24)))
         (date (format-time current-time))
         (due-date (format-time payment-time))
         (issuer (mito:find-dao 'entity :alias issuer-alias))
         (payer (mito:find-dao 'entity :alias payer-alias)))
    (destructuring-bind (inv fees) (make-payment issuer
                                                 payer
                                                 date
                                                 due-date
                                                 fees-args)
      (mito:insert-dao inv)
      (mapcar #'mito:insert-dao fees)
      (compile-invoice-pdf inv fees out-dir))))

(defun connect (&optional (prodp nil))
  "Connect to the DB."
  (mito:connect-toplevel :postgres
                         :username "invogen"
                         :database-name (if prodp "invogen" "invogen_dev")))

(defun run (from to fees-args days-to-pay &optional (prodp nil))
  (let* ((mito:*connection* (dbi:connect :postgres
                                         :username "invogen"
                                         :database-name (if prodp
                                                            "invogen"
                                                            "invogen_dev")))
         (out-dir (if prodp
                      "invoices/"
                      "invoices-dev/")))
    (ensure-database)
    (ensure-directories-exist out-dir)
    (let ((invoice-path (create-invoice from to fees-args days-to-pay out-dir)))
      (format t "Invoice has been created and saved to ~a~%" invoice-path))))

