(defpackage #:invogen
  (:use :cl :alexandria :iterate))
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

(defun ensure-entity (e)
  (unless (mito:find-dao 'entity :id (entity-id e))
    (mito:insert-dao e)))

(defun ensure-database ()
  (mapcar #'mito:ensure-table-exists '(entity invoice fee)))

(defun drop-tables (tables)
  (mapcar #'mito:recreate-table tables))

;; TODO: move from latex to something simpler
(defun compile-tex (invoice-file)
  (uiop:run-program
   (concatenate 'string "pdflatex " (pathname-name invoice-file) "." (pathname-type invoice-file))
   :force-shell t))

(defun compile-invoice-template (template invoice fees inv-id)
  (let ((issuer (invoice-issuer invoice))
        (payer (invoice-payer invoice)))
    (funcall
     (clt:compile-template template
                           :start-delimiter "<|"
                           :start-echo-delimiter "<|="
                           :end-delimiter "|>")
     (list :inv-id            inv-id
           :issuer-name       (entity-name issuer)
           :issuer-address    (entity-address issuer)
           :issuer-phone      (entity-phone issuer)
           :issuer-tax-id     (entity-tax-id issuer)
           :issuer-iban       (entity-iban issuer)
           :issuer-swift      (entity-swift issuer)
           :issuer-legal-form (entity-legal-form issuer)
           :payer-name        (entity-name payer)
           :payer-address     (entity-address payer)
           :payer-id          (entity-id payer)
           :payer-vat-id      (entity-vat-id payer)
           :invoice-due-date  (invoice-due-date invoice)
           :invoice-date      (invoice-date invoice)
           :total             (format nil "~,2f"
                                      (/
                                       (iterate (for fee in fees)
                                         (summing (* (fee-quantity fee) (fee-price fee))))
                                       100))
           :fees              (iterate (for fee in fees)
                                (collecting (list
                                             (fee-description fee)
                                             (princ-to-string (fee-quantity fee))
                                             (format nil "~,2f" (/ (fee-price fee) 100))
                                             (format nil "~,2f" (/ (* (fee-quantity fee) (fee-price fee)) 100)))))))))

(defun compile-invoice-pdf (invoice fees out-file template-tex)
  (uiop:with-current-directory ((uiop:pathname-directory-pathname out-file))
    (let* ((invoice-id (princ-to-string (mito:object-id invoice)))
           (file-name (pathname-name out-file))
           (log-file (make-pathname :name file-name :type "log"))
           (aux-file (make-pathname :name file-name :type "aux"))
           (tex-file (make-pathname :name file-name :type "tex"))
           (pdf-file (make-pathname :name file-name :type "pdf")))
      (write-string-into-file (compile-invoice-template template-tex invoice fees invoice-id)
                              tex-file
                              :if-exists :supersede)
      (compile-tex tex-file)
      (mapcar #'delete-file (list log-file aux-file tex-file))
      (rename-file pdf-file (make-pathname :name file-name :type (pathname-type out-file))))))

(defun format-time (time)
  "Convert the unix epoch `TIME' to a DD-MM-YYYY string."
  (multiple-value-bind (_s _m _h day month year) (decode-universal-time time)
    (declare (ignore _s _m _h))
    (format nil "~2,'0d.~2,'0d.~d" day month year)))

(defun create-invoice (issuer-alias payer-alias fees-args days-to-pay)
  (let* ((current-time (get-universal-time))
         (payment-time (+ current-time (* days-to-pay 60 60 24)))
         (date (format-time current-time))
         (due-date (format-time payment-time))
         (issuer (mito:find-dao 'entity :alias issuer-alias))
         (payer (mito:find-dao 'entity :alias payer-alias))
         (invoice (make-instance 'invoice
                                 :issuer issuer
                                 :payer payer
                                 :date date
                                 :due-date due-date))
         (fees (iterate (for args in fees-args)
                 (collecting (apply #'make-fee (append args (list invoice)))))))
    (values invoice fees)))

(defun connect-db (production)
  "Connect to the DB."
  (mito:connect-toplevel :postgres
                         :username "invogen"
                         :database-name (if production "invogen" "invogen_dev")))

(defun run (from to fees-args days-to-pay out-dir template-tex)
  (ensure-database)
  (ensure-directories-exist out-dir)
  (multiple-value-bind (invoice fees)
      (create-invoice from to fees-args days-to-pay)
    (mito:insert-dao invoice)
    (mapcar #'mito:insert-dao fees)
    (let ((out-file (make-pathname :name (concatenate 'string "invoice-" (princ-to-string (mito:object-id invoice)))
                                   :directory (pathname-directory out-dir)
                                   :type "pdf")))
      (compile-invoice-pdf invoice fees out-file template-tex)
      (format t "Invoice has been created and saved to ~a~%" out-file)
      out-file)))

(opts:define-opts
  (:name :help
   :description "Compile your invoices with Lisp and Latex"
   :short #\h
   :long "help")
  (:name :production
   :description "Compile for production"
   :short #\f
   :long "production")
  (:name :from
   :description "Issuer alias"
   :short #\F
   :long "from"
   :required t
   :arg-parser #'identity)
  (:name :to
   :description "Payer alias"
   :short #\t
   :long "to"
   :required t
   :arg-parser #'identity)
  (:name :description
   :description "Fee description"
   :short #\D
   :long "description"
   :required t
   :arg-parser #'identity)
  (:name :price
   :description "Price of the fee"
   :short #\p
   :long "price"
   :arg-parser #'parse-integer
   :required t)
  (:name :days-to-pay
   :description "Days to pay"
   :short #\d
   :long "days-to-pay"
   :default 14
   :arg-parser #'parse-integer))

(defun main ()
  (let* ((opts (opts:get-opts))
         (template-tex (uiop:read-file-string "template.tex"))
         (out-dir (if (getf opts :production)
                      #p"invoices/"
                      #p"invoices-dev/"))
         (mito:*connection* (dbi:connect :postgres
                                         :username "invogen"
                                         :database-name (if (getf opts :production)
                                                            "invogen"
                                                            "invogen_dev"))))
    (run (getf opts :from)
         (getf opts :to)
         (list (list (getf opts :description)
                     1
                     (getf opts :price)))
         (getf opts :days-to-pay)
         out-dir
         template-tex))
  (uiop:quit))
