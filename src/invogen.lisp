(ql:quickload 'cl-template)
(ql:quickload 'alexandria)
(ql:quickload 'mito)
(ql:quickload 'unix-options)

(defpackage #:invogen
  (:use :cl :alexandria)
  (:export #:main))
(in-package #:invogen)

(require 'cl-template)
(require 'alexandria)
(require 'mito)
(require 'unix-options)

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

(defun make-some-fees (fees-args-list)
  (mapcar (lambda (args) (apply #'make-fee args))
          fees-args-list))

(defun ensure-entity (e)
  (unless (mito:find-dao 'entity :id (entity-id e))
    (mito:insert-dao e)))

(defun init-database ()
  (mapcar #'mito:ensure-table-exists '(entity invoice fee)))

(defun drop-tables (tables)
  (mapcar #'mito:recreate-table tables))

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

(defun compile-invoice-pdf (inv fees &optional (out-dir "invoices/"))
  (uiop:with-current-directory (out-dir)
   (let* ((invoice-id (princ-to-string (mito:object-id inv)))
          (log-file (concatenate 'string "invoice-" invoice-id ".log"))
          (aux-file (concatenate 'string "invoice-" invoice-id ".aux"))
          (tex-file (concatenate 'string "invoice-" invoice-id ".tex")))
     (write-string-into-file
      (compile-inv-template (uiop:read-file-string "template.tex")
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
         (fees-args (mapcar (lambda (list)
                              (append list (list inv)))
                            fees-args-list))
         (fees (make-some-fees fees-args)))
    (list inv fees)))

(defun format-time (time)
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
      (let ((result-file (compile-invoice-pdf inv fees out-dir)))
        (mito:insert-dao inv)
        (mapcar #'mito:insert-dao fees)
        result-file))))

(defun run (from to fees-args days-to-pay &optional (prodp nil))
  (let* ((mito:*connection* (dbi:connect :sqlite3
                                         :database-name (if prodp
                                                            "invoices.db"
                                                            "invoices-dev.db")))
         (out-dir (if prodp
                      "invoices/"
                      "invoices-dev/")))
    (init-database)
    (ensure-directories-exist out-dir)
    ;; TODO: create the .tex template file somehow
    (let ((invoice-path (create-invoice from to fees-args days-to-pay out-dir)))
      (format t "Invoice has been created and saved to ~a~%" invoice-path))))

(defun parse-cli (args)
  ;; TODO: I don't like that, there's unix-opts
  (unix-options:with-cli-options
      (args)
      (help prod unix-options:&parameters days fees)
    (let ((parsed (make-hash-table)))
      (setf (gethash 'help parsed) help)
      (setf (gethash 'prodp parsed) prod)
      (setf (gethash 'days-to-pay parsed) (parse-integer (or days "15")))
      (setf (gethash 'fees parsed) (with-input-from-string (s fees)
                                     (mapcar #'(lambda (fee)
                                                 (list (string (first fee))
                                                       (second fee)
                                                       (third fee)))
                                             (read s))))
      parsed)))

(defun help-msg ()
  (format
   nil
   (concatenate
    'string
    "    --help to write this message and exit~%"
    "    --prod to connect to invoiced.db instead of invoices-dev.db~%"
    "    --fees ((DESCRIPTION QUANTITY PRICE) ...) -- that one's reqiured~%"
    "    --days INT to set due-date in INT days~%")))

(defun main ()
  (let* ((cli-args (parse-cli (uiop:command-line-arguments)))
         (help (gethash 'help cli-args))
         (prodp (gethash 'prodp cli-args))
         (days-to-pay (gethash 'days-to-pay cli-args))
         (fees (gethash 'fees cli-args)))
    (if (or help (null fees))
        (format t (help-msg))
        (run "eugene" "vacuumlabs" fees days-to-pay prodp)))
  (uiop:quit))
