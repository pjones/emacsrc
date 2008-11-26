;;; org-eva.el --- Submit invoicing data from OrgMode to Eva
;;
;; Copyright (C) 2008 pmade inc. (Peter Jones pjones@pmade.com)
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;
;; Commentary:
;;
(eval-when-compile
  (require 'cl)
  (require 'org)
  (require 'org-invoice))

(require 'url)
(require 'url-http)
(require 'xml)

(defgroup org-eva nil
  "OrgMode Invoice Helper for Eva"
  :tag "Org-Eva" :group 'org-invoice)

(defcustom org-eva-username nil
  "Your Eva username (email address)."
  :type 'string :group 'org-eva)

(defcustom org-eva-password nil
  "Your Eva password."
  :type 'string :group 'org-eva)

(defcustom org-eva-host "eva.pmade.com"
  "The name of the host that is running Eva."
  :type 'string :group 'org-eva)

(defcustom org-eva-port nil
  "The port that Eva is running on."
  :type 'string :group 'org-eva)

(defcustom org-eva-use-ssl t
  "Whether or not to use SSL when talking to Eva."
  :type 'string :group 'org-eva)

(defcustom org-eva-item-qty-as-hours t
  "Tell Eva to display invoice items with hours."
  :type 'string :group 'org-eva)

(defcustom org-eva-items-with-date t
  "Tell Eva to display invoice items with dates."
  :type 'string :group 'org-eva)

(defun org-eva-item-xml (item)
  "Convert the given invoice item into an XML node."
  (let ((title (cdr (assoc 'title item)))
        (work  (cdr (assoc 'work item)))
        (rate  (cdr (assoc 'rate item)))
        (date  (cdr (assoc 'raw-date item))))
    (concat "<item>"
            "<description><![CDATA[" title "]]></description>"
            "<quantity>" (number-to-string (/ work 60.0)) "</quantity>"
            "<unit-price>" (number-to-string rate) "</unit-price>"
            "<item-date>" (format-time-string "%Y-%m-%d" date) "</item-date>"
            "</item>")))
            
(defun org-eva-to-xml (invoice)
  "Convert the given invoice list to an XML document for
posting to Eva."
  (let* ((info (car invoice))
        (client-id (cdr (assoc 'eva-client info)))
        (net-days (cdr (assoc 'eva-net-days info)))
        (xml "<invoice>"))
    (setq xml (concat xml 
                      "<client-id>" client-id "</client-id>"
                      (when net-days (concat "<net-days>" net-days "</net-days>"))
                      "<show-as-hours>" (if org-eva-item-qty-as-hours "1" "0") "</show-as-hours>"
                      "<show-dates>" (if org-eva-items-with-date "1" "0") "</show-dates>"
                      "<items>"))
    (dolist (items (cdr invoice))
      (setq xml (concat xml (mapconcat 'org-eva-item-xml (cdr (cdr items)) ""))))
    (concat xml "</items></invoice>")))
                           
(defun org-eva-collect-invoice-info ()
  "Called from org-invoice to collect data about the invoice."
  (push (cons 'eva-client   (org-entry-get nil "EVA_CLIENT_ID"  t)) org-invoice-current-invoice)
  (push (cons 'eva-net-days (org-entry-get nil "EVA_NET_DAYS"   t)) org-invoice-current-invoice)
  (push (cons 'eva-invoice  (org-entry-get nil "EVA_INVOICE_ID" t)) org-invoice-current-invoice)
  (unless (cdr (assoc 'eva-client org-invoice-current-invoice))
    (error "Missing EVA_CLIENT_ID property")))

(defun org-eva-host-and-port ()
  "Return the Eva hostname with the port number attached."
  (concat org-eva-host ":" (or org-eva-port (and org-eva-use-ssl "443") "80")))

(defun org-eva-url (invoice)
  "Create and return a URL for the Eva invoice API."
  (let ((url (if org-eva-use-ssl "https://" "http://"))
        (id  (cdr (assoc 'eva-invoice invoice))))
    (concat url (org-eva-host-and-port) "/invoices"
            (when id (concat "/" id)) ".xml")))

(defun org-eva-cb (status invoice)
  "Called from the URL package after the POST completes."
  (let ((results (current-buffer))
        (orgbuf (cdr (assoc 'buffer (car invoice))))
        (orgpt  (cdr (assoc 'point (car invoice))))
        doc id private-url public-url)
    (when (plist-get status :error)
      (switch-to-buffer results)
      (error "Failed to send invoice to Eva"))
    (with-current-buffer results
      (goto-char (point-min))
      (re-search-forward "\n\n")
      (setq doc (xml-parse-region (point) (point-max))))
    (kill-buffer results)
    (setq id (car (last (car (xml-get-children (car doc) 'id)))))
    (setq private-url (car (last (car (xml-get-children (car doc) 'private-url)))))
    (setq public-url (car (last (car (xml-get-children (car doc) 'public-url)))))
    (with-current-buffer orgbuf
      (save-excursion
        (goto-char orgpt)
        (org-entry-put nil "EVA_INVOICE_ID" id)
        (org-entry-put nil "EVA_PRIVATE_URL" private-url)
        (org-entry-put nil "EVA_PUBLIC_URL" public-url)))))

(defun org-eva-submit ()
  "Submit the current invoice data to Eva.  If an invoice ID
property exists on the invoice tree already, an update will be
preformed.  Otherwise, a new invoice will be created."
  (interactive)
  (let (org-invoice-start-hook invoice xml url id)
    (save-excursion
      (org-invoice-goto-tree)
      (add-hook 'org-invoice-start-hook 'org-eva-collect-invoice-info)
      (setq invoice (org-invoice-collect-invoice-data)))
    (setq xml (org-eva-to-xml invoice))
    (setq id (cdr (assoc 'eva-invoice (car invoice))))
    (when (or id (y-or-n-p "No EVA_INVOICE_ID property found, create a new invoice? "))
      (setq url (org-eva-url (car invoice)))
      (let ((url-request-method (if id "PUT" "POST"))
            (url-request-extra-headers '(("Content-Type" . "text/xml")))
            (url-request-data xml)
            (basic-auth (concat org-eva-username ":" org-eva-password))
            (auth-key (org-eva-host-and-port)))
        (when (not (assoc auth-key (symbol-value url-basic-auth-storage)))
          (set url-basic-auth-storage
               (cons (list auth-key (cons "Eva API" (base64-encode-string basic-auth)))
                     (symbol-value url-basic-auth-storage))))
        (url-retrieve url 'org-eva-cb (list invoice))))))

(provide 'org-eva)
