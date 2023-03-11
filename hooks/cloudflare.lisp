#|
exec sbcl \
  --noinform \
  --disable-debugger \
  --eval "(ql:quickload '(drakma com.inuoe.jzon) :silent T)" \
  --load "$0" \
  --eval "(wg-manager.cloudflare::main)" \
  --quit \
  --end-toplevel-options "${@:1}"
|#

(defpackage #:wg-manager.cloudflare
  (:use #:cl))

(in-package #:wg-manager.cloudflare)

(defvar *zone*)
(defvar *bearer*)
(defvar *suffix* NIL)

(defun call (method endpoint &optional payload)
  (let ((data (com.inuoe.jzon:parse
               (drakma:http-request
                (format NIL "https://api.cloudflare.com/client/v4/~a" endpoint)
                :method method
                :content-type "application/json"
                :content (com.inuoe.jzon:stringify payload)
                :additional-headers `(("Authorization" . ,(format NIL "Bearer ~a" *bearer*)))
                :want-stream T))))
    (if (gethash "success" data)
        (gethash "result" data)
        (error "API call failed!~%~%~a" (com.inuoe.jzon:stringify data :pretty T)))))

(defun find-record (name)
  (loop for record across (gethash "result" (call :get (format NIL "zones/~a/dns_records" *zone*)))
        do (when (string-equal name (gethash "name" record))
             (return (gethash "id" record)))))

(defun main ()
  (destructuring-bind (self change ipv4 name &optional public-key note) sb-ext:*posix-argv*
    (declare (ignore self public-key))
    (setf *zone* (uiop:getenv "WG_CLOUDFLARE_ZONE"))
    (setf *bearer* (uiop:getenv "WG_CLOUDFLARE_BEARER"))
    (setf *suffix* (uiop:getenv "WG_CLOUDFLARE_SUFFIX"))
    (let ((name (format NIL "~a~@[.~a~]" name *suffix*)))
      (cond ((string-equal change "add")
             (call :post (format NIL "zones/~a/dns_records" *zone*)
                   `(("content" . ,ipv4)
                     ("name" . ,name)
                     ("proxied" . NIL)
                     ("type" . "A")
                     ("comment" . ,(or note 'null))
                     ("ttl" . 1))))
            ((string-equal change "remove")
             (call :delete (format NIL "zones/~a/dns_records/~a" *zone* (find-record name))))
            ((string-equal change "edit")
             (call :put (format NIL "zones/~a/dns_records/~a" *zone* (find-record name))
                   `(("content" . ,ipv4)
                     ("name" . ,name)
                     ("proxied" . NIL)
                     ("type" . "A")
                     ("comment" . ,(or note 'null))
                     ("ttl" . 1))))))))
