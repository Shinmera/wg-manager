#|
exec sbcl \
  --noinform \
  --disable-debugger \
  --eval "(ql:quickload '(postmodern cl-qrencode zippy) :silent T)" \
  --load "$0" \
  --eval "(wg-manager::main)" \
  --quit \
  --end-toplevel-options "${@:1}"
|#

(defpackage #:wg-manager
  (:use #:cl)
  (:local-nicknames
   (#:zippy #:org.shirakumo.zippy))
  (:export
   #:*postgres-host*
   #:*postgres-user*
   #:*postgres-pass*
   #:*postgres-db*
   #:*server-public-key*
   #:*server-private-key-file*
   #:*server-public-ip*
   #:*server-public-port*
   #:*server-internal-ip*
   #:*subnet*
   #:*device*
   #:read-config
   #:connect
   #:disconnect
   #:list-peers
   #:find-peer
   #:ensure-peer
   #:add-peer
   #:remove-peer
   #:start
   #:stop
   #:generate-config
   #:generate-qr
   #:generate-user-package))

(in-package #:wg-manager)

(defvar *postgres-host* "127.0.0.1")
(defvar *postgres-user* "wireguard")
(defvar *postgres-pass* NIL)
(defvar *postgres-db* "wireguard")
(defvar *server-public-key* NIL)
(defvar *server-private-key-file* NIL)
(defvar *server-public-ip* NIL)
(defvar *server-public-port* "51820")
(defvar *server-internal-ip* NIL)
(defvar *subnet* "10.1.3.")
(defvar *device* NIL)

(defun envvar (name)
  (let ((var (uiop:getenv name)))
    (when (and var (string/= "" var))
      var)))

(defun status (format &rest args)
  (format *debug-io* "~&[WG] ~?~%" format args)
  (force-output *debug-io*))

(defun read-config-file (file)
  (flet ((process-var (var val)
           (cond ((string-equal var "WG_POSTGRES_HOST") (setf *postgres-host* val))
                 ((string-equal var "WG_POSTGRES_USER") (setf *postgres-user* val))
                 ((string-equal var "WG_POSTGRES_PASS") (setf *postgres-pass* val))
                 ((string-equal var "WG_POSTGRES_DB") (setf *postgres-db* val))
                 ((string-equal var "WG_PUBLIC_KEY") (setf *server-public-key* val))
                 ((string-equal var "WG_PRIVATE_KEY_FILE") (setf *server-private-key-file* val))
                 ((string-equal var "WG_PUBLIC_IP") (setf *server-public-ip* val))
                 ((string-equal var "WG_PUBLIC_PORT") (setf *server-public-port* val))
                 ((string-equal var "WG_INTERNAL_IP") (setf *server-internal-ip* val))
                 ((string-equal var "WG_SUBNET") (setf *subnet* val))
                 ((string-equal var "WG_DEVICE") (setf *device* val))
                 ((string-equal var "WG_PUBLIC_KEY_FILE") (setf *server-public-key* (alexandria:read-file-into-string val))))))
    (with-open-file (stream file :if-does-not-exist NIL)
      (when stream
        (loop for line = (read-line stream NIL NIL)
              while line
              do (let ((pos (position #\= line)))
                   (when pos
                     (process-var (subseq line 0 pos) (subseq line (1+ pos))))))))))

(defun read-envvars ()
  (macrolet ((maybe-set (var envvar)
               `(let ((var (envvar ,envvar)))
                  (when var (setf ,var var)))))
    (maybe-set *postgres-host* "WG_POSTGRES_HOST")
    (maybe-set *postgres-user* "WG_POSTGRES_USER")
    (maybe-set *postgres-pass* "WG_POSTGRES_PASS")
    (maybe-set *postgres-db* "WG_POSTGRES_DB")
    (maybe-set *server-public-key* "WG_PUBLIC_KEY")
    (let ((var (envvar "WG_PUBLIC_KEY_FILE")))
      (when var (setf *server-public-key* (alexandria:read-file-into-string (envvar "WG_PUBLIC_KEY_FILE")))))
    (maybe-set *server-private-key-file* "WG_PRIVATE_KEY_FILE")
    (maybe-set *server-public-ip* "WG_PUBLIC_IP")
    (maybe-set *server-public-port* "WG_PUBLIC_PORT")
    (maybe-set *server-internal-ip* "WG_INTERNAL_IP")
    (maybe-set *subnet* "WG_SUBNET")
    (maybe-set *device* "WG_DEVICE")))

(defun read-config ()
  (read-config-file "/etc/wireguard/config")
  (read-config-file (format NIL "~a/.config/wireguard/config" (envvar "HOME")))
  (read-envvars)
  (unless *server-internal-ip*
    (setf *server-internal-ip* (format NIL "~a1" *subnet*))))

(defun run (input program &rest args)
  (string-right-trim
   '(#\Linefeed)
   (with-output-to-string (stream)
     (let ((error (make-string-output-stream)))
       (when (/= 0 (sb-ext:process-exit-code (sb-ext:run-program program args :input input :output stream :error error :search T)))
         (error "Failed to run ~a~{ ~a~}~%  ~a" program args (get-output-stream-string error)))))))

(defun connect ()
  (unless (and postmodern:*database* (postmodern:connected-p postmodern:*database*))
    (postmodern:connect-toplevel *postgres-db* *postgres-user* *postgres-pass* *postgres-host*)))

(defun disconnect ()
  (when (and postmodern:*database* (postmodern:connected-p postmodern:*database*))
    (postmodern:disconnect-toplevel)))

(defun init-database ()
  (connect)
  (unless (find "peers" (postmodern:list-all-tables) :test #'string= :key #'second)
    (postmodern:query (:create-table 'peers
                                     ((name :type (varchar 64) :primary-key t)
                                      (public-key :type (varchar 64) :unique t)
                                      (ipv4 :type (varchar 15) :unique t)
                                      (note :type text))))))

(defun list-peers ()
  (connect)
  (postmodern:query (:order-by (:select '* :from 'peers) 'ipv4) :plists))

(defun find-peer (name)
  (connect)
  (setf name (string-downcase name))
  (postmodern:query (:select '* :from 'peers :where (:= 'name name)) :plist))

(defun ensure-peer (peer-ish)
  (etypecase peer-ish
    (string
     (or (find-peer peer-ish)
         (error "No peer with name ~s found." peer-ish)))
    (cons
     peer-ish)))

(defun generate-next-ipv4 ()
  (let ((id (1+ (loop for peer in (list-peers)
                      maximize (parse-integer (getf peer :ipv4) :start (length *subnet*))))))
    (format NIL "~a~d" *subnet* id)))

(defun add-peer-to-network (peer &key (device *device*))
  (let* ((peer (ensure-peer peer))
         (ip (format NIL "~a/32" (getf peer :ipv4))))
    (status "Adding peer ~a" (getf peer :name))
    (run NIL "wg" "set" device "peer" (getf peer :public-key) "allowed-ips" ip)
    (run NIL "ip" "-4" "route" "add" ip "dev" device)
    peer))

(defun remove-peer-from-network (peer &key (device *device*))
  (let* ((peer (ensure-peer peer))
         (ip (format NIL "~a/32" (getf peer :ipv4))))
    (status "Removing peer ~a" (getf peer :name))
    (run NIL "wg" "set" device "peer" (getf peer :public-key) "remove")
    (run NIL "ip" "-4" "route" "delete" ip "dev" device)
    peer))

(defun add-peer (name &key public-key private-key ipv4 note (device *device*))
  (connect)
  (setf name (string-downcase name))
  (unless ipv4
    (setf ipv4 (generate-next-ipv4)))
  (unless public-key
    (unless private-key
      (setf private-key (run NIL "wg" "genkey")))
    (with-input-from-string (stream private-key)
      (setf public-key (run stream "wg" "pubkey"))))
  (postmodern:with-transaction ()
    (postmodern:query (:insert-into 'peers :set
                                    'name name
                                    'public-key public-key
                                    'ipv4 ipv4
                                    'note (or note "")))
    (let ((peer (find-peer name)))
      (if device
          (add-peer-to-network peer :device device)
          (postmodern:query (:notify 'wireguard-peers)))
      (list* peer :private-key private-key))))

(defun remove-peer (peer &key (device *device*))
  (connect)
  (postmodern:with-transaction ()
    (let ((peer (ensure-peer peer)))
      (postmodern:query (:delete-from 'peers :where (:= 'name (getf peer :name))))
      (if device
          (remove-peer-from-network peer :device device)
          (postmodern:query (:notify 'wireguard-peers))))))

(defun diff-peer-network (old new &key (device *device*))
  (let ((to-add (set-difference new old))
        (to-remove (set-difference old new)))
    (loop for peer in to-add do (add-peer-to-network peer :device device))
    (loop for peer in to-remove do (remove-peer-from-network peer :device device))
    new))

(defun start-wireguard (&key (device *device*) (port *server-public-port*) (internal-ip *server-internal-ip*) (private-key-file *server-private-key-file*))
  (status "Starting wireguard device ~a" device)
  (run NIL "ip" "link" "add" "dev" device "type" "wireguard")
  (run NIL "ip" "addr" "add" (format NIL "~a1/24" internal-ip) "dev" device)
  (run NIL "wg" "set" device "listen-port" port "private-key" private-key-file)
  (run NIL "iptables" "-A" "FORWARD" "-i" device "-j" "ACCEPT")
  (run NIL "iptables" "-A" "FORWARD" "-o" device "-j" "ACCEPT")
  (run NIL "iptables" "-t" "nat" "-A" "POSTROUTING" "-o" "eth0" "-j" "MASQUERADE"))

(defun stop-wireguard (&key (device *device*) (internal-ip *server-internal-ip*))
  (status "Stopping wireguard device ~a" device)
  (run NIL "iptables" "-D" "FORWARD" "-i" device "-j" "ACCEPT")
  (run NIL "iptables" "-D" "FORWARD" "-o" device "-j" "ACCEPT")
  (run NIL "iptables" "-t" "nat" "-D" "POSTROUTING" "-o" "eth0" "-j" "MASQUERADE")
  (run NIL "ip" "addr" "delete" (format NIL "~a1/24" internal-ip) "dev" device)
  (run NIL "ip" "link" "delete" "dev" device "type" "wireguard"))

(defun start ()
  (connect)
  (when *device*
    (start-wireguard)
    (unwind-protect
         (let ((current (diff-peer-network () (list-peers))))
           (postmodern:query (:listen 'wireguard-peers))
           (loop (cl-postgres:wait-for-notification postmodern:*database*)
                 (status "Waking up from PG notification")
                 (setf current (diff-peer-network current (list-peers)))))
      (stop-wireguard))))

(defun stop ()
  (when *device*
    (mapc #'remove-peer-from-network (list-peers))
    (stop-wireguard))
  (disconnect))

(defun generate-config (peer &key private-key)
  (let ((peer (ensure-peer peer)))
    (format NIL "[Interface]
Address = ~a/32
PrivateKey = ~a

[Peer]
PublicKey = ~a
Endpoint = ~a:~a
AllowedIPs = ~a.0/24"
            (getf peer :ipv4)
            (or private-key (getf peer :private-key) (error "Private key required."))
            *server-public-key*
            *server-public-ip*
            *server-public-port*
            *subnet*)))

(defun generate-qr (peer &key private-key path)
  (cl-qrencode:encode-png (generate-config peer :private-key private-key) :fpath path))

(defun generate-user-package (peer file &key password)
  (let* ((peer (ensure-peer peer))
         (name (getf peer :name))
         (zip (make-instance 'zippy:zip-file)))
    (flet ((add-file (content format &rest args)
             (let ((file-name (apply #'format NIL format args)))
               (vector-push-extend (make-instance 'zippy:zip-entry :zip-file zip :file-name file-name :content content)
                                   (zippy:entries zip)))))
      (add-file (getf peer :public-key) "~a.pub" name)
      (add-file (getf peer :private-key) "~a.key" name)
      (add-file (generate-config peer) "~a.conf" name)
      (uiop:with-temporary-file (:pathname qr)
        (add-file (generate-qr peer :path qr) name)
        (zippy:encode-file zip (merge-pathnames file name) :password password)))))

(defun main ()
  (read-config)
  (handler-case
      (destructuring-bind (self &optional (command "help") &rest args) sb-ext:*posix-argv*
        (cond ((string-equal command "start")
               (start))
              ((string-equal command "stop")
               (error "Not implemented lol"))
              ((string-equal command "list")
               (dolist (peer (list-peers))
                 (format *standard-output* "~15a ~32a ~45a~@[ ~a~]~%"
                         (getf peer :ipv4) (getf peer :name) (getf peer :public-key) (getf peer :note))))
              ((string-equal command "add")
               (let ((args ()) (package NIL) (package-pw NIL) (name (pop args)))
                 (unless name (error "PEER-NAME required"))
                 (loop for (key val) on args by #'cddr
                       do (cond ((string-equal key "--public-key") (setf (getf args :public-key) val))
                                ((string-equal key "--private-key") (setf (getf args :private-key) val))
                                ((string-equal key "--ipv4") (setf (getf args :ipv4) val))
                                ((string-equal key "--note") (setf (getf args :note) val))
                                ((string-equal key "--package") (setf package val))
                                ((string-equal key "--password") (setf package-pw val))
                                (T (error "Unknown key argument ~a" key))))
                 (let ((peer (apply #'add-peer name args)))
                   (when package (generate-user-package peer package :password package-pw))
                   (format *standard-output* "~{~a: ~a~%~}" peer))))
              ((string-equal command "remove")
               (remove-peer (or (first args) (error "PEER-NAME required"))))
              ((string-equal command "config")
               (format *standard-output* "~@[WG_POSTGRES_HOST=~a~%~]" *postgres-host*)
               (format *standard-output* "~@[WG_POSTGRES_USER=~a~%~]" *postgres-user*)
               (format *standard-output* "~@[WG_POSTGRES_PASS=~a~%~]" *postgres-pass*)
               (format *standard-output* "~@[WG_POSTGRES_DB=~a~%~]" *postgres-db*)
               (format *standard-output* "~@[WG_SERVER_PUBLIC_KEY=~a~%~]" *server-public-key*)
               (format *standard-output* "~@[WG_SERVER_PRIVATE_KEY_FILE=~a~%~]" *server-private-key-file*)
               (format *standard-output* "~@[WG_SERVER_PUBLIC_IP=~a~%~]" *server-public-ip*)
               (format *standard-output* "~@[WG_SERVER_PUBLIC_PORT=~a~%~]" *server-public-port*)
               (format *standard-output* "~@[WG_SERVER_INTERNAL_IP=~a~%~]" *server-internal-ip*)
               (format *standard-output* "~@[WG_SUBNET=~a~%~]" *subnet*)
               (format *standard-output* "~@[WG_DEVICE=~a~%~]" *device*))
              ((string-equal command "help")
               (format *error-output* "Usage: ~a [command] ...

Command can be:
  start  --- Start the wireguard server
  stop   --- Stop the wireguard server
  list   --- List known peers
  add    --- Add a new peer. Prints the peer info on completion.
    NAME                 --- The name of the peer
      --public-key KEY   --- The public key of the peer. If not passed is auto-generated
      --private-key KEY  --- The private key of the peer. If not passed is auto-generated
      --ipv4 IP          --- The IP address of the peer. If not passed is auto-generated
      --note NOTE        --- An optional note about the peer
      --package FILE     --- If passed, output a config package to the given file
      --password PASS    --- If passed, encrypt the package with the given password
  remove --- Remove a peer
    NAME                 --- The name of the peer to remove
  config --- Print the current configuration
  help   --- Show this help

The following configuration variables exist:

  WG_POSTGRES_HOST       --- The hostname of the postgres server [127.0.0.1]
  WG_POSTGRES_USER       --- The user to connect to postgres with [wireguard]
  WG_POSTGRES_PASS       --- The password of the postgres user
  WG_POSTGRES_DB         --- The postgres database to use [wireguard]
  WG_PUBLIC_KEY          --- The public key of the wireguard server
  WG_PUBLIC_KEY_FILE     --- The public key file of the wireguard server
  WG_PRIVATE_KEY_FILE    --- The private key file of the wireguard server
  WG_PUBLIC_IP           --- The public internet-facing IP of the wireguard server
  WG_PUBLIC_PORT         --- The public port of the wireguard server [51820]
  WG_INTERNAL_IP         --- The internal IP of the wireguard server [SUBNET1]
  WG_SUBNET              --- The subnet of the VPN [10.1.3.]
  WG_DEVICE              --- The wireguard device to manage

If WG_DEVICE is set, it is assumed that this is run on the wireguard server itself
and IP configuration is adapted accordingly. Otherwise, it is assumed that this is
a remote server and merely the database is updated. If the server is running, it
should notice the change and update the IP configuration automatically.

The variables are first read from a file at /etc/wireguard/config
Then from $HOME/.config/wireguard/config
Then from environment variables
" self))
              (T (error "Unknown command ~s" command))))
    (error (e)
      (status "Error: ~a" e)
      (sb-ext:exit :code 1))))

