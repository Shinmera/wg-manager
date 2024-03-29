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
(defvar *server-public-key-file* NIL)
(defvar *server-public-ip* NIL)
(defvar *server-public-port* "51820")
(defvar *server-internal-ip* NIL)
(defvar *subnet* "10.1.3.")
(defvar *hooks* NIL)
(defvar *device* NIL)

(defun envvar (name)
  (let ((var (uiop:getenv name)))
    (when (and var (string/= "" var))
      var)))

(defun status (format &rest args)
  (format *debug-io* "~&[WG] ~?~%" format args)
  (force-output *debug-io*))

(defun run (input program &rest args)
  (string-right-trim
   '(#\Linefeed)
   (with-output-to-string (stream)
     (let ((error (make-string-output-stream)))
       (when (/= 0 (sb-ext:process-exit-code (sb-ext:run-program program args :input input :output stream :error error :search T)))
         (error "Failed to run ~a~{ ~a~}~%  ~a" program args (get-output-stream-string error)))))))

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
                 ((string-equal var "WG_PUBLIC_KEY_FILE") (setf *server-public-key-file* val))
                 ((string-equal var "WG_HOOK") (push val *hooks*))
                 (T (setf (uiop:getenv var) val)))))
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
    (maybe-set *server-public-key-file* "WG_PUBLIC_KEY_FILE")
    (maybe-set *server-private-key-file* "WG_PRIVATE_KEY_FILE")
    (maybe-set *server-public-ip* "WG_PUBLIC_IP")
    (maybe-set *server-public-port* "WG_PUBLIC_PORT")
    (maybe-set *server-internal-ip* "WG_INTERNAL_IP")
    (maybe-set *subnet* "WG_SUBNET")
    (maybe-set *device* "WG_DEVICE")
    (let ((var (envvar "WG_HOOK")))
      (when var (push var *hooks*)))))

(defun read-config ()
  (read-config-file "/etc/wireguard/config")
  (read-config-file (format NIL "~a/.config/wireguard/config" (envvar "HOME")))
  (read-envvars)
  (unless *server-internal-ip*
    (setf *server-internal-ip* (format NIL "~a1" *subnet*)))
  (when *device*
    (unless *server-public-key-file*
      (setf *server-public-key-file* (format NIL "/etc/wireguard/~a.pub" *device*)))
    (unless *server-private-key-file*
      (setf *server-private-key-file* (format NIL "/etc/wireguard/~a.key" *device*))
      (unless (probe-file *server-private-key-file*)
        (alexandria:write-string-into-file (run NIL "wg" "genkey") *server-private-key-file*)
        (alexandria:write-string-into-file (run *server-private-key-file* "wg" "pubkey") *server-public-key-file*)))
    (unless *server-public-key*
      (setf *server-public-key* (alexandria:read-file-into-string *server-public-key-file*)))))

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
                                      (note :type text))))
    (postmodern:query (:insert-into 'peers :set
                                    'name (run NIL "hostname")
                                    'public-key *server-public-key*
                                    'ipv4 *server-internal-ip*
                                    'note "WireGuard"))))

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
  (loop for i from 1 below 254
        for ip = (format NIL "~a~d" *subnet* i)
        do (when (and (string/= ip *server-internal-ip*)
                      (null (postmodern:query (:select 'ipv4 :from 'peers :where (:= 'ipv4 ip)))))
             (return ip))))

(defun add-peer-to-network (peer &key (device *device*))
  (let* ((peer (ensure-peer peer))
         (ip (format NIL "~a/32" (getf peer :ipv4))))
    (status "Adding peer ~a ~a" (getf peer :ipv4) (getf peer :name))
    (run NIL "wg" "set" device "peer" (getf peer :public-key) "allowed-ips" ip)
    peer))

(defun remove-peer-from-network (peer &key (device *device*))
  (let* ((peer (ensure-peer peer)))
    (status "Removing peer ~a ~a" (getf peer :ipv4) (getf peer :name))
    (run NIL "wg" "set" device "peer" (getf peer :public-key) "remove")
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
      (run-hooks "add" peer)
      (list* :private-key private-key peer))))

(defun remove-peer (peer &key (device *device*))
  (connect)
  (postmodern:with-transaction ()
    (let ((peer (ensure-peer peer)))
      (postmodern:query (:delete-from 'peers :where (:= 'name (getf peer :name))))
      (if device
          (remove-peer-from-network peer :device device)
          (postmodern:query (:notify 'wireguard-peers)))
      (run-hooks "remove" peer)
      peer)))

(defun edit-peer (peer &key name public-key ipv4 note (device *device*))
  (connect)
  (postmodern:with-transaction ()
    (let* ((peer (ensure-peer peer))
           (old-peer (copy-list peer)))
      (flet ((update (karg field value)
               (postmodern:query (:update 'peers :set field value :where (:= 'name (getf peer :name))))
               (setf (getf peer karg) value)))
        (when public-key (update :public-key 'public-key public-key))
        (when note (update :note 'note note))
        (when ipv4 (update :ipv4 'ipv4 ipv4))
        (when name (update :name 'name name)))
      (when (or public-key ipv4)
        (cond (device
               (remove-peer-from-network old-peer :device device)
               (add-peer-to-network peer :device device))
              (T
               (postmodern:query (:notify 'wireguard-peers))))))))

(defun plist= (a b)
  (and (= (length a) (length b))
       (loop for (k av) on a by #'cddr
             for bv = (getf b k)
             always (equal av bv))))

(defun diff-peer-network (old new)
  (dolist (peer old)
    (unless (find peer new :test #'plist=)
      (with-simple-restart (continue "Ignore the failure")
        (remove-peer-from-network peer))))
  (dolist (peer new new)
    (unless (find peer old :test #'plist=)
      (with-simple-restart (continue "Ignore the failure")
        (add-peer-to-network peer)))))

(defun start-wireguard (&key (device *device*) (port *server-public-port*) (internal-ip *server-internal-ip*) (private-key-file *server-private-key-file*))
  (status "Starting wireguard device ~a ~a" device internal-ip)
  (run NIL "ip" "link" "add" "dev" device "type" "wireguard")
  (run NIL "ip" "-4" "addr" "add" (format NIL "~a/24" internal-ip) "dev" device)
  (run NIL "ip" "link" "set" "mtu" "1420" "up" "dev" device)
  (run NIL "wg" "set" device "listen-port" port "private-key" private-key-file)
  (run NIL "iptables" "-A" "FORWARD" "-i" device "-j" "ACCEPT")
  (run NIL "iptables" "-A" "FORWARD" "-o" device "-j" "ACCEPT")
  (run NIL "iptables" "-t" "nat" "-A" "POSTROUTING" "-o" "eth0" "-j" "MASQUERADE"))

(defun stop-wireguard (&key (device *device*) (internal-ip *server-internal-ip*))
  (status "Stopping wireguard device ~a" device)
  (run NIL "iptables" "-D" "FORWARD" "-i" device "-j" "ACCEPT")
  (run NIL "iptables" "-D" "FORWARD" "-o" device "-j" "ACCEPT")
  (run NIL "iptables" "-t" "nat" "-D" "POSTROUTING" "-o" "eth0" "-j" "MASQUERADE")
  (run NIL "ip" "-4" "addr" "delete" (format NIL "~a/24" internal-ip) "dev" device)
  (run NIL "ip" "link" "delete" "dev" device "type" "wireguard"))

(defun start ()
  (connect)
  (init-database)
  (when *device*
    (start-wireguard)
    (unwind-protect
         (let ((current (diff-peer-network () (list-peers))))
           (postmodern:query (:listen 'wireguard-peers))
           (status "Waiting for changes")
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
Address = ~a/24
PrivateKey = ~a

[Peer]
PublicKey = ~a
Endpoint = ~a:~a
AllowedIPs = ~a0/24
PersistentKeepalive = 25"
            (getf peer :ipv4)
            (or private-key (getf peer :private-key) (error "Private key required."))
            *server-public-key*
            *server-public-ip*
            *server-public-port*
            *subnet*)))

(defun generate-readme (peer)
  (format NIL "=== Installation Instructions ===
This is a brief guide that should tell you how to get set up with
WireGuard to connect to the VPN. If you have any issues, please
contact the system administrator.

This ZIP contains sensitive information that uniquely identifies you
on the VPN. Do *NOT* share it with anyone else, and make sure the ZIP
file and its contents cannot be easily read by other programs and
services.

Ideally after installation delete this ZIP file and its contents.
Note that you can only connect one device with the certificate in this
ZIP file. If you require more devices, please request additional
certificates from your system administrator.

=== Windows / MacOS ===
1. Download and run the installer:
   - Windows: https://www.wireguard.com/install/
   - MacOS: https://apps.apple.com/us/app/wireguard/id1451685025
2. Start WireGuard and click \"Import tunnels from file\"
3. Select the wg0.conf file

=== Linux ===
1. Install the Wireguard Package:
   - Ubuntu/Debian: sudo apt install wireguard
   - Arch: sudo pacman -S wireguard-tools
   - Fedora: sudo dnf install wireguard-tools
2. sudo unzip this.zip -d /etc/wireguard/
3. sudo systemctl start wg-quick@wg0

To enable the connection automatically on boot:
1. sudo systemctl enable wg-quick@wg0

=== Smart Phones ===
1. Install the WireGuard app:
  - Android: https://play.google.com/store/apps/details?id=com.wireguard.android
  - iOS: https://apps.apple.com/us/app/wireguard/id1441195209
2. Open the app and add a new connection via QR code
3. Open the QR.png on your PC
4. Scan the QR code with your phone
" *server-internal-ip*))

(defun print-peer (peer &optional (stream *standard-output*))
  (format stream "~15a ~32a ~45a~@[ ~a~]~%"
          (getf peer :ipv4) (getf peer :name) (getf peer :public-key) (getf peer :note)))

(defun generate-qr (peer &key private-key path)
  (cl-qrencode:encode-png (generate-config peer :private-key private-key) :fpath path))

(defun generate-user-package (peer file &key password)
  (let* ((peer (ensure-peer peer))
         (name (getf peer :name))
         (zip (make-instance 'zippy:zip-file)))
    (flet ((add-file (content format &rest args)
             (let ((file-name (apply #'format NIL format args))
                   (content (if (stringp content) (babel:string-to-octets content) content)))
               (vector-push-extend (make-instance 'zippy:zip-entry :zip-file zip :file-name file-name :content content)
                                   (zippy:entries zip)))))
      (let ((name (substitute #\- #\/ name)))
        (add-file (getf peer :public-key) "~a.pub" name)
        (add-file (getf peer :private-key) "~a.key" name)
        (add-file (generate-config peer) "wg0.conf")
        (add-file (generate-readme peer) "README.txt")
        (uiop:with-temporary-file (:pathname qr)
          (add-file (generate-qr peer :path qr) "QR.png")
          (zippy:compress-zip zip (merge-pathnames file name) :password password))))))

(defun run-hooks (change peer)
  (dolist (hook *hooks*)
    (sb-ext:run-program hook (list change (getf peer :ipv4) (getf peer :name) (getf peer :public-key) (getf peer :note))
                        :output *standard-output* :error *error-output* :search T)))

(defun print-config (&optional (stream *standard-output*))
  (format stream "~@[WG_POSTGRES_HOST=~a~%~]" *postgres-host*)
  (format stream "~@[WG_POSTGRES_USER=~a~%~]" *postgres-user*)
  (format stream "~@[WG_POSTGRES_PASS=~a~%~]" *postgres-pass*)
  (format stream "~@[WG_POSTGRES_DB=~a~%~]" *postgres-db*)
  (format stream "~@[WG_SERVER_PUBLIC_KEY_FILE=~a~%~]" *server-public-key-file*)
  (format stream "~@[WG_SERVER_PRIVATE_KEY_FILE=~a~%~]" *server-private-key-file*)
  (format stream "~@[WG_SERVER_PUBLIC_IP=~a~%~]" *server-public-ip*)
  (format stream "~@[WG_SERVER_PUBLIC_PORT=~a~%~]" *server-public-port*)
  (format stream "~@[WG_SERVER_INTERNAL_IP=~a~%~]" *server-internal-ip*)
  (format stream "~@[WG_SUBNET=~a~%~]" *subnet*)
  (format stream "~@[WG_DEVICE=~a~%~]" *device*))

(defun main ()
  (read-config)
  (handler-case
      (destructuring-bind (self &optional (command "help") &rest args) sb-ext:*posix-argv*
        (cond ((string-equal command "start")
               (handler-bind ((error (lambda (e)
                                       (format *debug-io* "[ERROR] ~a" e)
                                       (continue e))))
                 (start)))
              ((string-equal command "stop")
               (error "Not implemented lol"))
              ((string-equal command "list")
               (mapc #'print-peer (list-peers)))
              ((string-equal command "add")
               (let ((add-args ()) (package NIL) (package-pw NIL) (name (pop args)))
                 (unless name (error "PEER-NAME required"))
                 (loop for (key val) on args by #'cddr
                       do (cond ((string-equal key "--public-key") (setf (getf add-args :public-key) val))
                                ((string-equal key "--private-key") (setf (getf add-args :private-key) val))
                                ((string-equal key "--ipv4") (setf (getf add-args :ipv4) val))
                                ((string-equal key "--note") (setf (getf add-args :note) val))
                                ((string-equal key "--package") (setf package val))
                                ((string-equal key "--password") (setf package-pw val))
                                (T (error "Unknown key argument ~a" key))))
                 (let ((peer (apply #'add-peer name add-args)))
                   (when package (generate-user-package peer package :password package-pw))
                   (format *standard-output* "~{~a: ~a~%~}" peer))))
              ((string-equal command "remove")
               (remove-peer (or (first args) (error "PEER-NAME required"))))
              ((string-equal command "edit")
               (let ((edit-args ()) (package NIL) (package-pw NIL) (name (pop args)))
                 (unless name (error "PEER-NAME required"))
                 (loop for (key val) on args by #'cddr
                       do (cond ((string-equal key "--public-key") (setf (getf edit-args :public-key) val))
                                ((string-equal key "--ipv4") (setf (getf edit-args :ipv4) val))
                                ((string-equal key "--note") (setf (getf edit-args :note) val))
                                ((string-equal key "--name") (setf (getf edit-args :name) val))
                                (T (error "Unknown key argument ~a" key))))
                 (let ((peer (apply #'edit-peer name edit-args)))
                   (format *standard-output* "~{~a: ~a~%~}" peer))))
              ((string-equal command "install")
               (let ((unit "wg-server") (*device* (or *device* "wg0")) (start T) (enable T))
                 (loop for (key val) on args by #'cddr
                       do (cond ((string-equal key "--device") (setf *device* val))
                                ((string-equal key "--unit") (setf unit val))
                                ((string-equal key "--start") (setf start (string-equal val "true")))
                                ((string-equal key "--enable") (setf enable (string-equal val "true")))
                                (T (error "Unknown key argument ~a" key))))
                 (status "Installing ~a for ~a" unit *device*)
                 (with-open-file (stream (format NIL "/etc/systemd/system/~a.service" unit) :direction :output)
                   (format stream "[Unit]
Description=WireGuard Server
Requires=network.target
After=network.target

[Service]
ExecStart=~a start
Restart=on-failure
RestartSec=5s

[Install]
WantedBy=multi-user.target
" (truename self)))
                 (with-open-file (stream "/etc/wireguard/config" :direction :output :if-exists NIL)
                   (when stream (print-config stream)))
                 (when start (run NIL "systemctl" "start" unit))
                 (when enable (run NIL "systemctl" "enable" unit))))
              ((string-equal command "config")
               (print-config *standard-output*))
              ((string-equal command "help")
               (format *error-output* "Usage: ~a [command] ...

Command can be:
  start  --- Start the wireguard server
  stop   --- Stop the wireguard server
  list   --- List known peers
  add    --- Add a new peer. Prints the peer info on completion.
    NAME                 --- The name of the peer
    --public-key KEY     --- The public key of the peer. If not passed
                             is auto-generated
    --private-key KEY    --- The private key of the peer. If not
                             passed is auto-generated
    --ipv4 IP            --- The IP address of the peer. If not passed
                             is auto-generated
    --note NOTE          --- An optional note about the peer
    --package FILE       --- If passed, output a config package to the
                             given file
    --password PASS      --- If passed, encrypt the package with the
                             given password
  remove --- Remove a peer
    NAME                 --- The name of the peer to remove
  edit   --- Edit a peer's information
    NAME
    --public-key KEY     --- The new public key of the peer
    --ipv4 IP            --- The new IP address of the peer
    --note NOTE          --- An optional note about the peer
    --name NEW-NAME      --- The new name of the peer
  install --- Install a basic server setup with systemd
    --device DEVICE      --- The name of the device to use [wg0]
    --unit UNIT          --- The service unit name to use [wg-server]
    --start BOOLEAN      --- Whether to start the service [true]
    --enable BOOLEAN     --- Whether to enable the service [true]
  config  --- Print the current configuration
  help    --- Show this help

The following configuration variables exist:

  WG_POSTGRES_HOST       --- The hostname of the postgres server
                             [127.0.0.1]
  WG_POSTGRES_USER       --- The user to connect to postgres with
                             [wireguard]
  WG_POSTGRES_PASS       --- The password of the postgres user
  WG_POSTGRES_DB         --- The postgres database to use [wireguard]
  WG_PUBLIC_KEY_FILE     --- The public key file of the wireguard
                             server [/etc/wireguard/$WG_DEVICE.pub]
  WG_PRIVATE_KEY_FILE    --- The private key file of the wireguard
                             server [/etc/wireguard/$WG_DEVICE.key]
  WG_PUBLIC_IP           --- The public internet-facing IP of the
                             wireguard server
  WG_PUBLIC_PORT         --- The public port of the wireguard server
                             [51820]
  WG_INTERNAL_IP         --- The internal IP of the wireguard server
                             [$WG_SUBNET 1]
  WG_SUBNET              --- The subnet of the VPN [10.1.3.]
  WG_DEVICE              --- The wireguard device to manage
  WG_HOOK                --- Adds the given program to a list of
                             hooks that will be run after changing
                             an entry. The program will receive the
                             following arguments:
    CHANGE IPV4 NAME PUBLIC-KEY NOTE
                             Wherein CHANGE is one of: 
                               add remove edit

Any configuration variables not part of this set will be re-exported
as environment variables so that other hooks may make use of it.

If WG_DEVICE is set, it is assumed that this is run on the wireguard
server itself and IP configuration is adapted accordingly. Otherwise,
it is assumed that this is a remote server and merely the database is
updated. If the server is running, it should notice the change and
update the IP configuration automatically.

The variables are first read from a file at /etc/wireguard/config
Then from $HOME/.config/wireguard/config
Then from environment variables
" self))
              (T (error "Unknown command ~s" command))))
    (sb-sys:interactive-interrupt ()
      (status "Exiting from interrupt")
      (sb-ext:exit :code 0))
    (error (e)
      (status "Error: ~a" e)
      (sb-ext:exit :code 1))))

