;;; ruby-dev-core.el — Core communication functions for ruby-dev.el

(require 'json)
(require 'cl)

;;;###autoload
(defgroup ruby-dev nil
  "Extension to ruby-mode to communicate with a live Ruby session."
  :prefix "ruby-dev-")

;;;###autoload
(defgroup ruby-dev-faces nil
  "Faces used by ruby-dev."
  :group 'ruby-dev
  :group 'faces)

(defvar ruby-dev-path (if load-file-name (file-name-directory load-file-name))
  "Path to the directory containing ruby-dev. Mostly used to run the
ruby script it is related to.")

;;;###autoload
(defcustom ruby-dev-autostart t
  "When non-nil, interactive commands that need to start ruby-dev will do it
automatically."
  :group 'ruby-dev
  :type  'boolean)

;;;###autoload
(defcustom ruby-dev-script-path (expand-file-name "ruby-dev.rb" ruby-dev-path)
  "Path to the script to start a ruby dev server."
  :group 'ruby-dev
  :type 'string)

;;;###autoload
(defcustom ruby-dev-ruby-executable "ruby"
  "Name of the executable to start Ruby."
  :group 'ruby-dev
  :type 'string)

(defvar ruby-dev-process nil
  "Process used to send commands, etc. to the Ruby shell.")

(defvar ruby-dev-received-output ""
  "Output received by the current process, waiting to be processed.

When a complete line is found in this output, it is removed from this string and passed
to `ruby-dev-enqueue-response'.")

(defvar ruby-dev-response-queue nil
  "List of JSON responses sent by the server.

If you're waiting for a response to be added to this queue, use
`ruby-dev-read-response' to retrieve it.")

(defvar ruby-dev-special-handlers nil
  "Association list for handlers used by asynchronous commands.

If a response contains a key stored in this list, it is passed to the matching
function, instead of going through the regular, synchronous processing queue.")

(defun ruby-dev-running-p ()
  "Returns non-nil if ruby-dev is running."
  (and ruby-dev-process (process-live-p ruby-dev-process)))

;;;###autoload
(defun ruby-dev ()
  "Starts the shell used for Ruby development

If the process is already running, the user is given the choice to restart it
or to cancel this operation."
  (interactive)
  (if (ruby-dev-running-p)
      (when (yes-or-no-p "ruby-dev already started. Restart it? ")
        (ruby-dev-restart-process))
    (ruby-dev-start-process)))

;;;###autoload
(defun ruby-dev-connect (host port)
  "Starts the shell used for Ruby development

If the process is already running, the user is given the choice to restart it
or to cancel this operation."
  (interactive
   (list
    (read-string "Host: " "127.0.0.1")
    (string-to-number (read-string "Port: " "6475"))))
  (unless (and (ruby-dev-running-p)
               (not (yes-or-no-p "ruby-dev already started. Restart it? ")))
    (ruby-dev-stop-process)
    (setq ruby-dev-process (open-network-stream "ruby-dev" nil host port))
    (set-process-filter ruby-dev-process 'ruby-dev-process-filter)))

;;;###autoload
(defun ruby-dev-start-maybe ()
  "Like `ruby-dev', but doesn't do anything if the process is running already."
  (interactive)
  (if ruby-dev-process
      (unless (process-live-p ruby-dev-process)
        (ruby-dev-restart-process))
    (ruby-dev-start-process)))

(defmacro ruby-dev-ensure ()
  "Macro called by interactive functions to ensure `ruby-dev' is running.

This is a macro only because it needs to call `called-interactively-p'."
  '(when (called-interactively-p 'any)
     (if ruby-dev-autostart (ruby-dev-start-maybe)
       (unless (ruby-dev-running-p)
         (when (yes-or-no-p "No ruby-dev process started. Start it? ")
           (ruby-dev))))))

;;;###autoload
(defun ruby-dev-restart-process ()
  "Restarts the ruby-dev process."
  (interactive)
  (ruby-dev-stop-process)
  (ruby-dev-start-process))

;;;###autoload
(defun ruby-dev-stop-process ()
  "Kills the ruby-dev process (or connection)."
  (interactive)
  (when ruby-dev-process
    (if (process-live-p ruby-dev-process) (delete-process ruby-dev-process))
    (setq ruby-dev-process nil)))

(defun ruby-dev-start-process ()
  "Actually starts the process.

This does not check if there's another ruby-dev process running at the moment.
If you want to start the process safely, you should always use `ruby-dev'."
  (setq ruby-dev-process
        (start-process "ruby-dev" nil ruby-dev-ruby-executable
                       ruby-dev-script-path))
  (set-process-filter ruby-dev-process 'ruby-dev-process-filter))

(defun ruby-dev-send-request (type &rest args)
  "Send a request to the ruby-dev process.

Requests are JSON objects that must be on a single line. They always have a
type attribute (set to TYPE). ARGS is a series of key-value pairs, where keys
are keywords.

To retrieve the response, see `ruby-dev-read-response' for synchronous commands,
`ruby-dev-special-handlers' for asynchrounous commands.

Example:

    (ruby-dev-send-request \"object-info\" :object \"Foo\")"
  (process-send-string ruby-dev-process
                       (concat
                        (apply 'ruby-dev-make-request type args) "\n")))

(defun ruby-dev-make-request (type &rest args)
  "Returns the JSON object to send to the process.

See `ruby-dev-send-request' for details."
  (json-encode `(:type ,type ,@args)))

(defun ruby-dev-read-response ()
  "Blocks until the process sends us a new answer that isn't processed by one of
the special handlers.

The response is an association list. You may want to use the
`with-ruby-dev-data' macro while processing it."
  (loop until ruby-dev-response-queue do
        (accept-process-output ruby-dev-process))
  (pop ruby-dev-response-queue))

(defun ruby-dev-match-special-handler (response)
  "Checks if a special handler should process the response.

If such a handler is found, it is returned as a cons cell with the following
form: (KEY . FUNCTION). If not, nil is returned."
  (find-if (lambda (match) (assoc (car match) response)) ruby-dev-special-handlers))

(defun ruby-dev-enqueue-response (line)
  "Function called when a full line of input has been received.

It parses the response. If a special handler is found for this response, it is
called. Otherwise, the response is just added at the end of a queue, waiting to
be read by a command."
  (let ((response (with-temp-buffer
                    (save-excursion (insert line))
                    (condition-case ex
                        (json-read)
                      ('json-readtable-error
                       (message "ruby-dev failed to parse line: %s" line)
                       nil)))))
    (when response
      (let ((handler (ruby-dev-match-special-handler response)))
        (if handler (funcall (cdr handler) response)
          (setq ruby-dev-response-queue
                (append ruby-dev-response-queue (list response))))))))

(defun ruby-dev-process-filter (process string)
  "Function that is called whenever input is retrieved from the process.

The received input is added to the current one. If full lines are found, they
are sent to be processed."
  (setq ruby-dev-received-output (concat ruby-dev-received-output string))
  (loop for eol = (position ?\n ruby-dev-received-output)
        while eol do
    (let ((cur  (subseq ruby-dev-received-output 0 (1+ eol)))
          (rest (subseq ruby-dev-received-output (1+ eol))))
      (unwind-protect
          (ruby-dev-enqueue-response cur)
        (setq ruby-dev-received-output rest)))))

(defmacro with-ruby-dev-data (slots response &rest body)
  "Macro that allows to deconstruct parsed JSON objects easily.

SLOTS is a list of elements to retrieve. Each slot must be either a symbol
which is both the key to retrieve and the variable name to store it in, or
a two-element list of the form (VAR KEY).

RESPONSE is the name of the expression that evaluates to the response. It is
guaranteed to be evaluated only once."
  (let ((response-var (gensym)))
    (flet ((retrieve-slot (s)
              (if (symbolp s) `(,s (cdr (assoc ',s ,response-var)))
                `(,(first s) (cdr (assoc ',(nth 1 s) ,response-var))))))
      `(let* ((,response-var ,response)
              ,@(mapcar #'retrieve-slot slots))
         ,@body))))

(provide 'ruby-dev-core)
