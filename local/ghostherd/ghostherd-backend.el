;;; ghostherd-backend.el --- Host backends for ghostherd -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The slot between ghostherd's control plane and whatever actually owns
;; the PTY.  Everything above this file talks about sessions; everything
;; below it knows how to capture a screen, type into it, and tell whether
;; the process is still alive.
;;
;; Capabilities, not product names.  A backend has to answer four
;; questions -- what is on the screen, how do I type, is it alive, how do
;; I start and stop it -- and the rest of ghostherd is written against
;; those and nothing else.
;;
;; Two implementors:
;;
;; - `ghostel' (here).  The agent is an Emacs child process in a ghostel
;;   buffer.  The buffer *is* the host: killing it kills the agent.
;; - `tmux' (ghostherd-tmux.el).  The agent is a tmux session on a
;;   dedicated socket.  The buffer, when there is one, is only a client.
;;
;; Dispatch is on the backend symbol rather than on the session, because
;; a session is one struct type whichever host it came from.  Call sites
;; use the `ghostherd--host-*' wrappers, which read the symbol off the
;; session; a session keeps the backend it was born with.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function ghostel "ghostel" (&optional arg))
(declare-function ghostel-exec "ghostel" (buffer program &optional args))
(declare-function ghostel-send-string "ghostel" (string))
(declare-function ghostel-paste-string "ghostel" (string))
(declare-function ghostel-send-key "ghostel" (key-name &optional mods))

;; Internal ghostel buffer-locals (not public API, but stable enough).
(defvar ghostel--process)
(defvar ghostel--buffer-identity)
(defvar ghostel--title)
(defvar ghostel-progress-function)
(defvar ghostel-kill-buffer-on-exit)
(defvar ghostel-query-before-killing)
(defvar ghostel-buffer-name)
(defvar ghostel-eval-cmds)
(defvar ghostel-exit-functions)
(defvar ghostel-command-start-functions)
(defvar ghostel-command-finish-functions)

;; server.el's, read for `GHOSTHERD_SOCKET' in `ghostherd-agent-environment'.
(declare-function ghostherd-memory-rpc-url "ghostherd-memory")
(defvar server-name)


;;; Customization

;; The group lives here rather than in ghostherd.el because this file
;; loads first and already needs it for the options below.
(defgroup ghostherd nil
  "Manage AI agent CLIs running in ghostel terminals."
  :group 'tools
  :prefix "ghostherd-")

(defcustom ghostherd-backend 'ghostel
  "Host used for agents started from now on.

Only affects `ghostherd-spawn': an existing session keeps the backend
it was born with, so switching this does not strand the herd you
already have.

`ghostel' runs the agent as an Emacs child process in a ghostel buffer.
`tmux' runs it in a tmux session on a dedicated socket, which survives
Emacs and can be read without anything being attached to it."
  :type '(choice (const ghostel) (const tmux) symbol)
  :group 'ghostherd)

(defcustom ghostherd-known-backends '(ghostel tmux)
  "Backends `ghostherd-restore' asks about agents it does not know.

Not the same list as what `ghostherd-backend' may be set to: restore has
to sweep every host that could be holding a herd, including the one you
have since stopped spawning on."
  :type '(repeat symbol)
  :group 'ghostherd)

(defcustom ghostherd-buffer-name-format "*ghostherd:%s*"
  "Format string for session buffer names.  %s is the session name."
  :type 'string
  :group 'ghostherd)

(defcustom ghostherd-spawn-delay 0.8
  "Seconds to wait for the shell prompt before launching the agent command.
Only used by the `ghostel' backend, which starts a shell and types the
command into it.  Backends that exec the agent directly ignore it."
  :type 'number
  :group 'ghostherd)

(defcustom ghostherd-screen-tail-lines 40
  "Number of lines from the bottom of the screen used for state detection."
  :type 'integer
  :group 'ghostherd)


;;; Session model

(cl-defstruct (ghostherd-session
               (:constructor ghostherd-session--create)
               (:copier nil))
  id
  name
  kind
  ;; The *view*, not the agent.  On the tmux backend nil means detached:
  ;; the agent is alive and nobody is looking at it.  That is a state of
  ;; the view, deliberately not of `state'.
  buffer
  ;; Which implementor owns this session, and the host's own name for it
  ;; (a tmux session name).  For `ghostel' the buffer is the host, so
  ;; `host-id' is just the id.
  (backend 'ghostel)
  host-id
  project
  command
  args
  (state 'starting)
  state-reason
  (started-at (current-time))
  last-active
  (seen t)
  (manual-state nil)
  notes
  ;; Last ConEmu OSC 9;4 report: the STATE symbol, its percentage (or nil),
  ;; and when it arrived.  Written on ghostel's VT-parser callpath, so
  ;; nothing here may be expensive to set; it is *read* by the poll path.
  progress-state
  progress-percent
  progress-at)

(defvar ghostherd--sessions (make-hash-table :test 'equal)
  "Map of session id string → `ghostherd-session'.")

(defvar-local ghostherd-session-id nil
  "Session id of the ghostherd session viewed in this buffer, if any.")

(defun ghostherd-get (id-or-name)
  "Return session for ID-OR-NAME, or nil.
Accepts session id, name, or a live buffer that views a session."
  (cond
   ((ghostherd-session-p id-or-name) id-or-name)
   ((bufferp id-or-name)
    (when-let* ((id (buffer-local-value 'ghostherd-session-id id-or-name)))
      (gethash id ghostherd--sessions)))
   ((stringp id-or-name)
    (or (gethash id-or-name ghostherd--sessions)
        (cl-find id-or-name (hash-table-values ghostherd--sessions)
                 :key #'ghostherd-session-name
                 :test #'equal)))
   (t nil)))


;;; Buffer helpers shared by the implementors

(defun ghostherd--require-ghostel ()
  "Load ghostel or signal an error."
  (unless (require 'ghostel nil t)
    (user-error "ghostherd requires the ghostel package")))

(defun ghostherd--buffer-name (name)
  "Return buffer name for session NAME."
  (format ghostherd-buffer-name-format name))

(defun ghostherd--buffer-tail (buffer &optional n)
  "Return the last N lines of BUFFER as a single string."
  (with-current-buffer buffer
    (let* ((n (or n ghostherd-screen-tail-lines))
           (end (point-max))
           (start (save-excursion
                    (goto-char end)
                    (forward-line (- n))
                    (point))))
      (buffer-substring-no-properties start end))))

(defun ghostherd--string-tail (string &optional n)
  "Return the last N lines of STRING.
The tmux capture is the whole visible pane, which is usually taller
than `ghostherd-screen-tail-lines'; trimming it here keeps both
backends feeding the rules the same amount of screen."
  ;; Drop the single newline the capture ends with before splitting.  A
  ;; 40-row pane trimmed to 40 lines otherwise loses its *top* line to a
  ;; trailing empty string, which is how the first thing an agent printed
  ;; goes missing from a screen that clearly still shows it.
  (let* ((string (or string ""))
         (string (if (string-suffix-p "\n" string)
                     (substring string 0 -1)
                   string))
         (lines (split-string string "\n"))
         (n (or n ghostherd-screen-tail-lines)))
    (string-join (last lines n) "\n")))

(defun ghostherd--buffer-title (buffer)
  "Return BUFFER's OSC 2 terminal title, or nil when it says nothing new.
Read straight off ghostel's buffer-local `ghostel--title' rather than
through `ghostel-buffer-name-function', which is how ghostel turns a
title into a buffer name -- taking that over would fight ghostherd's own
naming.  A title equal to the buffer name is the shell echoing us back."
  (when (and (buffer-live-p buffer) (boundp 'ghostel--title))
    (let ((title (buffer-local-value 'ghostel--title buffer)))
      (when (stringp title)
        (let ((title (string-trim title)))
          (unless (or (string-empty-p title)
                      (equal title (buffer-name buffer)))
            title))))))


;;; Agent identity
;;
;; An agent that does not know its own name cannot use half of what the
;; herd offers.  `bin/ghostherd message TO TEXT FROM' has to be *told*
;; who is speaking, so the wrapper defaulted the sender to "user" -- a
;; lie whenever the caller was another agent -- and nothing a CLI reports
;; about itself could be attributed at all.
;;
;; Nobody can tell it after the fact, either: the value has to be in the
;; process environment before the agent execs.  So identity is injected
;; at spawn, by every implementor, from one list.

(defun ghostherd-agent-environment (backend plist)
  "Return VAR=VALUE strings identifying the session PLIST describes.
BACKEND is the implementor doing the spawning.

`GHOSTHERD_SOCKET' is the emacsclient socket, not a tmux one, and it is
here because `bin/ghostherd' already reads it: an agent hosted by tmux
has no `ghostel_cmd' -- ghostel's shell integration is installed in the
shell ghostel spawns, which there is `tmux attach' -- so the wrapper is
its only way back into the herd, and it should reach *this* Emacs
without anyone having configured it.

`GHOSTHERD_RPC' is the sidecar JSON-RPC URL.  Agents mail siblings
through it so they need neither the binary nor emacsclient.  Omitted
until the sidecar has a bound port.

It is omitted when server.el has not been loaded, which is not a case
worth handling: an Emacs `emacsclient' can reach has loaded it, and
`emacsclient' with no `-s' looks for the same default the variable
would have held.  server.el is deliberately not required for it --
loading a subsystem to read one variable off it would be backwards."
  (append
   (list (format "GHOSTHERD_SESSION=%s" (plist-get plist :name))
         (format "GHOSTHERD_BACKEND=%s" backend))
   (when (and (boundp 'server-name) (stringp server-name))
     (list (format "GHOSTHERD_SOCKET=%s" server-name)))
   (when (and (fboundp 'ghostherd-memory-rpc-url)
              (ghostherd-memory-rpc-url))
     (list (format "GHOSTHERD_RPC=%s" (ghostherd-memory-rpc-url))))))


;;; Key names

(defconst ghostherd-key-aliases
  '(("esc"       . ("escape"    . nil))
    ("escape"    . ("escape"    . nil))
    ("ret"       . ("return"    . nil))
    ("return"    . ("return"    . nil))
    ("enter"     . ("return"    . nil))
    ("tab"       . ("tab"       . nil))
    ("space"     . ("space"     . nil))
    ("up"        . ("up"        . nil))
    ("down"      . ("down"      . nil))
    ("left"      . ("left"      . nil))
    ("right"     . ("right"     . nil))
    ("backspace" . ("backspace" . nil))
    ("C-c"       . ("c"         . "ctrl"))
    ("C-d"       . ("d"         . "ctrl"))
    ("C-z"       . ("z"         . "ctrl")))
  "Map friendly key names to `ghostel-send-key' (KEY-NAME . MODS) pairs.

Exists so callers -- including agent shells going through `ghostel_cmd'
-- can say \"esc\" or \"C-c\" without knowing ghostel's encoder
vocabulary.  Anything not listed is passed through unchanged, so the
full vocabulary stays reachable.")

(defun ghostherd--resolve-key (key)
  "Return (KEY-NAME . MODS) for KEY, via `ghostherd-key-aliases'."
  (or (alist-get key ghostherd-key-aliases nil nil #'equal)
      (cons key nil)))


;;; The backend slot

(cl-defgeneric ghostherd-backend-capture (backend session &optional n)
  "Return the last N lines of SESSION's screen, as a string.
This is what the screen rules, `ghostherd-read', `ghostherd-explain'
and `ghostherd-wait-output' all read.")

(cl-defgeneric ghostherd-backend-screens (_backend _sessions _n _callback)
  "Fetch the last N lines of every session in SESSIONS at once, if you can.

CALLBACK is called with one argument, an alist of session id → screen
string.  A session missing from it, or present with nothing on it, is one
the caller must read individually -- so a partial answer is a valid
answer, which matters because a host may abandon a batch halfway.

Return non-nil to claim the tick: the caller will then *not* read any
session itself, and expects either a callback or a deliberate silence
\(a fetch already in flight covers this tick as well).  Return nil --
the default -- and the caller falls back to `ghostherd-backend-capture'
per session, which is the right answer whenever a screen is already in
Emacs and costs nothing to read.

Why this exists at all: the poll path used to run one subprocess *per
agent* per tick, synchronously, on the timer.  Six agents meant six
forks every 1.5 seconds inside the redisplay-adjacent path, and a host
that stopped answering froze Emacs rather than the herd.  A host that can
answer for the whole herd in one round trip should say so here."
  nil)

(cl-defgeneric ghostherd-backend-scrollback (_backend _session _lines)
  "Return up to LINES of SESSION's history, or nil when it keeps none.

Distinct from `ghostherd-backend-capture', which is deliberately the
*visible* pane and nothing more -- scrollback is where a permission
prompt that has already scrolled away still lives, and matching one
would be matching something that is no longer true.  Reading it on
purpose is a different act from detecting on it."
  nil)

(cl-defgeneric ghostherd-backend-send-text (backend session text submit)
  "Type TEXT into SESSION, pressing Return when SUBMIT is non-nil.")

(cl-defgeneric ghostherd-backend-send-keys (backend session keys)
  "Send KEYS -- friendly names, see `ghostherd-key-aliases' -- to SESSION.")

(cl-defgeneric ghostherd-backend-scroll (_backend _session _lines)
  "Scroll SESSION's view back by LINES, or forward when LINES is negative.

Distinct from `ghostherd-backend-scrollback\=', which hands you the
history as text to read elsewhere.  This moves the *live view*, so the
buffer you are already in shows older output and then comes back --
which is what an Emacs buffer does, and what people expect of anything
that looks like one.

Returns non-nil when the backend handled it, so a caller can fall back
to ordinary Emacs scrolling."
  nil)

(cl-defgeneric ghostherd-backend-live-p (backend session)
  "Return non-nil when SESSION's host still exists.

\"Exists\", not \"is running\": a host that kept an exited agent around
so its last screen can still be read is live here and `dead' in
`ghostherd-backend-exited-p'.  Callers use this to decide whether the
session can be addressed at all.")

(cl-defgeneric ghostherd-backend-exited-p (_backend _session)
  "Return a reason string when SESSION's agent process has exited, else nil.

Defaults to nil -- a host that cannot tell has to let the screen rules
speak, since claiming an exit that did not happen is the one wrong
answer the state machine cannot recover from."
  nil)

(cl-defgeneric ghostherd-backend-title (_backend _session)
  "Return SESSION's terminal title, or nil."
  nil)

(cl-defgeneric ghostherd-backend-spawn (backend plist)
  "Start an agent described by PLIST on BACKEND.

PLIST carries :name :kind :command :args :directory :project :notes.
Returns a plist with :host-id and, when the backend's host is itself an
Emacs buffer, :buffer.  Displaying the result is the caller's job --
see `ghostherd-backend-view'.")

(cl-defgeneric ghostherd-backend-kill (_backend _session)
  "Kill SESSION's host process."
  nil)

(cl-defgeneric ghostherd-backend-rename (_backend _session _new-name)
  "Tell the host that SESSION is now called NEW-NAME."
  nil)

(cl-defgeneric ghostherd-backend-view (backend session)
  "Return a live Emacs buffer showing SESSION, creating one if needed.")

(cl-defgeneric ghostherd-backend-list (_backend)
  "Return recipe plists for hosted agents this Emacs does not know about.
Used to restore the herd on startup.  Backends whose agents cannot
outlive Emacs return nil."
  nil)

(cl-defgeneric ghostherd-backend-view-is-host-p (_backend)
  "Return non-nil when a session's view buffer runs the agent itself.

The load-bearing difference between the two backends.  Where it is
true, the view process exiting means the agent died; where it is false
it only means a client detached, and treating that as death would turn
every closed window into a crash notification."
  nil)


;;; Session-level wrappers

(defun ghostherd--backend-of (session)
  "Return SESSION's backend symbol.

Guards the one failure this indirection makes possible and unreadable.
Redefining `ghostherd-session' -- which adding the backend slot did --
does not migrate the structs already in the registry, so a session made
by the previous definition is read through the new accessors and every
field after the changed one comes back shifted.  What reaches the
generics is then a project path where a symbol belongs, and the report
is a `cl-no-applicable-method' naming neither the cause nor the cure,
once per redisplay, forever.

Structs cannot be versioned after the fact, so this cannot be repaired
-- only named."
  (let ((backend (ghostherd-session-backend session)))
    (if (symbolp backend)
        backend
      (error "Session %s predates the current `ghostherd-session' \
definition (backend reads as %S).  Reloading ghostherd does not migrate \
live sessions; clear the registry with (clrhash ghostherd--sessions) -- \
agent buffers are left alone -- or restart Emacs"
             (ghostherd-session-name session) backend))))

(defun ghostherd--host-capture (session &optional n)
  "Return the last N lines of SESSION's screen."
  (ghostherd-backend-capture (ghostherd--backend-of session) session n))

(defun ghostherd--host-send-text (session text &optional submit)
  "Type TEXT into SESSION, submitting when SUBMIT is non-nil."
  (ghostherd-backend-send-text
   (ghostherd--backend-of session) session text submit))

(defun ghostherd--host-send-keys (session keys)
  "Send KEYS to SESSION."
  (ghostherd-backend-send-keys (ghostherd--backend-of session) session keys))

(defun ghostherd--host-scrollback (session lines)
  "Return up to LINES of SESSION's history, or nil."
  (ghostherd-backend-scrollback (ghostherd--backend-of session) session lines))

(defun ghostherd--host-scroll (session lines)
  "Scroll SESSION's view back by LINES.  Non-nil when the host handled it."
  (ghostherd-backend-scroll (ghostherd--backend-of session) session lines))

(defun ghostherd--host-live-p (session)
  "Return non-nil when SESSION's host still exists."
  (and session
       (ghostherd-backend-live-p (ghostherd--backend-of session) session)))

(defun ghostherd--host-exited-p (session)
  "Return why SESSION's agent exited, or nil if it has not."
  (ghostherd-backend-exited-p (ghostherd--backend-of session) session))

(defun ghostherd--host-title (session)
  "Return SESSION's terminal title, or nil."
  (ghostherd-backend-title (ghostherd--backend-of session) session))

(defun ghostherd--host-kill (session)
  "Kill SESSION's host process."
  (ghostherd-backend-kill (ghostherd--backend-of session) session))

(defun ghostherd--host-rename (session new-name)
  "Tell SESSION's host it is now called NEW-NAME."
  (ghostherd-backend-rename (ghostherd--backend-of session) session new-name))

(defun ghostherd--host-view (session)
  "Return a live buffer showing SESSION."
  (ghostherd-backend-view (ghostherd--backend-of session) session))


;;; The ghostel implementor
;;
;; Extracted unchanged from what ghostherd did before there was a slot:
;; the agent is an Emacs child process, the buffer is the host, and
;; everything is read off the buffer text.

(defun ghostherd--shell-quote-args (args)
  "Shell-quote ARGS list into a single string."
  (mapconcat #'shell-quote-argument args " "))

(defun ghostherd--build-launch-string (_kind command args)
  "Build the shell command line for COMMAND and ARGS.
_KIND is reserved for kind-specific quoting later."
  (when command
    (string-trim
     (concat command
             (when args
               (concat " " (ghostherd--shell-quote-args args)))))))

(defun ghostherd--launch-in-buffer (buffer launch)
  "Send LAUNCH command + RET in ghostel BUFFER after a short delay."
  (when (and launch (not (string-empty-p launch)))
    (run-at-time
     ghostherd-spawn-delay nil
     (lambda ()
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when (derived-mode-p 'ghostel-mode)
             (ghostel-send-string (concat launch "\n")))))))))

(defun ghostherd--ghostel-buffer (session)
  "Return SESSION's ghostel buffer, or signal."
  (let ((buffer (ghostherd-session-buffer session)))
    (unless (buffer-live-p buffer)
      (user-error "Session buffer is not live"))
    buffer))

(cl-defmethod ghostherd-backend-view-is-host-p ((_backend (eql ghostel)))
  t)

(cl-defmethod ghostherd-backend-capture
  ((_backend (eql ghostel)) session &optional n)
  (ghostherd--buffer-tail (ghostherd--ghostel-buffer session) n))

(cl-defmethod ghostherd-backend-send-text
  ((_backend (eql ghostel)) session text submit)
  (with-current-buffer (ghostherd--ghostel-buffer session)
    (unless (derived-mode-p 'ghostel-mode)
      (user-error "Session buffer is not a ghostel terminal"))
    ;; Prefer bracketed paste for multi-line / long prompts.
    (if (fboundp 'ghostel-paste-string)
        (ghostel-paste-string text)
      (ghostel-send-string text))
    (when submit
      (ghostel-send-key "return"))))

(cl-defmethod ghostherd-backend-send-keys
  ((_backend (eql ghostel)) session keys)
  (with-current-buffer (ghostherd--ghostel-buffer session)
    (unless (derived-mode-p 'ghostel-mode)
      (user-error "Session buffer is not a ghostel terminal"))
    (dolist (key keys)
      (pcase-let ((`(,name . ,mods) (ghostherd--resolve-key key)))
        (ghostel-send-key name mods)))))

(cl-defmethod ghostherd-backend-live-p ((_backend (eql ghostel)) session)
  (buffer-live-p (ghostherd-session-buffer session)))

(cl-defmethod ghostherd-backend-exited-p ((_backend (eql ghostel)) session)
  (let* ((buffer (ghostherd-session-buffer session))
         (proc (and (buffer-live-p buffer)
                    (buffer-local-value 'ghostel--process buffer))))
    (and proc (not (process-live-p proc)) "process exited")))

(cl-defmethod ghostherd-backend-scrollback
  ((_backend (eql ghostel)) session lines)
  ;; The buffer *is* the history here, and it is already an Emacs buffer
  ;; you can move around in -- so this exists for symmetry rather than
  ;; because anyone needs it.
  (ghostherd--buffer-tail (ghostherd--ghostel-buffer session) lines))

(cl-defmethod ghostherd-backend-title ((_backend (eql ghostel)) session)
  (ghostherd--buffer-title (ghostherd-session-buffer session)))

(cl-defmethod ghostherd-backend-view ((_backend (eql ghostel)) session)
  (ghostherd--ghostel-buffer session))

(cl-defmethod ghostherd-backend-kill ((_backend (eql ghostel)) session)
  ;; The buffer is the host; `ghostherd-kill' kills it directly.
  (ignore session))

(cl-defmethod ghostherd-backend-rename
  ((_backend (eql ghostel)) session new-name)
  (let ((buffer (ghostherd-session-buffer session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (rename-buffer (ghostherd--buffer-name new-name) t)
        (setq-local ghostherd-session-id new-name)
        (setq-local ghostel--buffer-identity (buffer-name))))))

(cl-defmethod ghostherd-backend-list ((_backend (eql ghostel)))
  ;; Agents are Emacs children here; when Emacs is gone, so are they.
  nil)

(cl-defmethod ghostherd-backend-spawn ((_backend (eql ghostel)) plist)
  (ghostherd--require-ghostel)
  (let* ((name (plist-get plist :name))
         (bufname (ghostherd--buffer-name name))
         (launch (ghostherd--build-launch-string
                  (plist-get plist :kind)
                  (plist-get plist :command)
                  (plist-get plist :args)))
         buffer)
    (when (get-buffer bufname)
      (user-error "Buffer already exists: %s" bufname))
    (let ((default-directory (plist-get plist :directory))
          (ghostel-buffer-name bufname)
          ;; The shell ghostel starts inherits this, and exports it to the
          ;; agent command typed into it afterwards -- which is why the
          ;; binding has to be in force around `ghostel', not around the
          ;; launch string.
          (process-environment
           (append (ghostherd-agent-environment 'ghostel plist)
                   process-environment))
          ;; Keep exited agent buffers so the herd can mark them dead.
          (ghostel-kill-buffer-on-exit nil))
      (setq buffer (ghostel t))
      ;; `ghostel' may reuse/rename; force our identity/name.
      (with-current-buffer buffer
        (rename-buffer bufname t)
        (setq-local ghostel-kill-buffer-on-exit nil)
        (setq-local ghostherd-session-id name)
        (setq-local ghostel--buffer-identity bufname)
        (ghostherd--launch-in-buffer buffer launch)))
    (list :buffer buffer :host-id name :started (and launch t))))

(provide 'ghostherd-backend)
;;; ghostherd-backend.el ends here
