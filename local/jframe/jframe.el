;;; jframe.el --- summary -*- lexical-binding: t -*-

;; Author: jing
;; Maintainer: jing
;; Version: version
;; Keywords: frame


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:


(defcustom infinite-gap 10
  "Gap in pixels between windows."
  :group 'infinite
  :type 'integer)

(defvar infinite--animation-duration 0.33)

(defcustom infinite-animation-speed 100
  "Animation speed in percent.
Lower values slow down animation, higher values speed it up."
  :set (lambda (_ val)
         (setq infinite--animation-duration (/ 0.33 (/ val 100.0))))
  :group 'infinite
  :type 'integer)

(defcustom infinite-animate-transitions t
  "Animation speed in percent.
Lower values slow down animation, higher values speed it up."
  :group 'infinite
  :type 'boolean
  :package-version '(infinite "0.0.5"))

(defvar infinite--base-frame nil)
(defvar infinite--last-frame nil)
(defvar infinite--default-pixel-width 560)
(defvar infinite--default-pixel-height 720)

(defvar infinite--header-line-format
  (concat " "
          (propertize
           " x "
           'face '(:foreground unspecified :background "red")
           'pointer 'hand
           'help-echo "close window"
           'local-map (let ((map (make-sparse-keymap)))
                        (define-key map [header-line mouse-1] 'infinite-close-frame)
                        map))
          " "
          (propertize
           " ^ "
           'face '(:foreground unspecified :background "green")
           'pointer 'hand
           'help-echo "maximize window"
           'local-map (let ((map (make-sparse-keymap)))
                        (define-key map [header-line mouse-1] #'infinite-maximize-frame)
                        map))
          " "
          " %b"))

(defun infinite-frame-p (frame)
  "Check if the FRAME is a window spawned by infinite."
  (frame-parameter frame 'infinite-frame))

(defun infinite-maximize-frame (event)
  "Maximize the frame based on the EVENT."
  (interactive "e")
  (let* ((window (posn-window (event-start event)))
         (frame (window-frame window))
         (parent-window (frame-root-window (frame-parent frame))))
    (setq infinite--last-frame (window-frame window))
    (dolist (f (frame-list))
      (when (infinite-frame-p f)
        (make-frame-invisible f t)))
    (select-window parent-window)
    (switch-to-buffer (window-buffer window))
    (infinite-maximized 1)))

(defun infinite-unmaximize-frame (_)
  "Unmaximize the current buffer into a frame."
  (interactive "e")
  (switch-to-buffer " *infinite*")
  (delete-other-windows)
  (dolist (f (frame-list))
    (when (infinite-frame-p f)
      (make-frame-visible f)))
  (select-frame infinite--last-frame)
  (infinite-maximized -1))

(defun infinite-close-frame (event)
  "Close currently selected frame based on EVENT.
Removes it from the list of infinite frames."
  (interactive "e")
  (let* ((frame (window-frame (posn-window (event-start event))))
         (parent (frame-parameter frame 'infinite-parent)))
    (when (frame-live-p parent)
      (infinite--animate-focus-transition
       (frame-position parent) (frame-position frame)))
    (delete-frame frame t)
    (when (frame-live-p parent)
      (select-frame parent))))

(defun infinite--pan-desktop (dx dy &optional pos-function)
  "Move all frames according to DX and DY pixel deltas.
Optional argument POS-FUNCTION extracts position of each frame."
  (dolist (f (frame-list))
    (when (infinite-frame-p f)
      (let ((p (if (functionp pos-function)
                   (funcall pos-function f)
                 (frame-position f))))
        (modify-frame-parameters
         f
         `((user-position . t)
           (left . (+ ,(- (car p) dx)))
           (top . (+ ,(- (cdr p) dy)))))))))

(defun infinite--track-mouse (_)
  "Track mouse dragging events.
Moves all visible child frames that were opened in the infinite
buffer."
  (interactive "e")
  (track-mouse
    (catch 'done
      (while t
        (let ((beginning-position (mouse-pixel-position))
              event)
          (setq event (read-event))
          (pcase (event-basic-type event)
            ('mouse-movement
             (let* ((current-position (mouse-pixel-position))
                    (dx (- (cadr beginning-position)
                           (cadr current-position)))
                    (dy (- (cddr beginning-position)
                           (cddr current-position))))
               (setq track-mouse 'dragging)
               (infinite--pan-desktop dx dy)))
            ('mouse-1 (throw 'done nil))))))))

(defun infinite--track-wheel (event)
  "Track mouse scrolling EVENT.
Moves all visible child frames that were opened in the infinite
buffer."
  (interactive "e")
  (let* ((delta
          (pcase (event-basic-type event)
            ('wheel-right (cons -10 0))
            ('wheel-left (cons 10 0))
            ('wheel-up (cons 0 -10))
            ('wheel-down (cons 0 10))))
         (dx (car delta))
         (dy (cdr delta)))
    (infinite--pan-desktop dx dy)))

(defun infinite--lerp (start-pos end-pos time)
  "Linearly interpolate between START-POS END-POS over TIME."
  (cons (floor (+ (car start-pos) (* (- (car end-pos) (car start-pos)) time)))
        (floor (+ (cdr start-pos) (* (- (cdr end-pos) (cdr start-pos)) time)))))

(defun infinite--ease-out (time)
  "Easing function of TIME."
  (- 1 (* (- 1 time) (- 1 time))))

(defun infinite--original-pos (frame)
  "Return original position of the FRAME.
Used for animation only."
  (frame-parameter frame 'infinite-original-pos))

(defun infinite--animate-transition (old-pos new-pos start-time)
  "Animate all frames moving.
Given a new position NEW-POS and an old position OLD-POS,
interpolate between them while moving all visible frames, such
that the new position is moved to the old position.

This function sets up a timer to call itself on the next tick.
START-TIME represents the original moment in time this function
was called."
  (let* ((time (float-time (time-subtract (current-time) start-time)))
         (new-pos* (infinite--lerp
                    old-pos
                    new-pos
                    (infinite--ease-out
                     (/ time infinite--animation-duration))))
         (dx (- (car old-pos) (car new-pos*)))
         (dy (- (cdr old-pos) (cdr new-pos*))))
    (infinite--pan-desktop dx dy #'infinite--original-pos)
    (when (< time infinite--animation-duration)
      (run-at-time
       0.01 nil
       #'infinite--animate-transition
       old-pos new-pos start-time))))

(defun infinite--clamp (low x &optional hi)
  "Clamp X between LOW and HI boundaries."
  (min (max low x)
       (or hi most-positive-fixnum)))

(defun infinite--clamp-pos (pos)
  "Clamp given position POS between screen boundaries.
Respects default frame widht and height and the gap setting."
  (let* ((window (frame-root-window infinite--base-frame))
         (width (window-pixel-width window))
         (height (window-pixel-height window)))
    (cons
     (infinite--clamp
      infinite-gap
      (car pos)
      (infinite--clamp
       infinite-gap
       (- width infinite--default-pixel-width infinite-gap)))
     (infinite--clamp
      infinite-gap
      (cdr pos)
      (infinite--clamp
       infinite-gap
       (- height infinite--default-pixel-height infinite-gap))))))

(defun infinite--animate-focus-transition (old-pos new-pos)
  "Animate the apperance of a new frame.
Moves view from OLD-POS to a NEW-POS."
  (when infinite-animate-transitions
    (dolist (f (frame-list))
      (when (infinite-frame-p f)
        (set-frame-parameter f 'infinite-original-pos (frame-position f))))
    (infinite--animate-transition old-pos (infinite--clamp-pos new-pos) (current-time))))

(defun infinite--space-occupied-p (p1x p1y p2x p2y)
  "Check if space is already occupied by another frame.
P1X P1Y P2X P2Y are the top left and bottom right x and y
components of a rectangle.  See `infinite--rectangle-overlap-p'
for more info."
  (catch 'yes
    (dolist (f (frame-list))
      (when (and (infinite-frame-p f)
                 (frame-visible-p f))
        (let* ((pos (frame-position f))
               (p3x (car pos))
               (p3y (cdr pos))
               (p4x (+ p3x (frame-pixel-width f)))
               (p4y (+ p3y (frame-pixel-height f))))
          (when (infinite--rectangle-overlap-p
                 p1x p1y p2x p2y
                 p3x p3y p4x p4y)
            (throw 'yes f)))))))

(defun infinite--rectangle-overlap-p
    ( p1x p1y p2x p2y
      p3x p3y p4x p4y)
  "Check if two rectangles overlap.
P1X P1Y P2X P2Y correspond to coordinates of the first rectangle
top left and bottom right x and y components respectively.  P3X
P3Y P4X P4Y are the same for the second rectangle.

 P1__________
  |          |
  |     P3___|______
  |      |   |      |
  |      |   |      |
  |______|___|      |
         |   P2     |
         |__________|
                    P4"
  (not (or (< p2y p3y) (> p1y p4y) (< p2x p3x) (> p1x p4x))))

(defun infinite--screen-center-pos ()
  "Return a position on the screen that will center a new frame."
  (let ((window (frame-root-window infinite--base-frame)))
    (cons (- (/ (window-pixel-width window) 2)
             (/ infinite--default-pixel-width 2))
          (- (/ (window-pixel-height window) 2)
             (/ infinite--default-pixel-height 2)))))

(defun infinite--find-free-space (&optional direction frame)
  "Find nearest free space for a given FRAME.
If FRAME is not provided starts the search from the top left
corner.  DIRECTION corresponds to initial search direction, but
doesn't ultimatively mean that the space will be in that
direction."
  (catch 'pos
    (let (f dir)
      (setq f frame dir (or direction 'right))
      (while t
        (let* ((pos (if f (frame-position f)
                      (infinite--screen-center-pos)))
               (x (car pos))
               (y (cdr pos))
               (pos (pcase dir
                      ('right
                       (cons (+ x (if f (frame-pixel-width f) 0) infinite-gap) y))
                      ('left
                       (cons (- x (if f infinite--default-pixel-width 0) infinite-gap) y))
                      ('below
                       (cons x (+ y (if f (frame-pixel-height f) 0) infinite-gap)))
                      ('above
                       (cons x (- y (if f infinite--default-pixel-height 0) infinite-gap)))))
               (colliding-frame
                (infinite--space-occupied-p
                 (car pos) (cdr pos)
                 (+ (car pos) infinite--default-pixel-width)
                 (+ (cdr pos) infinite--default-pixel-height))))
          (when (null colliding-frame) (throw 'pos pos))
          (setq dir
                (if (null f) 'right
                  (pcase dir
                    ('right 'below)
                    ('below 'left)
                    ('left 'below)
                    ('above 'right))))
          (setq f colliding-frame))))))

(defun infinite--open-side-window (window _ side)
  "Open a new window to the SIDE of the given WINDOW."
  (let* ((frame (window-frame window))
         (new-pos (infinite--find-free-space side (window-frame window)))
         (window (frame-selected-window
                  (infinite--make-frame new-pos frame))))
    (infinite--animate-focus-transition
     new-pos (frame-position frame))
    window))

(defun infinite--new-window (&rest _)
  "Open a new window."
  (let* ((new-pos (infinite--find-free-space))
         (center-pos (infinite--screen-center-pos))
         (frame (infinite--make-frame new-pos)))
    (infinite--animate-focus-transition
     new-pos
     center-pos)
    (frame-selected-window frame)))

(defun infinite--make-frame (pos &optional parent buffer norecord _)
  "Make frame that obeys infinite rules.
Frame is positioned at POS, and may optionally contain a given
BUFFER with the respect to the NORECORD parameter.  PARENT is not
used to specify the `parent-frame' parameter, but to indicate the
parent frame to focuse when the frame is killed."
  (let ((nframe (make-frame
                 `((left . ,(car pos)) (top . ,(cdr pos))
                   (width . 80) (height . 42)
                   (parent-frame . ,infinite--base-frame)
                   (infinite-parent . ,parent)
                   (child-frame-border-width . 2)
                   (drag-with-header-line . t)
                   (drag-internal-border . t)
                   (undecorated . t)
                   (minibuffer . nil)
                   (infinite-frame . t)))))
    (setq infinite--default-pixel-width
          (frame-pixel-width nframe)
          infinite--default-pixel-height
          (frame-pixel-height nframe))
    (select-frame nframe)
    (when buffer
      (switch-to-buffer buffer norecord))
    (let ((window (frame-root-window nframe)))
      (set-window-parameter
       window
       'header-line-format
       infinite--header-line-format)
      (set-window-parameter
       window
       'split-window
       #'infinite--open-side-window))
    nil))

(defvar-keymap infinite-mode-map
  :doc "The key map used by `infinite-mode'."
  "<down-mouse-1>" #'infinite--track-mouse
  "<wheel-left>"   #'infinite--track-wheel
  "<wheel-right>"  #'infinite--track-wheel
  "<wheel-up>"     #'infinite--track-wheel
  "<wheel-down>"   #'infinite--track-wheel
  "<remap> <find-file>" #'infinite-visit-file
  "<remap> <switch-to-buffer>" #'infinite-open-buffer)

(define-derived-mode infinite-mode fundamental-mode
  "Infinite"
  "Major mode for Infinite window system.
Don't call manually, instead use `infinite-start'."
  :keymap infinite-mode-map
  (setq infinite--base-frame (window-frame (get-buffer-window))
        mode-line-format nil)
  (make-local-variable 'minor-mode-overriding-map-alist)
  (push `(pixel-scroll-precision-mode . ,infinite-mode-map)
        minor-mode-overriding-map-alist)
  (let ((window (frame-root-window)))
    (set-window-parameter
     window
     'split-window
     #'infinite--new-window)))

;;;###autoload
(define-minor-mode infinite-maximized
  "Maximized mode for infinite frame."
  :group 'infinite
  :global t
  :lighter (:propertize
            " unmaximize"
            help-echo "unmaximize current window"
            pointer hand
            local-map (keymap
                       (mode-line keymap
                                  (mouse-1 . infinite-unmaximize-frame)))))

;;;###autoload
(defun infinite-start ()
  "Create infinite space that can be panned with mouse and spawn windows on it."
  (interactive)
  (delete-other-windows)
  (switch-to-buffer " *infinite*")
  (read-only-mode)
  (infinite-mode)
  (delete-frame (infinite--make-frame (cons -1000 -1000)))
  (setq split-height-threshold nil)
  (setq split-width-threshold 0))

(defun infinite-open-buffer (buffer-or-name &optional norecord _)
  "Display buffer BUFFER-OR-NAME in the new window.

If optional argument NORECORD is non-nil, do not put the buffer
at the front of the buffer list, and do not make the window
displaying it the most recently selected one."
  (interactive "B")
  (let ((center-pos (infinite--screen-center-pos))
        (pos (infinite--find-free-space)))
    (infinite--make-frame pos nil buffer-or-name norecord)
    (infinite--animate-focus-transition pos center-pos)))

(defun infinite-visit-file (filename &optional wildcards &rest _)
  "Edit file FILENAME.
Switch to a window visiting file FILENAME, creating one in the
infinite buffer.

Interactively, or if WILDCARDS is non-nil in a call from Lisp,
expand wildcards (if any) and visit multiple files.  You can
suppress wildcard expansion by setting ‘find-file-wildcards’ to nil."
  (interactive
   (find-file-read-args "Find file: "
                        (confirm-nonexistent-file-or-buffer)))
  (let* ((value (find-file-noselect filename nil nil wildcards))
         (values (if (listp value) value (list value)))
         last-pos)
    (dolist (value values)
      (let ((pos (infinite--find-free-space)))
        (setq last-pos pos)
        (infinite--make-frame pos nil value)))
    (message "%S %S" last-pos
     (infinite--screen-center-pos))
    (infinite--animate-focus-transition
     last-pos
     (infinite--screen-center-pos))))


(provide 'jframe)

;;; jframe.el ends here
