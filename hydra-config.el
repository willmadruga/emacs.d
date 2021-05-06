;;; hydra-config --- Summary Hydra configuration  ;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;
;; All Hydra shortcuts initiates with C-SPC
;;
;; Some of them were taken from https://github.com/abo-abo/hydra/blob/master/hydra-examples.el
;;
;; Shortcuts:
;; C-SPC +
;; [v] Visual related
;; [t] Toggle modes/functions
;; [h] Helpful
;; [r] Rectangle editing functions
;; [n] Netsuite sdfcli
;; [m] Miscelanea
;;
;;; Code:

(global-unset-key (kbd "C-SPC"))

(defhydra hydra-visual (:color pink :hint nil)
  "
  ^Window Adjustments^   ^Font Size^
^^^^^-----------------------------------
        ^🡱^              _=_ Increase
      ^🡰  🡲^            _-_ Decrease
        ^🡳^
"

  ("=" text-scale-increase)
  ("-" text-scale-decrease)

  ("<left>" hydra-move-splitter-left)
  ("<down>" hydra-move-splitter-down)
  ("<up>" hydra-move-splitter-up)
  ("<right>" hydra-move-splitter-right)

  ("q" nil "quit" :color blue))
(global-set-key (kbd "C-SPC v") 'hydra-visual/body)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar whitespace-mode nil)

(require 'modus-themes)
(require 'writefreely)
(require 'olivetti)

(defhydra hydra-toggle (:color pink :hint nil)
  "
^General^
^^^^^-------
_a_ abbrev-mode:          %`abbrev-mode
_c_ column-number-mode:   %`column-number-mode
_d_ debug-on-error:       %`debug-on-error
_f_ auto-fill-mode:       %`auto-fill-function
_l_ line-number-mode:     %`line-number-mode
_L_ display-line-numbers: %`display-line-numbers-mode
_o_ olivetti-mode:        %`olivetti-mode
_t_ truncate-lines:       %`truncate-lines
_w_ whitespace-mode:      %`whitespace-mode

^Graphical^
^^^---------
_M_ menu-bar-mode     %`menu-bar-mode
_T_ tool-bar-mode     %`tool-bar-mode
_S_ scroll-bar-mode   %`scroll-bar-mode
_F_ fringe-mode       %`fringe-mode
_W_ writefreely-mode: %`writefreely-mode
_X_ modus-themes
"
  ;; General
  ("a" abbrev-mode nil)
  ("c" column-number-mode nil)
  ("d" toggle-debug-on-error nil)
  ("f" auto-fill-mode nil)
  ("l" line-number-mode nil)
  ("L" display-line-numbers-mode nil)
  ("o" olivetti-mode nil)
  ("t" toggle-truncate-lines nil)
  ("w" whitespace-mode nil)
  ("W" writefreely-mode nil)

  ;; Graphical Environment
  ("M" menu-bar-mode nil)
  ("T" tool-bar-mode nil)
  ("S" scroll-bar-mode nil)
  ("F" fringe-mode nil)
  ("X" modus-themes-toggle nil)

  ("q" nil "quit"))
(global-set-key (kbd "C-SPC t") 'hydra-toggle/body)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'helpful)
(defhydra hydra-helpful (:color pink :hint nil)
  "
  ^Helpful^
^^^^^  -------
_c_   Callable        _o_   Command
_f_   Function        _v_   Variable
_k_   Key             _a_   At point
"
  ("c" helpful-callable)
  ("f" helpful-function)
  ("k" helpful-key)
  ("o" helpful-command)
  ("v" helpful-variable)
  ("a" helpful-at-point)
  ("q" nil "quit"))
(global-set-key (kbd "C-SPC h") 'hydra-helpful/body)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'rect)
(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                           :color pink
                           :post (deactivate-mark))
  "
  ^🡱^     ^_d_ Delete      ^_s_ Replace String        ^_u_ Undo
^🡰  🡲^   ^_y_ Yank        ^_r_ Reset                 ^_x_ Kill
  ^🡳^     ^_n_ New copy    ^_q_ Quit
"
  ("<left>" rectangle-backward-char nil)
  ("<right>" rectangle-forward-char nil)
  ("<up>" rectangle-previous-line nil)
  ("<down>" rectangle-next-line nil)
  ("n" copy-rectangle-as-kill nil)
  ("d" delete-rectangle nil)
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)) nil)
  ("y" yank-rectangle nil)
  ("u" undo nil)
  ("s" string-rectangle nil)
  ("x" kill-rectangle nil)
  ("q" nil nil))
(global-set-key (kbd "C-SPC r") 'hydra-rectangle/body)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; require sdfcli.el
(cond
 ((file-exists-p "~/src/netsuite-sdf/sdfcli.el")
  (progn
    (defhydra hydra-netsuite (:color pink :hint nil)
      "
  ^ 2020^         ^2021^
^^^^^  -------------------
_c_ Create
_d_ Deploy       _D_ Deploy
_u_ Upload       _U_ Upload

"
      ("c" netsuite/create-project)
      ("d" netsuite/deploy)
      ("u" netsuite/upload-buffer)
      ("D" netsuite/deploy21)
      ("U" netsuite/upload-buffer21)
      ("q" nil "quit"))
    (global-set-key (kbd "C-SPC n") 'hydra-netsuite/body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'elpher)
(require 'hnreader)
(require 'counsel)
(require 'restart-emacs)
(defhydra hydra-misc (:color pink :hint nil)
  "
  ^Miscelanea^
^^^^^  -------
_e_ Elpher
_h_ Hacker News
_r_ Recentf

_K_ Kill Emacs
_R_ Restart Emacs
"
  ("e" elpher)
  ("h" hnreader-news)
  ("r" counsel-recentf)

  ("K" kill-emacs)
  ("R" restart-emacs)

  ("q" nil "quit"))
(global-set-key (kbd "C-SPC m") 'hydra-misc/body)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helpers

(require 'windmove)
(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(provide 'hydra-config)

;;; hydra-config.el ends here
