;;; hydra-config --- Summary Hydra configuration  ;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;
;; All Hydra shortcut prefix is:
;; C-SPC on Emacs
;; C-@   on emacs-nox
;;
;; Some of them were taken from https://github.com/abo-abo/hydra/blob/master/hydra-examples.el
;;
;; Shortcuts:

;; <prefix> + [v] Visual related
;; <prefix> + [t] Toggle modes/functions
;; <prefix> + [h] Helpful
;; <prefix> + [r] Rectangle editing functions
;; <prefix> + [n] Netsuite sdfcli
;; <prefix> + [m] Miscelanea
;; <prefix> + [g] Toggles for graphical environment
;;
;;; Code:

(wmad/package-install 'hydra)
(require 'hydra)

(global-unset-key (kbd "C-SPC"))
(global-unset-key (kbd "C-@"))

;;;;;;;;;;;;;;; VISUAL ;;;;;;;;;;;;;;;;;
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
(global-set-key (kbd "C-@ v") 'hydra-visual/body)

;;;;;;;;;;;;;;; TOGGLE ;;;;;;;;;;;;;;;;;
(defvar whitespace-mode nil)

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
_o_ origami-toggle-node
_O_ olivetti-mode:        %`olivetti-mode
_s_ speedbar:
_t_ truncate-lines:       %`truncate-lines
_w_ whitespace-mode:      %`whitespace-mode
"
  ;; General
  ("a" abbrev-mode nil)
  ("c" column-number-mode nil)
  ("d" toggle-debug-on-error nil)
  ("f" auto-fill-mode nil)
  ("l" line-number-mode nil)
  ("L" display-line-numbers-mode nil)
  ("o" origami-toggle-node nil)
  ("O" olivetti-mode nil)
  ("s" speedbar nil)
  ("t" toggle-truncate-lines nil)
  ("w" whitespace-mode nil)

  ("q" nil "quit"))
(global-set-key (kbd "C-SPC t") 'hydra-toggle/body)
(global-set-key (kbd "C-@ t") 'hydra-toggle/body)

;;;;;;;;;;;;;;; GRAPHICAL ;;;;;;;;;;;;;;;;;
(and (display-graphic-p)
     (require 'modus-themes)
     (require 'writefreely)
     (require 'olivetti)
     (defhydra hydra-toggle-graphical (:color pink :hint nil)
       "
^Toggle^
^^^---------
_m_ menu-bar-mode
_t_ tool-bar-mode
_s_ scroll-bar-mode
_f_ fringe-mode
_w_ writefreely-mode
_x_ modus-themes
"
       ;; Graphical Environment
       ("m" menu-bar-mode nil)
       ("t" tool-bar-mode nil)
       ("s" scroll-bar-mode nil)
       ("f" fringe-mode nil)
       ("w" writefreely-mode nil)
       ("x" modus-themes-toggle nil)

       ("q" nil "quit"))
     (global-set-key (kbd "C-SPC g") 'hydra-toggle-graphical/body)
     )

;;;;;;;;;;;;;;; HELPFUL ;;;;;;;;;;;;;;;;;
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
(global-set-key (kbd "C-@ h") 'hydra-helpful/body)

;;;;;;;;;;;;;;; RECTANGLE ;;;;;;;;;;;;;;;;;
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
(global-set-key (kbd "C-@ r") 'hydra-rectangle/body)

;;;;;;;;;;;;;;; NETSUITE ;;;;;;;;;;;;;;;;;
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
    (global-set-key (kbd "C-SPC n") 'hydra-netsuite/body)
    (global-set-key (kbd "C-@ n") 'hydra-netsuite/body))))

;;;;;;;;;;;;;;; MISC ;;;;;;;;;;;;;;;;;
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
_n_ News (rss)
_r_ Recentf

_K_ Kill Emacs
_R_ Restart Emacs
"
  ("e" elpher)
  ("h" hnreader-news)
  ("n" newsticker-treeview)
  ("r" counsel-recentf)

  ("K" kill-emacs)
  ("R" restart-emacs)

  ("q" nil "quit"))
(global-set-key (kbd "C-SPC m") 'hydra-misc/body)
(global-set-key (kbd "C-@ m") 'hydra-misc/body)


;;;;;;;;;;;;;;; HELPER FUNCTIONS ;;;;;;;;;;;;;;;;;
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
