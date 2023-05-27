; start package.el with emacs
(require 'package)
; initialize package.el
(package-initialize)

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(require 'package)
(dolist (source '(("melpa" . "https://melpa.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes '(wombat))
 '(help-at-pt-display-when-idle '(flymake-overlay) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.9)
 '(line-number-mode nil)
 '(package-selected-packages
   '(company-box pyvenv lsp-ui lsp-mode smartparens web-mode flx-ido flex-autopair js2-mode magit wsd-mode exec-path-from-shell toml-mode rust-playground rustic flycheck-pyflakes auto-complete-c-headers yasnippet-snippets flymake ac-c-headers google-c-style cmake-mode rainbow-delimiters flycheck use-package haskell-mode elpy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#ff0000" :height 1.6))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#dda000" :height 1.5))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#f7fd10" :height 1.4))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "green" :height 1.3))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "brightblue" :height 1.2))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "violet" :height 1.1))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "purple" :height 1.0))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "grey" :height 0.9))))
 '(rainbow-delimiters-unmatched-face ((t (:background "cyan" :height 0.8)))))

(require 'use-package)

;;; no tab
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4 indent-tabs-mode t)

;; rainbow brackets
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; rainbow-delimiters-mode setup, with decreasing bracket size

;; auto close bracket insertion. New in emacs 24
(electric-pair-mode 1)

;;; programming part
;;; shortkeys:
(global-set-key (kbd "C-c c")        'comment-region)
(global-set-key (kbd "C-c C")    'uncomment-region)


;; Python Stuff

;;; python stuff
;; Use flycheck-pyflakes for python. Seems to work a little better.
(use-package lsp-mode
  :config
  (setq lsp-idle-delay 0.5
        lsp-enable-symbol-highlighting t
        lsp-enable-snippet nil  ;; Not supported by company capf, which is the recommended company backend
        lsp-pyls-plugins-flake8-enabled t)
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)

     ;; Disable these as they're duplicated by flake8
     ("pyls.plugins.pycodestyle.enabled" nil t)
     ("pyls.plugins.mccabe.enabled" nil t)
     ("pyls.plugins.pyflakes.enabled" nil t)))
  :hook
  ((python-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  ;(evil-normal-state-map)
  )

(use-package lsp-ui
 :config (setq lsp-ui-sideline-show-hover t
                lsp-ui-sideline-delay 0.5
                lsp-ui-doc-delay 5
                lsp-ui-sideline-ignore-duplicates t
				lsp-ui-doc-delay 0.2
                lsp-ui-doc-position 'bottom
                lsp-ui-doc-alignment 'frame
                lsp-ui-doc-header nil
                lsp-ui-doc-include-signature t
                lsp-ui-doc-use-childframe t)
  :commands lsp-ui-mode
)

(use-package pyvenv
  :ensure t
  :defer t
  :diminish
  :config
	(setenv "WORKON_HOME" "/home/laura/warteapparat_working/warteapparat")
	;(pyvenv-tracking-mode 1))  ; Automatically use pyvenv-workon via dir-locals (not used yet)
	; Show python venv name in modeline2
	(setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
	(pyvenv-mode t))


(use-package company
	:after lsp-mode
	:hook (lsp-mode . company-mode)
  :ensure
  :bind
  (:map company-active-map
              ("C-n". company-select-next)
              ("C-p". company-select-previous)
              ("M-<". company-select-first)
              ("M->". company-select-last))
  (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  ;(:map company-mode-map
  ;      ("<tab>". tab-indent-or-complete)
  ;      ("TAB". tab-indent-or-complete))
  
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

; company box - icon support für nicht shell mode
;(use-package company-box
;  :hook (company-mode . company-box-mode))

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

;;; start yasnippet with emacs
(require 'yasnippet)
;; (yas-global-mode 1)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

;;; flycheck-pyflakes
;(require 'flycheck-pyflakes)
;(add-hook 'python-mode-hook 'flycheck-mode)

;; haskell stuff
(require 'haskell-mode)
(use-package haskell-mode
	     :ensure t
	     :config
	     (add-hook 'haskell-mode-hook
		       (lambda ()
			 (interactive-haskell-mode)
			 (flycheck-mode))))

;; C/C++ stuff
; einrückungen 2 spaces:
(setq-default c-basic-offset 2 c-default-style "linux")
;(setq-default tab-width 2 indent-tabs-mode t)

(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'c-mode-hook 'set-newline-and-indent)
(add-hook 'c++-mode-hook 'set-newline-and-indent)

;; start auto-complete with emacs
;(require 'auto-complete)
; do default config for auto-complete
;(require 'auto-complete-config)
;(ac-config-default)

; let's define a function which initializes auto-complete-c-headers and gets called for c/c++ hooks
(defun my:ac-c-header-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
 ; (add-to-list 'achead:include-directories '"/home/major/c_zeuch/cpp11/lambdatest")
  (setq achead:include-directories
							 (append '("/usr/lib/gcc/x86_64-pc-linux-gnu/11.1.0/../../../../include/c++/11.1.0"
                "/usr/lib/gcc/x86_64-pc-linux-gnu/11.1.0/../../../../include/c++/11.1.0/x86_64-pc-linux-gnu"
                "/usr/lib/gcc/x86_64-pc-linux-gnu/11.1.0/../../../../include/c++/11.1.0/backward"
                "/usr/lib/gcc/x86_64-pc-linux-gnu/11.1.0/include"
                "/usr/local/include"
                "/usr/lib/gcc/x86_64-pc-linux-gnu/11.1.0/include-fixed"
                "/usr/include")
	     achead:include-directories))
)
; now let's call this function from c/c++ hooks
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)

;start iedit
(require 'iedit) 

;start google-c-style with emacs
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

; CEDET stuff
; turn on Semantic 
(semantic-mode 1)
; let's define a function which adds semantic as a suggestion backend to auto complete
; and hook this function to c-mode-common-hook 
; ;connect semantic to autocpmplete
(defun my:add-semantic-to-autocomplete() 
  (add-to-list 'ac-sources 'ac-source-semantic)
)
(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)
; needs to save results of its parsing

;turn on ede mode
(global-ede-mode 1)

(global-display-line-numbers-mode)

;; Rust stuff
(with-eval-after-load 'rust-mode
  ; remove rust-mode from the list of file extension mappings
  (setq auto-mode-alist (rassq-delete-all 'rust-mode auto-mode-alist))
  )
(add-hook 'rust-mode-hook
          (lambda () (error "Don't use rust-mode, use rustic")
						))

(use-package rustic
	:ensure
	:bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c d" . dap-hydra)
              ("C-c C-c h" . lsp-ui-doc-glance))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))
(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))
;; for rust-analyzer integration


;; Create / cleanup rust scratch projects quickly
;;(use-package rust-playground :ensure)


;; for Cargo.toml and other config files
(use-package toml-mode :ensure)

;; setting up debugging support with dap-mode

(use-package exec-path-from-shell
  :ensure
  :init (exec-path-from-shell-initialize))

(when (executable-find "lldb-mi")
  (use-package dap-mode
    :ensure
    :config
    (dap-ui-mode)
    (dap-ui-controls-mode 1)

    (require 'dap-lldb)
    (require 'dap-gdb-lldb)
    ;; installs .extension/vscode
    (dap-gdb-lldb-setup)
    (dap-register-debug-template
     "Rust::LLDB Run Configuration"
     (list :type "lldb"
           :request "launch"
           :name "LLDB::Run"
	   :gdbpath "rust-lldb"
           ;; uncomment if lldb-mi is not in PATH
           ;; :lldbmipath "path/to/lldb-mi"
     ))))

;;; magit
(add-to-list 'load-path "~/.emacs.d/site-lisp/magit/lisp")
(require 'magit)
(with-eval-after-load 'info
	(info-initialize)
	(add-to-list 'Info-directory-list "~/.emacs.d/site-lisp/magit/Documentation/")
	)
(global-set-key (kbd "C-c m s") 'magit-status)
(global-set-key (kbd "C-c m l") 'magit-log)


;; web javascript
;(add-hook 'js-mode-hook #'smartparens-mode)

;; javascript
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(setq js2-mode-hook
  '(lambda () (progn
    (set-variable 'indent-tabs-mode nil))))

(setq html-mode-hook
  '(lambda () (progn
    (set-variable 'indent-tabs-mode nil))))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

;;js2-mode steals TAB, let's steal it back for yasnippet
(defun js2-tab-properly ()
  (interactive)
  (let ((yas/fallback-behavior 'return-nil))
    (unless (yas/expand)
      (indent-for-tab-command)
      (if (looking-back "^\s*")
          (back-to-indentation)))))
(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "TAB") 'js2-tab-properly))

(set-cursor-color "#aaaaaa")

;flex-pair
;(require 'flex-autopair)
;(flex-autopair-mode 1)

;;; flx-ido
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(require 'web-mode)
(add-hook 'web-mode-hook 'autopair-mode)
(add-to-list 'auto-mode-alist '("\\.php$" . web-mode))
