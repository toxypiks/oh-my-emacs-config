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
	 '(exec-path-from-shell toml-mode rust-playground rustic flycheck-pyflakes auto-complete-c-headers yasnippet-snippets flymake ac-c-headers google-c-style cmake-mode rainbow-delimiters flycheck use-package haskell-mode elpy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#ff0000" :height 1.6))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#dda000" :height 1.5))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#f7fd10" :height 1.4))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "green" :height 1.3))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "blue" :height 1.2))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "violet" :height 1.1))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "purple" :height 1.0))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "grey" :height 0.9))))
 '(rainbow-delimiters-unmatched-face ((t (:background "cyan" :height 0.8)))))

(require 'use-package)

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
;; elpy
;; Fixing a key binding bug in elpy
(elpy-enable)
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
(define-key global-map (kbd "C-c o") 'iedit-mode)

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
(setq-default tab-width 2 indent-tabs-mode t)

(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'c-mode-hook 'set-newline-and-indent)
(add-hook 'c++-mode-hook 'set-newline-and-indent)

;; start auto-complete with emacs
(require 'auto-complete)
; do default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)

;;; start yasnippet with emacs
(require 'yasnippet)
;; (yas-global-mode 1)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

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
