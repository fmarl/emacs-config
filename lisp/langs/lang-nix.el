;;; lang-nix.el --- Nix development setup -*- lexical-binding: t; -*-

(defgroup nixcmds nil
  "Run Nix commands from Emacs."
  :group 'tools
  :prefix "nixcmds-")

(defcustom nixcmds-privilege-command "sudo -S "
  "Command used to gain root privileges (e.g. sudo or doas)."
  :type 'string
  :group 'nixcmds)

(defcustom nixos-rebuild-base-command "nixos-rebuild"
  "Base nixos-rebuild command."
  :type 'string
  :group 'nixcmds)

(defcustom darwin-rebuild-base-command "darwin-rebuild"
  "Base darwin-rebuild command."
  :type 'string
  :group 'nixcmds)

(defcustom home-manager-base-command "home-manager"
  "Base home-manager command."
  :type 'string
  :group 'nixcmds)

(defun nixcmds--project-root ()
  "Return project root directory or signal an error."
  (or (when-let ((proj (project-current)))
        (project-root proj))
      (user-error "No projekt-root found (project.el)")))

(defun nixcmds--flake-root ()
  "Return directory containing flake.nix."
  (let ((root (nixcmds--project-root)))
    (if (file-exists-p (expand-file-name "flake.nix" root))
        root
      (user-error "No flake.nix found in project-root"))))

(defun nixcmds--run (basecommand subcommand)
  "Run nixcmds SUBCOMMAND using compile with password via SUDO_ASKPASS."
  (let* ((default-directory (nixcmds--flake-root))
         (password (read-passwd "Password? "))
         (askpass-script (make-temp-file "emacs-askpass-" nil ".sh"))
         (cmd (format "%s %s %s --flake ."
                      basecommand
                      subcommand
                      (if (string-prefix-p "sudo" nixcmds-privilege-command)
                          "-A" ""))))
    ;; Create temporary askpass script
    (with-temp-file askpass-script
      (insert "#!/bin/sh\n")
      (insert (format "echo %s\n" (shell-quote-argument password))))
    (set-file-modes askpass-script #o700)
    ;; Set SUDO_ASKPASS and run command
    (let ((process-environment
           (cons (format "SUDO_ASKPASS=%s" askpass-script)
                 process-environment)))
      (compile (concat nixcmds-privilege-command cmd)))
    ;; Clean up askpass script after a delay
    (run-at-time 5 nil
                 (lambda (file)
                   (when (file-exists-p file)
                     (delete-file file)))
                 askpass-script)))

;;;###autoload
(defun nixos-rebuild-switch ()
  "Run nixos-rebuild switch."
  (interactive)
  (nixcmds--run nixos-rebuild-base-command "switch"))

;;;###autoload
(defun home-manager-switch ()
  "Run home-manager switch."
  (interactive)
  (nixcmds--run home-manager-base-command "switch"))

;;;###autoload
(defun darwin-rebuild-switch ()
  "Run darwin-rebuild switch."
  (interactive)
  (nixcmds--run darwin-rebuild-base-command "switch"))

;;;###autoload
(defun nixos-rebuild-test ()
  "Run nixos-rebuild test."
  (interactive)
  (nixcmds--run nixos-rebuild-base-command "test"))

;;;###autoload
(defun nixos-rebuild-boot ()
  "Run nixos-rebuild boot."
  (interactive)
  (nixcmds--run nixos-rebuild-base-command "boot"))

(defvar nixcmds-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c x s") #'nixos-rebuild-switch)
    (define-key map (kbd "C-c x t") #'nixos-rebuild-test)
    (define-key map (kbd "C-c x b") #'nixos-rebuild-boot)
    (define-key map (kbd "C-c x h") #'home-manager-switch)
    (define-key map (kbd "C-c x d") #'darwin-rebuild-switch)
    map)
  "Keymap for `nixcmds-mode'.")

;;;###autoload
(define-minor-mode nixcmds-mode
  "Minor mode for running Nix commands."
  :lighter " NixCmds"
  :keymap nixcmds-mode-map)

(use-package nix-ts-mode
  :init (setq treesit-font-lock-level 4)
  :hook (nix-ts-mode . nixcmds-mode)
  :mode "\\.nix\\'")

(provide 'lang-nix)
