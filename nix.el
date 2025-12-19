;; -*- lexical-binding: t; -*-

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
  "Run nixcmds SUBCOMMAND using compile."
  (let* ((default-directory (nixcmds--flake-root))
         (cmd (string-join
			 (list
			  (concat "echo " (shell-quote-argument (read-passwd "Password? ")) " | ")
			  nixcmds-privilege-command
			  basecommand
			  subcommand
			  "--flake .")
			 " ")))
    (compile cmd)))

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
    (define-key map (kbd "C-c n s") #'nixos-rebuild-switch)
    (define-key map (kbd "C-c n t") #'nixos-rebuild-test)
    (define-key map (kbd "C-c n b") #'nixos-rebuild-boot)
    (define-key map (kbd "C-c n h") #'home-manager-switch)
    (define-key map (kbd "C-c n d") #'darwin-rebuild-switch)
    map)
  "Keymap for `nixcmds-mode'.")

;;;###autoload
(define-minor-mode nixcmds-mode
  "Minor mode for running Nix commands."
  :lighter " NixCmds"
  :keymap nixcmds-mode-map)

(use-package nix-ts-mode
  :init (setq treesit-font-lock-level 4)
  :mode "\\.nix\\'")
