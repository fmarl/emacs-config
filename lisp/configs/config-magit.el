;;; config-magit.el --- Magit related config -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'my-cache)

(use-package magit
  :bind (("C-c u" . magit-status-quick)))

(use-package transient
  :defer t
  :custom
  (transient-history-file (my/cache-file "transient/history.el"))
  (transient-levels-file (my/cache-file "transient/levels.el"))
  (transient-values-file (my/cache-file "transient/values.el")))

(provide 'config-magit)

;;; config-magit.el ends here
