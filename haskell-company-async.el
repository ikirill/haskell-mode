;;; haskell-company-async.el --- Haskell async completion for company.
;;; Commentary:
;;
;; This file defines an asynchronous backend for `company-mode', using
;; `haskell-interactive-mode'.
;;
;; To enable: call `haskell-company-async-setup', or add
;; `haskell-company-async' to `company-backends', but make sure that
;; it is added before the `company-capf' backend, or else the capf
;; (synchronous) backend will get called instead.
;;
;;; Code:

(eval-when-compile (require 'cl))
(require 'cl-macs)
(require 'dash)

(require 'company)
(require 'haskell-doc)

(defun haskell-company-async-setup ()
  (interactive)
  (add-to-list 'company-backends #'haskell-company-async))

(defun haskell-company-async (command &optional arg &rest _rest-args)
  (pcase command
    ('prefix
     (and (haskell-session-maybe)
        (looking-at-p "\\_>")
        (save-match-data
          (cl-destructuring-bind (ident-start . ident-end) (haskell-ident-pos-at-point)
            (or (-when-let* ((is-import (save-excursion (goto-char ident-start)
                                                       (looking-back "import +\\(?:qualified +\\)?" (line-beginning-position))))
                            (import-start (match-beginning 0)))
                 (buffer-substring-no-properties import-start ident-end))
               (buffer-substring-no-properties ident-start ident-end))))))
    ('candidates
     (cl-assert arg)
     (cons :async
           (apply-partially #'haskell-company-async-fetcher arg)))
    ('duplicates nil)
    ('sorted t)
    ('meta (haskell-process-get-type arg #'identity t))
    ('doc-buffer
     (let* ((process (haskell-session-process (haskell-session-maybe)))
            (request (concat ":info " arg))
            (response (haskell-process-queue-sync-request process request)))
       (unless (string-match-p "\\`$" response)
         (company-doc-buffer response))))
    ;; ;; annotation is very slow here as it tries to get types one by one.
    ;; ('annotation
    ;;  (save-match-data
    ;;    (let ((arg-type (haskell-process-get-type arg #'identity t)))
    ;;      (when (string-match " \\(?:::\\|∷\\)" arg-type)
    ;;        (substring arg-type (match-beginning 0))))))
    ))

(defcustom haskell-company-async-max-completions
  1024
  "Limit the number of requested completions to this many.

This is useful for those modules that have a very large number of
available completions, which can slow things down."
  :group 'haskell
  :type '(choice (const :tag "All completions" nil)
                 (integer :tag "Upper bound")))

(defun haskell-company-async-fetcher (prefix callback)
  (cl-assert (haskell-session-maybe))
  (lexical-let
      ((process (haskell-session-process (haskell-session-maybe)))
       (callback callback)
       (ghci-command
        (concat ":complete repl "
                (if haskell-company-async-max-completions
                    (format "%d " haskell-company-async-max-completions)
                  "")
                (haskell-string-literal-encode prefix)))
       (process-response
        (lambda (response)
          (save-match-data
            ;; First line contains a common prefix (e.g., "import "),
            ;; which we prepend to every candidate.
            (let* ((valid-header (string-match "\\`\\([0-9]+\\) \\([0-9]+\\) \\(.*\\)" response))
                   (candidate-prefix (haskell-string-literal-decode (match-string 3 response)))
                   (all-completions (substring response (match-end 0)))
                   (candidates (mapcar #'haskell-string-literal-decode
                                       (split-string all-completions "\r?\n" t))))
              (if (string= candidate-prefix "")
                  candidates
                (mapcar (apply-partially #'concat candidate-prefix) candidates)))))))
    (haskell-process-queue-command
     process
     (make-haskell-command
      :go (lambda (_) (haskell-process-send-string process ghci-command))
      :complete
      (lambda (_ response)
        (funcall callback (funcall process-response response)))))))

(provide 'haskell-company-async)
;;; haskell-company-async.el ends here
