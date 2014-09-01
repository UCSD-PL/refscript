;;; flycheck-rsc.el --- A flycheck checker for RefScript

;; Modified from flycheck-hdevtools.el by Steve Purcell

;; Author: Ranjit Jhala <jhala@cs.ucsd.edu>
;; URL: https://github.com/ucsd-pl/RefScript/syntax/flycheck-rsc.el
;; Keywords: convenience languages tools
;; Package-Requires: ((flycheck "0.15"))
;; Version: 20140801.00
;; X-Original-Version: DEV

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Adds a flycheck syntax checker for TypeScript based on RefScript.

;;;; Setup

;; (eval-after-load 'flycheck '(require 'flycheck-rsc))

;;; Code:

(require 'flycheck)

(flycheck-define-checker typescript-rsc
  "A Refinement Type Checker for TypeScript.

See URL `https://github.com/ucsd-pl/RefScript'."
  :command
  ("rsc" source-inplace)
  :error-patterns
  (
   (error line-start (file-name) ":" line ":" column ":"
          (message
	   (one-or-more " ") (one-or-more not-newline)
	   (zero-or-more "\n"
			 (one-or-more " ")
			 (zero-or-more not-newline)))
          line-end)

   (error line-start (file-name) ":" line ":" column "-" (one-or-more digit) ":"
	  (message
	   (one-or-more " ") (one-or-more not-newline)
	   (zero-or-more "\n"
			 (one-or-more " ")
			 (zero-or-more not-newline)))
          line-end)

   (error line-start (file-name) ":" line ":" column "-" (one-or-more digit) ":" (one-or-more digit) ":"
	  (message
	   (one-or-more " ") (one-or-more not-newline)
	   (zero-or-more "\n"
			 (one-or-more " ")
			 (zero-or-more not-newline)))
          line-end)
   )
  :error-filter
  (lambda (errors)
    (-> errors
      flycheck-dedent-error-messages
      flycheck-sanitize-errors))
  :modes (typescript-mode)
  ;; :next-checkers ((warnings-only . haskell-hlint))
)

(add-to-list 'flycheck-checkers 'typescript-rsc)

(provide 'flycheck-rsc)

;;; flycheck-rsc.el ends here
