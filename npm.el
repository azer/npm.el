;;; npm.el --- Create and manage NPM packages from Emacs

;; Copyright (C) 2013  Azer Koçulu

;; Author: Azer Koçulu <azer@kodfabrik.com>
;; Keywords: tools, languages, processes
;; Version: 0.1

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

;; All functionality is available through interactive functions.
;;
;; Bindings are up to you. I use:
;;
;; (global-set-key (kbd "M-n i") 'npm-install)
;; (global-set-key (kbd "M-n n") 'npm-new)
;; (global-set-key (kbd "M-n d") 'npm-new-dependency)
;; (global-set-key (kbd "M-n e") 'npm-nodemon-exec)
;; (global-set-key (kbd "M-n p") 'npm-publish)
;; (global-set-key (kbd "M-n t") 'npm-test)
;; (global-set-key (kbd "M-n v") 'npm-version)

;;; Code:

(require 'cl)
(require 'json)

(setq +npm-dev-dir+ "~/dev")

(setq npm-vars-name "hello-world")
(setq npm-vars-desc "")
(setq npm-vars-author (user-login-name))
(setq npm-vars-git-user (user-login-name))
(setq npm-vars-test-cmd "node test")
(setq npm-vars-license "BSD")
(setq npm-vars-main "index.js")
(setq npm-vars-new-dependency "")
(setq npm-vars-deps "")
(setq npm-vars-keywords "")
(setq npm-vars-last-search-keyword "")
(setq npm-vars-version "0.0.0")

(defun npm-git ()
  (concat "git@github.com:" npm-vars-git-user "/" npm-vars-name ".git"))

(setq json-encoding-pretty-print t)

(defun flatten-list (list) (apply #'nconc list))

(defun is-dev-dependency (dp) (plist-get dp :dev))
(defun is-empty (str) (string= "" str))

(defun npm-package-json (name desc version main test-cmd keywords deps git author license)
  (let (dev-deps (content '()))
    (setq dev-deps (plist-get deps :dev))
    (setq deps (plist-get deps :deps))

    (setq content `(:license ,license))
    (plist-put content :author author)
    (plist-put content :repository `(:type "git" :url ,git))

    (if (> (length keywords) 0) (plist-put content :keywords keywords))
    (if (> (length dev-deps) 0) (plist-put content :devDependencies dev-deps))
    (if (> (length deps) 0) (plist-put content :dependencies deps))

    (plist-put content :scripts `(:test ,test-cmd))
    (plist-put content :main main)
    (plist-put content :description desc)
    (plist-put content :version version)
    (plist-put content :name name)

    (json-encode content)))

(defun npm-format-dependency (dp)
  (let (name ver)
    (setq name (make-keyword (plist-get dp :name)))
    (setq ver (plist-get dp :ver))
    (message "ver: %S" ver)
    `(,name ,ver)))


(defun npm-install ()
  "Install all dependencies"
  (interactive)
  (message "Installing dependencies...  (Check *npm* for the output)")
  (start-process "npm-install" "*npm*" "npm" "install")
  )

(defun npm-new ()
  "Create a new NPM project"

  (interactive)
  (setq npm-vars-name (read-from-minibuffer "Project Name: " npm-vars-name))
  (setq npm-vars-desc  (read-from-minibuffer "Description: " npm-vars-desc))
  (setq npm-vars-keywords (read-from-minibuffer "Keywords (http, parsing, etc): " npm-vars-keywords))
  (setq npm-vars-git (read-from-minibuffer "Git: " (npm-git)))

  (let (packagejson bf project-path manifest-filename readme)
    (setq packagejson (npm-package-json
                       npm-vars-name
                       npm-vars-desc
                       npm-vars-version
                       npm-vars-main
                       npm-vars-test-cmd
                       (npm-parse-keywords npm-vars-keywords)
                       (npm-parse-deps npm-vars-deps)
                       npm-vars-git
                       npm-vars-author
                       npm-vars-license))

    (setq readme (concat "## " npm-vars-name "\n\n"
                         npm-vars-desc "\n\n"
                         "## Install\n\n```bash\n$ npm install " npm-vars-name "\n```\n\n"
                         "## Usage\n\n ```js\n```\n\n"))

    (setq project-path (concat +npm-dev-dir+ "/" npm-vars-name))
    (setq manifest-filename (concat +npm-dev-dir+ "/" npm-vars-name "/package.json"))

    (message "Creating the new directory and files...")

    (make-directory project-path)
    (setq bf (get-buffer-create manifest-filename))
    (switch-to-buffer bf)
    (js2-mode)
    (insert packagejson)
    (json-pretty-print-buffer)
    (write-file manifest-filename)
    (shell-command-to-string (concat "git init && git remote add origin " npm-vars-git))
    (shell-command-to-string "echo 'node_modules\nnpm-debug.log\n.DS_Store' > .gitignore")
    (shell-command-to-string "echo 'test\ntest.js\nexample\nexamples' > .npmignore")
    (shell-command-to-string (concat "echo '" readme "' > README.md"))
    (setq bf (get-buffer-create manifest-filename))
    (npm-install)
    ))

(defun npm-new-dependency ()
  "Install and save new dependency"
  (interactive)
  (setq npm-vars-new-dependency (read-from-minibuffer "New dependency (e.g: minimist): " npm-vars-new-dependency))
  (message (concat "Installing " npm-vars-new-dependency))
  (start-process "npm-install" "*npm*" "npm" "install" "--save" npm-vars-new-dependency)
  )

(defun npm-new-dev-dependency ()
  "Install and save new development dependency"
  (interactive)
  (setq npm-vars-new-dependency (read-from-minibuffer "New dev dependency (e.g: tape): " npm-vars-new-dependency))
  (message (concat "Installing " npm-vars-new-dependency))
  (start-process "npm-install" "*npm*" "npm" "install" "--save-dev" npm-vars-new-dependency)
  )

(defun npm-parse-dependency (input)
  (let (name ver dev)
    (setq input (split-string input " "))
    (setq name (nth 0 input))
    (setq ver (nth 1 input))
    (setq dev (nth 2 input))
    `(:name ,name :ver ,ver :dev ,(not (not dev)))))

(defun npm-parse-deps (input)
  (let (deps dev-deps)
    (setq deps (remove-if 'is-empty (split-string input ", ")))
    (setq deps (mapcar 'npm-parse-dependency deps))

    (setq dev-deps (remove-if-not 'is-dev-dependency deps))
    (setq deps (remove-if 'is-dev-dependency deps))

    (setq deps (flatten-list (mapcar 'npm-format-dependency deps)))
    (setq dev-deps (flatten-list (mapcar 'npm-format-dependency dev-deps)))
    `(:dev ,dev-deps :deps ,deps)))

(defun npm-parse-keywords (input)
  (remove-if 'is-empty (split-string input ", ")))

(defun npm-patch ()
  "Npm version patch"
  (interactive)
  (message "Releasing a patch version... (Check *npm* for the output)")
  (start-process "npm-patch" "*npm*" "npm" "version" "patch")
  )

(defun npm-minor ()
  "Npm version minor"
  (interactive)
  (message "Releasing a minor version... (Check *npm* for the output)")
  (start-process "npm-minor" "*Messages*" "npm" "version" "minor")
  )

(defun npm-major ()
  "Npm version major"
  (interactive)
  (message "Releasing a major version... (Check *npm* for the output)")
  (start-process "npm-major" "*npm*" "npm" "version" "major")
  )

(defun npm-publish ()
  "Publish working package on NPM"
  (interactive)
  (message "Publishing on NPM... (Check *npm* for the output)")
  (start-process "npm-publish" "*npm*" "npm" "publish")
  )

(defun npm-search ()
  "npm search"
  (interactive)
  (setq npm-vars-last-search-keyword (read-from-minibuffer "Search NodeJS Modules: " npm-vars-last-search-keyword))
  (message (concat "Searching for " npm-vars-last-search-keyword))
  (start-process "npm-search" "*npm*" "npm" "search" npm-vars-last-search-keyword)
  )

(defun npm-test ()
  "Run test script"
  (interactive)
  ;;(call-process "/bin/bash" nil nil nil "-c" "npm test")
  ;;(shell-command "npm test")
  ;;(ansi-color-apply-on-region (point-min) (point-max))
  (let ((compilation-save-buffers-predicate 'ignore) (compilation-ask-about-save)) (compile "npm test"))
  )

(defun npm-version ()
  "Bump NPM version"
  (interactive)
  (let (version)
    (setq version (read-from-minibuffer "Bump version: "))
    (message (concat "Bumping version to" version " (Check *npm* for the output)"))
    (start-process "npm-version" "*npm*" "npm" "version" version))
  )

(defun make-keyword (symbol) (intern (format ":%s" symbol)))

(provide 'npm)

;;; npm.el ends here
