(require 'cl)

(setq +npm-dev-dir+ "~/dev")

(setq npm-vars-name "hello-world")
(setq npm-vars-desc "")
(setq npm-vars-author (user-login-name))
(setq npm-vars-git-user (user-login-name))
(setq npm-vars-test-cmd "mocha")
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
  (setq npm-vars-deps (read-from-minibuffer "Dependencies (e.g: optimist 0.x, request 2.x, mocha * dev): " npm-vars-deps))
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
                         "## Usage\n\n ```js\n```\n\n"
                         "![](https://dl.dropbox.com/s/9q2p5mrqnajys22/npmel.jpg)"))

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
    (shell-command-to-string "echo 'node_modules\nnpm-debug.log' > .gitignore")
    (shell-command-to-string "echo 'test\ntest.js\nexample\nexamples' > .npmignore")
    (shell-command-to-string (concat "echo '" readme "' > README.md"))
    (setq bf (get-buffer-create manifest-filename))
    (npm-install)
    ))

(defun npm-new-dependency ()
  "Install and save new dependency"
  (interactive)
  (setq npm-vars-new-dependency (read-from-minibuffer "New dependency (e.g: express): " npm-vars-new-dependency))
  (message (concat "Installing " npm-vars-new-dependency))
  (start-process "npm-install" "*npm*" "npm" "install" "--save" npm-vars-new-dependency)
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
  (message "Bumping NPM version... (Check *npm* for the output)")
  (start-process "npm-publish" "*npm*" "npm" "version" "patch")
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
  (start-process "npm-search" "*npm search*" "npm" "search" npm-vars-last-search-keyword)
  )

(defun npm-test ()
  "Run test script"
  (interactive)
  (compile "npm test")
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
