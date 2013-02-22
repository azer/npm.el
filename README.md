npm.el makes it convenient to create new NodeJS projects within Emacs.

* It asks you project name, dependencies and Git repo. Then creates;
  * A new folder and initializes Git with remotes
  * package.json, .gitignore and .npmignore files
  * A README.md file with project title, a short installation instruction and a nice looking cat photo!
* And opens the new package.json file.

**Screencast:** [GIF](https://dl.dropbox.com/s/jnuo3m5w5x0q8vw/npmel.gif?token_hash=AAGVHEepAk106ilHMtw_Oh6S_t3GISDDnJM9Yof6eEh1LQ)

### Installation

Require it. If you're a newbie, check out [emacs.js](http://github.com/azer/emacs.js).

### Usage

Do `meta-x npm-new` function. Or set a key-binding:

```elisp
(global-set-key (kbd "M-n") 'npm-new)
```

![](https://dl.dropbox.com/s/9q2p5mrqnajys22/npmel.jpg?token_hash=AAHqttN9DiGl63ma8KRw-G0cdalaiMzrvrOPGnOfDslDjw)
