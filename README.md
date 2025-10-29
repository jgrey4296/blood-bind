# Blood Bind 
Key Binding Overlord for emacs

## Why? 
General is... big.

## Features
- conflict detection
- re-triggerable for when modes interfere with my bindings
- disables standard keybinding mechanisms
- summarisable
- leader-keys and evil-states
- multiple binding profiles
- keymap naming for debugging
- no need for `eval-after-load` deferral
- no C-x, M-x nonsense, just leader keys, states, and domain specific bindings
- patterns come first, so you can sort them
- patterns carry the metadata of where they were declared
- extendable dsl tokens, compilation, and keymap transforms
- does *not* handle use or activation of keymaps, so does not interfere with evil or states machinery
- does *not* handle key lookup, so does not interfere with emacs machinery
- reserved spaces for bindings providing a certain symbol


## The DSL

``` emacs-lisp
(bloodbind! 


)
```


## Usage 
### Setup
j) declare bindings/profiles into `bloodbind--registry` using `bloodbind!`
2) declare any pre-compilation token transforms with `bloodbind-token-transform!`
3) declare compilation transforms with `bloodbind-entries-transform!`
4) declare post-compilation keymap transforms `bloodbind-map-transform!`
5) user registers interferences with `bloodbind-register-interference`

### Application
1) (optional) wipe all existing maps `bloodbind-wipe`
2) (optional) user disables interferences `bloodbind-disable-interference`
3) trigger `bloodbind-compile` to build a profile of keymaps
4) normal emacs use

### Later
1) inspect registry with `bloodbind-summary`
2) swap to a different profile with `bloodbind-apply`

## Emacs keybinding notes

### Current Builtin
- key-valid-p
- make-keymap
- keymap-set
- keymap-global-set
- keymap-local-set
- set-keymap-parent
- keymap-canonicalize
- keymap-parent
- set-keymap-parent
- make-composed-keymap
- keymap-lookup
- keymap--check
- key-parse

### Current External
- bind-keys*
- evil-define-key*
- 

### Legacy
- define-key 
- local-set-key
- global-set-key
- local-unset-key
- global-unset-key

### Key Representations
- string
- vector


###  Keymap Hierarchy:
https://www.gnu.org/software/emacs/manual/html_node/elisp/Controlling-Active-Maps.html
https://www.gnu.org/software/emacs/manual/html_node/elisp/Searching-Keymaps.html
https://github.com/noctuid/evil-guide
https://www.masteringemacs.org/article/mastering-key-bindings-emacs
NOTE: See evil-core.el for full design of evil keybindings
```
-> Overriding keymaps/overlay keymaps
--> `overriding-terminal-local-map`
--> `overriding-local-map`
-> Emulation mode keymaps : `emulation-mode-map-alists`
--> Evil keymaps
---> Intercept keymaps              : `evil-intercept-maps`
---> Local state keymap             : `evil-local-keymaps-alist`
---> Minor-mode keymaps             : `evil-minor-mode-keymaps-alist`
---> Auxiliary keymaps              : 
---> Overriding keymaps             : `evil-overriding-maps`
---> Global state keymap            : `evil-global-keymaps-alist`
---> Keymaps for other states       : 
-> Minor mode keymaps               : `minor-mode-map-alist`
-> Local keymap (`local-set-key')   : `current-local-map`
-> Global keymap (`global-set-key') : `current-global-map`
``` 
