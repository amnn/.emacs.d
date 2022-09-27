;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

(setenv "SHELL" "/opt/homebrew/bin/fish")
(setq shell-file-name "/opt/homebrew/bin/fish")

(setenv
 "LIBRARY_PATH"
 (concat
  "/opt/homebrew/opt/gcc/lib/gcc/current"
  ":/opt/homebrew/opt/libgccjit/lib/gcc/current"
  ":/opt/homebrew/opt/gcc/lib/gcc/current/gcc/aarch64-apple-darwin21/12"))
