;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

(add-to-list 'exec-path "/opt/homebrew/bin")

(setenv "LIBRARY_PATH"
        "/opt/homebrew/opt/gcc/lib/gcc/current:/opt/homebrew/opt/libgccjit/lib/gcc/current:/opt/homebrew/opt/gcc/lib/gcc/current/gcc/aarch64-apple-darwin21/12")
