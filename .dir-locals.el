;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((emacs-lisp-mode . ((eval .
                      (progn
                        (add-to-list 'imenu-generic-expression
                                     '("mb-exact macros" "^\\s-*(mb-search-define-exact\\s-+\\(\\(?:\\w\\|\\s_\\|\\\\.\\)+\\)" 1))
                        (add-to-list 'imenu-generic-expression
                                     '("mb-basic macros" "^\\s-*(mb-search-define-basic\\s-+\\(\\(?:\\w\\|\\s_\\|\\\\.\\)+\\)" 1)))))))
