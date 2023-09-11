;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((haskell-mode
   (haskell-process-type . ghci)
   (haskell-process-args-ghci . ("-ferror-spans" "-fdiagnostics-color=never"))
   (eval .
       (setq
         nix-bins/proj-dir
           (file-name-directory
             (let ((d (dir-locals-find-file ".")))
               (if (stringp d) d (car d))))
         haskell-process-path-ghci (concat nix-bins/proj-dir "bin/ghci")
     ))
   ))
