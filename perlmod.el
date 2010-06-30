;;; perlmod.el --- Open the source code of an installed perl module

;;; Commentary:
;;

(require 'perldoc)

(defvar perlmod-perl-buffer "")

(defun perlmod-open (module)
  (let* ((default-directory "/")
         (proc (start-process
                "perlmod" nil "perl" (concat "-m" module) "-e"
                (concat "my $mod = $ARGV[0]; $mod =~ s{::}{/}g;"
                        "$mod .= q{.pm}; print $INC{$mod}")
                module)))
    (set-process-filter
     proc (lambda (proc str)
            (setq perlmod-perl-buffer (concat perlmod-perl-buffer str))))
    (set-process-sentinel
     proc (lambda (proc event)
            (if (and
                 (string-equal event "finished\n")
                 (string-bytes perlmod-perl-buffer))
                (find-file perlmod-perl-buffer)
              (message "Module not found."))
            (setq perlmod-perl-buffer "")))))

(defun perlmod (&optional module re-cache)
  (interactive (list nil current-prefix-arg))
  (when (or re-cache (not perldoc-all-completions-alist))
    (message "Building completion list of all perl modules...")
    (perldoc-modules-alist t))
  (unless (stringp module)
    (setq module
          (completing-read "Perl module: " (perldoc-modules-alist) nil nil)))
  (cond
   ((stringp module)
    (perlmod-open module))
   (t
    (message "Nothing to find."))))

(provide 'perlmod)

;;; perlmod.el ends here
