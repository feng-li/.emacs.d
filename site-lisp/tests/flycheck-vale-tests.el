;;; flycheck-vale-tests.el --- Vale sync regression checks -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'flycheck-vale)

(defconst flycheck-vale-test--missing
  '((Code . "E100")
    (Text . "E100 [loadStyles] Runtime error\nstyle 'proselint' does not exist on StylesPath")))

(defmacro flycheck-vale-test--fixture (&rest body)
  "Run BODY with a fake, local-only Vale and isolated sync state."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "vale-sync-test-" t))
          (default-directory (file-name-as-directory directory))
          (flycheck-vale-executable (expand-file-name "vale" directory))
          (flycheck-vale-args nil)
          (flycheck-vale-auto-sync-missing-styles t)
          (flycheck-vale--sync-attempts (make-hash-table :test #'equal))
          (flycheck-vale--sync-contexts (make-hash-table :test #'equal))
          (flycheck-vale--sync-process nil)
          (flycheck-vale--sync-queue nil))
     (unwind-protect
         (progn
           (with-temp-file flycheck-vale-executable
             (insert "#!/usr/bin/env python3\n"
                     "import json, os, pathlib, sys, time\n"
                     "base = pathlib.Path(__file__).resolve().parent\n"
                     "args = sys.argv[1:]\n"
                     "config = str(base / 'global.ini')\n"
                     "for i, arg in enumerate(args):\n"
                     "    if arg == '--config': config = args[i + 1]\n"
                     "    elif arg.startswith('--config='): config = arg.split('=', 1)[1]\n"
                     "if args[-1] == 'ls-config':\n"
                     "    print(json.dumps({'ConfigFiles': [config], 'Paths': [str(base / 'styles')]}))\n"
                     "elif args[-1] == 'sync':\n"
                     "    def log(phase):\n"
                     "        with (base / 'calls.jsonl').open('a') as f:\n"
                     "            f.write(json.dumps({'phase': phase, 'args': args, 'cwd': os.getcwd()}) + '\\n')\n"
                     "    log('start')\n"
                     "    time.sleep(0.08)\n"
                     "    failed = (base / 'fail').exists()\n"
                     "    print('fake download failed' if failed else 'fake styles installed')\n"
                     "    log('finish')\n"
                     "    sys.exit(1 if failed else 0)\n"
                     "else: sys.exit(2)\n"))
           (set-file-modes flycheck-vale-executable #o700)
           ,@body)
       (when (process-live-p flycheck-vale--sync-process)
         (set-process-sentinel flycheck-vale--sync-process #'ignore)
         (delete-process flycheck-vale--sync-process))
       (delete-directory directory t))))

(defun flycheck-vale-test--wait ()
  "Wait for the fake jobs and their delayed rechecks, not network operations."
  (let ((deadline (+ (float-time) 5)))
    (while (and (or flycheck-vale--sync-process flycheck-vale--sync-queue)
                (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (should-not flycheck-vale--sync-process)
    (should-not flycheck-vale--sync-queue)
    (accept-process-output nil 0.15)))

(defun flycheck-vale-test--calls (directory)
  "Return the fake Vale's recorded calls from DIRECTORY."
  (let ((file (expand-file-name "calls.jsonl" directory)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (mapcar (lambda (line)
                  (json-parse-string line :object-type 'alist :array-type 'list))
                (split-string (buffer-string) "\n" t))))))

(ert-deftest flycheck-vale-sync-recognizes-only-missing-style-errors ()
  (should (flycheck-vale--missing-style-error-p flycheck-vale-test--missing))
  (dolist (data '(((Code . "E100") (Text . "[loadStyles] Invalid rule syntax"))
                  ((Code . "E100") (Text . "[config] missing configuration"))
                  ((Code . "E200") (Text . "[loadStyles] style 'x' does not exist on StylesPath"))
                  ((Check . "proselint.But") (Message . "Avoid this phrase"))))
    (should-not (flycheck-vale--missing-style-error-p data))))

(ert-deftest flycheck-vale-sync-option-disabled-keeps-runtime-error ()
  (let ((flycheck-vale-auto-sync-missing-styles nil))
    (with-temp-buffer
      (cl-letf (((symbol-function 'flycheck-vale--request-sync)
                 (lambda (&rest _) (ert-fail "Disabled option must not sync"))))
        (let ((errors (flycheck-vale--parse (json-encode flycheck-vale-test--missing)
                                            'vale (current-buffer))))
          (should (= (length errors) 1))
          (should (equal (flycheck-error-id (car errors)) "E100")))))))

(ert-deftest flycheck-vale-sync-other-runtime-errors-do-not-sync ()
  (let ((flycheck-vale-auto-sync-missing-styles t))
    (with-temp-buffer
      (cl-letf (((symbol-function 'flycheck-vale--request-sync)
                 (lambda (&rest _) (ert-fail "Other failures must not sync"))))
        (should (flycheck-vale--parse
                 "{\"Code\":\"E100\",\"Text\":\"Invalid YAML rule\"}" 'vale (current-buffer)))))))

(ert-deftest flycheck-vale-sync-normal-alerts-do-not-sync ()
  (let ((flycheck-vale-auto-sync-missing-styles t))
    (with-temp-buffer
      (insert "A sentence.")
      (cl-letf (((symbol-function 'flycheck-vale--request-sync)
                 (lambda (&rest _) (ert-fail "Normal prose alerts must not sync"))))
        (let ((errors (flycheck-vale--parse
                       "{\"stdin.txt\":[{\"Line\":1,\"Span\":[1,1],\"Severity\":\"warning\",\"Message\":\"A prose warning\",\"Check\":\"Test.Prose\"}]}"
                       'vale (current-buffer))))
          (should (= (length errors) 1))
          (should (eq (flycheck-error-level (car errors)) 'warning)))))))

(ert-deftest flycheck-vale-sync-arguments-preserve-config-not-lint-flags ()
  (let ((flycheck-vale-args '("--config" "/some config/local.ini" "--no-global"
                            "--filter=Severity>warning" "--ext=.tex")))
    (should (equal (flycheck-vale--sync-arguments)
                   '("--config" "/some config/local.ini" "--no-global"))))
  (let ((flycheck-vale-args '("--config=/project/local.ini" "--ignore-syntax")))
    (should (equal (flycheck-vale--sync-arguments) '("--config=/project/local.ini")))))

(ert-deftest flycheck-vale-sync-parser-starts-once-and-rechecks-after-success ()
  (flycheck-vale-test--fixture
    (with-temp-buffer
      (setq-local flycheck-mode t)
      (let ((buffer (current-buffer)) rechecked)
        (cl-letf (((symbol-function 'flycheck-buffer)
                   (lambda () (push (current-buffer) rechecked))))
          (dotimes (_ 3)
            (should (flycheck-vale--parse (json-encode flycheck-vale-test--missing)
                                         'vale buffer)))
          (should (processp flycheck-vale--sync-process))
          (flycheck-vale-test--wait)
          (should (equal rechecked (list buffer)))
          (flycheck-vale--parse (json-encode flycheck-vale-test--missing) 'vale buffer)
          (flycheck-vale-test--wait)
          (should (= (length (flycheck-vale-test--calls directory)) 2))
          (should (= (hash-table-count flycheck-vale--sync-attempts) 1)))))))

(ert-deftest flycheck-vale-sync-same-global-config-shares-job-across-buffers ()
  (flycheck-vale-test--fixture
    (let ((one (generate-new-buffer " *vale-sync-one*"))
          (two (generate-new-buffer " *vale-sync-two*"))
          rechecked)
      (unwind-protect
          (cl-letf (((symbol-function 'flycheck-buffer)
                     (lambda () (push (current-buffer) rechecked))))
            (dolist (buffer (list one two))
              (with-current-buffer buffer
                (setq-local default-directory (file-name-as-directory directory))
                (setq-local flycheck-mode t)))
            (make-directory (expand-file-name "another-project" directory))
            (with-current-buffer two
              (setq-local default-directory
                          (file-name-as-directory (expand-file-name "another-project" directory))))
            (flycheck-vale--request-sync one)
            (flycheck-vale--request-sync two)
            (flycheck-vale-test--wait)
            (should (= (hash-table-count flycheck-vale--sync-attempts) 1))
            (should (= (length (flycheck-vale-test--calls directory)) 2))
            (should (memq one rechecked))
            (should (memq two rechecked)))
        (kill-buffer one)
        (kill-buffer two)))))

(ert-deftest flycheck-vale-sync-failure-is-not-retried-until-manual-request ()
  (flycheck-vale-test--fixture
    (with-temp-file (expand-file-name "fail" directory) (insert "fail"))
    (with-temp-buffer
      (setq-local flycheck-mode t)
      (let (rechecked)
        (cl-letf (((symbol-function 'flycheck-buffer) (lambda () (setq rechecked t))))
          (flycheck-vale--request-sync (current-buffer))
          (flycheck-vale-test--wait)
          (should-not rechecked)
          (flycheck-vale--request-sync (current-buffer))
          (flycheck-vale-test--wait)
          (should (= (length (flycheck-vale-test--calls directory)) 2))
          (delete-file (expand-file-name "fail" directory))
          (flycheck-vale-sync-styles)
          (flycheck-vale-test--wait)
          (should rechecked)
          (should (= (length (flycheck-vale-test--calls directory)) 4)))))))

(ert-deftest flycheck-vale-sync-different-configurations-are-serialized ()
  (flycheck-vale-test--fixture
    (with-temp-buffer
      (setq-local flycheck-vale-args '("--config=first.ini"))
      (flycheck-vale--request-sync (current-buffer))
      (setq-local flycheck-vale-args '("--config=second.ini"))
      (flycheck-vale--request-sync (current-buffer))
      (should (= (length flycheck-vale--sync-queue) 1))
      (flycheck-vale-test--wait)
      (should (equal (mapcar (lambda (call) (alist-get 'phase call))
                             (flycheck-vale-test--calls directory))
                     '("start" "finish" "start" "finish"))))))

(ert-deftest flycheck-vale-sync-uses-checker-executable-and-config-flags ()
  (flycheck-vale-test--fixture
    (with-temp-buffer
      (setq-local flycheck-vale-args '("--config" "project.ini" "--no-global" "--filter=some-rule"))
      (flycheck-vale--request-sync (current-buffer))
      (should (equal (car (process-command flycheck-vale--sync-process)) flycheck-vale-executable))
      (flycheck-vale-test--wait)
      (let ((call (car (flycheck-vale-test--calls directory))))
        (should (equal (alist-get 'args call) '("--config" "project.ini" "--no-global" "sync")))
        (should (equal (file-name-as-directory (alist-get 'cwd call)) default-directory))))))

(ert-deftest flycheck-vale-sync-context-is-cached ()
  (flycheck-vale-test--fixture
    (let ((first (flycheck-vale--sync-context)))
      (cl-letf (((symbol-function 'call-process)
                 (lambda (&rest _) (ert-fail "Cached configuration must not invoke Vale again"))))
        (should (eq first (flycheck-vale--sync-context)))))))

(ert-deftest flycheck-vale-sync-manual-command-works-with-auto-option-disabled ()
  (flycheck-vale-test--fixture
    (let ((flycheck-vale-auto-sync-missing-styles nil))
      (with-temp-buffer
        (flycheck-vale-sync-styles)
        (flycheck-vale-test--wait)
        (should (= (length (flycheck-vale-test--calls directory)) 2))))))

(ert-deftest flycheck-vale-sync-startup-failure-is-not-repeated ()
  (flycheck-vale-test--fixture
    (with-temp-buffer
      (let ((calls 0))
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest _)
                     (setq calls (1+ calls))
                     (signal 'file-error '("Cannot start fake Vale")))))
          (flycheck-vale--request-sync (current-buffer))
          (flycheck-vale--request-sync (current-buffer))
          (should (= calls 1))
          (should-not flycheck-vale--sync-process)
          (should-not flycheck-vale--sync-queue))))))

(ert-deftest flycheck-vale-sync-killed-or-reconfigured-buffer-is-not-rechecked ()
  (flycheck-vale-test--fixture
    (with-temp-buffer
      (setq-local flycheck-mode t)
      (let ((fingerprint (flycheck-vale--sync-fingerprint)))
        (cl-letf (((symbol-function 'flycheck-buffer)
                   (lambda () (ert-fail "Reconfigured buffer must not be rechecked"))))
          (setq-local flycheck-vale-args '("--config=changed.ini"))
          (flycheck-vale--sync-recheck (current-buffer) fingerprint)
          (let ((dead (generate-new-buffer " *vale-sync-dead*")))
            (kill-buffer dead)
            (flycheck-vale--sync-recheck dead fingerprint)))))))

(ert-deftest flycheck-vale-sync-remote-buffers-are-excluded ()
  (with-temp-buffer
    (setq default-directory "/ssh:example:/project/")
    (cl-letf (((symbol-function 'flycheck-vale--sync-context)
               (lambda (&rest _) (ert-fail "Remote configuration must not be probed"))))
      (flycheck-vale--request-sync (current-buffer))
      (should-error (flycheck-vale-sync-styles) :type 'user-error))))

(provide 'flycheck-vale-tests)
;;; flycheck-vale-tests.el ends here
