;;; .loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (ack) "ack" "ack/ack.el" (20191 64784))
;;; Generated autoloads from ack/ack.el

(autoload 'ack "ack" "\
Not documented

\(fn COMMAND-ARGS)" t nil)

;;;***

;;;### (autoloads (anything-other-buffer anything-at-point anything)
;;;;;;  "anything" "anything/anything.el" (20191 65233))
;;; Generated autoloads from anything/anything.el

(autoload 'anything "anything" "\
Main function to execute anything sources.

Keywords supported:
:sources :input :prompt :resume :preselect :buffer :keymap :default :history
Extra keywords are supported and can be added, see below.

When call interactively with no arguments deprecated `anything-sources'
will be used if non--nil.

PLIST is a list like (:key1 val1 :key2 val2 ...) or
\(&optional sources input prompt resume preselect buffer keymap default history).

Basic keywords are the following:

:sources

Temporary value of `anything-sources'.  It also accepts a
symbol, interpreted as a variable of an anything source.  It
also accepts an alist representing an anything source, which is
detected by (assq 'name ANY-SOURCES)

:input

Temporary value of `anything-pattern', ie. initial input of minibuffer.

:prompt

Prompt other than \"pattern: \".

:resume

If t, Resurrect previously instance of `anything'.  Skip the initialization.
If 'noresume, this instance of `anything' cannot be resumed.

:preselect

Initially selected candidate.  Specified by exact candidate or a regexp.
Note that it is not working with delayed sources.

:buffer

`anything-buffer' instead of *anything*.

:keymap

`anything-map' for current `anything' session.

:default

A default argument that will be inserted in minibuffer with \\<minibuffer-local-map>\\[next-history-element].
When nil of not present `thing-at-point' will be used instead.

:history

By default all minibuffer input is pushed to `minibuffer-history',
if an argument HISTORY is provided, input will be pushed to HISTORY.
History element should be a symbol.

Of course, conventional arguments are supported, the two are same.

\(anything :sources sources :input input :prompt prompt :resume resume
           :preselect preselect :buffer buffer :keymap keymap :default default
           :history history)
\(anything sources input prompt resume preselect buffer keymap default history)

Other keywords are interpreted as local variables of this anything session.
The `anything-' prefix can be omitted.  For example,

\(anything :sources 'anything-c-source-buffers
           :buffer \"*buffers*\" :candidate-number-limit 10)

means starting anything session with `anything-c-source-buffers'
source in *buffers* buffer and set variable `anything-candidate-number-limit'
to 10 as session local variable.

\(fn &rest PLIST)" t nil)

(autoload 'anything-at-point "anything" "\
Call anything with symbol at point as initial input.
ANY-SOURCES ANY-INPUT ANY-PROMPT ANY-RESUME ANY-PRESELECT and ANY-BUFFER
are same args as in `anything'.

\(fn &optional ANY-SOURCES ANY-INPUT ANY-PROMPT ANY-RESUME ANY-PRESELECT ANY-BUFFER)" t nil)

(autoload 'anything-other-buffer "anything" "\
Simplified interface of `anything' with other `anything-buffer'.
Call `anything' with only ANY-SOURCES and ANY-BUFFER as args.

\(fn ANY-SOURCES ANY-BUFFER)" nil nil)

;;;***

;;;### (autoloads (anything-c-apropos anything-ucs anything-ratpoison-commands
;;;;;;  anything-c-run-external-command anything-eshell-history anything-esh-pcomplete
;;;;;;  anything-apt anything-world-time anything-select-xfont anything-top
;;;;;;  anything-create anything-execute-anything-command anything-call-source
;;;;;;  anything-surfraw anything-calcul-expression anything-eval-expression-with-eldoc
;;;;;;  anything-eval-expression anything-yaoddmuse-emacswiki-post-library
;;;;;;  anything-yaoddmuse-emacswiki-edit-or-view anything-all-mark-rings
;;;;;;  anything-global-mark-ring anything-mark-ring anything-simple-call-tree
;;;;;;  anything-bookmark-ext anything-manage-advice anything-M-x
;;;;;;  anything-filelist+ anything-filelist anything-do-zgrep anything-do-grep
;;;;;;  anything-dired-hardlink-file anything-dired-symlink-file
;;;;;;  anything-dired-copy-file anything-dired-rename-file anything-insert-file
;;;;;;  anything-write-file anything-find-files anything-c-copy-files-async
;;;;;;  anything-regexp anything-info-gnus anything-org-headlines
;;;;;;  anything-browse-code anything-occur anything-list-emacs-process
;;;;;;  anything-timers anything-bm-list anything-eev-anchors anything-emms
;;;;;;  anything-org-keywords anything-man-woman anything-register
;;;;;;  anything-c-insert-latex-math anything-c-pp-bookmarks anything-bookmarks
;;;;;;  anything-colors anything-firefox-bookmarks anything-w3m-bookmarks
;;;;;;  anything-locate anything-bbdb anything-buffers-list anything-for-buffers
;;;;;;  anything-yahoo-suggest anything-google-suggest anything-imenu
;;;;;;  anything-gentoo anything-minibuffer-history anything-show-kill-ring
;;;;;;  anything-info-emacs anything-info-at-point anything-recentf
;;;;;;  anything-for-files anything-mini anything-c-toggle-match-plugin
;;;;;;  anything-c-reset-adaptative-history anything-c-set-variable
;;;;;;  anything-c-call-interactively anything-w32-shell-execute-open-file
;;;;;;  anything-lisp-completion-or-file-name-at-point anything-lisp-completion-at-point-or-indent
;;;;;;  anything-c-complete-file-name-at-point anything-lisp-completion-at-point
;;;;;;  anything-completion-mode anything-yaoddmuse-cache-pages anything-yank-text-at-point
;;;;;;  anything-c-grep-run-save-buffer anything-c-grep-run-other-window-action
;;;;;;  anything-c-grep-run-default-action anything-c-grep-run-persistent-action
;;;;;;  anything-c-goto-next-file anything-c-goto-precedent-file
;;;;;;  anything-dired-mode anything-ff-run-kill-buffer-persistent
;;;;;;  anything-ff-persistent-delete anything-ff-properties-persistent
;;;;;;  anything-ff-run-print-file anything-ff-run-etags anything-ff-run-gnus-attach-files
;;;;;;  anything-ff-run-locate anything-ff-run-open-file-externally
;;;;;;  anything-ff-run-switch-other-frame anything-ff-run-switch-other-window
;;;;;;  anything-ff-run-switch-to-eshell anything-ff-run-complete-fn-at-point
;;;;;;  anything-ff-run-delete-file anything-ff-run-symlink-file
;;;;;;  anything-ff-run-ediff-merge-file anything-ff-run-ediff-file
;;;;;;  anything-ff-run-eshell-command-on-file anything-ff-run-load-file
;;;;;;  anything-ff-run-byte-compile-file anything-ff-run-rename-file
;;;;;;  anything-ff-run-copy-file anything-ff-run-zgrep anything-ff-run-pdfgrep
;;;;;;  anything-ff-run-grep anything-ff-run-switch-to-history anything-ff-run-toggle-auto-update
;;;;;;  anything-buffer-run-ediff anything-buffer-switch-to-elscreen
;;;;;;  anything-buffer-switch-other-frame anything-buffer-switch-other-window
;;;;;;  anything-buffer-run-query-replace anything-buffer-run-query-replace-regexp
;;;;;;  anything-buffer-run-zgrep anything-buffer-run-grep anything-buffer-run-kill-buffers
;;;;;;  anything-buffer-save-persistent anything-buffer-revert-persistent
;;;;;;  anything-buffer-diff-persistent anything-toggle-all-marks
;;;;;;  anything-unmark-all anything-mark-all anything-insert-buffer-name
;;;;;;  anything-test-sources anything-etags-help anything-pdfgrep-help
;;;;;;  anything-grep-help anything-generic-file-help anything-read-file-name-help
;;;;;;  anything-ff-help anything-c-buffer-help anything-configuration)
;;;;;;  "anything-config" "anything/anything-config.el" (20191 65233))
;;; Generated autoloads from anything/anything-config.el

(autoload 'anything-configuration "anything-config" "\
Customize `anything'.

\(fn)" t nil)

(defvar anything-command-map)

(autoload 'anything-c-buffer-help "anything-config" "\
Help command for anything buffers.

\(fn)" t nil)

(autoload 'anything-ff-help "anything-config" "\
Help command for `anything-find-files'.

\(fn)" t nil)

(autoload 'anything-read-file-name-help "anything-config" "\
Not documented

\(fn)" t nil)

(autoload 'anything-generic-file-help "anything-config" "\
Not documented

\(fn)" t nil)

(autoload 'anything-grep-help "anything-config" "\
Not documented

\(fn)" t nil)

(autoload 'anything-pdfgrep-help "anything-config" "\
Not documented

\(fn)" t nil)

(autoload 'anything-etags-help "anything-config" "\
The help function for etags.

\(fn)" t nil)

(autoload 'anything-test-sources "anything-config" "\
List all anything sources for test.
The output is sexps which are evaluated by \\[eval-last-sexp].

\(fn)" t nil)

(autoload 'anything-insert-buffer-name "anything-config" "\
Insert buffer name.

\(fn)" t nil)

(autoload 'anything-mark-all "anything-config" "\
Mark all visible unmarked candidates in current source.

\(fn)" t nil)

(autoload 'anything-unmark-all "anything-config" "\
Unmark all candidates in all sources of current anything session.

\(fn)" t nil)

(autoload 'anything-toggle-all-marks "anything-config" "\
Toggle all marks.
Mark all visible candidates of current source or unmark all candidates
visible or invisible in all sources of current anything session

\(fn)" t nil)

(autoload 'anything-buffer-diff-persistent "anything-config" "\
Toggle diff buffer without quitting anything.

\(fn)" t nil)

(autoload 'anything-buffer-revert-persistent "anything-config" "\
Revert buffer without quitting anything.

\(fn)" t nil)

(autoload 'anything-buffer-save-persistent "anything-config" "\
Save buffer without quitting anything.

\(fn)" t nil)

(autoload 'anything-buffer-run-kill-buffers "anything-config" "\
Run kill buffer action from `anything-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'anything-buffer-run-grep "anything-config" "\
Run Grep action from `anything-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'anything-buffer-run-zgrep "anything-config" "\
Run Grep action from `anything-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'anything-buffer-run-query-replace-regexp "anything-config" "\
Run Query replace regexp action from `anything-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'anything-buffer-run-query-replace "anything-config" "\
Run Query replace action from `anything-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'anything-buffer-switch-other-window "anything-config" "\
Run switch to other window action from `anything-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'anything-buffer-switch-other-frame "anything-config" "\
Run switch to other frame action from `anything-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'anything-buffer-switch-to-elscreen "anything-config" "\
Run switch to elscreen  action from `anything-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'anything-buffer-run-ediff "anything-config" "\
Run ediff action from `anything-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'anything-ff-run-toggle-auto-update "anything-config" "\
Not documented

\(fn)" t nil)

(autoload 'anything-ff-run-switch-to-history "anything-config" "\
Run Switch to history action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-grep "anything-config" "\
Run Grep action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-pdfgrep "anything-config" "\
Run Pdfgrep action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-zgrep "anything-config" "\
Run Grep action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-copy-file "anything-config" "\
Run Copy file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-rename-file "anything-config" "\
Run Rename file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-byte-compile-file "anything-config" "\
Run Byte compile file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-load-file "anything-config" "\
Run Load file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-eshell-command-on-file "anything-config" "\
Run eshell command on file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-ediff-file "anything-config" "\
Run Ediff file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-ediff-merge-file "anything-config" "\
Run Ediff merge file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-symlink-file "anything-config" "\
Run Symlink file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-delete-file "anything-config" "\
Run Delete file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-complete-fn-at-point "anything-config" "\
Run complete file name action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-switch-to-eshell "anything-config" "\
Run switch to eshell action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-switch-other-window "anything-config" "\
Run switch to other window action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-switch-other-frame "anything-config" "\
Run switch to other frame action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-open-file-externally "anything-config" "\
Run open file externally command action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-locate "anything-config" "\
Run locate action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-gnus-attach-files "anything-config" "\
Run gnus attach files command action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-etags "anything-config" "\
Run Etags command action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-run-print-file "anything-config" "\
Run Print file action from `anything-c-source-find-files'.

\(fn)" t nil)

(autoload 'anything-ff-properties-persistent "anything-config" "\
Show properties without quitting anything.

\(fn)" t nil)

(autoload 'anything-ff-persistent-delete "anything-config" "\
Delete current candidate without quitting.

\(fn)" t nil)

(autoload 'anything-ff-run-kill-buffer-persistent "anything-config" "\
Execute `anything-ff-kill-buffer-fname' whitout quitting.

\(fn)" t nil)

(defvar anything-dired-mode "Enable anything completion in Dired functions.\nBindings affected are C, R, S, H.\nThis is deprecated for Emacs24+ users, use `ac-mode' instead." "\
Non-nil if Anything-Dired mode is enabled.
See the command `anything-dired-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `anything-dired-mode'.")

(custom-autoload 'anything-dired-mode "anything-config" nil)

(autoload 'anything-dired-mode "anything-config" "\
Toggle Anything-Dired mode on or off.
Interactively, with no prefix argument, toggle the mode.
With universal prefix ARG turn mode on.
With zero or negative ARG turn mode off.
\\{anything-dired-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'anything-c-goto-precedent-file "anything-config" "\
Go to precedent file in anything grep/etags buffers.

\(fn)" t nil)

(autoload 'anything-c-goto-next-file "anything-config" "\
Go to precedent file in anything grep/etags buffers.

\(fn)" t nil)

(autoload 'anything-c-grep-run-persistent-action "anything-config" "\
Run grep persistent action from `anything-do-grep-1'.

\(fn)" t nil)

(autoload 'anything-c-grep-run-default-action "anything-config" "\
Run grep default action from `anything-do-grep-1'.

\(fn)" t nil)

(autoload 'anything-c-grep-run-other-window-action "anything-config" "\
Run grep goto other window action from `anything-do-grep-1'.

\(fn)" t nil)

(autoload 'anything-c-grep-run-save-buffer "anything-config" "\
Run grep save results action from `anything-do-grep-1'.

\(fn)" t nil)

(autoload 'anything-yank-text-at-point "anything-config" "\
Yank text at point in minibuffer.

\(fn)" t nil)

(autoload 'anything-yaoddmuse-cache-pages "anything-config" "\
Fetch the list of files on emacswiki and create cache file.
If load is non--nil load the file and feed `yaoddmuse-pages-hash'.

\(fn &optional LOAD)" t nil)

(defvar anything-completion-mode nil "\
Non-nil if Anything-Completion mode is enabled.
See the command `anything-completion-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `anything-completion-mode'.")

(custom-autoload 'anything-completion-mode "anything-config" nil)

(autoload 'anything-completion-mode "anything-config" "\
Toggle generic anything completion.

All functions in Emacs that use `completing-read'
or `read-file-name' and friends will use anything interface
when this mode is turned on.
However you can modify this behavior for functions of your choice
with `anything-completing-read-handlers-alist'.

Called with a positive arg, turn on unconditionally, with a
negative arg turn off.
You can turn it on with `ac-mode'.

Some crap emacs functions may not be supported,
e.g `ffap-alternate-file' and maybe others
You can add such functions to `anything-completing-read-handlers-alist'
with a nil value.

Note: This mode will work only partially on Emacs23.

\(fn &optional ARG)" t nil)

(autoload 'anything-lisp-completion-at-point "anything-config" "\
Anything lisp symbol completion at point.

\(fn)" t nil)

(autoload 'anything-c-complete-file-name-at-point "anything-config" "\
Complete file name at point.

\(fn)" t nil)

(autoload 'anything-lisp-completion-at-point-or-indent "anything-config" "\
First call indent and second call complete lisp symbol.
The second call should happen before `anything-lisp-completion-or-indent-delay',
after this delay, next call will indent again.
After completion, next call is always indent.
See that like click and double mouse click.
One hit indent, two quick hits maybe indent and complete.

\(fn ARG)" t nil)

(autoload 'anything-lisp-completion-or-file-name-at-point "anything-config" "\
Complete lisp symbol or filename at point.
Filename completion happen if filename is started in
or between double quotes.

\(fn)" t nil)

(autoload 'anything-w32-shell-execute-open-file "anything-config" "\
Not documented

\(fn FILE)" t nil)

(autoload 'anything-c-call-interactively "anything-config" "\
Execute CMD-OR-NAME as Emacs command.
It is added to `extended-command-history'.
`anything-current-prefix-arg' is used as the command's prefix argument.

\(fn CMD-OR-NAME)" nil nil)

(autoload 'anything-c-set-variable "anything-config" "\
Set value to VAR interactively.

\(fn VAR)" t nil)

(autoload 'anything-c-reset-adaptative-history "anything-config" "\
Delete all `anything-c-adaptive-history' and his file.
Useful when you have a old or corrupted `anything-c-adaptive-history-file'.

\(fn)" t nil)

(autoload 'anything-c-toggle-match-plugin "anything-config" "\
Toggle anything-match-plugin.

\(fn)" t nil)

(autoload 'anything-mini "anything-config" "\
Preconfigured `anything' lightweight version (buffer -> recentf).

\(fn)" t nil)

(autoload 'anything-for-files "anything-config" "\
Preconfigured `anything' for opening files.
ffap -> recentf -> buffer -> bookmark -> file-cache -> files-in-current-dir -> locate.

\(fn)" t nil)

(autoload 'anything-recentf "anything-config" "\
Preconfigured `anything' for `recentf'.

\(fn)" t nil)

(autoload 'anything-info-at-point "anything-config" "\
Preconfigured `anything' for searching info at point.
With a prefix-arg insert symbol at point.

\(fn ARG)" t nil)

(autoload 'anything-info-emacs "anything-config" "\
Preconfigured anything for Emacs manual index.

\(fn)" t nil)

(autoload 'anything-show-kill-ring "anything-config" "\
Preconfigured `anything' for `kill-ring'.
It is drop-in replacement of `yank-pop'.
You may bind this command to M-y.
First call open the kill-ring browser, next calls move to next line.

\(fn)" t nil)

(autoload 'anything-minibuffer-history "anything-config" "\
Preconfigured `anything' for `minibuffer-history'.

\(fn)" t nil)

(autoload 'anything-gentoo "anything-config" "\
Preconfigured `anything' for gentoo linux.

\(fn)" t nil)

(autoload 'anything-imenu "anything-config" "\
Preconfigured `anything' for `imenu'.

\(fn)" t nil)

(autoload 'anything-google-suggest "anything-config" "\
Preconfigured `anything' for google search with google suggest.

\(fn)" t nil)

(autoload 'anything-yahoo-suggest "anything-config" "\
Preconfigured `anything' for Yahoo searching with Yahoo suggest.

\(fn)" t nil)

(autoload 'anything-for-buffers "anything-config" "\
Preconfigured `anything' for buffer.

\(fn)" t nil)

(autoload 'anything-buffers-list "anything-config" "\
Enhanced preconfigured `anything' for buffer.

\(fn)" t nil)

(autoload 'anything-bbdb "anything-config" "\
Preconfigured `anything' for BBDB.

Needs BBDB.

http://bbdb.sourceforge.net/

\(fn)" t nil)

(autoload 'anything-locate "anything-config" "\
Preconfigured `anything' for Locate.
Note: you can add locate options after entering pattern.
See 'man locate' for valid options.

You can specify a specific database with prefix argument ARG (C-u).
Many databases can be used: navigate and mark them.
See also `anything-locate-with-db'.

To create a user specific db, use
\"updatedb -l 0 -o db_path -U directory\".
Where db_path is a filename matched by
`anything-locate-db-file-regexp'.

\(fn ARG)" t nil)

(autoload 'anything-w3m-bookmarks "anything-config" "\
Preconfigured `anything' for w3m bookmark.

Needs w3m and emacs-w3m.

http://w3m.sourceforge.net/
http://emacs-w3m.namazu.org/

\(fn)" t nil)

(autoload 'anything-firefox-bookmarks "anything-config" "\
Preconfigured `anything' for firefox bookmark.
You will have to enable html bookmarks in firefox:
open about:config in firefox and double click on this line to enable value to true:

user_pref(\"browser.bookmarks.autoExportHTML\", false);

You should have now:

user_pref(\"browser.bookmarks.autoExportHTML\", true);

After closing firefox, you will be able to browse you bookmarks.

\(fn)" t nil)

(autoload 'anything-colors "anything-config" "\
Preconfigured `anything' for color.

\(fn)" t nil)

(autoload 'anything-bookmarks "anything-config" "\
Preconfigured `anything' for bookmarks.

\(fn)" t nil)

(autoload 'anything-c-pp-bookmarks "anything-config" "\
Preconfigured `anything' for bookmarks (pretty-printed).

\(fn)" t nil)

(autoload 'anything-c-insert-latex-math "anything-config" "\
Preconfigured anything for latex math symbols completion.

\(fn)" t nil)

(autoload 'anything-register "anything-config" "\
Preconfigured `anything' for Emacs registers.

\(fn)" t nil)

(autoload 'anything-man-woman "anything-config" "\
Preconfigured `anything' for Man and Woman pages.

\(fn)" t nil)

(autoload 'anything-org-keywords "anything-config" "\
Preconfigured `anything' for org keywords.

\(fn)" t nil)

(autoload 'anything-emms "anything-config" "\
Preconfigured `anything' for emms sources.

\(fn)" t nil)

(autoload 'anything-eev-anchors "anything-config" "\
Preconfigured `anything' for eev anchors.

\(fn)" t nil)

(autoload 'anything-bm-list "anything-config" "\
Preconfigured `anything' for visible bookmarks.

Needs bm.el

http://cvs.savannah.gnu.org/viewvc/*checkout*/bm/bm/bm.el

\(fn)" t nil)

(autoload 'anything-timers "anything-config" "\
Preconfigured `anything' for timers.

\(fn)" t nil)

(autoload 'anything-list-emacs-process "anything-config" "\
Preconfigured `anything' for emacs process.

\(fn)" t nil)

(autoload 'anything-occur "anything-config" "\
Preconfigured Anything for Occur source.
If region is active, search only in region,
otherwise search in whole buffer.

\(fn)" t nil)

(autoload 'anything-browse-code "anything-config" "\
Preconfigured anything to browse code.

\(fn)" t nil)

(autoload 'anything-org-headlines "anything-config" "\
Preconfigured anything to show org headlines.

\(fn)" t nil)

(autoload 'anything-info-gnus "anything-config" "\
Preconfigured anything to browse Gnus Manual.

\(fn)" t nil)

(autoload 'anything-regexp "anything-config" "\
Preconfigured anything to build regexps.
`query-replace-regexp' can be run from there against found regexp.

\(fn)" t nil)

(autoload 'anything-c-copy-files-async "anything-config" "\
Preconfigured anything to copy file list FLIST to DEST asynchronously.

\(fn)" t nil)

(autoload 'anything-find-files "anything-config" "\
Preconfigured `anything' for anything implementation of `find-file'.
Called with a prefix arg show history if some.
Don't call it from programs, use `anything-find-files-1' instead.
This is the starting point for nearly all actions you can do on files.

\(fn ARG)" t nil)

(autoload 'anything-write-file "anything-config" "\
Preconfigured `anything' providing completion for `write-file'.

\(fn)" t nil)

(autoload 'anything-insert-file "anything-config" "\
Preconfigured `anything' providing completion for `insert-file'.

\(fn)" t nil)

(autoload 'anything-dired-rename-file "anything-config" "\
Preconfigured `anything' to rename files from dired.

\(fn)" t nil)

(autoload 'anything-dired-copy-file "anything-config" "\
Preconfigured `anything' to copy files from dired.

\(fn)" t nil)

(autoload 'anything-dired-symlink-file "anything-config" "\
Preconfigured `anything' to symlink files from dired.

\(fn)" t nil)

(autoload 'anything-dired-hardlink-file "anything-config" "\
Preconfigured `anything' to hardlink files from dired.

\(fn)" t nil)

(autoload 'anything-do-grep "anything-config" "\
Preconfigured anything for grep.
Contrarily to Emacs `grep' no default directory is given, but
the full path of candidates in ONLY.
That allow to grep different files not only in `default-directory' but anywhere
by marking them (C-<SPACE>). If one or more directory is selected
grep will search in all files of these directories.
You can use also wildcard in the base name of candidate.
If a prefix arg is given use the -r option of grep.
The prefix arg can be passed before or after start.
See also `anything-do-grep-1'.

\(fn)" t nil)

(autoload 'anything-do-zgrep "anything-config" "\
Preconfigured anything for zgrep.

\(fn CANDIDATE)" nil nil)

(autoload 'anything-filelist "anything-config" "\
Preconfigured `anything' to open files instantly.

See `anything-c-filelist-file-name' docstring for usage.

\(fn)" t nil)

(autoload 'anything-filelist+ "anything-config" "\
Preconfigured `anything' to open files/buffers/bookmarks instantly.

This is a replacement for `anything-for-files'.
See `anything-c-filelist-file-name' docstring for usage.

\(fn)" t nil)

(autoload 'anything-M-x "anything-config" "\
Preconfigured `anything' for Emacs commands.
It is `anything' replacement of regular `M-x' `execute-extended-command'.

\(fn)" t nil)

(autoload 'anything-manage-advice "anything-config" "\
Preconfigured `anything' to disable/enable function advices.

\(fn)" t nil)

(autoload 'anything-bookmark-ext "anything-config" "\
Preconfigured `anything' for bookmark-extensions sources.
Needs bookmark-ext.el:
<http://mercurial.intuxication.org/hg/emacs-bookmark-extension>.
Contain also `anything-c-source-google-suggest'.

\(fn)" t nil)

(autoload 'anything-simple-call-tree "anything-config" "\
Preconfigured `anything' for simple-call-tree. List function relationships.

Needs simple-call-tree.el.
http://www.emacswiki.org/cgi-bin/wiki/download/simple-call-tree.el

\(fn)" t nil)

(autoload 'anything-mark-ring "anything-config" "\
Preconfigured `anything' for `anything-c-source-mark-ring'.

\(fn)" t nil)

(autoload 'anything-global-mark-ring "anything-config" "\
Preconfigured `anything' for `anything-c-source-global-mark-ring'.

\(fn)" t nil)

(autoload 'anything-all-mark-rings "anything-config" "\
Preconfigured `anything' for `anything-c-source-global-mark-ring' and `anything-c-source-mark-ring'.

\(fn)" t nil)

(autoload 'anything-yaoddmuse-emacswiki-edit-or-view "anything-config" "\
Preconfigured `anything' to edit or view EmacsWiki page.

Needs yaoddmuse.el.

http://www.emacswiki.org/emacs/download/yaoddmuse.el

\(fn)" t nil)

(autoload 'anything-yaoddmuse-emacswiki-post-library "anything-config" "\
Preconfigured `anything' to post library to EmacsWiki.

Needs yaoddmuse.el.

http://www.emacswiki.org/emacs/download/yaoddmuse.el

\(fn)" t nil)

(autoload 'anything-eval-expression "anything-config" "\
Preconfigured anything for `anything-c-source-evaluation-result'.

\(fn ARG)" t nil)

(autoload 'anything-eval-expression-with-eldoc "anything-config" "\
Preconfigured anything for `anything-c-source-evaluation-result' with `eldoc' support. 

\(fn)" t nil)

(autoload 'anything-calcul-expression "anything-config" "\
Preconfigured anything for `anything-c-source-calculation-result'.

\(fn)" t nil)

(autoload 'anything-surfraw "anything-config" "\
Preconfigured `anything' to search PATTERN with search ENGINE.

\(fn PATTERN ENGINE)" t nil)

(autoload 'anything-call-source "anything-config" "\
Preconfigured `anything' to call anything source.

\(fn)" t nil)

(autoload 'anything-execute-anything-command "anything-config" "\
Preconfigured `anything' to execute preconfigured `anything'.

\(fn)" t nil)

(autoload 'anything-create "anything-config" "\
Preconfigured `anything' to do many create actions from STRING.
See also `anything-create--actions'.

\(fn &optional STRING INITIAL-INPUT)" t nil)

(autoload 'anything-top "anything-config" "\
Preconfigured `anything' for top command.

\(fn)" t nil)

(autoload 'anything-select-xfont "anything-config" "\
Preconfigured `anything' to select Xfont.

\(fn)" t nil)

(autoload 'anything-world-time "anything-config" "\
Preconfigured `anything' to show world time.

\(fn)" t nil)

(autoload 'anything-apt "anything-config" "\
Preconfigured `anything' : frontend of APT package manager.
With a prefix arg reload cache.

\(fn ARG)" t nil)

(autoload 'anything-esh-pcomplete "anything-config" "\
Preconfigured anything to provide anything completion in eshell.

\(fn)" t nil)

(autoload 'anything-eshell-history "anything-config" "\
Preconfigured anything for eshell history.

\(fn)" t nil)

(autoload 'anything-c-run-external-command "anything-config" "\
Preconfigured `anything' to run External PROGRAM asyncronously from Emacs.
If program is already running exit with error.
You can set your own list of commands with
`anything-c-external-commands-list'.

\(fn PROGRAM)" t nil)

(autoload 'anything-ratpoison-commands "anything-config" "\
Preconfigured `anything' to execute ratpoison commands.

\(fn)" t nil)

(autoload 'anything-ucs "anything-config" "\
Preconfigured anything for `ucs-names' math symbols.

\(fn)" t nil)

(autoload 'anything-c-apropos "anything-config" "\
Preconfigured anything to describe commands, functions, variables and faces.

\(fn)" t nil)

;;;***

;;;### (autoloads (apache-mode) "apache-mode" "apache-mode/apache-mode.el"
;;;;;;  (20191 64850))
;;; Generated autoloads from apache-mode/apache-mode.el

(autoload 'apache-mode "apache-mode" "\
Major mode for editing Apache configuration files.

\(fn)" t nil)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))

;;;***

;;;### (autoloads (coffee-mode) "coffee-mode" "coffee-mode/coffee-mode.el"
;;;;;;  (20194 33736))
;;; Generated autoloads from coffee-mode/coffee-mode.el

(autoload 'coffee-mode "coffee-mode" "\
Major mode for editing CoffeeScript.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))

(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

;;;***

;;;### (autoloads (ghc-core-mode ghc-core-create-core) "ghc-core"
;;;;;;  "haskell-mode/ghc-core.el" (20194 33645))
;;; Generated autoloads from haskell-mode/ghc-core.el

(autoload 'ghc-core-create-core "ghc-core" "\
Compiled and load the current buffer as tidy core

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.hcr\\'" . ghc-core-mode))

(autoload 'ghc-core-mode "ghc-core" "\
Major mode for GHC Core files.

\(fn)" t nil)

;;;***

;;;### (autoloads (haskell-c-mode) "haskell-c" "haskell-mode/haskell-c.el"
;;;;;;  (20194 33645))
;;; Generated autoloads from haskell-mode/haskell-c.el

(add-to-list 'auto-mode-alist '("\\.hsc\\'" . haskell-c-mode))

(autoload 'haskell-c-mode "haskell-c" "\
Major mode for Haskell FFI files.

\(fn)" t nil)

;;;***

;;;### (autoloads (haskell-cabal-mode) "haskell-cabal" "haskell-mode/haskell-cabal.el"
;;;;;;  (20194 33645))
;;; Generated autoloads from haskell-mode/haskell-cabal.el

(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))

(autoload 'haskell-cabal-mode "haskell-cabal" "\
Major mode for Cabal package description files.

\(fn)" t nil)

;;;***

;;;### (autoloads (haskell-decl-scan-mode) "haskell-decl-scan" "haskell-mode/haskell-decl-scan.el"
;;;;;;  (20194 33645))
;;; Generated autoloads from haskell-mode/haskell-decl-scan.el

(autoload 'haskell-decl-scan-mode "haskell-decl-scan" "\
Minor mode for declaration scanning for Haskell mode.
Top-level declarations are scanned and listed in the menu item \"Declarations\".
Selecting an item from this menu will take point to the start of the
declaration.

\\[haskell-ds-forward-decl] and \\[haskell-ds-backward-decl] move forward and backward to the start of a declaration.

Under XEmacs, the following keys are also defined:

\\[fume-list-functions] lists the declarations of the current buffer,
\\[fume-prompt-function-goto] prompts for a declaration to move to, and
\\[fume-mouse-function-goto] moves to the declaration whose name is at point.

This may link with `haskell-doc' (only for Emacs currently).

For non-literate and LaTeX-style literate scripts, we assume the
common convention that top-level declarations start at the first
column.  For Bird-style literate scripts, we assume the common
convention that top-level declarations start at the third column,
ie. after \"> \".

Anything in `font-lock-comment-face' is not considered for a
declaration.  Therefore, using Haskell font locking with comments
coloured in `font-lock-comment-face' improves declaration scanning.

To turn on declaration scanning for all Haskell buffers, add this to
.emacs:

  (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)

To turn declaration scanning on for the current buffer, call
`turn-on-haskell-decl-scan'.

Literate Haskell scripts are supported: If the value of
`haskell-literate' (automatically set by the Haskell mode of
Moss&Thorn) is `bird', a Bird-style literate script is assumed.  If it
is nil or `tex', a non-literate or LaTeX-style literate script is
assumed, respectively.

Invokes `haskell-decl-scan-mode-hook'.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (haskell-doc-show-type haskell-doc-mode) "haskell-doc"
;;;;;;  "haskell-mode/haskell-doc.el" (20194 33645))
;;; Generated autoloads from haskell-mode/haskell-doc.el

(autoload 'haskell-doc-mode "haskell-doc" "\
Enter `haskell-doc-mode' for showing fct types in the echo area.
See variable docstring.

\(fn &optional ARG)" t nil)

(defalias 'turn-on-haskell-doc-mode 'haskell-doc-mode)

(autoload 'haskell-doc-show-type "haskell-doc" "\
Show the type of the function near point.
For the function under point, show the type in the echo area.
This information is extracted from the `haskell-doc-prelude-types' alist
of prelude functions and their types, or from the local functions in the
current buffer.

\(fn &optional SYM)" t nil)

;;;***

;;;### (autoloads (haskell-indent-mode) "haskell-indent" "haskell-mode/haskell-indent.el"
;;;;;;  (20194 33645))
;;; Generated autoloads from haskell-mode/haskell-indent.el

(autoload 'haskell-indent-mode "haskell-indent" "\
``Intelligent'' Haskell indentation mode.
This deals with the layout rule of Haskell.
\\[haskell-indent-cycle] starts the cycle which proposes new
possibilities as long as the TAB key is pressed.  Any other key
or mouse click terminates the cycle and is interpreted except for
RET which merely exits the cycle.
Other special keys are:
    \\[haskell-indent-insert-equal]
      inserts an =
    \\[haskell-indent-insert-guard]
      inserts an |
    \\[haskell-indent-insert-otherwise]
      inserts an | otherwise =
these functions also align the guards and rhs of the current definition
    \\[haskell-indent-insert-where]
      inserts a where keyword
    \\[haskell-indent-align-guards-and-rhs]
      aligns the guards and rhs of the region
    \\[haskell-indent-put-region-in-literate]
      makes the region a piece of literate code in a literate script

Invokes `haskell-indent-hook' if not nil.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (haskell-indentation-mode) "haskell-indentation"
;;;;;;  "haskell-mode/haskell-indentation.el" (20194 33645))
;;; Generated autoloads from haskell-mode/haskell-indentation.el

(autoload 'haskell-indentation-mode "haskell-indentation" "\
Haskell indentation mode that deals with the layout rule.
It rebinds RET, DEL and BACKSPACE, so that indentations can be
set and deleted as if they were real tabs.  It supports
autofill-mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (haskell-hayoo haskell-hoogle literate-haskell-mode
;;;;;;  haskell-mode) "haskell-mode" "haskell-mode/haskell-mode.el"
;;;;;;  (20194 33645))
;;; Generated autoloads from haskell-mode/haskell-mode.el

(add-to-list 'load-path (or (file-name-directory load-file-name) (car load-path)))

(autoload 'haskell-mode "haskell-mode" "\
Major mode for editing Haskell programs.
Blank lines separate paragraphs, comments start with `-- '.
\\<haskell-mode-map>
Literate scripts are supported via `literate-haskell-mode'.
The variable `haskell-literate' indicates the style of the script in the
current buffer.  See the documentation on this variable for more details.

Modules can hook in via `haskell-mode-hook'.  The following modules
are supported with an `autoload' command:

   `haskell-decl-scan', Graeme E Moss
     Scans top-level declarations, and places them in a menu.

   `haskell-doc', Hans-Wolfgang Loidl
     Echoes types of functions or syntax of keywords when the cursor is idle.

   `haskell-indentation', Kristof Bastiaensen
     Intelligent semi-automatic indentation Mk2

   `haskell-indent', Guy Lapalme
     Intelligent semi-automatic indentation.

   `haskell-simple-indent', Graeme E Moss and Heribert Schuetz
     Simple indentation.

Module X is activated using the command `turn-on-X'.  For example,
`haskell-indent' is activated using `turn-on-haskell-indent'.
For more information on a module, see the help for its `X-mode'
function.  Some modules can be deactivated using `turn-off-X'.  (Note
that `haskell-doc' is irregular in using `turn-(on/off)-haskell-doc-mode'.)

Use `haskell-version' to find out what version this is.

Invokes `haskell-mode-hook'.

\(fn)" t nil)

(autoload 'literate-haskell-mode "haskell-mode" "\
As `haskell-mode' but for literate scripts.

\(fn)" t nil)
(add-to-list 'auto-mode-alist        '("\\.\\(?:[gh]s\\|hi\\)\\'" . haskell-mode))
(add-to-list 'auto-mode-alist        '("\\.l[gh]s\\'" . literate-haskell-mode))
(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode))

(autoload 'haskell-hoogle "haskell-mode" "\
Do a Hoogle search for QUERY.

\(fn QUERY)" t nil)

(defalias 'hoogle 'haskell-hoogle)

(autoload 'haskell-hayoo "haskell-mode" "\
Do a Hayoo search for QUERY.

\(fn QUERY)" t nil)

(defalias 'hayoo 'haskell-hayoo)

;;;***

;;;### (autoloads (inferior-haskell-find-haddock inferior-haskell-find-definition
;;;;;;  inferior-haskell-info inferior-haskell-type inferior-haskell-load-and-run
;;;;;;  inferior-haskell-load-file switch-to-haskell) "inf-haskell"
;;;;;;  "haskell-mode/inf-haskell.el" (20194 33645))
;;; Generated autoloads from haskell-mode/inf-haskell.el

(defalias 'run-haskell 'switch-to-haskell)

(autoload 'switch-to-haskell "inf-haskell" "\
Show the inferior-haskell buffer.  Start the process if needed.

\(fn &optional ARG)" t nil)

(autoload 'inferior-haskell-load-file "inf-haskell" "\
Pass the current buffer's file to the inferior haskell process.
If prefix arg \\[universal-argument] is given, just reload the previous file.

\(fn &optional RELOAD)" t nil)

(autoload 'inferior-haskell-load-and-run "inf-haskell" "\
Pass the current buffer's file to haskell and then run a COMMAND.

\(fn COMMAND)" t nil)

(autoload 'inferior-haskell-type "inf-haskell" "\
Query the haskell process for the type of the given expression.
If optional argument `insert-value' is non-nil, insert the type above point
in the buffer.  This can be done interactively with the \\[universal-argument] prefix.
The returned info is cached for reuse by `haskell-doc-mode'.

\(fn EXPR &optional INSERT-VALUE)" t nil)

(autoload 'inferior-haskell-info "inf-haskell" "\
Query the haskell process for the info of the given expression.

\(fn SYM)" t nil)

(autoload 'inferior-haskell-find-definition "inf-haskell" "\
Attempt to locate and jump to the definition of the given expression.

\(fn SYM)" t nil)

(autoload 'inferior-haskell-find-haddock "inf-haskell" "\
Find and open the Haddock documentation of SYM.
Make sure to load the file into GHCi or Hugs first by using C-c C-l.
Only works for functions in a package installed with ghc-pkg, or
whatever the value of `haskell-package-manager-name' is.

This function needs to find which package a given module belongs
to.  In order to do this, it computes a module-to-package lookup
alist, which is expensive to compute (it takes upwards of five
seconds with more than about thirty installed packages).  As a
result, we cache it across sessions using the cache file
referenced by `inferior-haskell-module-alist-file'. We test to
see if this is newer than `haskell-package-conf-file' every time
we load it.

\(fn SYM)" t nil)

;;;***

;;;### (autoloads (php-mode php-file-patterns) "php-mode" "php-mode/php-mode.el"
;;;;;;  (20194 32522))
;;; Generated autoloads from php-mode/php-mode.el

(defvar php-file-patterns '("\\.php[s34]?\\'" "\\.phtml\\'" "\\.inc\\'") "\
List of file patterns for which to automatically invoke `php-mode'.")

(custom-autoload 'php-file-patterns "php-mode" nil)

(autoload 'php-mode "php-mode" "\
Major mode for editing PHP code.

\\{php-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (python-pep8) "python-pep8" "python-pep8/python-pep8.el"
;;;;;;  (20191 65453))
;;; Generated autoloads from python-pep8/python-pep8.el

(autoload 'python-pep8 "python-pep8" "\
Run PEP8, and collect output in a buffer.
While pep8 runs asynchronously, you can use \\[next-error] (M-x next-error),
or \\<python-pep8-mode-map>\\[compile-goto-error] in the grep output buffer, to go to the lines where pep8 found matches.

\(fn)" t nil)

(defalias 'pep8 'python-pep8)

;;;***

;;;### (autoloads (global-smart-tab-mode smart-tab-mode smart-tab-mode-on
;;;;;;  smart-tab) "smart-tab" "smart-tab/smart-tab.el" (20191 65515))
;;; Generated autoloads from smart-tab/smart-tab.el

(autoload 'smart-tab "smart-tab" "\
Try to 'do the smart thing' when tab is pressed.
`smart-tab' attempts to expand the text before the point or
indent the current line or selection.

In a regular buffer, `smart-tab' will attempt to expand with
either `hippie-expand' or `dabbrev-expand', depending on the
value of `smart-tab-using-hippie-expand'. Alternatively, if
`auto-complete-mode' is enabled in the current buffer,
`auto-complete' will be used to attempt expansion. If the mark is
active, or PREFIX is \\[universal-argument], then `smart-tab'
will indent the region or the current line (if the mark is not
active).

\(fn &optional PREFIX)" t nil)

(autoload 'smart-tab-mode-on "smart-tab" "\
Turn on `smart-tab-mode'.

\(fn)" nil nil)

(autoload 'smart-tab-mode "smart-tab" "\
Enable `smart-tab' to be used in place of tab.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

\(fn &optional ARG)" t nil)

(defvar global-smart-tab-mode nil "\
Non-nil if Global-Smart-Tab mode is enabled.
See the command `global-smart-tab-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-smart-tab-mode'.")

(custom-autoload 'global-smart-tab-mode "smart-tab" nil)

(autoload 'global-smart-tab-mode "smart-tab" "\
Toggle Smart-Tab mode in every possible buffer.
With prefix ARG, turn Global-Smart-Tab mode on if and only if
ARG is positive.
Smart-Tab mode is enabled in all buffers where
`smart-tab-mode-on' would do it.
See `smart-tab-mode' for more information on Smart-Tab mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (zencoding-preview zencoding-expand-yas zencoding-mode
;;;;;;  zencoding-expand-line) "zencoding-mode" "zencoding-mode/zencoding-mode.el"
;;;;;;  (20194 33451))
;;; Generated autoloads from zencoding-mode/zencoding-mode.el

(autoload 'zencoding-expand-line "zencoding-mode" "\
Replace the current line's zencode expression with the corresponding expansion.
If prefix ARG is given or region is visible call `zencoding-preview' to start an
interactive preview.

Otherwise expand line directly.

For more information see `zencoding-mode'.

\(fn ARG)" t nil)

(autoload 'zencoding-mode "zencoding-mode" "\
Minor mode for writing HTML and CSS markup.
With zen coding for HTML and CSS you can write a line like

  ul#name>li.item*2

and have it expanded to

  <ul id=\"name\">
    <li class=\"item\"></li>
    <li class=\"item\"></li>
  </ul>

This minor mode defines keys for quick access:

\\{zencoding-mode-keymap}

Home page URL `http://www.emacswiki.org/emacs/ZenCoding'.

See also `zencoding-expand-line'.

\(fn &optional ARG)" t nil)

(autoload 'zencoding-expand-yas "zencoding-mode" "\
Not documented

\(fn)" t nil)

(autoload 'zencoding-preview "zencoding-mode" "\
Expand zencode between BEG and END interactively.
This will show a preview of the expanded zen code and you can
accept it or skip it.

\(fn BEG END)" t nil)

;;;***

;;;### (autoloads nil nil ("anything/anything-match-plugin.el" "anything/extensions/anything-complete.el"
;;;;;;  "anything/extensions/anything-gtags.el" "anything/extensions/anything-obsolete.el"
;;;;;;  "anything/extensions/anything-show-completion.el" "el-get/el-get-install.el"
;;;;;;  "el-get/el-get.el" "haskell-mode/haskell-checkers.el" "haskell-mode/haskell-font-lock.el"
;;;;;;  "haskell-mode/haskell-ghci.el" "haskell-mode/haskell-hugs.el"
;;;;;;  "haskell-mode/haskell-simple-indent.el" "haskell-mode/haskell-site-file.el"
;;;;;;  "smooth-scrolling/smooth-scrolling.el" "tail/tail.el") (20194
;;;;;;  33736 397712))

;;;***

(provide '.loaddefs)
;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; .loaddefs.el ends here
