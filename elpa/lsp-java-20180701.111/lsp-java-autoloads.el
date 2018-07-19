;;; lsp-java-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lsp-java" "lsp-java.el" (0 0 0 0))
;;; Generated autoloads from lsp-java.el

(let ((loads (get 'lsp-java 'custom-loads))) (if (member '"lsp-java" loads) nil (put 'lsp-java 'custom-loads (cons '"lsp-java" loads))))

(defvar lsp-java-server-install-dir (locate-user-emacs-file "eclipse.jdt.ls/server/") "\
Install directory for eclipse.jdt.ls-server.
The slash is expected at the end.")

(custom-autoload 'lsp-java-server-install-dir "lsp-java" t)

(defvar lsp-java-java-path "java" "\
Path of the java executable.")

(custom-autoload 'lsp-java-java-path "lsp-java" t)

(defvar lsp-java-workspace-dir (expand-file-name (locate-user-emacs-file "workspace/")) "\
LSP java workspace directory.")

(custom-autoload 'lsp-java-workspace-dir "lsp-java" t)

(defvar lsp-java-workspace-cache-dir (expand-file-name (locate-user-emacs-file "workspace/.cache/")) "\
LSP java workspace cache directory.")

(custom-autoload 'lsp-java-workspace-cache-dir "lsp-java" t)

(defvar lsp-java--workspace-folders nil "\
LSP java workspace folders storing files downloaded from JDT.")

(custom-autoload 'lsp-java--workspace-folders "lsp-java" t)

(defvar lsp-java-vmargs '("-noverify" "-Xmx1G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication") "\
Specifies extra VM arguments used to launch the Java Language Server.

Eg. use `-noverify -Xmx1G -XX:+UseG1GC
-XX:+UseStringDeduplication` to bypass class
verification,increase the heap size to 1GB and enable String
deduplication with the G1 Garbage collector")

(custom-autoload 'lsp-java-vmargs "lsp-java" t)

(defvar lsp-java-incomplete-classpath 'warning "\
Specifies the severity of the message when the classpath is incomplete for a Java file.")

(custom-autoload 'lsp-java-incomplete-classpath "lsp-java" t)

(defvar lsp-java-update-build-configuration 'automatic "\
Specifies how modifications on build files update the Java classpath/configuration.")

(custom-autoload 'lsp-java-update-build-configuration "lsp-java" t)

(defvar lsp-java-import-exclusions '("**/node_modules/**" "**/.metadata/**" "**/archetype-resources/**" "**/META-INF/maven/**") "\
Configure glob patterns for excluding folders.")

(custom-autoload 'lsp-java-import-exclusions "lsp-java" t)

(defvar lsp-java-favorite-static-members '("org.junit.Assert.*" "org.junit.Assume.*" "org.junit.jupiter.api.Assertions.*" "org.junit.jupiter.api.Assumptions.*" "org.junit.jupiter.api.DynamicContainer.*" "org.junit.jupiter.api.DynamicTest.*") "\
Defines a list of static members or types with static members.

 Content assist will propose those static members even if the
 import is missing.")

(custom-autoload 'lsp-java-favorite-static-members "lsp-java" t)

(defvar lsp-java-import-order '("java" "javax" "com" "org") "\
Defines the sorting order of import statements.

A package or type name prefix (e.g. 'org.eclipse') is a valid entry. An import is always added to the most specific group.")

(custom-autoload 'lsp-java-import-order "lsp-java" t)

(defvar lsp-java-trace-server 'off "\
Traces the communication between Emacs and the Java language server.")

(custom-autoload 'lsp-java-trace-server "lsp-java" t)

(defvar lsp-java-enable-file-watch nil "\
Defines whether the client will monitor the files for changes.")

(custom-autoload 'lsp-java-enable-file-watch "lsp-java" t)

(defvar lsp-java-format-enabled t "\
Specifies whether or not formatting is enabled on the language server.")

(custom-autoload 'lsp-java-format-enabled "lsp-java" t)

(defvar lsp-java-format-settings-url nil "\
Specifies the file path to the formatter xml url.")

(custom-autoload 'lsp-java-format-settings-url "lsp-java" t)

(defvar lsp-java-format-settings-profile nil "\
Specifies the formatter profile name.")

(custom-autoload 'lsp-java-format-settings-profile "lsp-java" t)

(defvar lsp-java-format-comments-enabled nil "\
Preference key used to include the comments during the formatting.")

(custom-autoload 'lsp-java-format-comments-enabled "lsp-java" t)

(defvar lsp-java-save-action-organize-imports t "\
Organize imports on save.")

(custom-autoload 'lsp-java-save-action-organize-imports "lsp-java" t)

(defvar lsp-java-organize-imports t "\
Specifies whether or not organize imports is enabled as a save action.")

(custom-autoload 'lsp-java-organize-imports "lsp-java" t)

(defvar lsp-java-bundles nil "\
List of bundles that will be loaded in the JDT server.")

(custom-autoload 'lsp-java-bundles "lsp-java" t)

(defvar lsp-java-import-gradle-enabled t "\
Enable/disable the Gradle importer.")

(custom-autoload 'lsp-java-import-gradle-enabled "lsp-java" t)

(defvar lsp-java-import-maven-enabled t "\
Enable/disable the Maven importer.")

(custom-autoload 'lsp-java-import-maven-enabled "lsp-java" t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-java" '("lsp-java-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lsp-java-autoloads.el ends here
