(in-package :editor)

;; list of symbols starting with either ':' or '&', and global variables (for auto completion)

(defvar *keywords-list*
  '(":abort"
    ":above"
    ":absolute"
    ":accessor"
    ":adjustable"
    ":after"
    ":all"
    ":allocation"
    ":allow-other-keys"
    ":always"
    ":amount"
    ":and"
    ":anonymous"
    ":ansi"
    ":ansi-cl"
    ":answer-variable"
    ":append"
    ":apropos"
    ":arg"
    ":args"
    ":argument-precedence-order"
    ":arguments"
    ":around"
    ":array"
    ":atsignp"
    ":back"
    ":backtrace"
    ":backward-search"
    ":base"
    ":bds"
    ":before"
    ":below"
    ":big-endian"
    ":binding-stack"
    ":block"
    ":block-end"
    ":boehm-gc"
    ":br"
    ":break"
    ":break-after"
    ":broken-at"
    ":bs"
    ":buffer-start-column"
    ":by"
    ":byte"
    ":bytecodes"
    ":call"
    ":callback"
    ":capitalize"
    ":case"
    ":catch"
    ":c/c++"
    ":cdecl"
    ":cdr-1"
    ":cdr-14"
    ":cdr-5"
    ":cdr-7"
    ":cf"
    ":c-file"
    ":char"
    ":character"
    ":circle"
    ":cl"
    ":class"
    ":clos"
    ":clos-streams"
    ":cmp"
    ":cmu-format"
    ":code"
    ":%code"
    ":colinc"
    ":colnum"
    ":colonp"
    ":commands"
    ":common"
    ":common-lisp"
    ":compile-file"
    ":compiler"
    ":compile-toplevel"
    ":complaint"
    ":conc-name"
    ":cond"
    ":cond-after"
    ":cond-before"
    ":cons-entries"
    ":constant"
    ":constructor"
    ":continue"
    ":control-string"
    ":copier"
    ":correctablep"
    ":count"
    ":cr"
    ":create"
    ":crlf"
    ":cstream"
    ":cstring"
    ":current"
    ":data"
    ":data-file"
    ":datum"
    ":declarations"
    ":declare"
    ":default"
    ":default-initargs"
    ":defaults"
    ":delete-methods"
    ":depth"
    ":description"
    ":device"
    ":dffi"
    ":direct-default-initargs"
    ":direction"
    ":directory"
    ":direct-slots"
    ":direct-superclasses"
    ":disable"
    ":disassemble"
    ":displaced-index-offset"
    ":displaced-to"
    ":display"
    ":dll"
    ":dlopen"
    ":doc"
    ":documentation"
    ":dont-know"
    ":double"
    ":down"
    ":downcase"
    ":downfrom"
    ":downto"
    ":drive-letters"
    ":dtype"
    ":each"
    ":ecl"
    ":ecl-bytecmp"
    ":ecl-pde"
    ":ecl-read-write-lock"
    ":ecl-weak-hash"
    ":element-type"
    ":else"
    ":end"
    ":end1"
    ":end2"
    ":entries"
    ":environ"
    ":environment"
    ":eof"
    ":error"
    ":error-stream"
    ":escape"
    ":eval"
    ":execute"
    ":exit"
    ":exited"
    ":expected-type"
    ":export"
    ":export-from"
    ":extended"
    ":external"
    ":external-format"
    ":fas"
    ":fasl"
    ":ffi"
    ":file"
    ":fill"
    ":fill-pointer"
    ":finally"
    ":fixnum"
    ":flag-variable"
    ":float"
    ":force-load"
    ":for-keywords"
    ":form"
    ":format-arguments"
    ":format-ars"
    ":format-control"
    ":formatter"
    ":forward-search"
    ":frame-stack"
    ":from"
    ":from-end"
    ":frs"
    ":fs"
    ":full"
    ":fully-buffered"
    ":function"
    ":functions"
    ":generic-function"
    ":generic-function-class"
    ":gensym"
    ":go"
    ":help"
    ":help-stack"
    ":her"
    ":h-file"
    ":hide"
    ":hide-package"
    ":his"
    ":history"
    ":host"
    ":hp"
    ":hs"
    ":identity"
    ":identity-with-one-argument"
    ":ieee-floating-point"
    ":if-does-not-exist"
    ":if-error-exists"
    ":if-exists"
    ":if-input-does-not-exist"
    ":if-output-exists"
    ":ignore"
    ":implicit-for-required"
    ":import-from"
    ":in"
    ":include"
    ":inclusive"
    ":inclusive-permitted"
    ":index"
    ":infinity-data"
    ":inherited"
    ":initarg"
    ":initargs"
    ":initform"
    ":initfunction"
    ":initial-bindings"
    ":initial-contents"
    ":initial-element"
    ":initially"
    ":initial-offset"
    ":initial-p"
    ":initial-value"
    ":input"
    ":inspect"
    ":instance"
    ":int"
    ":int16-t"
    ":int32-t"
    ":int64-t"
    ":int8-t"
    ":interactive"
    ":interactive-function"
    ":intern"
    ":internal"
    ":invert"
    ":io"
    ":iso-8859-1"
    ":iteration-keywords"
    ":its"
    ":junk-allowed"
    ":key"
    ":key-and-value"
    ":keyword"
    ":keywords"
    ":kill-waiting"
    ":kind"
    ":lambda-expression"
    ":lambda-list"
    ":last"
    ":latin-1"
    ":ld"
    ":le"
    ":length"
    ":level"
    ":lexical-var"
    ":lexical-variables"
    ":lf"
    ":line"
    ":linear"
    ":line-buffered"
    ":line-relative"
    ":lines"
    ":link"
    ":linux"
    ":lisp-files"
    ":list-all"
    ":literal"
    ":little-endian"
    ":load"
    ":loadrc"
    ":load-toplevel"
    ":local"
    ":location"
    ":lock"
    ":lockable"
    ":long"
    ":long-float"
    ":long-long"
    ":macro"
    ":macros"
    ":mandatory"
    ":mask"
    ":message"
    ":metaclass"
    ":method"
    ":method-class"
    ":method-combination"
    ":method-from-defgeneric-p"
    ":miser"
    ":miser-width"
    ":mode"
    ":module"
    ":most-specific-first"
    ":most-specific-last"
    ":name"
    ":named"
    ":names"
    ":newest"
    ":newline"
    ":new-version"
    ":next"
    ":nicknames"
    ":no-error"
    ":no-interrupts"
    ":noloadrc"
    ":none"
    ":not"
    ":null-terminated-p"
    ":object"
    ":octets"
    ":of"
    ":offset"
    ":one-liner"
    ":operands"
    ":operation"
    ":operations"
    ":operator"
    ":optimize-slot-access"
    ":optional"
    ":options"
    ":or"
    ":order"
    ":output"
    ":output-file"
    ":overwrite"
    ":package"
    ":params"
    ":pass-through"
    ":path-keywords"
    ":pathname"
    ":per-line-prefix"
    ":per-line-prefix-end"
    ":pid"
    ":plist"
    ":pointer-self"
    ":pointer-void"
    ":pop"
    ":posn"
    ":possibilities"
    ":pprint-dispatch"
    ":pr"
    ":predicate"
    ":prefix"
    ":prefixed-api"
    ":prefix-length"
    ":preposition-groups"
    ":preserve"
    ":preserve-whitespace"
    ":pretty"
    ":previous"
    ":print"
    ":print-after"
    ":print-banner"
    ":print-function"
    ":print-object"
    ":priority"
    ":private"
    ":probe"
    ":process"
    ":prompt-hook"
    ":qualifiers"
    ":quiet"
    ":quit"
    ":radix"
    ":read"
    ":readably"
    ":reader"
    ":readers"
    ":read-only"
    ":read-only-p"
    ":recurse"
    ":recursive"
    ":rehash-size"
    ":rehash-threshold"
    ":relative"
    ":relativep"
    ":relative-package-names"
    ":remaining"
    ":rename"
    ":rename-and-delete"
    ":repeat"
    ":report"
    ":report-function"
    ":required"
    ":reset-count"
    ":resolve-symlinks"
    ":restart"
    ":returning"
    ":right-margin"
    ":rules"
    ":running"
    ":sealedp"
    ":search-list"
    ":section"
    ":section-column"
    ":section-end"
    ":sectionp"
    ":section-relative"
    ":section-start-line"
    ":separator-string"
    ":set"
    ":set-default-pathname"
    ":shadow"
    ":shadowing-import"
    ":shadowing-import-from"
    ":shared"
    ":shared-data-file"
    ":short"
    ":side-effects"
    ":signaled"
    ":size"
    ":skip"
    ":slot-definition"
    ":slot-names"
    ":special"
    ":specializers"
    ":start"
    ":start1"
    ":start2"
    ":start-column"
    ":%status"
    ":stdcall"
    ":step"
    ":stop"
    ":stopped"
    ":stream"
    ":string"
    ":struct"
    ":suffix"
    ":suffix-length"
    ":supersede"
    ":supporting-libraries"
    ":switch"
    ":symbol-types"
    ":system-library"
    ":system-p"
    ":sysv"
    ":tables"
    ":tag"
    ":target"
    ":temporary-pathname"
    ":temp-variable"
    ":tempvars"
    ":test"
    ":test-function"
    ":test-not"
    ":the"
    ":then"
    ":threads"
    ":to"
    ":tr"
    ":trace"
    ":type"
    ":type-keywords"
    ":types"
    ":type-symbols"
    ":ucs-2"
    ":ucs-2be"
    ":ucs-2le"
    ":ucs-4"
    ":ucs-4be"
    ":ucs-4le"
    ":uint16-t"
    ":uint32-t"
    ":uint64-t"
    ":uint8-t"
    ":unblocked"
    ":unhide"
    ":unhide-all"
    ":unhide-package"
    ":unhp"
    ":unicode"
    ":union"
    ":unix"
    ":unix64"
    ":unknown"
    ":unmask"
    ":unsigned"
    ":unsigned-byte"
    ":unsigned-char"
    ":unsigned-int"
    ":unsigned-long"
    ":unsigned-long-long"
    ":unsigned-short"
    ":unspecific"
    ":until"
    ":untr"
    ":untrace"
    ":up"
    ":upcase"
    ":upfrom"
    ":upto"
    ":us-ascii"
    ":use"
    ":user-data"
    ":utf-8"
    ":value"
    ":variables"
    ":verbose"
    ":version"
    ":void"
    ":wait"
    ":waiting"
    ":walker"
    ":walk-form"
    ":walk-function"
    ":weakness"
    ":which"
    ":while"
    ":wild"
    ":wild-inferiors"
    ":win64"
    ":with"
    ":word-buffer"
    ":write"
    ":writer"
    ":writers"
    ":x86_64"
    ":zombi"
    "&allow-other-keys"
    "&aux"
    "&body"
    "&environment"
    "&key"
    "&optional"
    "&rest"
    "&whole"
    "*break-on-signals*"
    "*compile-file-pathname*"
    "*compile-file-truename*"
    "*compile-print*"
    "*compile-verbose*"
    "*debug-io*"
    "*debugger-hook*"
    "*default-pathname-defaults*"
    "*error-output*"
    "*features*"
    "*gensym-counter*"
    "*load-pathname*"
    "*load-print*"
    "*load-truename*"
    "*load-verbose*"
    "*macroexpand-hook*"
    "*modules*"
    "*package*"
    "*print-array*"
    "*print-base*"
    "*print-case*"
    "*print-circle*"
    "*print-escape*"
    "*print-gensym*"
    "*print-length*"
    "*print-level*"
    "*print-lines*"
    "*print-miser-width*"
    "*print-pprint-dispatch*"
    "*print-pretty*"
    "*print-radix*"
    "*print-readably*"
    "*print-right-margin*"
    "*query-io*"
    "*random-state*"
    "*read-base*"
    "*read-default-float-format*"
    "*read-eval*"
    "*read-suppress*"
    "*readtable*"
    "*standard-input*"
    "*standard-output*"
    "*terminal-io*"
    "*trace-output*"))

(defvar *keywords*
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (kw *keywords-list*)
      (setf (gethash kw ht) t))
    ht))
