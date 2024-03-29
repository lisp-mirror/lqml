(in-package :editor)

;;; list of symbols starting with either ':' (including loop keywords) or '&',
;;; and global variables (for auto completion)

(defvar *keywords-list*
  '(":abort"
    ":above"
    ":absolute"
    ":accessor"
    ":across"
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
    ":appending"
    ":apropos"
    ":arg"
    ":args"
    ":argument-precedence-order"
    ":arguments"
    ":around"
    ":array"
    ":as"
    ":atsignp"
    ":back"
    ":backtrace"
    ":backward-search"
    ":base"
    ":bds"
    ":before"
    ":being"
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
    ":c-file"
    ":c/c++"
    ":call"
    ":callback"
    ":capitalize"
    ":case"
    ":catch"
    ":cdecl"
    ":cdr-1"
    ":cdr-14"
    ":cdr-5"
    ":cdr-7"
    ":cf"
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
    ":colinc"
    ":collect"
    ":collecting"
    ":colnum"
    ":colonp"
    ":commands"
    ":common"
    ":common-lisp"
    ":compile-file"
    ":compile-toplevel"
    ":compiler"
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
    ":counting"
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
    ":direct-slots"
    ":direct-superclasses"
    ":direction"
    ":directory"
    ":disable"
    ":disassemble"
    ":displaced-index-offset"
    ":displaced-to"
    ":display"
    ":dll"
    ":dlopen"
    ":do"
    ":doc"
    ":documentation"
    ":doing"
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
    ":external-symbol"
    ":external-symbols"
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
    ":for"
    ":for-keywords"
    ":force-load"
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
    ":h-file"
    ":hash-key"
    ":hash-keys"
    ":hash-value"
    ":hash-values"
    ":help"
    ":help-stack"
    ":her"
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
    ":if"
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
    ":initial-offset"
    ":initial-p"
    ":initial-value"
    ":initially"
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
    ":into"
    ":invert"
    ":io"
    ":iso-8859-1"
    ":it"
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
    ":line-buffered"
    ":line-relative"
    ":linear"
    ":lines"
    ":link"
    ":linux"
    ":lisp-files"
    ":list-all"
    ":literal"
    ":little-endian"
    ":load"
    ":load-toplevel"
    ":loadrc"
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
    ":maximize"
    ":maximizing"
    ":message"
    ":metaclass"
    ":method"
    ":method-class"
    ":method-combination"
    ":method-from-defgeneric-p"
    ":minimize"
    ":minimizing"
    ":miser"
    ":miser-width"
    ":mode"
    ":module"
    ":most-specific-first"
    ":most-specific-last"
    ":name"
    ":named"
    ":names"
    ":nconc"
    ":nconcing"
    ":never"
    ":new-version"
    ":newest"
    ":newline"
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
    ":of-type"
    ":offset"
    ":on"
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
    ":prefix-length"
    ":prefixed-api"
    ":preposition-groups"
    ":present-symbol"
    ":present-symbols"
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
    ":read-only"
    ":read-only-p"
    ":readably"
    ":reader"
    ":readers"
    ":recurse"
    ":recursive"
    ":rehash-size"
    ":rehash-threshold"
    ":relative"
    ":relative-package-names"
    ":relativep"
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
    ":return"
    ":returning"
    ":right-margin"
    ":rules"
    ":running"
    ":sealedp"
    ":search-list"
    ":section"
    ":section-column"
    ":section-end"
    ":section-relative"
    ":section-start-line"
    ":sectionp"
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
    ":start-column"
    ":start1"
    ":start2"
    ":status"
    ":stdcall"
    ":step"
    ":stop"
    ":stopped"
    ":stream"
    ":string"
    ":struct"
    ":suffix"
    ":suffix-length"
    ":sum"
    ":summing"
    ":supersede"
    ":supporting-libraries"
    ":switch"
    ":symbol"
    ":symbol-types"
    ":symbols"
    ":system-library"
    ":system-p"
    ":sysv"
    ":tables"
    ":tag"
    ":target"
    ":temp-variable"
    ":temporary-pathname"
    ":tempvars"
    ":test"
    ":test-function"
    ":test-not"
    ":the"
    ":then"
    ":thereis"
    ":threads"
    ":to"
    ":tr"
    ":trace"
    ":type"
    ":type-keywords"
    ":type-symbols"
    ":types"
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
    ":unless"
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
    ":using"
    ":utf-8"
    ":value"
    ":variables"
    ":verbose"
    ":version"
    ":void"
    ":wait"
    ":waiting"
    ":walk-form"
    ":walk-function"
    ":walker"
    ":weakness"
    ":when"
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
