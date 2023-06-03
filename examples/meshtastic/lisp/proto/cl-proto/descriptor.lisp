;;; descriptor.proto.lisp
;;;
;;; Generated by the protocol buffer compiler. DO NOT EDIT!

(cl:in-package #:common-lisp-user)

#+sbcl
(cl:progn
 (cl:eval-when (:compile-toplevel) (sb-ext:restrict-compiler-policy 'cl:debug 0 1))
 (cl:declaim (cl:optimize (sb-c:store-coverage-data 0))))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:unless (cl:find-package "CL-PROTOBUFS.GOOGLE.PROTOBUF")
    (cl:defpackage "CL-PROTOBUFS.GOOGLE.PROTOBUF" (:use)
                   (:local-nicknames (#:pi #:cl-protobufs.implementation)))))

(cl:in-package "CL-PROTOBUFS.GOOGLE.PROTOBUF")

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
(pi:define-schema 'descriptor
    :package "google.protobuf")
)


;;; Top-Level messages

(pi:define-message file-descriptor-set
    ()
  ;; Fields
  (file
   :index 1 :type file-descriptor-proto :kind :message :label (:repeated :list) :json-name "file"))

(pi:define-message file-descriptor-proto
    ()
  ;; Fields
  (name
   :index 1 :type cl:string :kind :scalar :label (:optional) :json-name "name")
  (package
   :index 2 :type cl:string :kind :scalar :label (:optional) :json-name "package")
  (dependency
   :index 3 :type cl:string :kind :scalar :label (:repeated :list) :json-name "dependency")
  (public-dependency
   :index 10 :type cl-protobufs:int32 :kind :scalar :label (:repeated :list) :json-name "publicDependency")
  (weak-dependency
   :index 11 :type cl-protobufs:int32 :kind :scalar :label (:repeated :list) :json-name "weakDependency")
  (message-type
   :index 4 :type descriptor-proto :kind :message :label (:repeated :list) :json-name "messageType")
  (enum-type
   :index 5 :type enum-descriptor-proto :kind :message :label (:repeated :list) :json-name "enumType")
  (service
   :index 6 :type service-descriptor-proto :kind :message :label (:repeated :list) :json-name "service")
  (extension
   :index 7 :type field-descriptor-proto :kind :message :label (:repeated :list) :json-name "extension")
  (options
   :index 8 :type file-options :kind :message :label (:optional) :json-name "options")
  (source-code-info
   :index 9 :type source-code-info :kind :message :label (:optional) :json-name "sourceCodeInfo")
  (syntax
   :index 12 :type cl:string :kind :scalar :label (:optional) :json-name "syntax"))

(pi:define-message descriptor-proto
    ()
  ;; Nested messages

  (pi:define-message descriptor-proto.extension-range
      ()
    ;; Fields
    (start
     :index 1 :type cl-protobufs:int32 :kind :scalar :label (:optional) :json-name "start")
    (end
     :index 2 :type cl-protobufs:int32 :kind :scalar :label (:optional) :json-name "end")
    (options
     :index 3 :type extension-range-options :kind :message :label (:optional) :json-name "options"))

  (pi:define-message descriptor-proto.reserved-range
      ()
    ;; Fields
    (start
     :index 1 :type cl-protobufs:int32 :kind :scalar :label (:optional) :json-name "start")
    (end
     :index 2 :type cl-protobufs:int32 :kind :scalar :label (:optional) :json-name "end"))
  ;; Fields
  (name
   :index 1 :type cl:string :kind :scalar :label (:optional) :json-name "name")
  (field
   :index 2 :type field-descriptor-proto :kind :message :label (:repeated :list) :json-name "field")
  (extension
   :index 6 :type field-descriptor-proto :kind :message :label (:repeated :list) :json-name "extension")
  (nested-type
   :index 3 :type descriptor-proto :kind :message :label (:repeated :list) :json-name "nestedType")
  (enum-type
   :index 4 :type enum-descriptor-proto :kind :message :label (:repeated :list) :json-name "enumType")
  (extension-range
   :index 5 :type descriptor-proto.extension-range :kind :message :label (:repeated :list) :json-name "extensionRange")
  (oneof-decl
   :index 8 :type oneof-descriptor-proto :kind :message :label (:repeated :list) :json-name "oneofDecl")
  (options
   :index 7 :type message-options :kind :message :label (:optional) :json-name "options")
  (reserved-range
   :index 9 :type descriptor-proto.reserved-range :kind :message :label (:repeated :list) :json-name "reservedRange")
  (reserved-name
   :index 10 :type cl:string :kind :scalar :label (:repeated :list) :json-name "reservedName"))

(pi:define-message extension-range-options
    ()
  ;; Fields
  (uninterpreted-option
   :index 999 :type uninterpreted-option :kind :message :label (:repeated :list) :json-name "uninterpretedOption")
  ;; Extension ranges
  (pi:define-extension 1000 536870911))

(pi:define-message field-descriptor-proto
    ()
  ;; Nested enums

  (pi:define-enum field-descriptor-proto.type
      ()
    (:type-double :index 1)
    (:type-float :index 2)
    (:type-int64 :index 3)
    (:type-uint64 :index 4)
    (:type-int32 :index 5)
    (:type-fixed64 :index 6)
    (:type-fixed32 :index 7)
    (:type-bool :index 8)
    (:type-string :index 9)
    (:type-group :index 10)
    (:type-message :index 11)
    (:type-bytes :index 12)
    (:type-uint32 :index 13)
    (:type-enum :index 14)
    (:type-sfixed32 :index 15)
    (:type-sfixed64 :index 16)
    (:type-sint32 :index 17)
    (:type-sint64 :index 18))

  (pi:define-enum field-descriptor-proto.label
      ()
    (:label-optional :index 1)
    (:label-required :index 2)
    (:label-repeated :index 3))
  ;; Fields
  (name
   :index 1 :type cl:string :kind :scalar :label (:optional) :json-name "name")
  (number
   :index 3 :type cl-protobufs:int32 :kind :scalar :label (:optional) :json-name "number")
  (label
   :index 4 :type field-descriptor-proto.label :kind :enum :label (:optional) :json-name "label" :default :label-optional)
  (type
   :index 5 :type field-descriptor-proto.type :kind :enum :label (:optional) :json-name "type" :default :type-double)
  (type-name
   :index 6 :type cl:string :kind :scalar :label (:optional) :json-name "typeName")
  (extendee
   :index 2 :type cl:string :kind :scalar :label (:optional) :json-name "extendee")
  (default-value
   :index 7 :type cl:string :kind :scalar :label (:optional) :json-name "defaultValue")
  (oneof-index
   :index 9 :type cl-protobufs:int32 :kind :scalar :label (:optional) :json-name "oneofIndex")
  (json-name
   :index 10 :type cl:string :kind :scalar :label (:optional) :json-name "jsonName")
  (options
   :index 8 :type field-options :kind :message :label (:optional) :json-name "options")
  (proto3-optional
   :index 17 :type cl:boolean :kind :scalar :label (:optional) :json-name "proto3Optional"))

(pi:define-message oneof-descriptor-proto
    ()
  ;; Fields
  (name
   :index 1 :type cl:string :kind :scalar :label (:optional) :json-name "name")
  (options
   :index 2 :type oneof-options :kind :message :label (:optional) :json-name "options"))

(pi:define-message enum-descriptor-proto
    ()
  ;; Nested messages

  (pi:define-message enum-descriptor-proto.enum-reserved-range
      ()
    ;; Fields
    (start
     :index 1 :type cl-protobufs:int32 :kind :scalar :label (:optional) :json-name "start")
    (end
     :index 2 :type cl-protobufs:int32 :kind :scalar :label (:optional) :json-name "end"))
  ;; Fields
  (name
   :index 1 :type cl:string :kind :scalar :label (:optional) :json-name "name")
  (value
   :index 2 :type enum-value-descriptor-proto :kind :message :label (:repeated :list) :json-name "value")
  (options
   :index 3 :type enum-options :kind :message :label (:optional) :json-name "options")
  (reserved-range
   :index 4 :type enum-descriptor-proto.enum-reserved-range :kind :message :label (:repeated :list) :json-name "reservedRange")
  (reserved-name
   :index 5 :type cl:string :kind :scalar :label (:repeated :list) :json-name "reservedName"))

(pi:define-message enum-value-descriptor-proto
    ()
  ;; Fields
  (name
   :index 1 :type cl:string :kind :scalar :label (:optional) :json-name "name")
  (number
   :index 2 :type cl-protobufs:int32 :kind :scalar :label (:optional) :json-name "number")
  (options
   :index 3 :type enum-value-options :kind :message :label (:optional) :json-name "options"))

(pi:define-message service-descriptor-proto
    ()
  ;; Fields
  (name
   :index 1 :type cl:string :kind :scalar :label (:optional) :json-name "name")
  (method
   :index 2 :type method-descriptor-proto :kind :message :label (:repeated :list) :json-name "method")
  (options
   :index 3 :type service-options :kind :message :label (:optional) :json-name "options"))

(pi:define-message method-descriptor-proto
    ()
  ;; Fields
  (name
   :index 1 :type cl:string :kind :scalar :label (:optional) :json-name "name")
  (input-type
   :index 2 :type cl:string :kind :scalar :label (:optional) :json-name "inputType")
  (output-type
   :index 3 :type cl:string :kind :scalar :label (:optional) :json-name "outputType")
  (options
   :index 4 :type method-options :kind :message :label (:optional) :json-name "options")
  (client-streaming
   :index 5 :type cl:boolean :kind :scalar :label (:optional) :json-name "clientStreaming" :default cl:nil)
  (server-streaming
   :index 6 :type cl:boolean :kind :scalar :label (:optional) :json-name "serverStreaming" :default cl:nil))

(pi:define-message file-options
    ()
  ;; Nested enums

  (pi:define-enum file-options.optimize-mode
      ()
    (:speed :index 1)
    (:code-size :index 2)
    (:lite-runtime :index 3))
  ;; Fields
  (java-package
   :index 1 :type cl:string :kind :scalar :label (:optional) :json-name "javaPackage")
  (java-outer-classname
   :index 8 :type cl:string :kind :scalar :label (:optional) :json-name "javaOuterClassname")
  (java-multiple-files
   :index 10 :type cl:boolean :kind :scalar :label (:optional) :json-name "javaMultipleFiles" :default cl:nil)
  (java-generate-equals-and-hash
   :index 20 :type cl:boolean :kind :scalar :label (:optional) :json-name "javaGenerateEqualsAndHash")
  (java-string-check-utf8
   :index 27 :type cl:boolean :kind :scalar :label (:optional) :json-name "javaStringCheckUtf8" :default cl:nil)
  (optimize-for
   :index 9 :type file-options.optimize-mode :kind :enum :label (:optional) :json-name "optimizeFor" :default :speed)
  (go-package
   :index 11 :type cl:string :kind :scalar :label (:optional) :json-name "goPackage")
  (cc-generic-services
   :index 16 :type cl:boolean :kind :scalar :label (:optional) :json-name "ccGenericServices" :default cl:nil)
  (java-generic-services
   :index 17 :type cl:boolean :kind :scalar :label (:optional) :json-name "javaGenericServices" :default cl:nil)
  (py-generic-services
   :index 18 :type cl:boolean :kind :scalar :label (:optional) :json-name "pyGenericServices" :default cl:nil)
  (php-generic-services
   :index 42 :type cl:boolean :kind :scalar :label (:optional) :json-name "phpGenericServices" :default cl:nil)
  (deprecated
   :index 23 :type cl:boolean :kind :scalar :label (:optional) :json-name "deprecated" :default cl:nil)
  (cc-enable-arenas
   :index 31 :type cl:boolean :kind :scalar :label (:optional) :json-name "ccEnableArenas" :default cl:t)
  (objc-class-prefix
   :index 36 :type cl:string :kind :scalar :label (:optional) :json-name "objcClassPrefix")
  (csharp-namespace
   :index 37 :type cl:string :kind :scalar :label (:optional) :json-name "csharpNamespace")
  (swift-prefix
   :index 39 :type cl:string :kind :scalar :label (:optional) :json-name "swiftPrefix")
  (php-class-prefix
   :index 40 :type cl:string :kind :scalar :label (:optional) :json-name "phpClassPrefix")
  (php-namespace
   :index 41 :type cl:string :kind :scalar :label (:optional) :json-name "phpNamespace")
  (php-metadata-namespace
   :index 44 :type cl:string :kind :scalar :label (:optional) :json-name "phpMetadataNamespace")
  (ruby-package
   :index 45 :type cl:string :kind :scalar :label (:optional) :json-name "rubyPackage")
  (uninterpreted-option
   :index 999 :type uninterpreted-option :kind :message :label (:repeated :list) :json-name "uninterpretedOption")
  ;; Extension ranges
  (pi:define-extension 1000 536870911))

(pi:define-message message-options
    ()
  ;; Fields
  (message-set-wire-format
   :index 1 :type cl:boolean :kind :scalar :label (:optional) :json-name "messageSetWireFormat" :default cl:nil)
  (no-standard-descriptor-accessor
   :index 2 :type cl:boolean :kind :scalar :label (:optional) :json-name "noStandardDescriptorAccessor" :default cl:nil)
  (deprecated
   :index 3 :type cl:boolean :kind :scalar :label (:optional) :json-name "deprecated" :default cl:nil)
  (map-entry
   :index 7 :type cl:boolean :kind :scalar :label (:optional) :json-name "mapEntry")
  (uninterpreted-option
   :index 999 :type uninterpreted-option :kind :message :label (:repeated :list) :json-name "uninterpretedOption")
  ;; Extension ranges
  (pi:define-extension 1000 536870911))

(pi:define-message field-options
    ()
  ;; Nested enums

  (pi:define-enum field-options.c-type
      ()
    (:string :index 0)
    (:cord :index 1)
    (:string-piece :index 2))

  (pi:define-enum field-options.js-type
      (:name "JSType")
    (:js-normal :index 0)
    (:js-string :index 1)
    (:js-number :index 2))
  ;; Fields
  (ctype
   :index 1 :type field-options.c-type :kind :enum :label (:optional) :json-name "ctype" :default :string)
  (packed
   :index 2 :type cl:boolean :kind :scalar :label (:optional) :json-name "packed")
  (jstype
   :index 6 :type field-options.js-type :kind :enum :label (:optional) :json-name "jstype" :default :js-normal)
  (lazy
   :index 5 :type cl:boolean :kind :scalar :label (:optional) :json-name "lazy" :default cl:nil)
  (deprecated
   :index 3 :type cl:boolean :kind :scalar :label (:optional) :json-name "deprecated" :default cl:nil)
  (weak
   :index 10 :type cl:boolean :kind :scalar :label (:optional) :json-name "weak" :default cl:nil)
  (uninterpreted-option
   :index 999 :type uninterpreted-option :kind :message :label (:repeated :list) :json-name "uninterpretedOption")
  ;; Extension ranges
  (pi:define-extension 1000 536870911))

(pi:define-message oneof-options
    ()
  ;; Fields
  (uninterpreted-option
   :index 999 :type uninterpreted-option :kind :message :label (:repeated :list) :json-name "uninterpretedOption")
  ;; Extension ranges
  (pi:define-extension 1000 536870911))

(pi:define-message enum-options
    ()
  ;; Fields
  (allow-alias
   :index 2 :type cl:boolean :kind :scalar :label (:optional) :json-name "allowAlias")
  (deprecated
   :index 3 :type cl:boolean :kind :scalar :label (:optional) :json-name "deprecated" :default cl:nil)
  (uninterpreted-option
   :index 999 :type uninterpreted-option :kind :message :label (:repeated :list) :json-name "uninterpretedOption")
  ;; Extension ranges
  (pi:define-extension 1000 536870911))

(pi:define-message enum-value-options
    ()
  ;; Fields
  (deprecated
   :index 1 :type cl:boolean :kind :scalar :label (:optional) :json-name "deprecated" :default cl:nil)
  (uninterpreted-option
   :index 999 :type uninterpreted-option :kind :message :label (:repeated :list) :json-name "uninterpretedOption")
  ;; Extension ranges
  (pi:define-extension 1000 536870911))

(pi:define-message service-options
    ()
  ;; Fields
  (deprecated
   :index 33 :type cl:boolean :kind :scalar :label (:optional) :json-name "deprecated" :default cl:nil)
  (uninterpreted-option
   :index 999 :type uninterpreted-option :kind :message :label (:repeated :list) :json-name "uninterpretedOption")
  ;; Extension ranges
  (pi:define-extension 1000 536870911))

(pi:define-message method-options
    ()
  ;; Nested enums

  (pi:define-enum method-options.idempotency-level
      ()
    (:idempotency-unknown :index 0)
    (:no-side-effects :index 1)
    (:idempotent :index 2))
  ;; Fields
  (deprecated
   :index 33 :type cl:boolean :kind :scalar :label (:optional) :json-name "deprecated" :default cl:nil)
  (idempotency-level
   :index 34 :type method-options.idempotency-level :kind :enum :label (:optional) :json-name "idempotencyLevel" :default :idempotency-unknown)
  (uninterpreted-option
   :index 999 :type uninterpreted-option :kind :message :label (:repeated :list) :json-name "uninterpretedOption")
  ;; Extension ranges
  (pi:define-extension 1000 536870911))

(pi:define-message uninterpreted-option
    ()
  ;; Nested messages

  (pi:define-message uninterpreted-option.name-part
      ()
    ;; Fields
    (name-part
     :index 1 :type cl:string :kind :scalar :label (:required) :json-name "namePart")
    (is-extension
     :index 2 :type cl:boolean :kind :scalar :label (:required) :json-name "isExtension"))
  ;; Fields
  (name
   :index 2 :type uninterpreted-option.name-part :kind :message :label (:repeated :list) :json-name "name")
  (identifier-value
   :index 3 :type cl:string :kind :scalar :label (:optional) :json-name "identifierValue")
  (positive-int-value
   :index 4 :type cl-protobufs:uint64 :kind :scalar :label (:optional) :json-name "positiveIntValue")
  (negative-int-value
   :index 5 :type cl-protobufs:int64 :kind :scalar :label (:optional) :json-name "negativeIntValue")
  (double-value
   :index 6 :type cl:double-float :kind :scalar :label (:optional) :json-name "doubleValue")
  (string-value
   :index 7 :type cl-protobufs:byte-vector :kind :scalar :label (:optional) :json-name "stringValue")
  (aggregate-value
   :index 8 :type cl:string :kind :scalar :label (:optional) :json-name "aggregateValue"))

(pi:define-message source-code-info
    ()
  ;; Nested messages

  (pi:define-message source-code-info.location
      ()
    ;; Fields
    (path
     :index 1 :type cl-protobufs:int32 :kind :scalar :label (:repeated :list) :json-name "path" :packed cl:t)
    (span
     :index 2 :type cl-protobufs:int32 :kind :scalar :label (:repeated :list) :json-name "span" :packed cl:t)
    (leading-comments
     :index 3 :type cl:string :kind :scalar :label (:optional) :json-name "leadingComments")
    (trailing-comments
     :index 4 :type cl:string :kind :scalar :label (:optional) :json-name "trailingComments")
    (leading-detached-comments
     :index 6 :type cl:string :kind :scalar :label (:repeated :list) :json-name "leadingDetachedComments"))
  ;; Fields
  (location
   :index 1 :type source-code-info.location :kind :message :label (:repeated :list) :json-name "location"))

(pi:define-message generated-code-info
    ()
  ;; Nested messages

  (pi:define-message generated-code-info.annotation
      ()
    ;; Fields
    (path
     :index 1 :type cl-protobufs:int32 :kind :scalar :label (:repeated :list) :json-name "path" :packed cl:t)
    (source-file
     :index 2 :type cl:string :kind :scalar :label (:optional) :json-name "sourceFile")
    (begin
     :index 3 :type cl-protobufs:int32 :kind :scalar :label (:optional) :json-name "begin")
    (end
     :index 4 :type cl-protobufs:int32 :kind :scalar :label (:optional) :json-name "end"))
  ;; Fields
  (annotation
   :index 1 :type generated-code-info.annotation :kind :message :label (:repeated :list) :json-name "annotation"))


(cl:export '(aggregate-value
             allow-alias
             annotation
             begin
             cc-enable-arenas
             cc-generic-services
             client-streaming
             csharp-namespace
             ctype
             default-value
             dependency
             deprecated
             descriptor
             descriptor-proto
             descriptor-proto.extension-range
             descriptor-proto.reserved-range
             double-value
             end
             enum-descriptor-proto
             enum-descriptor-proto.enum-reserved-range
             enum-options
             enum-type
             enum-value-descriptor-proto
             enum-value-options
             extendee
             extension
             extension-range
             extension-range-options
             field
             field-descriptor-proto
             field-descriptor-proto.label
             field-descriptor-proto.label-int-to-keyword
             field-descriptor-proto.label-keyword-to-int
             field-descriptor-proto.type
             field-descriptor-proto.type-int-to-keyword
             field-descriptor-proto.type-keyword-to-int
             field-options
             field-options.c-type
             field-options.c-type-int-to-keyword
             field-options.c-type-keyword-to-int
             field-options.js-type
             field-options.js-type-int-to-keyword
             field-options.js-type-keyword-to-int
             file
             file-descriptor-proto
             file-descriptor-set
             file-options
             file-options.optimize-mode
             file-options.optimize-mode-int-to-keyword
             file-options.optimize-mode-keyword-to-int
             generated-code-info
             generated-code-info.annotation
             go-package
             idempotency-level
             identifier-value
             input-type
             is-extension
             java-generate-equals-and-hash
             java-generic-services
             java-multiple-files
             java-outer-classname
             java-package
             java-string-check-utf8
             json-name
             jstype
             label
             lazy
             leading-comments
             leading-detached-comments
             location
             map-entry
             message-options
             message-set-wire-format
             message-type
             method
             method-descriptor-proto
             method-options
             method-options.idempotency-level
             method-options.idempotency-level-int-to-keyword
             method-options.idempotency-level-keyword-to-int
             name
             name-part
             negative-int-value
             nested-type
             no-standard-descriptor-accessor
             number
             objc-class-prefix
             oneof-decl
             oneof-descriptor-proto
             oneof-index
             oneof-options
             optimize-for
             options
             output-type
             package
             packed
             path
             php-class-prefix
             php-generic-services
             php-metadata-namespace
             php-namespace
             positive-int-value
             proto3-optional
             public-dependency
             py-generic-services
             reserved-name
             reserved-range
             ruby-package
             server-streaming
             service
             service-descriptor-proto
             service-options
             source-code-info
             source-code-info.location
             source-file
             span
             start
             string-value
             swift-prefix
             syntax
             trailing-comments
             type
             type-name
             uninterpreted-option
             uninterpreted-option.name-part
             value
             weak
             weak-dependency))
