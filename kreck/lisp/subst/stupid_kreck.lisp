SBCL 2.2.1  Port: 4005  Pid: 544
; SWANK 2.26
CL-USER> (defparameter dbg nil)

(defmacro fn (args &body body) 
  `(lambda ,args ,@body)) ...
; in: DEFMACRO TEST
;     (SB-INT:NAMED-DS-BIND (:MACRO TEST . DEFMACRO)
;         (NAME &BODY EXPRS)
;         (CDR #:EXPR)
;       (DECLARE (SB-C::CONSTANT-VALUE NAME EXPRS))
;       (BLOCK TEST `(AND ,@(MAPCAR #'TEST-1 EXPRS))))
; ==>
;   (LET* ((#:G0
;           (SB-C::CHECK-DS-LIST/&REST (CDR #:EXPR) 1 1 '(# NAME &BODY EXPRS)))
;          (NAME (POP #:G0))
;          (EXPRS #:G0))
;     (DECLARE (SB-C::CONSTANT-VALUE NAME EXPRS))
;     (BLOCK TEST `(AND ,@(MAPCAR #'TEST-1 EXPRS))))
; 
; caught STYLE-WARNING:
;   The variable NAME is defined but never used.
; 
; compilation unit finished
;   caught 1 STYLE-WARNING condition
; in: DEFUN SUBJ-OP
;     (SB-INT:NAMED-LAMBDA SUBJ-OP
;         (SUBJ ARGS)
;       (BLOCK SUBJ-OP SUBJ))
; 
; caught STYLE-WARNING:
;   The variable ARGS is defined but never used.
; 
; compilation unit finished
;   caught 1 STYLE-WARNING condition
; in: DEFUN QUOT-OP
;     (SB-INT:NAMED-LAMBDA QUOT-OP
;         (SUBJ ARGS)
;       (BLOCK QUOT-OP (CAR ARGS)))
; 
; caught STYLE-WARNING:
;   The variable SUBJ is defined but never used.
; 
; compilation unit finished
;   caught 1 STYLE-WARNING condition
; in: DEFUN DEBUG-OP
;     (UNPARSE ARGS)
; 
; caught STYLE-WARNING:
;   undefined function: COMMON-LISP-USER::UNPARSE
; 
; compilation unit finished
;   Undefined function:
;     UNPARSE
;   caught 1 STYLE-WARNING condition
; in: DEFUN KRECK
;     (CONS NIL *DEFS*)
; 
; caught WARNING:
;   undefined variable: COMMON-LISP-USER::*DEFS*
; 
; compilation unit finished
;   Undefined variable:
;     *DEFS*
;   caught 1 WARNING condition
(((#<FUNCTION DEFGET-OP> "*") ((#<FUNCTION DEFGET-OP> "$"))
  (#<FUNCTION DEFGET-OP> "this"))
 (((#<FUNCTION DEFGET-OP> "c") (#<FUNCTION DEFGET-OP> "arg1")
   (#<FUNCTION DEFGET-OP> "arg2")))
 ((1 2 3) NIL ("~") ("$" . #<FUNCTION SUBJ-OP>) ("q" . #<FUNCTION QUOT-OP>)
  ("<" . #<FUNCTION CAR-OP>) (">" . #<FUNCTION CDR-OP>)
  ("c" . #<FUNCTION CONS-OP>) ("*" . #<FUNCTION EVAL-OP>)
  ("?" . #<FUNCTION IF-OP>) ("@" . #<FUNCTION DEFGET-OP>)
  ("!d" . #<FUNCTION DEBUG-OP>)
  ("args" (#<FUNCTION DEFGET-OP> "<") ((#<FUNCTION DEFGET-OP> "$")))
  ("arg1" (#<FUNCTION DEFGET-OP> "<") (#<FUNCTION DEFGET-OP> "args"))
  ("arg2" (#<FUNCTION DEFGET-OP> "<")
   ((#<FUNCTION DEFGET-OP> ">") (#<FUNCTION DEFGET-OP> "args")))
  ("arg3" (#<FUNCTION DEFGET-OP> "<")
   ((#<FUNCTION DEFGET-OP> ">")
    ((#<FUNCTION DEFGET-OP> ">") (#<FUNCTION DEFGET-OP> "args"))))
  ("ctx" (#<FUNCTION DEFGET-OP> ">") ((#<FUNCTION DEFGET-OP> "$")))
  ("locs" (#<FUNCTION DEFGET-OP> "<")
   ((#<FUNCTION DEFGET-OP> ">") ((#<FUNCTION DEFGET-OP> "$"))))
  ("defs" (#<FUNCTION DEFGET-OP> ">")
   ((#<FUNCTION DEFGET-OP> ">") ((#<FUNCTION DEFGET-OP> "$"))))
  ("this" (#<FUNCTION DEFGET-OP> "<") (#<FUNCTION DEFGET-OP> "locs"))
  ("calr" (#<FUNCTION DEFGET-OP> "<")
   ((#<FUNCTION DEFGET-OP> ">") (#<FUNCTION DEFGET-OP> "locs")))
  ("non-rec-fexpr-ctx" (#<FUNCTION DEFGET-OP> "c")
   ((#<FUNCTION DEFGET-OP> "c") (#<FUNCTION DEFGET-OP> "~")
    ((#<FUNCTION DEFGET-OP> "c") ((#<FUNCTION DEFGET-OP> "$"))
     (#<FUNCTION DEFGET-OP> "~")))
   (#<FUNCTION DEFGET-OP> "defs"))
  ("*^" (#<FUNCTION DEFGET-OP> "c")
   ((#<FUNCTION DEFGET-OP> "q")
    ((#<FUNCTION DEFGET-OP> "*")
     ((#<FUNCTION DEFGET-OP> "*") (#<FUNCTION DEFGET-OP> "calr")
      ((#<FUNCTION DEFGET-OP> "q") (#<FUNCTION DEFGET-OP> "calr")))
     ((#<FUNCTION DEFGET-OP> "*") (#<FUNCTION DEFGET-OP> "calr")
      (#<FUNCTION DEFGET-OP> "arg1"))))
   (#<FUNCTION DEFGET-OP> "non-rec-fexpr-ctx"))
  ("w" (#<FUNCTION DEFGET-OP> "c")
   ((#<FUNCTION DEFGET-OP> "q")
    ((#<FUNCTION DEFGET-OP> "c")
     ((#<FUNCTION DEFGET-OP> "*^") (#<FUNCTION DEFGET-OP> "arg1"))
     (#<FUNCTION DEFGET-OP> "~")))
   (#<FUNCTION DEFGET-OP> "non-rec-fexpr-ctx"))
  ("l" (#<FUNCTION DEFGET-OP> "c")
   ((#<FUNCTION DEFGET-OP> "q")
    ((#<FUNCTION DEFGET-OP> "*") ((#<FUNCTION DEFGET-OP> "$"))
     (#<FUNCTION DEFGET-OP> "this")))
   ((#<FUNCTION DEFGET-OP> "c")
    ((#<FUNCTION DEFGET-OP> "c")
     ((#<FUNCTION DEFGET-OP> "q")
      ((#<FUNCTION DEFGET-OP> "?") (#<FUNCTION DEFGET-OP> "args")
       ((#<FUNCTION DEFGET-OP> "c")
        ((#<FUNCTION DEFGET-OP> "*") (#<FUNCTION DEFGET-OP> "calr")
         (#<FUNCTION DEFGET-OP> "arg1"))
        ((#<FUNCTION DEFGET-OP> "*")
         ((#<FUNCTION DEFGET-OP> "c")
          ((#<FUNCTION DEFGET-OP> ">") (#<FUNCTION DEFGET-OP> "args"))
          (#<FUNCTION DEFGET-OP> "ctx"))
         (#<FUNCTION DEFGET-OP> "this")))))
     ((#<FUNCTION DEFGET-OP> "c") ((#<FUNCTION DEFGET-OP> "$"))
      (#<FUNCTION DEFGET-OP> "~")))
    (#<FUNCTION DEFGET-OP> "defs")))
  ("l" (#<FUNCTION DEFGET-OP> "c") ((#<FUNCTION DEFGET-OP> "q") NIL)
   ((#<FUNCTION DEFGET-OP> "c") (#<FUNCTION DEFGET-OP> "~")
    ((#<FUNCTION DEFGET-OP> "c") ((#<FUNCTION DEFGET-OP> "$"))
     (#<FUNCTION DEFGET-OP> "~"))))
  ("fexpr-gate" (#<FUNCTION DEFGET-OP> "c")
   ((#<FUNCTION DEFGET-OP> "q")
    ((#<FUNCTION DEFGET-OP> "c")
     ((#<FUNCTION DEFGET-OP> "q")
      ((#<FUNCTION DEFGET-OP> "*") ((#<FUNCTION DEFGET-OP> "$"))
       (#<FUNCTION DEFGET-OP> "this")))
     ((#<FUNCTION DEFGET-OP> "c")
      ((#<FUNCTION DEFGET-OP> "c") (#<FUNCTION DEFGET-OP> "arg1"))
      ((#<FUNCTION DEFGET-OP> "c")
       ((#<FUNCTION DEFGET-OP> "*") (#<FUNCTION DEFGET-OP> "calr")
        ((#<FUNCTION DEFGET-OP> "q") ((#<FUNCTION DEFGET-OP> "$"))))
       (#<FUNCTION DEFGET-OP> "~"))
      (#<FUNCTION DEFGET-OP> "defs"))))
   (#<FUNCTION DEFGET-OP> "non-rec-fexpr-ctx"))))
CL-USER> (kreck '(1 2 3) '(l (> args) 1))
((2 3) 1)
CL-USER> (kreck '(1 2 3) '(fexpr-gate (c arg1 arg2)))
(((#<FUNCTION DEFGET-OP> "*") ((#<FUNCTION DEFGET-OP> "$"))
  (#<FUNCTION DEFGET-OP> "this"))
 (((#<FUNCTION DEFGET-OP> "c") (#<FUNCTION DEFGET-OP> "arg1")
   (#<FUNCTION DEFGET-OP> "arg2")))
 ((1 2 3) NIL ("~") ("$" . #<FUNCTION SUBJ-OP>) ("q" . #<FUNCTION QUOT-OP>)
  ("<" . #<FUNCTION CAR-OP>) (">" . #<FUNCTION CDR-OP>)
  ("c" . #<FUNCTION CONS-OP>) ("*" . #<FUNCTION EVAL-OP>)
  ("?" . #<FUNCTION IF-OP>) ("@" . #<FUNCTION DEFGET-OP>)
  ("!d" . #<FUNCTION DEBUG-OP>)
  ("args" (#<FUNCTION DEFGET-OP> "<") ((#<FUNCTION DEFGET-OP> "$")))
  ("arg1" (#<FUNCTION DEFGET-OP> "<") (#<FUNCTION DEFGET-OP> "args"))
  ("arg2" (#<FUNCTION DEFGET-OP> "<")
   ((#<FUNCTION DEFGET-OP> ">") (#<FUNCTION DEFGET-OP> "args")))
  ("arg3" (#<FUNCTION DEFGET-OP> "<")
   ((#<FUNCTION DEFGET-OP> ">")
    ((#<FUNCTION DEFGET-OP> ">") (#<FUNCTION DEFGET-OP> "args"))))
  ("ctx" (#<FUNCTION DEFGET-OP> ">") ((#<FUNCTION DEFGET-OP> "$")))
  ("locs" (#<FUNCTION DEFGET-OP> "<")
   ((#<FUNCTION DEFGET-OP> ">") ((#<FUNCTION DEFGET-OP> "$"))))
  ("defs" (#<FUNCTION DEFGET-OP> ">")
   ((#<FUNCTION DEFGET-OP> ">") ((#<FUNCTION DEFGET-OP> "$"))))
  ("this" (#<FUNCTION DEFGET-OP> "<") (#<FUNCTION DEFGET-OP> "locs"))
  ("calr" (#<FUNCTION DEFGET-OP> "<")
   ((#<FUNCTION DEFGET-OP> ">") (#<FUNCTION DEFGET-OP> "locs")))
  ("non-rec-fexpr-ctx" (#<FUNCTION DEFGET-OP> "c")
   ((#<FUNCTION DEFGET-OP> "c") (#<FUNCTION DEFGET-OP> "~")
    ((#<FUNCTION DEFGET-OP> "c") ((#<FUNCTION DEFGET-OP> "$"))
     (#<FUNCTION DEFGET-OP> "~")))
   (#<FUNCTION DEFGET-OP> "defs"))
  ("*^" (#<FUNCTION DEFGET-OP> "c")
   ((#<FUNCTION DEFGET-OP> "q")
    ((#<FUNCTION DEFGET-OP> "*")
     ((#<FUNCTION DEFGET-OP> "*") (#<FUNCTION DEFGET-OP> "calr")
      ((#<FUNCTION DEFGET-OP> "q") (#<FUNCTION DEFGET-OP> "calr")))
     ((#<FUNCTION DEFGET-OP> "*") (#<FUNCTION DEFGET-OP> "calr")
      (#<FUNCTION DEFGET-OP> "arg1"))))
   (#<FUNCTION DEFGET-OP> "non-rec-fexpr-ctx"))
  ("w" (#<FUNCTION DEFGET-OP> "c")
   ((#<FUNCTION DEFGET-OP> "q")
    ((#<FUNCTION DEFGET-OP> "c")
     ((#<FUNCTION DEFGET-OP> "*^") (#<FUNCTION DEFGET-OP> "arg1"))
     (#<FUNCTION DEFGET-OP> "~")))
   (#<FUNCTION DEFGET-OP> "non-rec-fexpr-ctx"))
  ("l" (#<FUNCTION DEFGET-OP> "c")
   ((#<FUNCTION DEFGET-OP> "q")
    ((#<FUNCTION DEFGET-OP> "*") ((#<FUNCTION DEFGET-OP> "$"))
     (#<FUNCTION DEFGET-OP> "this")))
   ((#<FUNCTION DEFGET-OP> "c")
    ((#<FUNCTION DEFGET-OP> "c")
     ((#<FUNCTION DEFGET-OP> "q")
      ((#<FUNCTION DEFGET-OP> "?") (#<FUNCTION DEFGET-OP> "args")
       ((#<FUNCTION DEFGET-OP> "c")
        ((#<FUNCTION DEFGET-OP> "*") (#<FUNCTION DEFGET-OP> "calr")
         (#<FUNCTION DEFGET-OP> "arg1"))
        ((#<FUNCTION DEFGET-OP> "*")
         ((#<FUNCTION DEFGET-OP> "c")
          ((#<FUNCTION DEFGET-OP> ">") (#<FUNCTION DEFGET-OP> "args"))
          (#<FUNCTION DEFGET-OP> "ctx"))
         (#<FUNCTION DEFGET-OP> "this")))))
     ((#<FUNCTION DEFGET-OP> "c") ((#<FUNCTION DEFGET-OP> "$"))
      (#<FUNCTION DEFGET-OP> "~")))
    (#<FUNCTION DEFGET-OP> "defs")))
  ("l" (#<FUNCTION DEFGET-OP> "c") ((#<FUNCTION DEFGET-OP> "q") NIL)
   ((#<FUNCTION DEFGET-OP> "c") (#<FUNCTION DEFGET-OP> "~")
    ((#<FUNCTION DEFGET-OP> "c") ((#<FUNCTION DEFGET-OP> "$"))
     (#<FUNCTION DEFGET-OP> "~"))))
  ("fexpr-gate" (#<FUNCTION DEFGET-OP> "c")
   ((#<FUNCTION DEFGET-OP> "q")
    ((#<FUNCTION DEFGET-OP> "c")
     ((#<FUNCTION DEFGET-OP> "q")
      ((#<FUNCTION DEFGET-OP> "*") ((#<FUNCTION DEFGET-OP> "$"))
       (#<FUNCTION DEFGET-OP> "this")))
     ((#<FUNCTION DEFGET-OP> "c")
      ((#<FUNCTION DEFGET-OP> "c") (#<FUNCTION DEFGET-OP> "arg1"))
      ((#<FUNCTION DEFGET-OP> "c")
       ((#<FUNCTION DEFGET-OP> "*") (#<FUNCTION DEFGET-OP> "calr")
        ((#<FUNCTION DEFGET-OP> "q") ((#<FUNCTION DEFGET-OP> "$"))))
       (#<FUNCTION DEFGET-OP> "~"))
      (#<FUNCTION DEFGET-OP> "defs"))))
   (#<FUNCTION DEFGET-OP> "non-rec-fexpr-ctx"))))
CL-USER> (kreck '(1 2 3) '($))
((1 2 3) NIL ("~") ("$" . #<FUNCTION SUBJ-OP>) ("q" . #<FUNCTION QUOT-OP>)
 ("<" . #<FUNCTION CAR-OP>) (">" . #<FUNCTION CDR-OP>)
 ("c" . #<FUNCTION CONS-OP>) ("*" . #<FUNCTION EVAL-OP>)
 ("?" . #<FUNCTION IF-OP>) ("@" . #<FUNCTION DEFGET-OP>)
 ("!d" . #<FUNCTION DEBUG-OP>)
 ("args" (#<FUNCTION DEFGET-OP> "<") ((#<FUNCTION DEFGET-OP> "$")))
 ("arg1" (#<FUNCTION DEFGET-OP> "<") (#<FUNCTION DEFGET-OP> "args"))
 ("arg2" (#<FUNCTION DEFGET-OP> "<")
  ((#<FUNCTION DEFGET-OP> ">") (#<FUNCTION DEFGET-OP> "args")))
 ("arg3" (#<FUNCTION DEFGET-OP> "<")
  ((#<FUNCTION DEFGET-OP> ">")
   ((#<FUNCTION DEFGET-OP> ">") (#<FUNCTION DEFGET-OP> "args"))))
 ("ctx" (#<FUNCTION DEFGET-OP> ">") ((#<FUNCTION DEFGET-OP> "$")))
 ("locs" (#<FUNCTION DEFGET-OP> "<")
  ((#<FUNCTION DEFGET-OP> ">") ((#<FUNCTION DEFGET-OP> "$"))))
 ("defs" (#<FUNCTION DEFGET-OP> ">")
  ((#<FUNCTION DEFGET-OP> ">") ((#<FUNCTION DEFGET-OP> "$"))))
 ("this" (#<FUNCTION DEFGET-OP> "<") (#<FUNCTION DEFGET-OP> "locs"))
 ("calr" (#<FUNCTION DEFGET-OP> "<")
  ((#<FUNCTION DEFGET-OP> ">") (#<FUNCTION DEFGET-OP> "locs")))
 ("non-rec-fexpr-ctx" (#<FUNCTION DEFGET-OP> "c")
  ((#<FUNCTION DEFGET-OP> "c") (#<FUNCTION DEFGET-OP> "~")
   ((#<FUNCTION DEFGET-OP> "c") ((#<FUNCTION DEFGET-OP> "$"))
    (#<FUNCTION DEFGET-OP> "~")))
  (#<FUNCTION DEFGET-OP> "defs"))
 ("*^" (#<FUNCTION DEFGET-OP> "c")
  ((#<FUNCTION DEFGET-OP> "q")
   ((#<FUNCTION DEFGET-OP> "*")
    ((#<FUNCTION DEFGET-OP> "*") (#<FUNCTION DEFGET-OP> "calr")
     ((#<FUNCTION DEFGET-OP> "q") (#<FUNCTION DEFGET-OP> "calr")))
    ((#<FUNCTION DEFGET-OP> "*") (#<FUNCTION DEFGET-OP> "calr")
     (#<FUNCTION DEFGET-OP> "arg1"))))
  (#<FUNCTION DEFGET-OP> "non-rec-fexpr-ctx"))
 ("w" (#<FUNCTION DEFGET-OP> "c")
  ((#<FUNCTION DEFGET-OP> "q")
   ((#<FUNCTION DEFGET-OP> "c")
    ((#<FUNCTION DEFGET-OP> "*^") (#<FUNCTION DEFGET-OP> "arg1"))
    (#<FUNCTION DEFGET-OP> "~")))
  (#<FUNCTION DEFGET-OP> "non-rec-fexpr-ctx"))
 ("l" (#<FUNCTION DEFGET-OP> "c")
  ((#<FUNCTION DEFGET-OP> "q")
   ((#<FUNCTION DEFGET-OP> "*") ((#<FUNCTION DEFGET-OP> "$"))
    (#<FUNCTION DEFGET-OP> "this")))
  ((#<FUNCTION DEFGET-OP> "c")
   ((#<FUNCTION DEFGET-OP> "c")
    ((#<FUNCTION DEFGET-OP> "q")
     ((#<FUNCTION DEFGET-OP> "?") (#<FUNCTION DEFGET-OP> "args")
      ((#<FUNCTION DEFGET-OP> "c")
       ((#<FUNCTION DEFGET-OP> "*") (#<FUNCTION DEFGET-OP> "calr")
        (#<FUNCTION DEFGET-OP> "arg1"))
       ((#<FUNCTION DEFGET-OP> "*")
        ((#<FUNCTION DEFGET-OP> "c")
         ((#<FUNCTION DEFGET-OP> ">") (#<FUNCTION DEFGET-OP> "args"))
         (#<FUNCTION DEFGET-OP> "ctx"))
        (#<FUNCTION DEFGET-OP> "this")))))
    ((#<FUNCTION DEFGET-OP> "c") ((#<FUNCTION DEFGET-OP> "$"))
     (#<FUNCTION DEFGET-OP> "~")))
   (#<FUNCTION DEFGET-OP> "defs")))
 ("l" (#<FUNCTION DEFGET-OP> "c") ((#<FUNCTION DEFGET-OP> "q") NIL)
  ((#<FUNCTION DEFGET-OP> "c") (#<FUNCTION DEFGET-OP> "~")
   ((#<FUNCTION DEFGET-OP> "c") ((#<FUNCTION DEFGET-OP> "$"))
    (#<FUNCTION DEFGET-OP> "~"))))
 ("fexpr-gate" (#<FUNCTION DEFGET-OP> "c")
  ((#<FUNCTION DEFGET-OP> "q")
   ((#<FUNCTION DEFGET-OP> "c")
    ((#<FUNCTION DEFGET-OP> "q")
     ((#<FUNCTION DEFGET-OP> "*") ((#<FUNCTION DEFGET-OP> "$"))
      (#<FUNCTION DEFGET-OP> "this")))
    ((#<FUNCTION DEFGET-OP> "c")
     ((#<FUNCTION DEFGET-OP> "c") (#<FUNCTION DEFGET-OP> "arg1"))
     ((#<FUNCTION DEFGET-OP> "c")
      ((#<FUNCTION DEFGET-OP> "*") (#<FUNCTION DEFGET-OP> "calr")
       ((#<FUNCTION DEFGET-OP> "q") ((#<FUNCTION DEFGET-OP> "$"))))
      (#<FUNCTION DEFGET-OP> "~"))
     (#<FUNCTION DEFGET-OP> "defs"))))
  (#<FUNCTION DEFGET-OP> "non-rec-fexpr-ctx")))
CL-USER> (kreck '(1 2 3) '(l (> args) 1))
((2 3) 1)
CL-USER> (kreck '(1 2 3) '((fexpr-gate (c arg1 arg2)) 1 2))
def * not found
   [Condition of type SIMPLE-ERROR]
; Evaluation aborted on NIL
CL-USER> ; Quit to level 1
CL-USER> ; Evaluation aborted on #<SIMPLE-ERROR "def * not found
;" {1002CD12C3}>
CL-USER> (defparameter dbg nil)

(defmacro fn (args &body body) 
  `(lambda ,args ,@body)) ...
WARNING: redefining COMMON-LISP-USER::FN in DEFMACRO
WARNING: redefining COMMON-LISP-USER::THREAD-SUB in DEFUN
WARNING: redefining COMMON-LISP-USER::$$-> in DEFMACRO
WARNING: redefining COMMON-LISP-USER::TEST-1 in DEFUN
; in: DEFMACRO TEST
;     (SB-INT:NAMED-DS-BIND (:MACRO TEST . DEFMACRO)
;         (NAME &BODY EXPRS)
;         (CDR #:EXPR)
;       (DECLARE (SB-C::CONSTANT-VALUE NAME EXPRS))
;       (BLOCK TEST `(AND ,@(MAPCAR #'TEST-1 EXPRS))))
; ==>
;   (LET* ((#:G0
;           (SB-C::CHECK-DS-LIST/&REST (CDR #:EXPR) 1 1 '(# NAME &BODY EXPRS)))
;          (NAME (POP #:G0))
;          (EXPRS #:G0))
;     (DECLARE (SB-C::CONSTANT-VALUE NAME EXPRS))
;     (BLOCK TEST `(AND ,@(MAPCAR #'TEST-1 EXPRS))))
; 
; caught STYLE-WARNING:
;   The variable NAME is defined but never used.
; 
; compilation unit finished
;   caught 1 STYLE-WARNING condition
WARNING: redefining COMMON-LISP-USER::TEST in DEFMACRO
WARNING: redefining COMMON-LISP-USER::TEST-EQUAL in DEFMACRO
WARNING: redefining COMMON-LISP-USER::DUMP in DEFMACRO
WARNING: redefining COMMON-LISP-USER::KRECK-EVAL in DEFUN
; in: DEFUN SUBJ-OP
;     (SB-INT:NAMED-LAMBDA SUBJ-OP
;         (SUBJ ARGS)
;       (BLOCK SUBJ-OP SUBJ))
; 
; caught STYLE-WARNING:
;   The variable ARGS is defined but never used.
; 
; compilation unit finished
;   caught 1 STYLE-WARNING condition
WARNING: redefining COMMON-LISP-USER::SUBJ-OP in DEFUN
; in: DEFUN QUOT-OP
;     (SB-INT:NAMED-LAMBDA QUOT-OP
;         (SUBJ ARGS)
;       (BLOCK QUOT-OP (CAR ARGS)))
; 
; caught STYLE-WARNING:
;   The variable SUBJ is defined but never used.
; 
; compilation unit finished
;   caught 1 STYLE-WARNING condition
WARNING: redefining COMMON-LISP-USER::QUOT-OP in DEFUN
WARNING: redefining COMMON-LISP-USER::CAR-OP in DEFUN
WARNING: redefining COMMON-LISP-USER::CDR-OP in DEFUN
WARNING: redefining COMMON-LISP-USER::CONS-OP in DEFUN
WARNING: redefining COMMON-LISP-USER::EVAL-OP in DEFUN
WARNING: redefining COMMON-LISP-USER::IF-OP in DEFUN
WARNING: redefining COMMON-LISP-USER::DEFGET-OP in DEFUN
WARNING: redefining COMMON-LISP-USER::DEBUG-OP in DEFUN
WARNING: redefining COMMON-LISP-USER::PARSE in DEFUN
WARNING: redefining COMMON-LISP-USER::UNPARSE in DEFUN
WARNING: redefining COMMON-LISP-USER::DEF-MAKE in DEFUN
WARNING: redefining COMMON-LISP-USER::KRECK in DEFUN
def * not found
   [Condition of type SIMPLE-ERROR]
; Evaluation aborted on NIL
CL-USER> ; Quit to level 1
CL-USER> ; Evaluation aborted on #<SIMPLE-ERROR "def * not found
;" {10034C5443}>
CL-USER> (defparameter dbg nil)

(defmacro fn (args &body body) 
  `(lambda ,args ,@body)) ...
WARNING: redefining COMMON-LISP-USER::FN in DEFMACRO
WARNING: redefining COMMON-LISP-USER::THREAD-SUB in DEFUN
WARNING: redefining COMMON-LISP-USER::$$-> in DEFMACRO
WARNING: redefining COMMON-LISP-USER::TEST-1 in DEFUN
; in: DEFMACRO TEST
;     (SB-INT:NAMED-DS-BIND (:MACRO TEST . DEFMACRO)
;         (NAME &BODY EXPRS)
;         (CDR #:EXPR)
;       (DECLARE (SB-C::CONSTANT-VALUE NAME EXPRS))
;       (BLOCK TEST `(AND ,@(MAPCAR #'TEST-1 EXPRS))))
; ==>
;   (LET* ((#:G0
;           (SB-C::CHECK-DS-LIST/&REST (CDR #:EXPR) 1 1 '(# NAME &BODY EXPRS)))
;          (NAME (POP #:G0))
;          (EXPRS #:G0))
;     (DECLARE (SB-C::CONSTANT-VALUE NAME EXPRS))
;     (BLOCK TEST `(AND ,@(MAPCAR #'TEST-1 EXPRS))))
; 
; caught STYLE-WARNING:
;   The variable NAME is defined but never used.
; 
; compilation unit finished
;   caught 1 STYLE-WARNING condition
WARNING: redefining COMMON-LISP-USER::TEST in DEFMACRO
WARNING: redefining COMMON-LISP-USER::TEST-EQUAL in DEFMACRO
WARNING: redefining COMMON-LISP-USER::DUMP in DEFMACRO
WARNING: redefining COMMON-LISP-USER::KRECK-EVAL in DEFUN
; in: DEFUN SUBJ-OP
;     (SB-INT:NAMED-LAMBDA SUBJ-OP
;         (SUBJ ARGS)
;       (BLOCK SUBJ-OP SUBJ))
; 
; caught STYLE-WARNING:
;   The variable ARGS is defined but never used.
; 
; compilation unit finished
;   caught 1 STYLE-WARNING condition
WARNING: redefining COMMON-LISP-USER::SUBJ-OP in DEFUN
; in: DEFUN QUOT-OP
;     (SB-INT:NAMED-LAMBDA QUOT-OP
;         (SUBJ ARGS)
;       (BLOCK QUOT-OP (CAR ARGS)))
; 
; caught STYLE-WARNING:
;   The variable SUBJ is defined but never used.
; 
; compilation unit finished
;   caught 1 STYLE-WARNING condition
WARNING: redefining COMMON-LISP-USER::QUOT-OP in DEFUN
WARNING: redefining COMMON-LISP-USER::CAR-OP in DEFUN
WARNING: redefining COMMON-LISP-USER::CDR-OP in DEFUN
WARNING: redefining COMMON-LISP-USER::CONS-OP in DEFUN
WARNING: redefining COMMON-LISP-USER::EVAL-OP in DEFUN
WARNING: redefining COMMON-LISP-USER::IF-OP in DEFUN
WARNING: redefining COMMON-LISP-USER::DEFGET-OP in DEFUN
WARNING: redefining COMMON-LISP-USER::DEBUG-OP in DEFUN
WARNING: redefining COMMON-LISP-USER::PARSE in DEFUN
WARNING: redefining COMMON-LISP-USER::UNPARSE in DEFUN
WARNING: redefining COMMON-LISP-USER::DEF-MAKE in DEFUN
WARNING: redefining COMMON-LISP-USER::KRECK in DEFUN
def * not found
   [Condition of type SIMPLE-ERROR]
; Evaluation aborted on NIL
CL-USER> ; Quit to level 1
CL-USER> ; Evaluation aborted on #<SIMPLE-ERROR "def * not found
;" {1003B3C603}>
CL-USER> (defparameter dbg nil)

(defmacro fn (args &body body) 
  `(lambda ,args ,@body)) ...
WARNING: redefining COMMON-LISP-USER::FN in DEFMACRO
WARNING: redefining COMMON-LISP-USER::THREAD-SUB in DEFUN
WARNING: redefining COMMON-LISP-USER::$$-> in DEFMACRO
WARNING: redefining COMMON-LISP-USER::TEST-1 in DEFUN
; in: DEFMACRO TEST
;     (SB-INT:NAMED-DS-BIND (:MACRO TEST . DEFMACRO)
;         (NAME &BODY EXPRS)
;         (CDR #:EXPR)
;       (DECLARE (SB-C::CONSTANT-VALUE NAME EXPRS))
;       (BLOCK TEST `(AND ,@(MAPCAR #'TEST-1 EXPRS))))
; ==>
;   (LET* ((#:G0
;           (SB-C::CHECK-DS-LIST/&REST (CDR #:EXPR) 1 1 '(# NAME &BODY EXPRS)))
;          (NAME (POP #:G0))
;          (EXPRS #:G0))
;     (DECLARE (SB-C::CONSTANT-VALUE NAME EXPRS))
;     (BLOCK TEST `(AND ,@(MAPCAR #'TEST-1 EXPRS))))
; 
; caught STYLE-WARNING:
;   The variable NAME is defined but never used.
; 
; compilation unit finished
;   caught 1 STYLE-WARNING condition
WARNING: redefining COMMON-LISP-USER::TEST in DEFMACRO
WARNING: redefining COMMON-LISP-USER::TEST-EQUAL in DEFMACRO
WARNING: redefining COMMON-LISP-USER::DUMP in DEFMACRO
WARNING: redefining COMMON-LISP-USER::KRECK-EVAL in DEFUN
; in: DEFUN SUBJ-OP
;     (SB-INT:NAMED-LAMBDA SUBJ-OP
;         (SUBJ ARGS)
;       (BLOCK SUBJ-OP SUBJ))
; 
; caught STYLE-WARNING:
;   The variable ARGS is defined but never used.
; 
; compilation unit finished
;   caught 1 STYLE-WARNING condition
WARNING: redefining COMMON-LISP-USER::SUBJ-OP in DEFUN
; in: DEFUN QUOT-OP
;     (SB-INT:NAMED-LAMBDA QUOT-OP
;         (SUBJ ARGS)
;       (BLOCK QUOT-OP (CAR ARGS)))
; 
; caught STYLE-WARNING:
;   The variable SUBJ is defined but never used.
; 
; compilation unit finished
;   caught 1 STYLE-WARNING condition
WARNING: redefining COMMON-LISP-USER::QUOT-OP in DEFUN
WARNING: redefining COMMON-LISP-USER::CAR-OP in DEFUN
WARNING: redefining COMMON-LISP-USER::CDR-OP in DEFUN
WARNING: redefining COMMON-LISP-USER::CONS-OP in DEFUN
WARNING: redefining COMMON-LISP-USER::EVAL-OP in DEFUN
WARNING: redefining COMMON-LISP-USER::IF-OP in DEFUN
WARNING: redefining COMMON-LISP-USER::DEFGET-OP in DEFUN
WARNING: redefining COMMON-LISP-USER::DEBUG-OP in DEFUN
WARNING: redefining COMMON-LISP-USER::PARSE in DEFUN
WARNING: redefining COMMON-LISP-USER::UNPARSE in DEFUN
WARNING: redefining COMMON-LISP-USER::DEF-MAKE in DEFUN
WARNING: redefining COMMON-LISP-USER::KRECK in DEFUN
def c not found
   [Condition of type SIMPLE-ERROR]
; Evaluation aborted on NIL
CL-USER> ; Quit to level 1
CL-USER> ; Evaluation aborted on #<SIMPLE-ERROR "def c not found
;" {100419D893}>
CL-USER> (kreck '(1 2 3) '(fexpr-gate (c arg1 arg2)))
(((#<FUNCTION DEFGET-OP> "c") ((#<FUNCTION DEFGET-OP> "$"))
  (#<FUNCTION DEFGET-OP> "this"))
 (((#<FUNCTION DEFGET-OP> "c") (#<FUNCTION DEFGET-OP> "arg1")
   (#<FUNCTION DEFGET-OP> "arg2")))
 ((1 2 3) NIL ("~") ("$" . #<FUNCTION SUBJ-OP>) ("q" . #<FUNCTION QUOT-OP>)
  ("<" . #<FUNCTION CAR-OP>) (">" . #<FUNCTION CDR-OP>)
  ("c" . #<FUNCTION CONS-OP>) ("*" . #<FUNCTION EVAL-OP>)
  ("?" . #<FUNCTION IF-OP>) ("@" . #<FUNCTION DEFGET-OP>)
  ("!d" . #<FUNCTION DEBUG-OP>)
  ("args" (#<FUNCTION DEFGET-OP> "<") ((#<FUNCTION DEFGET-OP> "$")))
  ("arg1" (#<FUNCTION DEFGET-OP> "<") (#<FUNCTION DEFGET-OP> "args"))
  ("arg2" (#<FUNCTION DEFGET-OP> "<")
   ((#<FUNCTION DEFGET-OP> ">") (#<FUNCTION DEFGET-OP> "args")))
  ("arg3" (#<FUNCTION DEFGET-OP> "<")
   ((#<FUNCTION DEFGET-OP> ">")
    ((#<FUNCTION DEFGET-OP> ">") (#<FUNCTION DEFGET-OP> "args"))))
  ("ctx" (#<FUNCTION DEFGET-OP> ">") ((#<FUNCTION DEFGET-OP> "$")))
  ("locs" (#<FUNCTION DEFGET-OP> "<")
   ((#<FUNCTION DEFGET-OP> ">") ((#<FUNCTION DEFGET-OP> "$"))))
  ("defs" (#<FUNCTION DEFGET-OP> ">")
   ((#<FUNCTION DEFGET-OP> ">") ((#<FUNCTION DEFGET-OP> "$"))))
  ("this" (#<FUNCTION DEFGET-OP> "<") (#<FUNCTION DEFGET-OP> "locs"))
  ("calr" (#<FUNCTION DEFGET-OP> "<")
   ((#<FUNCTION DEFGET-OP> ">") (#<FUNCTION DEFGET-OP> "locs")))
  ("non-rec-fexpr-ctx" (#<FUNCTION DEFGET-OP> "c")
   ((#<FUNCTION DEFGET-OP> "c") (#<FUNCTION DEFGET-OP> "~")
    ((#<FUNCTION DEFGET-OP> "c") ((#<FUNCTION DEFGET-OP> "$"))
     (#<FUNCTION DEFGET-OP> "~")))
   (#<FUNCTION DEFGET-OP> "defs"))
  ("*^" (#<FUNCTION DEFGET-OP> "c")
   ((#<FUNCTION DEFGET-OP> "q")
    ((#<FUNCTION DEFGET-OP> "*")
     ((#<FUNCTION DEFGET-OP> "*") (#<FUNCTION DEFGET-OP> "calr")
      ((#<FUNCTION DEFGET-OP> "q") (#<FUNCTION DEFGET-OP> "calr")))
     ((#<FUNCTION DEFGET-OP> "*") (#<FUNCTION DEFGET-OP> "calr")
      (#<FUNCTION DEFGET-OP> "arg1"))))
   (#<FUNCTION DEFGET-OP> "non-rec-fexpr-ctx"))
  ("w" (#<FUNCTION DEFGET-OP> "c")
   ((#<FUNCTION DEFGET-OP> "q")
    ((#<FUNCTION DEFGET-OP> "c")
     ((#<FUNCTION DEFGET-OP> "*^") (#<FUNCTION DEFGET-OP> "arg1"))
     (#<FUNCTION DEFGET-OP> "~")))
   (#<FUNCTION DEFGET-OP> "non-rec-fexpr-ctx"))
  ("l" (#<FUNCTION DEFGET-OP> "c")
   ((#<FUNCTION DEFGET-OP> "q")
    ((#<FUNCTION DEFGET-OP> "*") ((#<FUNCTION DEFGET-OP> "$"))
     (#<FUNCTION DEFGET-OP> "this")))
   ((#<FUNCTION DEFGET-OP> "c")
    ((#<FUNCTION DEFGET-OP> "c")
     ((#<FUNCTION DEFGET-OP> "q")
      ((#<FUNCTION DEFGET-OP> "?") (#<FUNCTION DEFGET-OP> "args")
       ((#<FUNCTION DEFGET-OP> "c")
        ((#<FUNCTION DEFGET-OP> "*") (#<FUNCTION DEFGET-OP> "calr")
         (#<FUNCTION DEFGET-OP> "arg1"))
        ((#<FUNCTION DEFGET-OP> "*")
         ((#<FUNCTION DEFGET-OP> "c")
          ((#<FUNCTION DEFGET-OP> ">") (#<FUNCTION DEFGET-OP> "args"))
          (#<FUNCTION DEFGET-OP> "ctx"))
         (#<FUNCTION DEFGET-OP> "this")))))
     ((#<FUNCTION DEFGET-OP> "c") ((#<FUNCTION DEFGET-OP> "$"))
      (#<FUNCTION DEFGET-OP> "~")))
    (#<FUNCTION DEFGET-OP> "defs")))
  ("l" (#<FUNCTION DEFGET-OP> "c") ((#<FUNCTION DEFGET-OP> "q") NIL)
   ((#<FUNCTION DEFGET-OP> "c") (#<FUNCTION DEFGET-OP> "~")
    ((#<FUNCTION DEFGET-OP> "c") ((#<FUNCTION DEFGET-OP> "$"))
     (#<FUNCTION DEFGET-OP> "~"))))
  ("fexpr-gate" (#<FUNCTION DEFGET-OP> "c")
   ((#<FUNCTION DEFGET-OP> "q")
    ((#<FUNCTION DEFGET-OP> "c")
     ((#<FUNCTION DEFGET-OP> "q")
      ((#<FUNCTION DEFGET-OP> "c") ((#<FUNCTION DEFGET-OP> "$"))
       (#<FUNCTION DEFGET-OP> "this")))
     ((#<FUNCTION DEFGET-OP> "c")
      ((#<FUNCTION DEFGET-OP> "c") (#<FUNCTION DEFGET-OP> "arg1"))
      ((#<FUNCTION DEFGET-OP> "c")
       ((#<FUNCTION DEFGET-OP> "*") (#<FUNCTION DEFGET-OP> "calr")
        ((#<FUNCTION DEFGET-OP> "q") ((#<FUNCTION DEFGET-OP> "$"))))
       (#<FUNCTION DEFGET-OP> "~"))
      (#<FUNCTION DEFGET-OP> "defs"))))
   (#<FUNCTION DEFGET-OP> "non-rec-fexpr-ctx"))))
CL-USER> (defparameter dbg nil)

(defmacro fn (args &body body) 
  `(lambda ,args ,@body)) ...
WARNING: redefining COMMON-LISP-USER::FN in DEFMACRO
WARNING: redefining COMMON-LISP-USER::THREAD-SUB in DEFUN
WARNING: redefining COMMON-LISP-USER::$$-> in DEFMACRO
WARNING: redefining COMMON-LISP-USER::TEST-1 in DEFUN
; in: DEFMACRO TEST
;     (SB-INT:NAMED-DS-BIND (:MACRO TEST . DEFMACRO)
;         (NAME &BODY EXPRS)
;         (CDR #:EXPR)
;       (DECLARE (SB-C::CONSTANT-VALUE NAME EXPRS))
;       (BLOCK TEST `(AND ,@(MAPCAR #'TEST-1 EXPRS))))
; ==>
;   (LET* ((#:G0
;           (SB-C::CHECK-DS-LIST/&REST (CDR #:EXPR) 1 1 '(# NAME &BODY EXPRS)))
;          (NAME (POP #:G0))
;          (EXPRS #:G0))
;     (DECLARE (SB-C::CONSTANT-VALUE NAME EXPRS))
;     (BLOCK TEST `(AND ,@(MAPCAR #'TEST-1 EXPRS))))
; 
; caught STYLE-WARNING:
;   The variable NAME is defined but never used.
; 
; compilation unit finished
;   caught 1 STYLE-WARNING condition
WARNING: redefining COMMON-LISP-USER::TEST in DEFMACRO
WARNING: redefining COMMON-LISP-USER::TEST-EQUAL in DEFMACRO
WARNING: redefining COMMON-LISP-USER::DUMP in DEFMACRO
WARNING: redefining COMMON-LISP-USER::KRECK-EVAL in DEFUN
; in: DEFUN SUBJ-OP
;     (SB-INT:NAMED-LAMBDA SUBJ-OP
;         (SUBJ ARGS)
;       (BLOCK SUBJ-OP SUBJ))
; 
; caught STYLE-WARNING:
;   The variable ARGS is defined but never used.
; 
; compilation unit finished
;   caught 1 STYLE-WARNING condition
WARNING: redefining COMMON-LISP-USER::SUBJ-OP in DEFUN
; in: DEFUN QUOT-OP
;     (SB-INT:NAMED-LAMBDA QUOT-OP
;         (SUBJ ARGS)
;       (BLOCK QUOT-OP (CAR ARGS)))
; 
; caught STYLE-WARNING:
;   The variable SUBJ is defined but never used.
; 
; compilation unit finished
;   caught 1 STYLE-WARNING condition
WARNING: redefining COMMON-LISP-USER::QUOT-OP in DEFUN
WARNING: redefining COMMON-LISP-USER::CAR-OP in DEFUN
WARNING: redefining COMMON-LISP-USER::CDR-OP in DEFUN
WARNING: redefining COMMON-LISP-USER::CONS-OP in DEFUN
WARNING: redefining COMMON-LISP-USER::EVAL-OP in DEFUN
WARNING: redefining COMMON-LISP-USER::IF-OP in DEFUN
WARNING: redefining COMMON-LISP-USER::DEFGET-OP in DEFUN
WARNING: redefining COMMON-LISP-USER::DEBUG-OP in DEFUN
WARNING: redefining COMMON-LISP-USER::PARSE in DEFUN
WARNING: redefining COMMON-LISP-USER::UNPARSE in DEFUN
WARNING: redefining COMMON-LISP-USER::DEF-MAKE in DEFUN
WARNING: redefining COMMON-LISP-USER::KRECK in DEFUN
def * not found
   [Condition of type SIMPLE-ERROR]
; Evaluation aborted on NIL
CL-USER> ; Quit to level 1
CL-USER> ; Evaluation aborted on #<SIMPLE-ERROR "def * not found
;" {1004844433}>
CL-USER> (kreck '(1 2 3) '(fexpr-gate (c arg1 arg2)))
(((#<FUNCTION DEFGET-OP> "*") ((#<FUNCTION DEFGET-OP> "$"))
  (#<FUNCTION DEFGET-OP> "this"))
 (((#<FUNCTION DEFGET-OP> "c") (#<FUNCTION DEFGET-OP> "arg1")
   (#<FUNCTION DEFGET-OP> "arg2")))
 ((1 2 3) NIL ("~") ("$" . #<FUNCTION SUBJ-OP>) ("q" . #<FUNCTION QUOT-OP>)
  ("<" . #<FUNCTION CAR-OP>) (">" . #<FUNCTION CDR-OP>)
  ("c" . #<FUNCTION CONS-OP>) ("*" . #<FUNCTION EVAL-OP>)
  ("?" . #<FUNCTION IF-OP>) ("@" . #<FUNCTION DEFGET-OP>)
  ("!d" . #<FUNCTION DEBUG-OP>)
  ("args" (#<FUNCTION DEFGET-OP> "<") ((#<FUNCTION DEFGET-OP> "$")))
  ("arg1" (#<FUNCTION DEFGET-OP> "<") (#<FUNCTION DEFGET-OP> "args"))
  ("arg2" (#<FUNCTION DEFGET-OP> "<")
   ((#<FUNCTION DEFGET-OP> ">") (#<FUNCTION DEFGET-OP> "args")))
  ("arg3" (#<FUNCTION DEFGET-OP> "<")
   ((#<FUNCTION DEFGET-OP> ">")
    ((#<FUNCTION DEFGET-OP> ">") (#<FUNCTION DEFGET-OP> "args"))))
  ("ctx" (#<FUNCTION DEFGET-OP> ">") ((#<FUNCTION DEFGET-OP> "$")))
  ("locs" (#<FUNCTION DEFGET-OP> "<")
   ((#<FUNCTION DEFGET-OP> ">") ((#<FUNCTION DEFGET-OP> "$"))))
  ("defs" (#<FUNCTION DEFGET-OP> ">")
   ((#<FUNCTION DEFGET-OP> ">") ((#<FUNCTION DEFGET-OP> "$"))))
  ("this" (#<FUNCTION DEFGET-OP> "<") (#<FUNCTION DEFGET-OP> "locs"))
  ("calr" (#<FUNCTION DEFGET-OP> "<")
   ((#<FUNCTION DEFGET-OP> ">") (#<FUNCTION DEFGET-OP> "locs")))
  ("non-rec-fexpr-ctx" (#<FUNCTION DEFGET-OP> "c")
   ((#<FUNCTION DEFGET-OP> "c") (#<FUNCTION DEFGET-OP> "~")
    ((#<FUNCTION DEFGET-OP> "c") ((#<FUNCTION DEFGET-OP> "$"))
     (#<FUNCTION DEFGET-OP> "~")))
   (#<FUNCTION DEFGET-OP> "defs"))
  ("*^" (#<FUNCTION DEFGET-OP> "c")
   ((#<FUNCTION DEFGET-OP> "q")
    ((#<FUNCTION DEFGET-OP> "*")
     ((#<FUNCTION DEFGET-OP> "*") (#<FUNCTION DEFGET-OP> "calr")
      ((#<FUNCTION DEFGET-OP> "q") (#<FUNCTION DEFGET-OP> "calr")))
     ((#<FUNCTION DEFGET-OP> "*") (#<FUNCTION DEFGET-OP> "calr")
      (#<FUNCTION DEFGET-OP> "arg1"))))
   (#<FUNCTION DEFGET-OP> "non-rec-fexpr-ctx"))
  ("w" (#<FUNCTION DEFGET-OP> "c")
   ((#<FUNCTION DEFGET-OP> "q")
    ((#<FUNCTION DEFGET-OP> "c")
     ((#<FUNCTION DEFGET-OP> "*^") (#<FUNCTION DEFGET-OP> "arg1"))
     (#<FUNCTION DEFGET-OP> "~")))
   (#<FUNCTION DEFGET-OP> "non-rec-fexpr-ctx"))
  ("l" (#<FUNCTION DEFGET-OP> "c")
   ((#<FUNCTION DEFGET-OP> "q")
    ((#<FUNCTION DEFGET-OP> "*") ((#<FUNCTION DEFGET-OP> "$"))
     (#<FUNCTION DEFGET-OP> "this")))
   ((#<FUNCTION DEFGET-OP> "c")
    ((#<FUNCTION DEFGET-OP> "c")
     ((#<FUNCTION DEFGET-OP> "q")
      ((#<FUNCTION DEFGET-OP> "?") (#<FUNCTION DEFGET-OP> "args")
       ((#<FUNCTION DEFGET-OP> "c")
        ((#<FUNCTION DEFGET-OP> "*") (#<FUNCTION DEFGET-OP> "calr")
         (#<FUNCTION DEFGET-OP> "arg1"))
        ((#<FUNCTION DEFGET-OP> "*")
         ((#<FUNCTION DEFGET-OP> "c")
          ((#<FUNCTION DEFGET-OP> ">") (#<FUNCTION DEFGET-OP> "args"))
          (#<FUNCTION DEFGET-OP> "ctx"))
         (#<FUNCTION DEFGET-OP> "this")))))
     ((#<FUNCTION DEFGET-OP> "c") ((#<FUNCTION DEFGET-OP> "$"))
      (#<FUNCTION DEFGET-OP> "~")))
    (#<FUNCTION DEFGET-OP> "defs")))
  ("l" (#<FUNCTION DEFGET-OP> "c") ((#<FUNCTION DEFGET-OP> "q") NIL)
   ((#<FUNCTION DEFGET-OP> "c") (#<FUNCTION DEFGET-OP> "~")
    ((#<FUNCTION DEFGET-OP> "c") ((#<FUNCTION DEFGET-OP> "$"))
     (#<FUNCTION DEFGET-OP> "~"))))
  ("fexpr-gate" (#<FUNCTION DEFGET-OP> "c")
   ((#<FUNCTION DEFGET-OP> "q")
    ((#<FUNCTION DEFGET-OP> "c")
     ((#<FUNCTION DEFGET-OP> "q")
      ((#<FUNCTION DEFGET-OP> "*") ((#<FUNCTION DEFGET-OP> "$"))
       (#<FUNCTION DEFGET-OP> "this")))
     ((#<FUNCTION DEFGET-OP> "c")
      ((#<FUNCTION DEFGET-OP> "c") (#<FUNCTION DEFGET-OP> "arg1"))
      ((#<FUNCTION DEFGET-OP> "c")
       ((#<FUNCTION DEFGET-OP> "*") (#<FUNCTION DEFGET-OP> "calr")
        ((#<FUNCTION DEFGET-OP> "q") ((#<FUNCTION DEFGET-OP> "$"))))
       (#<FUNCTION DEFGET-OP> "~"))
      (#<FUNCTION DEFGET-OP> "defs"))))
   (#<FUNCTION DEFGET-OP> "non-rec-fexpr-ctx"))))
CL-USER> (cddr (kreck '(1 2 3) '(fexpr-gate (c arg1 arg2))))
(((1 2 3) NIL ("~") ("$" . #<FUNCTION SUBJ-OP>) ("q" . #<FUNCTION QUOT-OP>)
  ("<" . #<FUNCTION CAR-OP>) (">" . #<FUNCTION CDR-OP>)
  ("c" . #<FUNCTION CONS-OP>) ("*" . #<FUNCTION EVAL-OP>)
  ("?" . #<FUNCTION IF-OP>) ("@" . #<FUNCTION DEFGET-OP>)
  ("!d" . #<FUNCTION DEBUG-OP>)
  ("args" (#<FUNCTION DEFGET-OP> "<") ((#<FUNCTION DEFGET-OP> "$")))
  ("arg1" (#<FUNCTION DEFGET-OP> "<") (#<FUNCTION DEFGET-OP> "args"))
  ("arg2" (#<FUNCTION DEFGET-OP> "<")
   ((#<FUNCTION DEFGET-OP> ">") (#<FUNCTION DEFGET-OP> "args")))
  ("arg3" (#<FUNCTION DEFGET-OP> "<")
   ((#<FUNCTION DEFGET-OP> ">")
    ((#<FUNCTION DEFGET-OP> ">") (#<FUNCTION DEFGET-OP> "args"))))
  ("ctx" (#<FUNCTION DEFGET-OP> ">") ((#<FUNCTION DEFGET-OP> "$")))
  ("locs" (#<FUNCTION DEFGET-OP> "<")
   ((#<FUNCTION DEFGET-OP> ">") ((#<FUNCTION DEFGET-OP> "$"))))
  ("defs" (#<FUNCTION DEFGET-OP> ">")
   ((#<FUNCTION DEFGET-OP> ">") ((#<FUNCTION DEFGET-OP> "$"))))
  ("this" (#<FUNCTION DEFGET-OP> "<") (#<FUNCTION DEFGET-OP> "locs"))
  ("calr" (#<FUNCTION DEFGET-OP> "<")
   ((#<FUNCTION DEFGET-OP> ">") (#<FUNCTION DEFGET-OP> "locs")))
  ("non-rec-fexpr-ctx" (#<FUNCTION DEFGET-OP> "c")
   ((#<FUNCTION DEFGET-OP> "c") (#<FUNCTION DEFGET-OP> "~")
    ((#<FUNCTION DEFGET-OP> "c") ((#<FUNCTION DEFGET-OP> "$"))
     (#<FUNCTION DEFGET-OP> "~")))
   (#<FUNCTION DEFGET-OP> "defs"))
  ("*^" (#<FUNCTION DEFGET-OP> "c")
   ((#<FUNCTION DEFGET-OP> "q")
    ((#<FUNCTION DEFGET-OP> "*")
     ((#<FUNCTION DEFGET-OP> "*") (#<FUNCTION DEFGET-OP> "calr")
      ((#<FUNCTION DEFGET-OP> "q") (#<FUNCTION DEFGET-OP> "calr")))
     ((#<FUNCTION DEFGET-OP> "*") (#<FUNCTION DEFGET-OP> "calr")
      (#<FUNCTION DEFGET-OP> "arg1"))))
   (#<FUNCTION DEFGET-OP> "non-rec-fexpr-ctx"))
  ("w" (#<FUNCTION DEFGET-OP> "c")
   ((#<FUNCTION DEFGET-OP> "q")
    ((#<FUNCTION DEFGET-OP> "c")
     ((#<FUNCTION DEFGET-OP> "*^") (#<FUNCTION DEFGET-OP> "arg1"))
     (#<FUNCTION DEFGET-OP> "~")))
   (#<FUNCTION DEFGET-OP> "non-rec-fexpr-ctx"))
  ("l" (#<FUNCTION DEFGET-OP> "c")
   ((#<FUNCTION DEFGET-OP> "q")
    ((#<FUNCTION DEFGET-OP> "*") ((#<FUNCTION DEFGET-OP> "$"))
     (#<FUNCTION DEFGET-OP> "this")))
   ((#<FUNCTION DEFGET-OP> "c")
    ((#<FUNCTION DEFGET-OP> "c")
     ((#<FUNCTION DEFGET-OP> "q")
      ((#<FUNCTION DEFGET-OP> "?") (#<FUNCTION DEFGET-OP> "args")
       ((#<FUNCTION DEFGET-OP> "c")
        ((#<FUNCTION DEFGET-OP> "*") (#<FUNCTION DEFGET-OP> "calr")
         (#<FUNCTION DEFGET-OP> "arg1"))
        ((#<FUNCTION DEFGET-OP> "*")
         ((#<FUNCTION DEFGET-OP> "c")
          ((#<FUNCTION DEFGET-OP> ">") (#<FUNCTION DEFGET-OP> "args"))
          (#<FUNCTION DEFGET-OP> "ctx"))
         (#<FUNCTION DEFGET-OP> "this")))))
     ((#<FUNCTION DEFGET-OP> "c") ((#<FUNCTION DEFGET-OP> "$"))
      (#<FUNCTION DEFGET-OP> "~")))
    (#<FUNCTION DEFGET-OP> "defs")))
  ("l" (#<FUNCTION DEFGET-OP> "c") ((#<FUNCTION DEFGET-OP> "q") NIL)
   ((#<FUNCTION DEFGET-OP> "c") (#<FUNCTION DEFGET-OP> "~")
    ((#<FUNCTION DEFGET-OP> "c") ((#<FUNCTION DEFGET-OP> "$"))
     (#<FUNCTION DEFGET-OP> "~"))))
  ("fexpr-gate" (#<FUNCTION DEFGET-OP> "c")
   ((#<FUNCTION DEFGET-OP> "q")
    ((#<FUNCTION DEFGET-OP> "c")
     ((#<FUNCTION DEFGET-OP> "q")
      ((#<FUNCTION DEFGET-OP> "*") ((#<FUNCTION DEFGET-OP> "$"))
       (#<FUNCTION DEFGET-OP> "this")))
     ((#<FUNCTION DEFGET-OP> "c")
      ((#<FUNCTION DEFGET-OP> "c") (#<FUNCTION DEFGET-OP> "arg1"))
      ((#<FUNCTION DEFGET-OP> "c")
       ((#<FUNCTION DEFGET-OP> "*") (#<FUNCTION DEFGET-OP> "calr")
        ((#<FUNCTION DEFGET-OP> "q") ((#<FUNCTION DEFGET-OP> "$"))))
       (#<FUNCTION DEFGET-OP> "~"))
      d
      (`#<FUNCTION DEFGET-OP> "defs"))))
   (#<FUNCTION DEFGET-OP> "non-rec-fexpr-ctx"))))
CL-USER> (cddr (kreck '(1 2 3) '(fexpr-gate (c arg1 arg2))))
(((1 2 3) NIL ("~") ("$" . #<FUNCTION SUBJ-OP>) ("q" . #<FUNCTION QUOT-OP>)
  ("<" . #<FUNCTION CAR-OP>) (">" . #<FUNCTION CDR-OP>)
  ("c" . #<FUNCTION CONS-OP>) ("*" . #<FUNCTION EVAL-OP>)
  ("?" . #<FUNCTION IF-OP>) ("@" . #<FUNCTION DEFGET-OP>)
  ("!d" . #<FUNCTION DEBUG-OP>)
  ("args" (#<FUNCTION DEFGET-OP> "<") ((#<FUNCTION DEFGET-OP> "$")))
  ("arg1" (#<FUNCTION DEFGET-OP> "<") (#<FUNCTION DEFGET-OP> "args"))
  ("arg2" (#<FUNCTION DEFGET-OP> "<")
   ((#<FUNCTION DEFGET-OP> ">") (#<FUNCTION DEFGET-OP> "args")))
  ("arg3" (#<FUNCTION DEFGET-OP> "<")
   ((#<FUNCTION DEFGET-OP> ">")
    ((#<FUNCTION DEFGET-OP> ">") (#<FUNCTION DEFGET-OP> "args"))))
  ("ctx" (#<FUNCTION DEFGET-OP> ">") ((#<FUNCTION DEFGET-OP> "$")))
  ("locs" (#<FUNCTION DEFGET-OP> "<")
   ((#<FUNCTION DEFGET-OP> ">") ((#<FUNCTION DEFGET-OP> "$"))))
  ("defs" (#<FUNCTION DEFGET-OP> ">")
   ((#<FUNCTION DEFGET-OP> ">") ((#<FUNCTION DEFGET-OP> "$"))))
  ("this" (#<FUNCTION DEFGET-OP> "<") (#<FUNCTION DEFGET-OP> "locs"))
  ("calr" (#<FUNCTION DEFGET-OP> "<")
   ((#<FUNCTION DEFGET-OP> ">") (#<FUNCTION DEFGET-OP> "locs")))
  ("non-rec-fexpr-ctx" (#<FUNCTION DEFGET-OP> "c")
   ((#<FUNCTION DEFGET-OP> "c") (#<FUNCTION DEFGET-OP> "~")
    ((#<FUNCTION DEFGET-OP> "c") ((#<FUNCTION DEFGET-OP> "$"))
     (#<FUNCTION DEFGET-OP> "~")))
   (#<FUNCTION DEFGET-OP> "defs"))
  ("*^" (#<FUNCTION DEFGET-OP> "c")
   ((#<FUNCTION DEFGET-OP> "q")
    ((#<FUNCTION DEFGET-OP> "*")
     ((#<FUNCTION DEFGET-OP> "*") (#<FUNCTION DEFGET-OP> "calr")
      ((#<FUNCTION DEFGET-OP> "q") (#<FUNCTION DEFGET-OP> "calr")))
     ((#<FUNCTION DEFGET-OP> "*") (#<FUNCTION DEFGET-OP> "calr")
      (#<FUNCTION DEFGET-OP> "arg1"))))
   (#<FUNCTION DEFGET-OP> "non-rec-fexpr-ctx"))
  ("w" (#<FUNCTION DEFGET-OP> "c")
   ((#<FUNCTION DEFGET-OP> "q")
    ((#<FUNCTION DEFGET-OP> "c")
     ((#<FUNCTION DEFGET-OP> "*^") (#<FUNCTION DEFGET-OP> "arg1"))
     (#<FUNCTION DEFGET-OP> "~")))
   (#<FUNCTION DEFGET-OP> "non-rec-fexpr-ctx"))
  ("l" (#<FUNCTION DEFGET-OP> "c")
   ((#<FUNCTION DEFGET-OP> "q")
    ((#<FUNCTION DEFGET-OP> "*") ((#<FUNCTION DEFGET-OP> "$"))
     (#<FUNCTION DEFGET-OP> "this")))
   ((#<FUNCTION DEFGET-OP> "c")
    ((#<FUNCTION DEFGET-OP> "c")
     ((#<FUNCTION DEFGET-OP> "q")
      ((#<FUNCTION DEFGET-OP> "?") (#<FUNCTION DEFGET-OP> "args")
       ((#<FUNCTION DEFGET-OP> "c")
        ((#<FUNCTION DEFGET-OP> "*") (#<FUNCTION DEFGET-OP> "calr")
         (#<FUNCTION DEFGET-OP> "arg1"))
        ((#<FUNCTION DEFGET-OP> "*")
         ((#<FUNCTION DEFGET-OP> "c")
          ((#<FUNCTION DEFGET-OP> ">") (#<FUNCTION DEFGET-OP> "args"))
          (#<FUNCTION DEFGET-OP> "ctx"))
         (#<FUNCTION DEFGET-OP> "this")))))
     ((#<FUNCTION DEFGET-OP> "c") ((#<FUNCTION DEFGET-OP> "$"))
      (#<FUNCTION DEFGET-OP> "~")))
    (#<FUNCTION DEFGET-OP> "defs")))
  ("l" (#<FUNCTION DEFGET-OP> "c") ((#<FUNCTION DEFGET-OP> "q") NIL)
   ((#<FUNCTION DEFGET-OP> "c") (#<FUNCTION DEFGET-OP> "~")
    ((#<FUNCTION DEFGET-OP> "c") ((#<FUNCTION DEFGET-OP> "$"))
     (#<FUNCTION DEFGET-OP> "~"))))
  ("fexpr-gate" (#<FUNCTION DEFGET-OP> "c")
   ((#<FUNCTION DEFGET-OP> "q")
    ((#<FUNCTION DEFGET-OP> "c")
     ((#<FUNCTION DEFGET-OP> "q")
      ((#<FUNCTION DEFGET-OP> "*") ((#<FUNCTION DEFGET-OP> "$"))
       (#<FUNCTION DEFGET-OP> "this")))
     ((#<FUNCTION DEFGET-OP> "c")
      ((#<FUNCTION DEFGET-OP> "c") (#<FUNCTION DEFGET-OP> "arg1"))
      ((#<FUNCTION DEFGET-OP> "c")
       ((#<FUNCTION DEFGET-OP> "*") (#<FUNCTION DEFGET-OP> "calr")
        ((#<FUNCTION DEFGET-OP> "q") ((#<FUNCTION DEFGET-OP> "$"))))
       (#<FUNCTION DEFGET-OP> "~"))
      (#<FUNCTION DEFGET-OP> "defs"))))
   (#<FUNCTION DEFGET-OP> "non-rec-fexpr-ctx"))))
CL-USER> 
