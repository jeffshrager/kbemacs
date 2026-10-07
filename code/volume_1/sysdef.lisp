;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_1.pdf
;;; PDF pages 78-84; listing sysdef.

;;; -*- Mode:LISP; Package:USER; Base:10; Fonts: MEDFNB; -*-

;;; SYSDEF: System definition for the KBE (Knowledge-Based Editor)

;;; See the LispM init file for KBE to see how this file can be
;;; used for building a KBE system for temporary use or for dumping.

;;; Define a KBE package for our sources to live on.
;;;
;;;   The following symbols must be treated specially:
;;;
;;;   <-                  We use DICK's <- macro for now. It is like a
;;;                       multi-arg SETF. It is also in the process of
;;;                       being phased out.
;;;
;;;   LET, LET*           We use destructuring LET and LET* and probably
;;;                       will continue to since they are upward compatible
;;;                       with the system version. We don't want to force
;;;                       destructuring forms on other users of our band,
;;;                       though, so we shadow it as a courtesy.
;;;


(package-declare KBE GLOBAL 10000. nil
  (myrefname GLOBAL KBE)
  (shadow <- LET LET*))

(package-declare VANILLA GLOBAL 2000. nil
  (myrefname GLOBAL VANILLA))

;;; Kludge:
;;;
;;;   To avoid clobbering the system version of LET/LET*, we force
;;;   the file LISPM2;LET to load into our package. This causes a
;;;   problem because it expects to load on SI: and find the symbol
;;;   DEFMACRO-SET-INDENTATION-FOR-ZWEI there, which it does not. So here
;;;   we steal that symbol onto our package so that we can get LET loaded
;;;   correctly.

(multiple-value-bind (sym found? ignore)
    (intern-local-soft "DEFMACRO-SET-INDENTATION-FOR-ZWEI" 'SI)
  (cond ((not found?)
         (ferror nil "SI:DEFMACRO-SET-INDENTATION-FOR-ZWEI is gone!"))
        (t
         (intern sym 'KBE))))

;;; Define a logical device KBE: from which we can load our system.

(fs:add-logical-pathname-host "KBE" "OZ"
   '(("DEMO"   "PS:<KBE.DEMO>"  )
     ("DOC"    "PS:<KBE.DOC>"   )
     ("EMACS"  "PS:<EMACS>"     )
     ("KBE"    "PS:<KBE>"       )
     ("LISPM2" "SRC:<S.SYS2>"   )
     ("LMLIB"  "PS:<LMLIB>"     )
     ("MISC"   "PS:<KBE.MISC>"  )
     ("PA"     "PS:<PA>"        )
     ("BUGFIX" "PS:<KMP.LISPM.BUGS.FIXES>")
     ("PATCH"  "PS:<KBE.PATCH>" )
     ("SOURCE" "PS:<KBE.SOURCE>")
     ("TEST"   "PS:<KBE.TEST>"  )))

;;; The KBE system: a Knowledge-Based Editor

(defsystem KBE
  (:name              "Knowledge-Based-Editor")
  (:short-name        "KBE")
  (:pathname-default  "KBE: SOURCE;")
  (:patchable         "KBE: PATCH; ")
  (:module LET    "KBE: LISPM2; LET" :package KBE)
  (:module LETS   "KBE: LMLIB;  LETS")
  (:module GPRINT "KBE: LMLIB;  GPRINT")
  (:module PP     "KBE: LMLIB; PP")

  (:module BASE   ("BASIC"
                   "NOISE"
                   "SETS"
                   "CONDITIONS"
                   "ENGLISH"
                   "SYSTEM-MAGIC"
                   "FLAVOR-MAGIC"))
  (:module MACRO  ("MACRO"))           ; General utility macros
  (:module KVARS  "KVARS")             ; General KBE variables
  (:module KMAC   "KMAC"  )            ; KBE-specific macros
  (:module IMAC   "INTERFACE-MACROS")
  (:module MBOX   ("MBOX" "POST-OFFICE"))
  (:module BEHAVE "BEHAVIOR")
  (:module KUTIL  ("EVENTS" "ACTOR" "COMPLETE"))
  (:module COMMNT "COMMENT-MACRO")
  (:module RDMAC  "READMACRO")
  (:module OUTIL  "OUTIL" )        ; Archaic macros OVER, BEGIN, MATCH, ...
  (:module DTRACE "DTRACE")        ; Debugging macro support for DB
  (:module DUTIL  "DUTIL" )        ; General utility functions for DB
  (:module DMAC   "DMAC"  )        ; Main macro support for DB
  (:module DVARS  "DVARS" )        ; Variable set-up for DB
  (:module DB     "DB"    )        ; Database
  (:module GC     "GC"    )        ; Garbage Collector
  (:module DDEBUG "DDEBUG")        ; DB Debugging stuff
  (:module SURFACE "SURFACE") ;SURFACE, EXPAND, PRIMOP, DFLOW were FLOWS
  (:module EXPAND  "EXPAND" )
  (:module PRIMOP  "PRIMOP" )
  (:module DFLOW   "DFLOW"  )
  (:module FNS    ("UTIL" "SEGTRN" "ANALYZE" "CODER"))
  (:module PSEUDO "PSEUDO")
  (:module ED0    ("DICT" "GRAM"))
  (:module ED1    "REFER" )
  (:module ED2    "VERBS" )
  (:module ED3    "IMPLEMENT")
  (:module ADA    ("ADA-EVAL" "ADA-ANAL" "ADA-PARSE" "ADA-UNPARSE"))
  (:module VU     "VIEW"  )
  (:module PLI    "PLI"   )
  (:module DEFLIB "DEFLIB")
  (:module KBELIB ("SYSLIB" "KBELIB"))
  (:module DEMLIB ("DEMO-LIB"))
  (:module DEMADALIB ("DEMO-ADA-LIB"))
  (:module BRIDGE "BRIDGE")
  (:module KAOS   "KAOS"  )
  (:module SOCIETY "SOCIETY")
  (:module STNDRD  "STANDARD-ACTOR")
  (:module REMOTE "REMOTE")
  (:module SENSE  ("OVERLAPPING-REGIONS" "MOUSE-SENSITIVE-REGIONS"))
  (:module SIMPLE ("OPTIONS" "SIMPLE" "SIMPLE-ACTOR"))
  (:module MAGIC  "MAGIC-ACTOR")
  (:module TV     "TV-MAGIC")
  (:module ZMAGIC ("ZWEI-MAGIC" "POSSIBILITIES"))
  (:module BELIEF "BELIEF")
  (:module ZWEI   ("EDIT" "EDIT-ACTOR" "CONSISTENCY"))
  (:module CMDS   ("EDIT-COMMANDS"
                   "EDIT-MISC"
                   "EDIT-SIMPLE"
                   "EDIT-DEBUG"
                   "KBE-MODE"
                   "FOCUS"
                   "TEST"
                   "DOC"))
  (:module PARSE  "PARSE" )
  (:module EXPLAIN ("EXPLAIN" "DOCUMENT"))
  (:module HANDLERS "PBM-HANDLERS")
  (:module INIT   "INIT"  )      ; Various kinds of initializations
  (:module DEBUG  "DEBUG" )

  (:skip :readfile    OUTIL)

  (:fasload           PP)
  (:fasload           GPRINT)
  (:fasload           LET)
  (:fasload           LETS)

  (:compile-load-init BASE   (PP)
                             (:fasload PP LET LETS)
                             (:fasload PP))
  ;; The reason PP is 'required' by BASE is that it globalizes
  ;; some symbols which will be necessary to the correct loading
  ;; of other things.

  (:compile-load-init MACRO  (PP)
                             (:fasload PP LET LETS BASE)
                             (:fasload BASE))

  (:compile-load-init KVARS  (PP MACRO)
                             (:fasload PP LET LETS BASE MACRO)
                             (:fasload BASE))

  (:compile-load-init KMAC   (PP MACRO)
                             (:fasload PP LET LETS BASE MACRO KVARS)
                             (:fasload BASE KVARS))

  (:compile-load-init IMAC   (PP MACRO KMAC)
                             (:fasload PP LET LETS BASE MACRO KVARS KMAC)
                             (:fasload BASE KVARS KMAC))

  (:compile-load-init MBOX   (PP MACRO)
                             (:fasload PP LET LETS BASE MACRO KVARS IMAC)
                             (:fasload BASE KVARS IMAC))

  (:compile-load-init BEHAVE (PP MACRO IMAC)
                             (:fasload PP LET LETS BASE MACRO KVARS IMAC)
                             (:fasload BASE KVARS IMAC MBOX))

  (:compile-load-init KUTIL  (PP MACRO IMAC BEHAVE)
                             (:fasload PP LET LETS BASE MACRO KVARS
                                       IMAC BEHAVE)
                             (:fasload BASE KVARS IMAC MBOX BEHAVE))

  (:compile-load-init TV     (PP MACRO KMAC)
                             (:fasload PP LET LETS BASE MACRO KVARS KMAC)
                             (:fasload BASE KVARS KMAC))

  (:compile-load-init PSEUDO (PP MACRO KMAC)
                             (:fasload PP LET LETS BASE MACRO
                                       KVARS KMAC KUTIL)
                             (:fasload BASE KVARS KMAC KUTIL))

  (:compile-load-init ZMAGIC (PP MACRO IMAC TV)
                             (:fasload PP LET LETS MACRO IMAC TV)
                             (:fasload BASE KVARS IMAC TV))

  (:compile-load-init COMMNT (PP MACRO)
                             (:fasload PP LET LETS MACRO KMAC ZMAGIC)
                             (:fasload BASE KVARS ZMAGIC))

  (:compile-load-init RDMAC  (PP MACRO)
                             (:fasload PP LET LETS MACRO KMAC ZMAGIC)
                             (:fasload BASE KVARS ZMAGIC))

  (:compile-load-init OUTIL  (PP MACRO RDMAC)
                             ((:fasload PP LET LETS BASE MACRO KVARS RDMAC)
                              (:readfile OUTIL))
                             (:fasload BASE KVARS))

  (:compile-load-init DTRACE (PP MACRO KMAC)
                             (:fasload PP LET LETS BASE MACRO KVARS KMAC)
                             (:fasload BASE KVARS KMAC))

  (:compile-load-init DMAC   (PP MACRO KMAC)
                             (:fasload PP LET LETS BASE MACRO KVARS KMAC)
                             (:fasload BASE KVARS KMAC))

  (:compile-load-init DVARS  (PP MACRO KMAC DMAC DTRACE)
                             (:fasload PP LET LETS BASE MACRO KVARS
                                       KMAC DMAC DTRACE)
                             (:fasload BASE KVARS KMAC DMAC))

  (:compile-load-init DUTIL  (PP MACRO KMAC)
                             (:fasload PP LET LETS BASE MACRO KVARS
                                       KMAC DMAC DVARS)
                             (:fasload BASE KVARS KMAC DVARS))

  (:compile-load-init DB     (PP MACRO KMAC RDMAC OUTIL DMAC DTRACE)
                             (:fasload PP LET LETS BASE MACRO KVARS KMAC
                                       OUTIL DMAC DVARS DTRACE RDMAC)
                             (:fasload BASE KVARS KMAC DVARS))

  (:compile-load-init GC     (PP MACRO KMAC OUTIL DMAC)
                             (:fasload PP LET LETS BASE MACRO KVARS
                                       KMAC OUTIL DMAC DVARS)
                             (:fasload BASE KVARS KMAC DVARS))

  (:compile-load-init DDEBUG (PP MACRO KMAC OUTIL DMAC DTRACE)
                             (:fasload PP LET LETS BASE MACRO KVARS
                                       KMAC OUTIL DMAC DVARS DTRACE)
                             (:fasload BASE KVARS KMAC DVARS))

  (:compile-load-init SURFACE (PP MACRO KMAC RDMAC OUTIL DMAC)
                              (:fasload PP LET LETS BASE MACRO KVARS KMAC
                                        RDMAC OUTIL DMAC DVARS DB DUTIL)
                              (:fasload BASE KVARS KMAC DVARS))

  (:compile-load-init EXPAND (PP MACRO KMAC RDMAC OUTIL DMAC SURFACE)
                             (:fasload PP LET LETS BASE MACRO KVARS
                                       KMAC RDMAC OUTIL DMAC DVARS DB
                                       DUTIL SURFACE)
                             (:fasload BASE KVARS KMAC DVARS))

  (:compile-load-init PRIMOP (PP MACRO KMAC RDMAC OUTIL
                              DMAC SURFACE EXPAND)
                             (:fasload PP LET LETS BASE MACRO KVARS KMAC
                                       RDMAC OUTIL DMAC DVARS DB
                                       DUTIL SURFACE EXPAND)
                             (:fasload BASE KVARS KMAC DVARS))

  (:compile-load-init DFLOW  (PP MACRO KMAC RDMAC OUTIL DMAC SURFACE
                                 EXPAND PRIMOP)
                             (:fasload PP LET LETS BASE MACRO KVARS KMAC
                                       RDMAC OUTIL DMAC DVARS DB
                                       DUTIL SURFACE EXPAND PRIMOP)
                             (:fasload BASE KVARS KMAC DVARS))

  (:compile-load-init FNS    (PP MACRO KMAC RDMAC OUTIL DMAC)
                             (:fasload PP LET LETS BASE MACRO KVARS KMAC
                                       RDMAC OUTIL DMAC DVARS DB DUTIL)
                             (:fasload BASE KVARS KMAC DVARS))

  (:compile-load-init BRIDGE  (PP MACRO KMAC DMAC)
                              (:fasload PP LET LETS BASE MACRO KVARS KMAC
                                        DMAC DVARS)
                              (:fasload BASE KVARS KMAC DVARS))

  (:compile-load-init ED0    (PP MACRO KMAC PSEUDO)
                             (:fasload PP LET LETS BASE MACRO
                                       KVARS KMAC PSEUDO BRIDGE)
                             (:fasload BASE KVARS KMAC PSEUDO BRIDGE
                                       COMMNT))

  (:compile-load-init ED1    (PP MACRO KMAC OUTIL DMAC RDMAC PSEUDO)
                             (:fasload PP LET LETS BASE MACRO KVARS KMAC
                                       OUTIL RDMAC DMAC DVARS DB DUTIL ED0)
                             (:fasload BASE KVARS KMAC DVARS KUTIL ED0))

  (:compile-load-init ED2    (PP MACRO KMAC OUTIL DMAC RDMAC PSEUDO ED1)
                             (:fasload PP LET LETS BASE MACRO KVARS KMAC
                                       OUTIL RDMAC DMAC DVARS DB
                                       ED1 DUTIL)
                             (:fasload BASE KVARS KMAC DVARS KUTIL ED1))

  (:compile-load-init ED3    (PP MACRO KMAC OUTIL DMAC RDMAC
                              PSEUDO ED1 ED2)
                             (:fasload PP LET LETS BASE MACRO KVARS KMAC
                                       OUTIL RDMAC DMAC DVARS DB
                                       PSEUDO ED2 DUTIL)
                             (:fasload BASE KVARS KMAC DVARS KUTIL
                                       PSEUDO ED2))

  (:compile-load-init ADA    (GPRINT PP MACRO RDMAC KMAC PSEUDO)
                             (:fasload GPRINT PP LET LETS BASE MACRO RDMAC
                                       KVARS KMAC PSEUDO)
                             (:fasload GPRINT PP BASE KVARS KMAC KUTIL
                                       PSEUDO))

  (:compile-load-init VU     (PP MACRO KMAC DMAC PSEUDO)
                             (:fasload PP LET LETS BASE MACRO KVARS
                                       KMAC DMAC DVARS DB DUTIL TV PSEUDO)
                             (:fasload BASE KVARS KMAC DVARS TV PSEUDO))

  (:compile-load-init PLI    (GPRINT PP MACRO KMAC DMAC VU)
                             (:fasload GPRINT PP LET LETS BASE MACRO KVARS
                                       KMAC DMAC DVARS DB DUTIL VU)
                             (:fasload GPRINT BASE KVARS KMAC DVARS))

  (:compile-load-init DEFLIB (PP MACRO KMAC OUTIL DMAC RDMAC ED1)
                             (:fasload PP LET LETS BASE MACRO KVARS KMAC
                                       OUTIL RDMAC DMAC DVARS DB DUTIL
                                       SURFACE EXPAND PRIMOP DFLOW
                                       ED1 ED2)
                             (:fasload BASE KVARS KMAC DVARS))

  (:compile-load-init KBELIB (PP MACRO KMAC OUTIL DMAC RDMAC ED1
                              DEFLIB COMMNT)
                             (:fasload PP LET LETS BASE MACRO KVARS KMAC
                                       OUTIL RDMAC DMAC DVARS DB DUTIL FNS
                                       SURFACE EXPAND PRIMOP DFLOW
                                       ED1 ED2 ED3 DEFLIB COMMNT)
                             (:fasload BASE KVARS KMAC DVARS ED1))

  (:compile-load-init DEMLIB (PP MACRO KMAC OUTIL DMAC RDMAC ED1
                              DEFLIB KBELIB  COMMNT)
                             (:fasload PP LET LETS BASE MACRO KVARS KMAC
                                       OUTIL RDMAC DMAC DVARS DB DUTIL FNS
                                       SURFACE EXPAND PRIMOP DFLOW
                                       ED1 ED2 ED3 DEFLIB KBELIB
                                       COMMNT)
                             (:fasload BASE KVARS KMAC DVARS ED1))

  (:compile-load-init DEMADALIB (PP MACRO KMAC OUTIL DMAC RDMAC ED1 ADA
                                 DEFLIB KBELIB  COMMNT)
                             (:fasload PP LET LETS BASE MACRO KVARS KMAC
                                       OUTIL RDMAC DMAC DVARS DB DUTIL FNS
                                       SURFACE EXPAND PRIMOP DFLOW
                                       ED1 ED2 ED3 ADA DEFLIB KBELIB
                                       COMMNT)
                             (:fasload BASE KVARS KMAC DVARS ED1 ADA))

  (:compile-load-init KAOS    (PP MACRO KMAC)
                              (:fasload PP LET LETS BASE MACRO KVARS KMAC)
                              (:fasload BASE KVARS KMAC))

  (:compile-load-init BELIEF  (PP MACRO IMAC BEHAVE)
                              (:fasload PP LET LETS BASE MACRO KVARS IMAC
                                        BEHAVE KUTIL)
                              (:fasload BASE KVARS IMAC KUTIL))

  (:compile-load-init SOCIETY (PP MACRO IMAC BEHAVE)
                              (:fasload PP LET LETS BASE MACRO KVARS
                                        IMAC BEHAVE KUTIL BELIEF)
                              (:fasload BASE KVARS IMAC BEHAVE KUTIL
                                        BELIEF))

  (:compile-load-init STNDRD  (PP MACRO IMAC BEHAVE TV PSEUDO)
                              (:fasload PP LET LETS BASE MACRO KVARS
                                        IMAC BEHAVE KUTIL BELIEF
                                        SOCIETY TV PSEUDO)
                              (:fasload BASE KVARS IMAC BEHAVE KUTIL BELIEF
                                        SOCIETY TV PSEUDO))

  (:compile-load-init REMOTE  (PP MACRO IMAC KAOS SOCIETY)
                              (:fasload PP LET LETS BASE MACRO KVARS IMAC
                                        KAOS SOCIETY STNDRD)
                              (:fasload BASE KVARS IMAC KAOS SOCIETY
                                        STNDRD))

  (:compile-load      SENSE)

  (:compile-load-init SIMPLE (PP MACRO IMAC BEHAVE SOCIETY SENSE)
                             (:fasload PP LET LETS BASE MACRO KVARS IMAC
                                       BEHAVE KUTIL SOCIETY STNDRD VU TV
                                       ZMAGIC SENSE)
                             (:fasload BASE KVARS IMAC SOCIETY STNDRD
                                       VU TV BEHAVE KUTIL BRIDGE ZMAGIC
                                       SENSE))

  (:compile-load-init MAGIC  (PP MACRO IMAC BEHAVE SIMPLE ED1 SOCIETY)
                             (:fasload PP LET LETS BASE MACRO KVARS IMAC
                                       BEHAVE KUTIL SIMPLE ED1
                                       SOCIETY STNDRD ZMAGIC)
                             (:fasload BASE KVARS IMAC BEHAVE KUTIL ED1
                                       SOCIETY STNDRD BRIDGE ZMAGIC))

  (:compile-load-init ZWEI   (PP MACRO IMAC BEHAVE SIMPLE SOCIETY
                                 MAGIC ZMAGIC DMAC STNDRD)
                             (:fasload PP LET LETS BASE MACRO KVARS IMAC
                                       SIMPLE SOCIETY STNDRD MAGIC
                                       ZMAGIC DMAC BEHAVE KUTIL)
                             (:fasload BASE KVARS IMAC SOCIETY STNDRD
                                       BRIDGE ZMAGIC BEHAVE KUTIL))

  (:compile-load-init CMDS   (PP MACRO IMAC PSEUDO BEHAVE SIMPLE SOCIETY
                                 MAGIC ZMAGIC DMAC ZWEI)
                             (:fasload PP LET LETS BASE MACRO KVARS IMAC
                                       PSEUDO SIMPLE SOCIETY STNDRD MAGIC
                                       ZMAGIC DMAC  BEHAVE KUTIL ZWEI)
                             (:fasload BASE KVARS IMAC PSEUDO SOCIETY
                                       STNDRD BRIDGE ZMAGIC BEHAVE KUTIL
                                       ZWEI))

  (:compile-load-init PARSE  (PP MACRO IMAC)
                             (:fasload PP LET LETS BASE MACRO KVARS IMAC)
                             (:fasload BASE KVARS IMAC))

  (:compile-load-init EXPLAIN (PP MACRO KMAC OUTIL DMAC PSEUDO)
                              (:fasload PP LET LETS BASE MACRO KVARS KMAC
                                        OUTIL DMAC DVARS DB DUTIL PSEUDO)
                              (:fasload BASE KVARS KMAC DVARS PSEUDO))

  (:compile-load-init HANDLERS (PP MACRO KMAC OUTIL DMAC)
                               (:fasload PP LET LETS BASE MACRO KVARS KMAC
                                         OUTIL DMAC DVARS DB DUTIL)
                               (:fasload BASE KVARS KMAC DVARS EXPLAIN))

  (:compile-load-init INIT   (PP MACRO KMAC)
                             (:fasload PP LET LETS BASE MACRO KVARS KMAC)
                             (:fasload BASE KVARS KMAC))

  (:compile-load-init DEBUG  (PP MACRO KMAC SOCIETY)
                             (:fasload PP LET LETS BASE MACRO
                                       KVARS KMAC SOCIETY)
                             (:fasload BASE KVARS KMAC)))
