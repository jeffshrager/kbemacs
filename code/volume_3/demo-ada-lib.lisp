;;; -*- Mode:LISP; Package:KBE; Base:10; Fonts:MEDFNB; -*-
;;; Source: PDFs/Knowledge-Based_Editor_Jan-1985_volume_3.pdf
;;; Listing: OZ:<KBE.SOURCE>DEMO-ADA-LIB.LISP
;;; PDF pages 249-263 (printed source pages 1-15).

;;; (c) Copyright 1984, Massachusetts Institute of Technology

;this makes the lisp constraint things available for ada
;we have several cludges here because {} is exactly the same in lisp
;and so cannot be nested in an ada-guy unless he always knows it will.
(eval-when (eval load compile) ;because need ada fn defs to parse
    (def-ada (display_report) () nil (display_report))
    (defun display_report ()
  (with-open-file (file "report.txt" ':in)
    (do ((char (tyi file nil) (tyi file nil))
         (newlines 1))
        (nil)
      (do () ((not (eq char #\newline)))
        (incf newlines)
        (setq char (tyi file nil)))
      (cond ((= newlines 1) (terpri))
            ((= newlines 2) (terpri) (terpri))
            ((> newlines 2)
             (format T "~%<~2D blank lines>~%" (1- newlines))))
      (setq newlines 0)
      (if (null char) (return))
      (tyo char)))
  (values))
    (def-ada (rename) ((old (ada-string)) (new (ada-string))) nil
  (rename-all old new) )
    (def-ada (derived) ((x (any)) (y (any))) nil
  (derived |X-full| y))
    (def-ada (default) ((x (any)) (y (any))) nil
  (default |X-full| y))
    (def-ada (FILE_NAME_ROOT) ((thing (any))) (ada-string)
  (file-name-root |THING-full|))
;nausiating kludge for now.  Should really look up and figure
;out what kinds of records are in the file.
    (def-ada (RECORD_KEY_ROOT) ((thing (any))) (ada-string)
  (record_key_root-stub |THING-full|))
    (defun record_key_root-stub (ignore) "DEFECT_KEY")
    (def-ada (size_in_lines) ((x (any))) (integer)
  (ada-line-size |X-full|))
    (defun ada-line-size (segcase)
  (declare (special total-size))
  (if (not segcase) (*throw '2constraint-abort nil))
    (let ((total-size 0))
    (cond ((not-implemented? (~seg segcase)) 1)
          (T (mapcar #'ada-line-size-0 (all-subsegs (~seg segcase)))
             total-size))))
    (defun ada-line-size-0 (seg)
  (declare (special total-size))
   (when (??& (&* seg) @seg 'funcall 'ada-fn~new_line)
     (let* ((code (car (?@seg-code seg)))
            (numeric-arg (caddr code)))
       (if (listp numeric-arg) (setq numeric-arg (cadr numeric-arg)))
       (if (not (numberp numeric-arg)) (setq numeric-arg 1))
       (setq total-size (+ total-size numeric-arg)))))
 (def-ada (corresponding_file_name) ((thing (any))) (any)
;;; PDF page 250

  (corresponding-file-name |THING-full|))
    (defun corresponding-file-name (segcase)
  (let (root file-info)
    (if (not (and segcase
                  (setq root (file-name-root segcase))
                  (setq file-info (get-ada-file-info root))))
        (*throw '2constraint-abort nil))
    (ada-file-name-variable file-info)))
    (def-ada (corresponding_headings) ((thing (any))) (any)
  (corresponding-headings |THING-full|))
    (defun corresponding-headings (segcase)
  (let (cliche-name corresponding-name)
    (if (not (and segcase
                  (setq cliche-name (~01 ~clichename (??& segcase cliche)))
                  (setq corresponding-name
                        (symbolconc cliche-name "_HEADINGS") )
                  (get corresponding-name 'cliche-plan)))
        (*throw '2constraint-abort nil))
    (& (2get-cliche corresponding-name))))
;;; PDF page 251

    (def-ada (chain_field) ((ff (any)) (tf (any))) (any)
 (chain-field  |FF-full| |TF-full|))
    (defun chain-field (from-file to-file)
  (let (from-file-name to-file-name from-file-info to-file-info
        to-key-type field-name)
    (if (not (and from-file to-file
                  (setq from-file-name (file-name-root from-file))
                  (setq from-file-info (get-ada-file-info from-file-name) )
                  (setq to-file-name (file-name-root to-file))
                  (setq to-file-info (get-ada-file-info to-file-name) )
                  (setq to-key-type
                        (ncons (ada-file-key-type to-file-info)))
                  (setq field-name
                        (dolist (field (cdr (ada-file-record-def
                                              from-file-info)))
                          (if (equal (cadr field) to-key-type)
                              (return (car field)))))))
        (*throw '2constraint-abort nil))
    ;note how we turn it into a one arg thing here.
    `(ada-select #{the input rec} ,field-name)))

    (def-ada (corresponding_reporting) ((thing (any))) (any)
  (corresponding-reporting |THING-full|))
    (defun corresponding-reporting (segcase)
  (let (cliche-name record-read corresponding-name)
    (if (not (and segcase
                  (setq cliche-name (~01 ~clichename (??& segcase cliche)))
                  (not (eq cliche-name 'pseudo-subcliche) )
                  (setq record-read (determine-record-enumerated segcase))
                  (setq corresponding-name
                        (symbolconc "PRINT_" record-read) )
                  (get corresponding-name 'cliche-plan)))
        (*throw '2constraint-abort nil))
    (& (2get-cliche corresponding-name) )))
    (defun determine-record-enumerated (segcase)
  (let (outroles source record-name)
    (if (not (and segcase
                  (setq outroles (??& segcase slot * * 'output))
                  (null (cdr outroles))
                  (setq source (dflow-source
                                 (?& (~slotseg (car outroles)) input)))
                   (setq record-name (~01 ~varname (??& source @createvar)))
                   (symbolp record-name)) )
        (*throw  '2constraint-abort nil))
     record-name) )
;;; PDF page 252

    (defmacro def-ada-text-cliche (string)
 (ada-parse string))
    (def-ada (SUGGEST_VARIABLE_NAME) ((thing (any))) (ada-string)
  (suggest-variable-name |THING-full|))
    (defun suggest-variable-name (segcase)
  (let (outputs source var-name)
    (when (not (and segcase
                    (setq outputs (??& segcase output))
                    (null (cdr outputs) )
                    (setq source (dflow-source (car outputs)))
                    (setq var-name
                          (3make-up-name-for-returnval
                            (~seg~segcase source)))))
      (*throw '2constraint-abort nil))
    var-name) )
;nausiating kludge for now.  Should really Jook up and figure
;out what kinds of records are in the file.  This would
;in fact be easier if we were making the whole print-summary
;cliche new each time.
    (def-ada (CORRESPONDING_SUMMARIZED_FILE) ((thing (any))) (ada-string)
  (corresponding_summarized_file-stub |THING-full|))
    (defun corresponding_summarized_file-stub (ignore)
  'DEFECTS)
 ) ;end of eval-when needed for parsing of ada cliches
;This reads everything up to the next $ (dollar sign) which is the
;last character on a line and returns it as one huge string.
(eval-when (eval compile load)
    (defun read-literal-string (ignore stream)
  (let ((lines nil))
    (do ((line (readline stream nil) (readline stream nil)))
        ((null line))
      (when (and (not (string-equal line "") )
                 (= (aref line (1- (string-length line))) #/$))
        (push (substring line 0 (1- (string-length line))) lines)
        (return))
      (push line lines)
      (push #\return lines))
    (apply #'string-append (nreverse lines))))
    (set-syntax-/#-macro-char #/$ 'read-literal-string) ) ;dollar sign
(def-ada-text-cliche #$
cliche PRE_LOOP_COMPUTATION is
   primary roles ITEM;
   described roles ITEM;
   comment "makes the value {the item} available inside of a loop";
   constraints
      RENAME( "DATUM", SUGGEST_VARIABLE_NAME({the item}));
   end constraints;
   DATUM: {};
begin
   DATUM  := {the item};
    loop
       {DATUM, the output datum};
    end loop;
 end PRE_LOOP_COMPUTATION;$
)
;;; PDF page 253

;note it is important that-no cliche name is the same as a role name.
;note we cannot make {the data} really be a role because it ends
;up inside of a quoted list due to the way we handle ada.
;we can have the following so that it looks like a role however.
;a gross cludge to. be sure.
(def-ada-text-cliche #$
with FUNCTIONS;
use FUNCTIONS;
cliche CHAIN_FILE_DEFINITION is
   primary roles FILE_NAME;
   described roles FILE_NAME;
   comment "defines a file named {the file_name} of chain records";
   constraints
      RENAME("DATA_RECORD", FILE_NAME_ROOT({the file_name}));
   end constraints;
   DATA_RECORDS_NAME: constant STRING := {the file_name};
   subtype DATA_RECORD_INDEX_TYPE is INDEX_TYPE;      :
   type DATA_RECORD_TYPE is
      record
         {the data};
         NEXT: DATA_RECORD_INDEX_TYPE;
      end record;
   package DATA_RECORD_IO is
      new CHAINED_IO(DATA_RECORD_TYPE, DATA_RECORD_INDEX_TYPE) ;
   DATA_RECORDS: DATA_RECORD_IO.FILE_TYPE;
end CHAIN_FILE_DEFINITION;$
)
;The fake role key_length is the same kind of cludge as type above.
(def-ada-text-cliche #$
with FUNCTIONS;
use FUNCTIONS;
cliche KEYED_FILE_DEFINITION is
   primary roles FILE_NAME;
   described roles FILE_NAME;
   comment "defines a keyed file named {the file_name} of records";
   constraints
      RENAME("DATA_RECORD", FILE_NAME_ROOT({the file_name}));
   end constraints;
   DATA_RECORDS_NAME: constant STRING := {the file_name};
   subtype DATA_RECORD_KEY_TYPE is STRING(1..{the key_length})
   type DATA_RECORD_TYPE is
      record
         {the data};
      end record;
   package DATA_RECORD_IO is new KEYED_IO(DATA_RECORD_TYPE, DATA_RECORD_KEY_TYPE) ;
   DATA_RECORDS: DATA_RECORD_IO.FILE_TYPE;
end KEYED_FILE_DEFINITION;$
)
;;; PDF page 254

;you must give this cliche an argument in order for the renaming
;to work. (Also setq of FILE is out of order in that case.)
;note that FILE must not get renamed, or the dflow will not be right.
;you cannot call the record RECORD because that is an ADA keyword
;you cannot call it REC because that is a KBE keyword.
;all of the above  is true for most of the cliches below.
(def-ada-text-cliche #$
cliche READ is
   primary roles FILE, KEY;
   described roles FILE, KEY;
   comment "reads the record indexed by {the key} from {the file}";
   constraints
      RENAME("DATA_RECORD", FILE_NAME_ROOT({the file}));
      DEFAULT({the file_name}, CORRESPONDING_FILE_NAME({the file}));       :
   end constraints;
   DATA_RECORD: {};
   FILE: {};
begin
   FILE := {the file};
   OPEN(FILE, IN_FILE, {the file_name});
   READ(FILE, DATA_RECORD, {the key});
   {DATA_RECORD, the  output data_record};
   CLOSE(FILE);
exception
   when DEVICE_ERROR | END_ERROR | NAME_ERROR | STATUS_ERROR =>
      CLOSE(FILE); PUT("Data Base Inconsistent");
   when others => CLOSE(FILE); raise;
end READ;$
)
;;; PDF page 255

(def-ada-text-cliche #$
with TEXT_IO;
use TEXT_IO;
cliche QUERY_USER_FOR_KEY is
  primary  roles FILE;
   described roles FILE;
   comment "queries the user for a key to a record in {the file}";
   constraints
      RENAME ("DATA_RECORD", FILE_NAME_ROOT({the file}));
      DEFAULT({the file_name}, CORRESPONDING_FILE_NAME({the file}));
   end constraints;
   DATA_RECORD: {};
   DATA_RECORD_KEY: {};
   FILE: {};
begin
   FILE := {the file};
   OPEN(FILE, IN_FILE, {the file_name});
   loop
      begin       :
         NEWLINE;  PUT("Enter DATA_RECORD Key: "); GET(DATA_RECORD_KEY) ;
         READ(FILE, DATA_RECORD, DATA_RECORD_KEY) ;
         exit;
      exception
         when END_ERROR => PUT("Invalid DATA_RECORD Key"); NEWLINE;
      end;
   end loop;
   CLOSE(FILE);
   return DATA_RECORD_KEY;
exception
   when DEVICE_ERROR | END_ERROR | NAME_ERROR | STATUS_ERROR =>
      CLOSE(FILE); PUT("Data Base Inconsistent");
   when others => CLOSE(FILE); raise;
end QUERY_USER_FOR_KEY; $
)
;;; PDF page 256

(def-ada-text-cliche #$
cliche FILE_ENUMERATION is
  primary roles FILE;
  described roles FILE;
  comment "enumerates the records in {the file}";
  constraints
     RENAME("DATA_RECORD", FILE_NAME_ROOT({the file}));
     DEFAULT({the file_name}, CORRESPONDING_FILE_NAME({the file}));
  end constraints;
   FILE: {};
   DATA_RECORD: {};
begin
   FILE := {the file};
   OPEN(FILE, IN_FILE, {the file_name});
   while not END_OF_FILE(FILE) loop
      READ( FILE, DATA_RECORD);
      {DATA_RECORD, the output data_record};
   end loop;
   CLOSE(FILE);
exception
   when DEVICE_ERROR | END_ERROR | NAME_ERROR | STATUS_ERROR =>
      CLOSE(FILE); PUT("Data Base Inconsistent");
   when others => CLEAN_UP; raise;
end FILE_ENUMERATION;$
)
;;; PDF page 257

(def-ada-text-cliche #$
cliche CHAIN_ENUMERATION is
   primary roles MAIN_FILE, CHAIN_FILE, MAIN_FILE_KEY;
   described roles MAIN_FILE, CHAIN_FILE, MAIN_FILE_KEY;
   comment "enumerates the chain records in {the chain_file} starting
            from the header record indexed by {the main_file_key}";
   constraints
      RENAME("MAIN_RECORD", FILE_NAME_ROOT({the main_file}));
      RENAME( "CHAIN_RECORD", FILE_NAME_ROOT({the chain_file}));
      DEFAULT({the main_file_name},
              CORRESPONDING_FILE_NAME({the main_file}));
      DEFAULT({the chain_file_name},
              CORRESPONDING_FILE_NAME({the chain_file}));
      DEFAULT({the main_file_chain_field},
CHAIN_FIELD({the main_file}, {the chain_file}));
      DEFAULT({the chain_file_chain_field},
              CHAIN_FIELD({the chain_file}, {the chain_file}));
   end constraints;
   CHAIN_FILE: {};
   CHAIN_RECORD: {};
   CHAIN_RECORD_INDEX: {};
   MAIN_FILE: {};
   MAIN_RECORD: {};
   procedure CLEAN_UP is
   begin
      CLOSE(CHAIN_FILE); CLOSE(MAIN_FILE);
   exception
      when STATUS_ERROR => return;
   end CLEAN_UP;
begin
   CHAIN_FILE  := {the chain_file};
   MAIN_FILE  := {the main_file};
   OPEN(CHAIN_FILE,  IN_FILE, {the chain_file_name});
   OPEN(MAIN_FILE,  IN_FILE, {the main_file_name});
   READ(MAIN_FILE, MAIN_RECORD, {the main_file_key});
   CHAIN_RECORD_INDEX  := {the main_file_chain_field}(MAIN_RECORD) ;
   while  not NULL_INDEX(CHAIN_RECORD_INDEX) loop
       READ(CHAIN_FILE, CHAIN_RECORD, CHAIN_RECORD_INDEX) ;
       {CHAIN_RECORD, the output chain_record};
       CHAIN_RECORD_INDEX := {the chain_file_chain_field}(CHAIN_RECORD);
    end loop;
    CLEAN_UP ;
 exception
    when DEVICE_ERROR | END_ERROR | NAME_ERROR | STATUS_ERROR =>
       CLEAN_UP; PUT("Data Base Inconsistent");
    when others => CLEAN_UP; raise;
 end CHAIN_ENUMERATION; $
 )
;;; PDF page 258

(def-ada-text-cliche #$
with FUNCTIONS;
use FUNCTIONS;
cliche ALL_CHAINS_ENUMERATION is
   primary roles MAIN_FILE, CHAIN_FILE;
   described roles MAIN_FILE, CHAIN_FILE;
   comment "enumerates the header records in {the main_file} and
            enumerates the chain records in {the chain_file} starting
            from each header record";
   constraints
      RENAME( "MAIN_RECORD", FILE_NAME_ROOT({the main_file}));
      RENAME("CHAIN_RECORD", FILE_NAME_ROOT({the chain_file}));
      DEFAULT({the main_file_name},
              CORRESPONDING_FILE_NAME({the main_file}));
      DEFAULT({the chain_file_name},
              CORRESPONDING_FILE_NAME({the chain_file}));
      DEFAULT({the main_file_chain_field},
              CHAIN_FIELD({the main_file}, {the chain_file}));
      DEFAULT({the chain_file_chain_field},
              CHAIN_FIELD({the chain_file}, {the chain_file}));
   end constraints;
   CHAIN_FILE: {};
   CHAIN_RECORD: {};
   CHAIN_RECORD_INDEX: {};
   MAIN_FILE: {};
   MAIN_RECORD: {};
   procedure CLEAN_UP is
   begin
      CLOSE(CHAIN_FILE); CLOSE(MAIN_FILE);
   exception
      when STATUS_ERROR => return;
   end CLEAN_UP;
begin
   CHAIN_FILE  := {the chain_file};
   MAIN_FILE  := {the main_file};
   OPEN(CHAIN_FILE, IN_FILE, {the chain_file_name});
   OPEN(MAIN_FILE, IN_FILE, {the main_file_name});
   CHAIN_RECORD_INDEX  := NULL_INDEX;
   loop
      while NULL_INDEX(CHAIN_RECORD_INDEX) and not END_OF_FILE(MAIN_FILE) loop
         READ(MAIN_FILE, MAIN_RECORD) ;
         CHAIN_RECORD_INDEX  := {the main_file_chain_field}(MAIN_RECORD) ;
      end  loop;
      exit when NULL_INDEX(CHAIN_RECORD_INDEX) ;
      READ(CHAIN_FILE, CHAIN_RECORD, CHAIN_RECORD_INDEX) ;
       {CHAIN_RECORD, the output chain_record};
      CHAIN_RECORD_INDEX  := {the chain_file_chain_field}(CHAIN_RECORD) ;
   end  loop;
   CLEAN_UP;
exception
   when DEVICE_ERROR  | END_ERROR | NAME_ERROR | STATUS_ERROR =>
CLEAN_UP;  PUT("Data Base Inconsistent");
   when others  => CLEAN_UP; raise;
 end ALL_CHAINS_ENUMERATION;$
 )
;;; PDF page 259

(def-ada-text-cliche #$
with FUNCTIONS, TEXT_IO;
use FUNCTIONS, TEXT_IO;
cliche SIMPLE_REPORT is
   primary roles ENUMERATOR, PRINT_ITEM, EPILOG;
   described roles FILE_NAME, TITLE, ENUMERATOR, COLUMN_HEADINGS,
                   PRINT_ITEM, EPILOG;
   comment "prints a report of {the input structure of the enumerator}";
   constraints
      DEFAULT({the file_name}, "report.txt");
      DERIVED({the line_limit},
              65-SIZE_IN_LINES({the print_item})-SIZE_IN_LINES({the epilog}));
      DEFAULT({the print_item}, CORRESPONDING_REPORTING({the enumerator}));
      DEFAULT({the column_headings},
              CORRESPONDING_HEADINGS({the print_item}));
   end constraints;
   use INT_IO;
   CURRENT_DATE: constant STRING := FORMAT_DATE(CLOCK)
   DATA: {};
   TITLE: STRING(1..{});
   procedure CLEAN_UP is
   begin
      SET_OUTPUT(STANDARD_OUTPUT) ;
      CLOSE (REPORT) ;
   exception
      when STATUS_ERROR => return;
   end CLEAN_UP;
begin
   CREATE(REPORT, OUT_FILE, {the file_name});
   DATA := {the input structure of the enumerator};
   SET_OUTPUT (REPORT) ;
   TITLE := {the title};
   NEW_LINE(4); SET_COL(20); PUT(CURRENT_DATE); NEW_LINE(2);
   SET_COL(13); PUT(TITLE); NEW_LINE(60);
   while not {the empty_test of the enumerator}(DATA) loop
      if LINE > {the line_limit} then
         NEW_PAGE; NEW_LINE; PUT("Page: "); PUT(INTEGER(PAGE-1), 3);
         SET_COL(13); PUT(TITLE);
         SET_COL(61); PUT(CURRENT_DATE); NEW_LINE(2);
         {the column_headings}({CURRENT_OUTPUT, modified});
      end if;
      {the print_item}({CURRENT_OUTPUT, modified},
                       {the element_accessor of the enumerator}(DATA));
      DATA  := {the step of the enumerator}(DATA);
   end loop;
   {the epilog};
   CLEAN_UP;
exception
   when DEVICE_ERROR  | END_ERROR | NAME_ERROR | STATUS_ERROR =>
      CLEAN_UP; PUT("Data Base Inconsistent");
   when others => CLEAN_UP; raise;
end SIMPLE_REPORT;$
)
;;; PDF page 260

(def-ada-text-cliche #$
with DIRECT_IO;
cliche FILE_SELECTION is
   primary roles SOURCE_FILE;
   described roles SOURCE_FILE, SELECTION_TEST, RECORD_KEY_ACCESSOR;
   comment "creates a file containing selected record keys for
            {the source_file}";
   constraints
      RENAME("RECORD_KEY", RECORD_KEY_ROOT({the source_file}));
   end constraints;
   package SELECTION_IO is new DIRECT_IO(RECORD_KEY_TYPE) ;
   use SELECTION_IO;
   SELECTIONS: SELECTION_IO.FILE_TYPE;
   DATA: {};
   DATUM: {};
begin
   DATA  := {the source_file};
   CREATE(SELECTIONS, INOUT_FILE);
   while not  {the empty_test of the enumerator}(DATA) loop
      DATUM  := {the element_accessor of the enumerator}(DATA);
       if {the selection_test}(DATUM) then
         WRITE(SELECTIONS, {the record_key_accessor}(DATUM) );
      end  if;
      DATA  := {the step of the enumerator}(DATA);
   end  loop;
    {SELECTIONS, the output selection_file};
   CLOSE(SELECTIONS) ;
 exception
   when DEVICE_ERROR  | END_ERROR | NAME_ERROR | STATUS_ERROR =>
       CLOSE(SELECTIONS); PUT("Data Base Inconsistent");
   when others  => CLOSE(SELECTIONS); raise;
 end FILE_SELECTION;$
 )
;;; PDF page 261

;note you cannot let the input role source_file get renamed.
;to have source_record as a prefix whether you Vike it or not.
(def-ada-text-cliche #$
with DIRECT_IO;
cliche FILE_SUMMARIZATION is
   primary roles SOURCE_FILE;
   described roles SOURCE_FILE;
   comment "creates a file summarizing the frequency of occurance of
            the record keys stored in {the source file}";
   constraints
      RENAME ("SOURCE_RECORD", FILE_NAME_ROOT({the source_file}));
      RENAME("RECORD_KEY", RECORD_KEY_ROOT({the source_file}));
      DEFAULT({the source_file_name},
              CORRESPONDING_FILE_NAME({the source_file}));
   end constraints;
   type SUMMARY_TYPE is
      record
         COUNT: INTEGER;
         KEY: RECORD_KEY_TYPE;
      end record;
   package SUMMARY_IO is new DIRECT_IO(SUMMARY_TYPE) ;
   use SUMMARY_IO;
   SUMMARIES: SUMMARY_IO.FILE_TYPE;
   SOURCE_FILE: {};
   function BUILD_SUMMARY(COUNT: INTEGER; KEY: RECORD_KEY_TYPE)
                            return SUMMARY_TYPE  is
      begin return SUMMARY_TYPE'(COUNT, KEY); end BUILD_SUMMARY ;
   function SUMMARY_GREATER_THAN(X: SUMMARY_TYPE; Y: SUMMARY_TYPE)
                                   return BOOLEAN is
      begin return X.COUNT > Y.COUNT; end SUMMARY_GREATER_THAN;
   procedure SORT_SOURCE_RECORDS is
      new SORT_FILE(RECORD_KEY_TYPE, SOURCE_RECORD_IO.FILE_TYPE,
                    SOURCE_RECORD_IO.POSITIVE_COUNT) ;
   procedure SORT_SUMMARIES is
      new SORT_FILE(SUMMARY_TYPE, SUMMARY_IO.FILE_TYPE,
                    SUMMARY_IO.POSITIVE_COUNT, SUMMARY_GREATER_THAN) ;
   procedure SUMMARIZE_SOURCE_RECORDS  is
      new SUMMARIZE_FILE(RECORD_KEY_TYPE, SOURCE_RECORD_IO.FILE_TYPE,        :
                         SUMMARY_TYPE, SUMMARY_IO.FILE_TYPE, BUILD_SUMMARY) ;
   procedure CLEAN_UP is
      begin
         CLOSE(SOURCE_FILE); CLOSE(SUMMARIES) ;
      exception
         when STATUS_ERROR =>  return;
      end CLEAN_UP;
begin
   SOURCE_FILE  := {the source file};
   OPEN(SOURCE_FILE, INOUT_FILE, {the  source_file_name});
   CREATE(SUMMARIES, INOUT_FILE);
   SORT_SOURCE_RECORDS (SOURCE_FILE) ;
   SUMMARIZE_SOURCE_RECORDS(SOURCE_FILE, SUMMARIES);
   SORT_SUMMARIES( SUMMARIES) ;
   {SUMMARIES, the output summaries};
   CLEAN_UP ;
exception
   when DEVICE_ERROR  | END_ERROR | NAME_ERROR | STATUS_ERROR =>
      CLEAN_UP; PUT("Data Base  Inconsistent");
   when others  => CLEAN_UP; raise;
end FILE_SUMMARIZATION;$
 )
;;; PDF page 262

;These could be automatically generated for each particlar kind
;of summary file, but we just use a predefined general one for  simplicity.
(def-ada-text-cliche #$
with TEXT_IO;
use TEXT_IO;      :
cliche PRINT_SUMMARY_HEADINGS is
   comment "prints headings for printing repair  records";
begin
  PUT("  # Key    Name"); NEW_LINE(2);
end PRINT_SUMMARY_HEADINGS ;$
)
;note 'that you cannot have a variable name the same as the name of
;a function becuase my parser will parse  it as a zero arg fn call.
;we have to use the big expression for rename  because renames
;actually must happen first.
(def-ada-text-cliche #$
with TEXT_IO;
use TEXT_IO;
cliche PRINT_SUMMARY  is
   primary roles SUMMARY;
   described roles SUMMARY, SUMMARIZED_FILE;
   comment "prints out  the fields of {the summary}";
   constraints
      RENAME("SUMMARIZED_RECORD",
              FILE_NAME_ROOT (CORRESPONDING_SUMMARIZED_FILE({the summary})));
      DEFAULT({the summarized_file},
              CORRESPONDING_SUMMARIZED_FILE({the  summary}));
      DEFAULT({the  summarized_file_name},
              CORRESPONDING_FILE_NAME({the  summarized_file}));
   end constraints;
   SUMMARIZED_FILE:  {};
   SUMMARIZED_RECORD:  {};
   SUMMARY:  {};
begin
   SUMMARIZED_FILE := {the summarized_file};
   OPEN(SUMMARIZED_FILE,  IN_FILE, {the summarized_file_name});
   SUMMARY  := {the input summary};
   READ(SUMMARIZED_FILE,  SUMMARIZED_RECORD, SUMMARY. KEY);
   PUT(SUMMARY.COUNT,  3); SET_COL(5); PUT(SUMMARY.KEY);
   SET_COL(12);  PUT(SUMMARIZED_RECORD.NAME); NEWLINE;
   CLOSE (SUMMARIZED_FILE);
exception
   when DEVICE_ERROR  | END_ERROR | NAME_ERROR | STATUS_ERROR =>
      CLOSE(SUMMARIZED_FILE);  PUT("Data Base Inconsistent");
   when others  => CLOSE(SUMMARIZED_FILE); raise;
end  PRINT_SUMMARY ;$
 )
;;; PDF page 263

 ;these cliches are created during the demo, but are defined
 ;here to make testing easier.
(def-ada-text-cliche #$
 with TEXT_IO;
 use TEXT_IO;
 cliche PRINT_REPAIR_HEADINGS is
    comment "prints headings for printing repair records"; - .   E        eo
 begin
   PUT("   Date     Defect   Description/Comment"); NEW_LINE(2);
 end PRINT_REPAIR_HEADINGS;$
 )
(def-ada-text-cliche #$
with FUNCTIONS, MAINTENANCE_FILES, TEXT_IO;
 use FUNCTIONS, MAINTENANCE_FILES, TEXT_IO;
 cliche PRINT_REPAIR is
    primary roles REPAIR;
    described roles REPAIR;
    comment "prints out the fields of {the repair}";        see eS
    use DEFECT_IO;
    DEFECT: DEFECT_TYPE;
    REPAIR: REPAIR_TYPE;
 begin
    OPEN(DEFECTS, IN_FILE, DEFECTS_NAME);
    REPAIR := {the input repair};
    READ(DEFECTS, DEFECT, REPAIR.DEFECT);
    PUT(FORMAT_DATE(REPAIR.DATE));
    SET_COL(13); PUT(REPAIR.DEFECT) ;
    SET_COL(20); PUT(DEFECT.NAME); NEWLINE;
    SET_COL(22); PUT(REPAIR.COMMENT); NEWLINE;
    CLOSE(DEFECTS);
 exception
    when DEVICE_ERROR | END_ERROR | NAME_ERROR  | STATUS_ERROR =>
       CLOSE(DEFECTS); PUT("Data Base Inconsistent");
    when others => CLOSE(DEFECTS); raise;
 end PRINT_REPAIR;$
 )
