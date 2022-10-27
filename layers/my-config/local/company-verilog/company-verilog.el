
(require 'company)
(require 'cl-lib)
(require 'company-keywords)

(defgroup company-verilog nil
  "Completion backend for verilog-mode."
  :group 'company)

(defcustom company-verilog-keywords-ieee
  '("alias" "always" "always_comb" "always_ff" "always_latch" "and" "assert"
  "assign" "assume" "automatic" "before" "begin" "bind" "bins" "binsof" "bit"
  "break" "buf" "bufif0" "bufif1" "byte" "case" "casex" "casez" "cell" "chandle"
  "class" "clocking" "cmos" "config" "const" "constraint" "context" "continue"
  "cover" "covergroup" "coverpoint" "cross" "deassign" "default" "defparam"
  "design" "disable" "dist" "do" "edge" "else" "end" "endcase" "endclass"
  "endclocking" "endconfig" "endfunction" "endgenerate" "endgroup"
  "endinterface" "endmodule" "endpackage" "endprimitive" "endprogram"
  "endproperty" "endsequence" "endspecify" "endtable" "endtask" "enum" "event"
  "expect" "export" "extends" "extern" "final" "first_match" "for" "force"
  "foreach" "forever" "fork" "forkjoin" "function" "generate" "genvar" "highz0"
  "highz1" "if" "iff" "ifnone" "ignore_bins" "illegal_bins" "import" "incdir"
  "include" "initial" "inout" "input" "inside" "instance" "int" "integer"
  "interface" "intersect" "join" "join_any" "join_none" "large" "liblist"
  "library" "local" "localparam" "logic" "longint" "macromodule" "matches"
  "medium" "modport" "module" "nand" "negedge" "new" "nmos" "nor"
  "noshowcancelled" "not" "notif0" "notif1" "null" "or" "output" "package"
  "packed" "parameter" "pmos" "posedge" "primitive" "priority" "program"
  "property" "protected" "pull0" "pull1" "pulldown" "pullup"
  "pulsestyle_ondetect" "pulsestyle_onevent" "pure" "rand" "randc" "randcase"
  "randsequence" "rcmos" "real" "realtime" "ref" "reg" "release" "repeat"
  "return" "rnmos" "rpmos" "rtran" "rtranif0" "rtranif1" "scalared" "sequence"
  "shortint" "shortreal" "showcancelled" "signed" "small" "solve" "specify"
  "specparam" "static" "string" "strong0" "strong1" "struct" "super" "supply0"
  "supply1" "table" "tagged" "task" "this" "throughout" "time" "timeprecision"
  "timeunit" "tran" "tranif0" "tranif1" "tri" "tri0" "tri1" "triand" "trior"
  "trireg" "type" "typedef" "union" "unique" "unsigned" "use" "var" "vectored"
  "virtual" "void" "wait" "wait_order" "wand" "weak0" "weak1" "while" "wildcard"
  "wire" "with" "within" "wor" "xnor" "xor" )
  "A key-words list of verilog-mode "
  :group 'company-verilog
  )
(defcustom company-verilog-buildin-task
  ;; buildin system tasks
  '("$assertkill" "$assertoff"
  "$asserton" "$async$and$array" "$async$and$plane" "$async$nand$array"
  "$async$nand$plane" "$async$nor$array" "$async$nor$plane" "$async$or$array"
  "$async$or$plane" "$bits" "$bitstoreal" "$bitstoshortreal" "$cast" "$comment"
  "$countdrivers" "$countones" "$date" "$dimensions" "$display" "$displayb"
  "$displayh" "$displayo" "$dist_chi_square" "$dist_erlang" "$dist_exponential"
  "$dist_normal" "$dist_poisson" "$dist_t" "$dist_uniform" "$dumpall"
  "$dumpfile" "$dumpflush" "$dumplimit" "$dumpoff" "$dumpon" "$dumpports"
  "$dumpportsall" "$dumpportsflush" "$dumpportslimit" "$dumpportsoff"
  "$dumpportson" "$dumpvars" "$enddefinitions" "$error" "$exit" "$fatal"
  "$fclose" "$fdisplay" "$fdisplayb" "$fdisplayf" "$fdisplayh" "$fdisplayo"
  "$fell" "$feof" "$ferror" "$fflush" "$fgetc" "$fgets" "$finish" "$fmonitor"
  "$fmonitorb" "$fmonitorf" "$fmonitorh" "$fmonitoro" "$fopen" "$fread"
  "$fscanf" "$fseek" "$fsscanf" "$fstrobe" "$fstrobeb" "$fstrobef" "$fstrobeh"
  "$fstrobeo" "$ftell" "$fullskew" "$fwrite" "$fwriteb" "$fwritef" "$fwriteh"
  "$fwriteo" "$get_coverage" "$getpattern" "$high" "$history" "$hold"
  "$increment" "$incsave" "$info" "$input" "$isunbounded" "$isunknown" "$itor"
  "$key" "$left" "$list" "$load_coverage_db" "$log" "$low" "$monitor"
  "$monitorb" "$monitorh" "$monitoro" "$monitoroff" "$monitoron" "$nochange"
  "$nokey" "$nolog" "$onehot" "$onehot0" "$past" "$period" "$print" "$q_add"
  "$q_exam" "$q_full" "$q_initialize" "$q_random" "$q_remove" "$random"
  "$readmemb" "$readmemh" "$realtime" "$realtobits" "$recovery" "$recrem"
  "$removal" "$reset" "$reset_count" "$reset_value" "$restart" "$rewind"
  "$right" "$root" "$rose" "$rtoi" "$sampled" "$save" "$scale" "$scope"
  "$sdf_annotate" "$set_coverage_db_name" "$setup" "$setuphold" "$sformat"
  "$sformatf" "$shortrealtobits" "$showscopes" "$showvariables" "$showvars"
  "$signed" "$size" "$skew" "$sreadmemb" "$sreadmemh" "$sscanf" "$stable"
  "$stime" "$stop" "$strobe" "$strobeb" "$strobeh" "$strobeo" "$swrite"
  "$swriteb" "$swriteh" "$swriteo" "$sync$and$array" "$sync$and$plane"
  "$sync$nand$array" "$sync$nand$plane" "$sync$nor$array" "$sync$nor$plane"
  "$sync$or$array" "$sync$or$plane" "$test$plusargs" "$time" "$timeformat"
  "$timescale" "$timeskew" "$typename" "$typeof" "$ungetc" "$unit"
  "$unpacked_dimensions" "$unsigned" "$upscope" "$urandom" "$urandom_range"
  "$value$plusargs" "$var" "$vcdclose" "$version" "$warning" "$width" "$write"
  "$writeb" "$writeh" "$writememb" "$writememh" "$writeo" "$fsdbAutoSwitchDumpfile" 
  ;; verdi3, FSDB Dumping Commands Used with Verilog
  "$fsdbDumpfile" "$fsdbDumpflush" "$fsdbDumpon", "$fsdbDumpoff" "$fsdbDumpvars" 
  "$fsdbDumpvarsByFile" "$fsdbDumpFinish" "$fsdbDumpMDA" "$fsdbDumpSVA" "$fsdbLog"
  "$fsdbLog" "$fsdbReplay" "$fsdbSuppress" "$fsdbSwitchDumpfile"  )
"Buildin task list of verilog-mode "
:group 'company-verilog
)
(defcustom company-verilog-buildin-macro
  ;; macro
  '("`accelerate"
  "`autoexepand_vectornets" "`begin_keywords" "`celldefine" "`default_decay_time"
  "`default_nettype" "`default_trieg_distributed" "`default_trireg_strength"
  "`define" "`delay_mode_distributed" "`delay_mode_path" "`delay_mode_unit"
  "`delay_mode_zero" "`else" "`elsif" "`end_keywords" "`endcelldefine" "`endif"
  "`endprotect" "`endprotected" "`expand_vectornets" "`file" "`ifdef" "`ifndef"
  "`include" "`line" "`noaccelerate" "`noexpand_vectornets" "`noremove_gatenames"
  "`noremove_netnames" "`nounconnected_drive" "`pragma" "`protect" "`protected"
  "`remove_gatenames" "`remove_netnames" "`resetall" "`timescale"
  "`unconnected_drive" "`undef" "`uselib")
  "Macro list of verilog-mode "
  :group 'company-verilog
  )
(defcustom company-verilog-keywords-user '("AUTOINPUT" "AUTOOUTPUT" "AUTOWIRE" "AUTOREGINPUT" "AUTOARG" "AUTORESET" "autotags" "verilog-library-flags")
  "user define key words"
  :group 'company-verilog)

(defun company-verilog-make-key-words ()
  "merge `company-verilog-keywords-ieee' and `company-verilog-keywords-user'"
  `(,@company-verilog-keywords-ieee ,@company-verilog-keywords-user)
)

(add-to-list 'company-keywords-alist (cons 'verilog-mode (company-verilog-make-key-words)))

(defun company-verilog-thing-at-point ()
  "Return thing at point

skip whitespace if there is, the whitespace maybe insert by `electric-spacing' after \"<=\"
"
  (let* ((b (save-excursion (skip-chars-backward " " (point-at-bol))
                            (skip-chars-backward "a-zA-Z0-9_$`<=" (point-at-bol)) (point)))
         (e (point))
         (thing (buffer-substring-no-properties b e)))
    ;; return nil if not Buildin task/macro, let choice next company-backend
    (and (> (length thing) 1)
         (or (string-equal (substring thing 0 1) "$")
             (string-equal (substring thing 0 1) "`")
             (string-equal (substring thing 0 1) "<"))

         (all-completions thing  `(,@company-verilog-buildin-task
                                   ,@company-verilog-buildin-macro
                                   ,(format "<= %s" verilog-assignment-delay)))
         ;; (cl-remove-if-not
         ;;  (lambda(c)(string-prefix-p thing c))
         ;;  `(,@company-verilog-buildin-task ,@company-verilog-buildin-macro) )
         thing
         )))

(defun company-verilog-buildin-task-backend (command &optional arg &rest ignored)
  "company backend for veriog"
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-verilog-buildin-task-backend))
    (prefix (and (eq major-mode 'verilog-mode)
                 ;; company needs backends that in same group return same prefix
                 ;; this backend remove "`" from symbol that what citre-lang-verilog-get-symbol does
                 (company-verilog-thing-at-point)
                 ))
    (candidates
     (all-completions arg  `(,@company-verilog-buildin-task
                             ,@company-verilog-buildin-macro
                             ,(format "<= %s" verilog-assignment-delay))))
    ;; (cl-remove-if-not
    ;;  (lambda(c)(string-prefix-p arg c))
    ;;  `(,@company-verilog-buildin-task ,@company-verilog-buildin-macro) ))

    (sorted t)
    (no-cache t)
    ))

;; (provide 'company-verilog)
;;;###autoload
(define-minor-mode company-verilog
  "a minor mode to handle company backend for verilog"
  :group 'company
  :init-value nil
  ;; :lighter "CompanyVerilog"
  :global nil
  :after-hook nil

  (if company-verilog
      (progn
        (make-local-variable 'company-backends)
        (setq company-backends
              '( 
                company-files
                company-verilog-buildin-task-backend
                ;; (company-verilog-backend company-dabbrev-code company-gtags company-etags)
                (company-capf company-yasnippet company-keywords company-dabbrev-code)
                ;; (company-capf company-yasnippet company-dabbrev-code)
                ;; company-abbrev
                ))
         (make-local-variable 'company-dabbrev-code-ignore-case)
         (setq company-dabbrev-code-ignore-case t)
         (make-local-variable 'company-dabbrev-code-everywhere)
         (setq company-dabbrev-code-everywhere t)
        )
    (setq company-dabbrev-code-ignore-case nil)
    ))
