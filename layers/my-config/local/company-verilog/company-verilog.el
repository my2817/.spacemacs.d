
(require 'company)
(require 'cl-lib)
(require 'company-keywords)

(defgroup company-verilog nil
  "Completion backend for CMake."
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
  "wire" "with" "within" "wor" "xnor" "xor" "$assertkill" "$assertoff"
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
  "$writeb" "$writeh" "$writememb" "$writememh" "$writeo" "accelerate"
  "autoexepand_vectornets" "begin_keywords" "celldefine" "default_decay_time"
  "default_nettype" "default_trieg_distributed" "default_trireg_strength"
  "define" "delay_mode_distributed" "delay_mode_path" "delay_mode_unit"
  "delay_mode_zero" "else" "elsif" "end_keywords" "endcelldefine" "endif"
  "endprotect" "endprotected" "expand_vectornets" "file" "ifdef" "ifndef"
  "include" "line" "noaccelerate" "noexpand_vectornets" "noremove_gatenames"
  "noremove_netnames" "nounconnected_drive" "pragma" "protect" "protected"
  "remove_gatenames" "remove_netnames" "resetall" "timescale"
  "unconnected_drive" "undef" "uselib")
  "A key-words list of verilog-mode "
  :group 'company-verilog
  )
(defcustom company-verilog-keywords-user nil
  "user define key words"
  :group 'company-verilog)

(defun company-verilog-update-keywords-user ()
  (custom-set-variables
   '(company-verilog-keywords-user
     '("class_uvm_component" "class_uvm_object"
       "AUTOINPUT" "AUTOOUTPUT" "AUTOWIRE" "AUTOREGINPUT" "AUTOARGS" "AUTORESET" "autotags" "verilog-library-flags")))
  )

(defcustom company-verilog-keywords nil
  "keywords for company-verilog "
  :group 'company-verilog
 )
(defun company-verilog-make-key-words ()
  "merge `company-verilog-keywords-ieee' and `company-verilog-keywords-user'"
  (company-verilog-update-keywords-user)
  (mapcar (lambda (keyword)
            (add-to-list 'company-verilog-keywords keyword))
          company-verilog-keywords-ieee)
  (mapcar (lambda (keyword)
            (add-to-list 'company-verilog-keywords keyword))
          company-verilog-keywords-user))

(defun company-verilog-backend (command &optional arg &rest ignored)
  "company backend for veriog"
  (interactive (list 'interactive))

  (cl-case command
    (interactive (company-begin-backend 'company-verilog-backend))
    (prefix (and (eq major-mode 'verilog-mode)
                 (company-grab-symbol)))
    (candidates
     (setq-local key-words company-verilog-keywords )
     (add-to-list 'key-words (format-time-string "%Y-%m-%d %H:%M:%S"))
     (cl-remove-if-not
      (lambda (c) (string-prefix-p arg c))
      ;; company-verilog-keywords
      key-words
      ))
    (post-completion
     ;; (expand-abbrev)
     (yas/expand)
     )))

(add-to-list 'company-keywords-alist (cons 'verilog-mode company-verilog-keywords))
;; (make-variable-buffer-local 'company-backends)

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
        (company-verilog-make-key-words)
        (make-local-variable 'company-backends)
        (setq company-backends
              '( company-files
                 (company-verilog-backend company-dabbrev-code company-gtags company-etags)
                 company-abbrev
                 ))
         (make-local-variable 'company-dabbrev-code-ignore-case)
         (setq company-dabbrev-code-ignore-case t)
         (make-local-variable 'company-dabbrev-code-everywhere)
         (setq company-dabbrev-code-everywhere t)
        )
    (setq company-dabbrev-code-ignore-case nil)
    ))
