;;; tcl-dc-mode.el --- extension to Tcl mode for editing Synopsys DC scripts

;; Copyright (C) 2006, 2007 Reto Zimmermann

;; Author:      Reto Zimmermann <reto@gnu.org>
;; Maintainer:  Reto Zimmermann <reto@gnu.org>
;; RCS:         $Id: tcl-dc-mode.el,v 1.5 2008/08/11 15:23:54 reto Exp reto $
;; Keywords:    languages tcl dc_shell
;; WWW:		http://www.iis.ee.ethz.ch/~zimmi/emacs/tcl-dc-mode.html

(defconst tcl-dc-version "1.2"
  "Tcl DC Mode version number.")

(defconst tcl-dc-time-stamp "2008-08-11"
  "Tcl DC Mode time stamp for last update.")

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This package provides an extension to Tcl major mode for editing scripts for
;; Synopsys' Design Compiler Tcl shell (dc_shell-t).
;; It adds the following features:

;;   - Syntax highlighting
;;   - Word/command completion
;;   - Block commenting

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Documentation

;; See comment string of function `tcl-dc-mode' or type `C-c C-h' in Emacs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Installation:

;; Prerequisites:  GNU Emacs 20.X/21.X, XEmacs 20.X/21.X

;; Put `tcl-dc-mode.el' into the `site-lisp' directory of your Emacs
;; installation or into an arbitrary directory that is added to the load path
;; by the following line in your Emacs start-up file (`.emacs'):

;;   (setq load-path (cons (expand-file-name "<directory-name>") load-path))

;; If you already have the compiled `tcl-dc-mode.elc' file, put it in the same
;; directory.  Otherwise, byte-compile the source file:
;;   Emacs:  M-x byte-compile-file  ->  tcl-dc-mode.el
;;   Unix:   emacs -batch -q -no-site-file -f batch-byte-compile tcl-dc-mode.el

;; Add the following lines to the `site-start.el' file in the `site-lisp'
;; directory of your Emacs installation or to your Emacs start-up file
;; (`.emacs'):

;;   (autoload 'tcl-dc-mode "tcl-dc-mode" "Tcl DC Mode" t)
;;   (add-to-list 'auto-mode-alist '("\\.tcl\\'" . tcl-dc-mode))

;; In the last line the pattern for matching a file name or file extension can
;; be changed to any naming convention for DC script files.  Also, the last
;; line can be omitted and Tcl DC Mode be automatically invoked only for files
;; that have the following pattern on the first line:

;;   # -*- tcl-dc -*-

;; Alternatively, the following line can be used to always load Tcl DC Mode
;; along with Tcl mode:

;;   (add-hook 'tcl-mode-hook '(lambda () (require 'tcl-dc-mode) (tcl-dc-mode)))

;; Finally, Tcl DC Mode can also be turned on and off manually using the
;; command `M-x tcl-dc-mode'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup tcl-dc nil
  "Customizations for Tcl-DC Minor Mode."
  :prefix "tcl-dc-"
  :group 'languages)

(defcustom tcl-dc-intelligent-tab t
  "*Non-nil means `TAB' does indentation, word completion and tab insertion.
That is, if preceeding character is part of a word then complete word,
else if not at beginning of line then insert tab,
else if last command was a `TAB' or `RET' then dedent one step,
else indent current line.
If nil, TAB always indents current line."
  :type 'boolean
  :group 'tcl-dc)

(defcustom tcl-dc-extra-commands nil
  "List of additional DC commands for highlighting and completion."
  :type '(repeat (string :tag "Command"))
  :group 'tcl-dc)

(defcustom tcl-dc-extra-variables nil
  "List of additional DC variables for highlighting and completion."
  :type '(repeat (string :tag "Variable"))
  :group 'tcl-dc)

(defcustom tcl-dc-extra-attributes nil
  "List of additional DC attributes for highlighting and completion."
  :type '(repeat (string :tag "Attributes"))
  :group 'tcl-dc)

(defcustom tcl-dc-extra-highlight nil
  "Highlight extra keywords with special warning color."
  :type 'boolean
  :group 'tcl-dc)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings

(defvar tcl-dc-mode-map ()
  "Keymap for Tcl DC Mode.")

;; copy Tcl mode keymap
(require 'tcl)
(setq tcl-dc-mode-map (copy-keymap tcl-mode-map))

;; mode specific key bindings
(define-key tcl-dc-mode-map "\C-c\C-h" 'tcl-dc-doc-mode)
;(define-key tcl-dc-mode-map "\C-c\C-c" 'tcl-dc-comment-uncomment-region)
(define-key tcl-dc-mode-map "\C-c\C-c" 'comment-dwim)
;; add electric key bindings
(define-key tcl-dc-mode-map "\t" 'tcl-dc-electric-tab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax table

(defvar tcl-dc-he-syntax-table (copy-syntax-table tcl-mode-syntax-table)
  "Syntax table used for `hippie-exp'.")

;; add '_' to syntax table
(modify-syntax-entry ?_ "w" tcl-dc-he-syntax-table)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode definition

(defvar tcl-dc-mode nil
  "Tcl DC Mode.")

;; add to mode line
(add-to-list 'minor-mode-alist '(tcl-dc-mode " DC"))

(defun tcl-dc-mode ()
  "Extension to Tcl mode for editing Synopsys Design Compiler (DC) Tcl scripts.

Features:
---------

  WORD/COMMAND COMPLETION:  Typing `TAB' after a (not completed) word
    completes the word according to the following rules:
    - Searches the current buffer for words that start alike and inserts the
      first word found.  Re-typing `TAB' toggles through alternative word
      completions.
    - Searches other open buffers for words that start alike and does the
      above.
    - Searches for predefined Tcl/DC commands, variables and attributes that
      start alike.  If only one completion is found it is inserted.  If
      multiple completions are found, the common substring they all start
      with is inserted and the list of found completions is shown in the
      minibuffer.  Re-typing `TAB' inserts the first one and then toggles
      through alternative completions.

    Typing `TAB' after a non-word character inserts a tabulator stop (if not
    at the beginning of a line).

    This functionality can be turned off and the default `TAB' behavior
    restored using custom option `tcl-dc-intelligent-tab'.

  HIGHLIGHTING (fontification):  Tcl/DC commands and their options, user
    variables, predefined variables and predefined attributes are additionally
    using different colors.

    Additional DC commands, variables and attributes can be added for
    highlighting and word completion using custom options
    `tcl-dc-extra-commands', `tcl-dc-extra-variables' and
    `tcl-dc-extra-attributes' (e.g. for hidden commands or variables).  These
    can be highlighted with a special warning color by setting custom option
    `tcl-dc-extra-highlight' to t.

  COMMENTS:  `C-c C-c' comments out a region if not commented out, and
    uncomments a region if already commented out.

  DC VERSION:  2006.06.


Installation:
-------------

Find information about installation and ways to automatically invoke Tcl DC
Mode for certain files in the header of this source file (`tcl-dc-mode.el').


Maintenance:
------------

Feel free to send bug reports, questions and enhancement requests to
<reto@gnu.org> (only for Tcl DC Mode, not for Tcl mode).

Official distribution is at
<http://www.iis.ee.ethz.ch/~zimmi/emacs/tcl-dc-mode.html>.


                                                  The Dcsh Mode Maintainer
                                               Reto Zimmermann <reto@gnu.org>

Key bindings:
-------------

\\{tcl-mode-map}"
  (interactive)
  ;; load tcl-mode if not already active
  (unless (eq major-mode 'tcl-mode) (tcl-mode))
  ;; toggle mode
  (set (make-local-variable 'tcl-dc-mode) (not tcl-dc-mode))
  ;; set keymap
  (use-local-map (if tcl-dc-mode tcl-dc-mode-map tcl-mode-map))
  ;; set local variables
  (setq comment-padding " ")
  ;; update fontification
  (setq font-lock-defaults
	(if tcl-dc-mode
	    (list 'tcl-dc-font-lock-keywords nil nil '((?\_ . "w")))
	  '(tcl-font-lock-keywords)))
  ;; miscellaneous
  (set (make-local-variable 'hippie-expand-dabbrev-as-symbol) nil)
  (set-syntax-table tcl-dc-he-syntax-table)
  (when tcl-dc-mode
    (local-unset-key "\t")
    (set (make-local-variable 'indent-line-function)
         #'tcl-dc-electric-tab)
    (message "Tcl DC Mode %s.  Type C-c C-h for documentation." tcl-dc-version)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keywords
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; adjust variable bindings size
(when (< max-specpdl-size 2000)
  (setq max-specpdl-size 2000))

(defconst tcl-dc-keywords
  '(
    "after" "alias" "append" "apropos" "array" "binary" "break" "catch" "cd"
    "clock" "close" "concat" "continue" "create_command_group" "date"
    "define_proc_attributes" "echo" "else" "elseif" "encoding" "eof" "error"
    "error_info" "eval" "exec" "exit" "expr" "fblocked" "fconfigure" "fcopy"
    "file" "fileevent" "flush" "for" "foreach" "format"
    "get_command_option_values" "get_message_info" "get_unix_variable" "getenv"
    "gets" "glob" "global" "group_variable" "help" "history" "if" "incr" "info"
    "interp" "is_false" "is_true" "join" "lappend" "lindex" "linsert" "list"
    "llength" "lminus" "lrange" "lreplace" "ls" "lsearch" "lset" "lsort" "man"
    "namespace" "open" "package" "parse_proc_arguments" "pid"
    "print_message_info" "print_suppressed_messages" "print_variable_group"
    "printenv" "printvar" "proc" "proc_args" "proc_body" "puts" "pwd" "quit"
    "read" "redirect" "regexp" "regsub" "rename" "return" "scan" "seek" "set"
    "set_message_info" "set_unix_variable" "setenv" "sh" "socket" "source"
    "split" "string" "subst" "suppress_message" "switch" "tell" "then" "time"
    "trace" "unalias" "unset" "unsuppress_message" "update" "uplevel" "upvar"
    "variable" "vwait" "which" "while"
    )
  "List of Tcl keywords.")

(defconst tcl-dc-commands
  '(
    "acs_check_directories" "acs_compile_design" "acs_create_directories"
    "acs_customize_directory_structure" "acs_get_parent_partition"
    "acs_get_path" "acs_merge_design" "acs_read_hdl"
    "acs_recompile_design" "acs_refine_design" "acs_remove_dont_touch"
    "acs_report_attribute" "acs_report_directories"
    "acs_report_user_messages" "acs_reset_directory_structure"
    "acs_set_attribute" "acs_submit" "acs_submit_large" "acs_write_html"
    "add_module" "add_to_collection" "alib_analyze_libs" "all_clocks"
    "all_cluster_cells" "all_clusters" "all_connected"
    "all_critical_cells" "all_critical_pins" "all_designs"
    "all_dont_touch" "all_drc_violated_nets" "all_fanin" "all_fanout"
    "all_high_fanout" "all_ideal_nets" "all_inputs" "all_outputs"
    "all_registers" "all_scenarios" "all_threestate" "all_tieoff_cells"
    "analyze" "append_to_collection" "balance_buffer" "balance_registers"
    "cell_of" "change_link" "change_names" "characterize" "check_bindings"
    "check_bsd" "check_budget" "check_design" "check_dft" "check_error"
    "check_implementations" "check_isolation_cells" "check_level_shifters"
    "check_license" "check_mv_design" "check_scan" "check_synlib"
    "check_target_library_subset" "check_test" "check_timing"
    "clean_buffer_tree" "compare_collections" "compare_delay_calculation"
    "compare_lib" "compile" "compile_partitions" "compile_ultra"
    "connect_logic_one" "connect_logic_zero" "connect_net" "connect_pin"
    "connect_power_domain" "connect_power_net_info" "context_check"
    "copy_collection" "copy_design" "create_bsd_patterns" "create_bus"
    "create_cache" "create_cell" "create_clock" "create_cluster"
    "create_design" "create_generated_clock" "create_ilm"
    "create_mbist_standalone" "create_multibit" "create_mw_design"
    "create_net" "create_operating_conditions" "create_pass_directories"
    "create_port" "create_power_domain" "create_power_net_info"
    "create_qor_snapshot" "create_scenario" "create_test_clock"
    "create_test_protocol" "create_wire_load" "current_design"
    "current_design_name" "current_instance" "current_scenario"
    "current_test_mode" "dbatt" "dbatt_dc" "dc_allocate_budgets"
    "define_design_lib" "define_dft_design" "define_mbist_program"
    "define_name_rules" "define_test_mode" "define_user_attribute"
    "delete_operating_conditions" "derive_constraints" "dft_drc"
    "disconnect_net" "disconnect_power_net_info" "drive_of" "elaborate"
    "encrypt_lib" "extract_ilm" "filter" "filter_collection" "find"
    "foreach_in_collection" "get_alternative_lib_cells"
    "get_always_on_logic" "get_attribute" "get_buffers" "get_cells"
    "get_clocks" "get_clusters" "get_design_lib_path" "get_designs"
    "get_generated_clocks" "get_ilm_objects" "get_ilms"
    "get_lib_attribute" "get_lib_cells" "get_lib_pins" "get_libs"
    "get_license" "get_multibits" "get_nets" "get_object_name"
    "get_path_groups" "get_pins" "get_ports" "get_power_domains"
    "get_references" "get_rp_groups" "get_scan_cells_of_chain"
    "get_scan_chains" "get_scan_chains_by_name" "get_timing_paths"
    "get_zero_interconnect_delay_mode" "group" "group_path"
    "hookup_power_gating_ports" "hookup_testports" "identify_clock_gating"
    "identify_interface_logic" "index_collection" "infer_power_domains"
    "infer_test_protocol" "insert_clock_gating" "insert_dft"
    "insert_level_shifters" "insert_scan" "lib2saif" "license_users"
    "link" "list_attributes" "list_designs" "list_files" "list_instances"
    "list_libs" "list_licenses" "list_test_models" "list_test_modes"
    "load_of" "log_to_phys" "mem_dump_group" "merge_clock_gates"
    "merge_saif" "optimize_registers" "parent_cluster" "pipeline_design"
    "preview_dft" "preview_scan" "print_proc_new_vars"
    "propagate_annotated_delay_up" "propagate_constraints" "propagate_ilm"
    "propagate_placement" "propagate_switching_activity" "query_objects"
    "read_db" "read_ddc" "read_edif" "read_file" "read_lib"
    "read_milkyway" "read_parasitics" "read_partition" "read_pattern_info"
    "read_pin_map" "read_saif" "read_sdc" "read_sdf" "read_sverilog"
    "read_test_model" "read_test_protocol" "read_verilog" "read_vhdl"
    "reg_global_var" "remove_annotated_check" "remove_annotated_delay"
    "remove_annotated_transition" "remove_attribute"
    "remove_boundary_cell" "remove_bsd_ac_port" "remove_bsd_instruction"
    "remove_bus" "remove_cache" "remove_case_analysis"
    "remove_ccl_attribute" "remove_ccl_str_in_pattern_list" "remove_cell"
    "remove_cell_degradation" "remove_clock" "remove_clock_gating"
    "remove_clock_gating_check" "remove_clock_latency"
    "remove_clock_sense" "remove_clock_transition"
    "remove_clock_uncertainty" "remove_clusters" "remove_constraint"
    "remove_data_check" "remove_design" "remove_dft_configuration"
    "remove_dft_design" "remove_dft_equivalent_signals"
    "remove_dft_logic_usage" "remove_dft_signal"
    "remove_disable_clock_gating_check" "remove_disable_timing"
    "remove_driving_cell" "remove_fanout_load" "remove_from_collection"
    "remove_generated_clock" "remove_ideal_latency" "remove_ideal_net"
    "remove_ideal_network" "remove_ideal_transition" "remove_input_delay"
    "remove_isolate_ports" "remove_level_shifters" "remove_license"
    "remove_mbist_configuration" "remove_multibit" "remove_net"
    "remove_operand_isolation" "remove_output_delay"
    "remove_pass_directories" "remove_pin_map" "remove_pin_name_synonym"
    "remove_port" "remove_power_domain" "remove_power_net_info"
    "remove_propagated_clock" "remove_qor_snapshot" "remove_rtl_load"
    "remove_scan_group" "remove_scan_link" "remove_scan_path"
    "remove_scan_register_type" "remove_scan_replacement"
    "remove_scan_specification" "remove_scenario" "remove_sdc"
    "remove_target_library_subset" "remove_test_assume" "remove_test_mode"
    "remove_test_model" "remove_test_point_element" "remove_test_protocol"
    "remove_unconnected_ports" "remove_user_attribute"
    "remove_wire_load_min_block_size" "remove_wire_load_model"
    "remove_wire_load_selection_group" "rename_design" "reoptimize_design"
    "replace_clock_gates" "replace_synthetic" "report_annotated_check"
    "report_annotated_delay" "report_annotated_transition" "report_area"
    "report_attribute" "report_auto_ungroup"
    "report_autofix_configuration" "report_autofix_element"
    "report_boundary_cell" "report_bsd_ac_port" "report_buffer_tree"
    "report_buffer_tree_qor" "report_bus" "report_cache"
    "report_case_analysis" "report_cell" "report_clock"
    "report_clock_gating" "report_clock_gating_check"
    "report_clock_timing" "report_clock_tree" "report_clock_tree_power"
    "report_clusters" "report_compile_options" "report_constraint"
    "report_delay_calculation" "report_design" "report_design_lib"
    "report_dft" "report_dft_clock_controller" "report_dft_configuration"
    "report_dft_design" "report_dft_drc_rules"
    "report_dft_equivalent_signals" "report_dft_insertion_configuration"
    "report_dft_logic_usage" "report_dft_signal"
    "report_direct_power_rail_tie" "report_disable_timing"
    "report_dp_smartgen_options" "report_dw_rp_group_options"
    "report_fault" "report_fsm" "report_hierarchy" "report_hybrid_stats"
    "report_ideal_network" "report_internal_loads" "report_isolate_ports"
    "report_lib" "report_logicbist_configuration" "report_mbist_program"
    "report_mbist_trace" "report_mode" "report_multibit"
    "report_name_rules" "report_names" "report_net"
    "report_net_characteristics" "report_net_fanout"
    "report_operand_isolation" "report_operating_conditions"
    "report_partitions" "report_pass_data" "report_path_budget"
    "report_path_group" "report_pin_name_synonym" "report_port"
    "report_power" "report_power_calculation" "report_power_domain"
    "report_power_gating" "report_power_net_info" "report_power_pin_info"
    "report_qor" "report_qor_snapshot" "report_reference"
    "report_resources" "report_saif" "report_scan_chain"
    "report_scan_compression_configuration" "report_scan_configuration"
    "report_scan_group" "report_scan_link" "report_scan_path"
    "report_scan_register_type" "report_scan_replacement"
    "report_scan_state" "report_synlib" "report_target_library_subset"
    "report_test" "report_test_assume" "report_test_model"
    "report_test_point_element" "report_testability_configuration"
    "report_threshold_voltage_group" "report_timing"
    "report_timing_derate" "report_timing_requirements"
    "report_transitive_fanin" "report_transitive_fanout"
    "report_ultra_optimization" "report_use_test_model" "report_wire_load"
    "report_wrapper_configuration" "reset_autofix_configuration"
    "reset_autofix_element" "reset_bsd_configuration" "reset_design"
    "reset_dft_clock_controller" "reset_dft_configuration"
    "reset_dft_drc_rules" "reset_dft_insertion_configuration"
    "reset_logicbist_configuration" "reset_mbist_configuration"
    "reset_mbist_controller" "reset_mbist_programs" "reset_mbist_wrapper"
    "reset_mode" "reset_path" "reset_scan_compression_configuration"
    "reset_scan_configuration" "reset_switching_activity"
    "reset_test_mode" "reset_testability_configuration"
    "reset_testbench_parameters" "reset_timing_derate"
    "reset_wrapper_configuration" "rewire_clock_gating" "rtl2saif"
    "set_annotated_check" "set_annotated_delay" "set_annotated_transition"
    "set_attribute" "set_auto_disable_drc_nets" "set_autofix_async"
    "set_autofix_clock" "set_autofix_configuration" "set_autofix_element"
    "set_balance_registers" "set_boundary_cell" "set_boundary_cell_io"
    "set_boundary_optimization" "set_bsd_compliance"
    "set_bsd_configuration" "set_bsd_instruction" "set_bsd_linkage_port"
    "set_bsd_power_up_reset" "set_case_analysis" "set_cell_degradation"
    "set_cell_internal_power" "set_cle_options" "set_clock_gating_check"
    "set_clock_gating_enable_directives" "set_clock_gating_registers"
    "set_clock_gating_style" "set_clock_latency" "set_clock_sense"
    "set_clock_transition" "set_clock_uncertainty"
    "set_combinational_type" "set_compile_directives"
    "set_compile_partitions" "set_connection_class" "set_context_margin"
    "set_core_wrapper_configuration" "set_cost_priority"
    "set_critical_range" "set_current_command_mode" "set_data_check"
    "set_datapath_optimization" "set_default_drive"
    "set_default_driving_cell" "set_default_fanout_load"
    "set_default_input_delay" "set_default_load"
    "set_default_output_delay" "set_delay_calculation"
    "set_design_license" "set_dft_clock_controller"
    "set_dft_configuration" "set_dft_drc_configuration"
    "set_dft_drc_rules" "set_dft_equivalent_signals"
    "set_dft_insertion_configuration" "set_dft_logic_usage"
    "set_dft_optimization_configuration" "set_dft_rtl_configuration"
    "set_dft_signal" "set_direct_power_rail_tie"
    "set_disable_clock_gating_check" "set_disable_timing" "set_dont_touch"
    "set_dont_touch_network" "set_dont_use" "set_dp_smartgen_options"
    "set_drive" "set_driving_cell" "set_dw_rp_group_options" "set_equal"
    "set_false_path" "set_fanout_load" "set_fix_hold"
    "set_fix_multiple_port_nets" "set_flatten" "set_fsm_encoding"
    "set_fsm_encoding_style" "set_fsm_minimize" "set_fsm_order"
    "set_fsm_preserve_state" "set_fsm_state_vector" "set_ideal_latency"
    "set_ideal_net" "set_ideal_network" "set_ideal_transition"
    "set_impl_priority" "set_implementation" "set_input_delay"
    "set_input_transition" "set_isolate_ports"
    "set_level_shifter_strategy" "set_level_shifter_threshold"
    "set_lib_attribute" "set_libcell_dimensions" "set_libpin_location"
    "set_load" "set_local_link_library" "set_logic_dc" "set_logic_one"
    "set_logic_zero" "set_logicbist_configuration" "set_map_only"
    "set_max_area" "set_max_capacitance" "set_max_delay"
    "set_max_dynamic_power" "set_max_fanout" "set_max_leakage_power"
    "set_max_net_length" "set_max_peak_noise" "set_max_time_borrow"
    "set_max_total_power" "set_max_transition" "set_mbist_configuration"
    "set_mbist_controller" "set_mbist_element" "set_mbist_run"
    "set_mbist_wrapper" "set_message_severity" "set_min_capacitance"
    "set_min_delay" "set_min_library" "set_minimize_tree_delay" "set_mode"
    "set_model_drive" "set_model_load" "set_model_map_effort"
    "set_module_clock_edges" "set_module_clock_gates"
    "set_multibit_options" "set_multicycle_path" "set_mw_design"
    "set_mw_reference_library" "set_noise_slack_range"
    "set_operand_isolation_cell" "set_operand_isolation_scope"
    "set_operand_isolation_slack" "set_operand_isolation_style"
    "set_operating_conditions" "set_opposite" "set_optimize_registers"
    "set_output_delay" "set_pin_name_synonym" "set_port_fanout_number"
    "set_port_location" "set_power_gating_signal" "set_power_gating_style"
    "set_power_prediction" "set_prefer" "set_propagated_clock"
    "set_register_type" "set_relative_always_on" "set_replace_clock_gates"
    "set_resistance" "set_resource_allocation"
    "set_resource_implementation" "set_rtl_load" "set_scan_bidi"
    "set_scan_compression_configuration" "set_scan_configuration"
    "set_scan_element" "set_scan_group" "set_scan_link" "set_scan_path"
    "set_scan_register_type" "set_scan_replacement" "set_scan_segment"
    "set_scan_signal" "set_scan_state" "set_scan_style"
    "set_scan_tristate" "set_share_cse" "set_signal_type" "set_size_only"
    "set_state_for_retiming" "set_structure" "set_svf"
    "set_switching_activity" "set_synlib_dont_get_license"
    "set_target_library_subset" "set_test_assume" "set_test_dont_fault"
    "set_test_hold" "set_test_isolate" "set_test_point_element"
    "set_test_target" "set_testability_configuration"
    "set_testbench_parameters" "set_timing_derate" "set_timing_ranges"
    "set_transform_for_retiming" "set_true_delay_case_analysis"
    "set_ultra_optimization" "set_unconnected" "set_ungroup"
    "set_user_attribute" "set_user_budget" "set_vsdc"
    "set_wire_load_min_block_size" "set_wire_load_mode"
    "set_wire_load_model" "set_wire_load_selection_group"
    "set_wrapper_configuration" "set_zero_interconnect_delay_mode"
    "sh_list_key_bindings" "share_operations_on_one_resource"
    "shell_is_in_topographical_mode" "shell_is_in_xg_mode"
    "simplify_constants" "sizeof_collection" "sort_collection"
    "sub_designs_of" "sub_instances_of" "syntax_check" "trace_scan_chain"
    "translate" "ungroup" "uniquify" "update_lib" "update_timing"
    "use_test_model" "write" "write_bsdl" "write_compile_script"
    "write_design_lib_paths" "write_dw_rp_group" "write_environment"
    "write_file" "write_lib" "write_link_library" "write_makefile"
    "write_milkyway" "write_mw_verilog" "write_parasitics"
    "write_partition" "write_partition_constraints"
    "write_physical_script" "write_rtl_load" "write_saif" "write_scan_def"
    "write_script" "write_sdc" "write_sdf" "write_test" "write_test_model"
    "write_test_protocol"
    )
  "List of DC commands (2006.06 release).")

(defconst tcl-dc-variables
  '(
    "access_internal_pins" "acs_area_report_suffix" "acs_attr"
    "acs_autopart_max_area" "acs_autopart_max_percent"
    "acs_budgeted_cstr_suffix" "acs_compile_script_suffix"
    "acs_constraint_file_suffix" "acs_cstr_report_suffix" "acs_db_suffix"
    "acs_dc_exec" "acs_ddc_suffix" "acs_default_pass_name" "acs_dir"
    "acs_exclude_extensions" "acs_exclude_list"
    "acs_global_user_compile_strategy_script" "acs_hdl_source"
    "acs_hdl_verilog_define_list" "acs_insert_level_shifter"
    "acs_lic_wait" "acs_log_file_suffix" "acs_make_args" "acs_make_exec"
    "acs_makefile_name" "acs_num_parallel_jobs"
    "acs_override_report_suffix" "acs_override_script_suffix"
    "acs_preferred_target_compiler" "acs_qor_report_suffix"
    "acs_submit_log_uses_o_option" "acs_svf_suffix"
    "acs_timing_report_suffix" "acs_use_autopartition"
    "acs_use_default_delays" "acs_user_budgeting_script"
    "acs_user_compile_strategy_script_suffix"
    "acs_user_top_compile_strategy_script" "acs_verilog_extensions"
    "acs_vhdl_extensions" "acs_work_dir" "alib_library_analysis_path"
    "allow_input_delay_min_greater_than_max" "arch"
    "atpg_test_asynchronous_pins" "auto_index" "auto_link_disable"
    "auto_link_options" "auto_noexec" "auto_oldpath" "auto_path"
    "auto_ungroup_preserve_constraints" "auto_wire_load_selection"
    "bin_path" "bsd_max_in_switching_limit" "bsd_max_out_switching_limit"
    "bsd_physical_effort" "budget_generate_critical_range"
    "budget_map_clock_gating_cells" "bus_dimension_separator_style"
    "bus_extraction_style" "bus_inference_descending_sort"
    "bus_inference_style" "bus_minus_style" "bus_multiple_separator_style"
    "bus_naming_style" "bus_range_separator_style" "cache_dir_chmod_octal"
    "cache_file_chmod_octal" "cache_read" "cache_read_info" "cache_write"
    "cache_write_info" "case_analysis_log_file"
    "case_analysis_with_logic_constants" "ccl_enable_always"
    "change_names_dont_change_bus_members" "change_names_update_inst_tree"
    "check_design_allow_non_tri_drivers_on_tri_bus"
    "check_design_allow_unknown_wired_logic_type" "check_error_list"
    "collection_result_display_limit" "combo_itf_library"
    "command_log_file" "company" "compatibility_version"
    "compile_allow_dw_hierarchical_inverter_opt"
    "compile_assume_fully_decoded_three_state_busses"
    "compile_auto_ungroup_area_num_cells"
    "compile_auto_ungroup_count_leaf_cells"
    "compile_auto_ungroup_delay_num_cells"
    "compile_auto_ungroup_override_wlm"
    "compile_automatic_clock_phase_inference" "compile_checkpoint_phases"
    "compile_cpu_limit" "compile_create_mux_op_hierarchy"
    "compile_create_wire_load_table" "compile_dcl_performance_mode"
    "compile_delete_unloaded_sequential_cells"
    "compile_disable_hierarchical_inverter_opt"
    "compile_dont_touch_annotated_cell_during_inplace_opt"
    "compile_dont_use_dedicated_scanout" "compile_enable_dyn_max_cap"
    "compile_fix_cell_degradation" "compile_hold_reduce_cell_count"
    "compile_implementation_selection" "compile_instance_name_prefix"
    "compile_instance_name_suffix" "compile_log_format"
    "compile_negative_logic_methodology"
    "compile_no_new_cells_at_top_level"
    "compile_preserve_subdesign_interfaces" "compile_report_dp"
    "compile_retime_license_behavior"
    "compile_seqmap_enable_output_inversion" "compile_seqmap_no_scan_cell"
    "compile_seqmap_propagate_constants"
    "compile_seqmap_propagate_high_effort"
    "compile_sequential_area_recovery_old"
    "compile_slack_driven_buffering" "compile_ultra_ungroup_dw"
    "compile_update_annotated_delays_during_inplace_opt"
    "compile_use_fast_delay_mode" "compile_use_low_timing_effort"
    "context_check_status" "create_clock_no_input_delay" "current_design"
    "current_instance" "db_load_ccs_data" "dc_shell_mode"
    "ddc_allow_unknown_packed_commands" "ddc_verbose"
    "default_input_delay" "default_name_rules" "default_output_delay"
    "default_port_connection_class" "default_schematic_options"
    "design_library_file" "designer" "disable_auto_time_borrow"
    "disable_case_analysis" "disable_delta_slew_for_tran_cstr"
    "disable_library_transition_degradation" "disable_mdb_stop_points"
    "do_operand_isolation" "dont_bind_unused_pins_to_logic_constant"
    "dpcm_arc_sense_mapping" "dpcm_debuglevel" "dpcm_functionscope"
    "dpcm_level" "dpcm_libraries" "dpcm_rulepath" "dpcm_rulespath"
    "dpcm_slewlimit" "dpcm_tablepath" "dpcm_temperaturescope"
    "dpcm_version" "dpcm_voltagescope" "dpcm_wireloadscope"
    "duplicate_ports" "echo_include_commands"
    "edifin_autoconnect_offpageconnectors" "edifin_autoconnect_ports"
    "edifin_dc_script_flag" "edifin_delete_empty_cells"
    "edifin_delete_ripper_cells" "edifin_ground_net_name"
    "edifin_ground_net_property_name" "edifin_ground_net_property_value"
    "edifin_ground_port_name" "edifin_instance_property_name"
    "edifin_lib_in_osc_symbol" "edifin_lib_in_port_symbol"
    "edifin_lib_inout_osc_symbol" "edifin_lib_inout_port_symbol"
    "edifin_lib_logic_0_symbol" "edifin_lib_logic_1_symbol"
    "edifin_lib_mentor_netcon_symbol" "edifin_lib_out_osc_symbol"
    "edifin_lib_out_port_symbol" "edifin_lib_ripper_bits_property"
    "edifin_lib_ripper_bus_end" "edifin_lib_ripper_cell_name"
    "edifin_lib_ripper_view_name" "edifin_lib_route_grid"
    "edifin_lib_templates" "edifin_portinstance_disabled_property_name"
    "edifin_portinstance_disabled_property_value"
    "edifin_portinstance_property_name" "edifin_power_net_name"
    "edifin_power_net_property_name" "edifin_power_net_property_value"
    "edifin_power_port_name" "edifin_use_identifier_in_rename"
    "edifin_view_identifier_property_name" "edifout_dc_script_flag"
    "edifout_design_name" "edifout_designs_library_name"
    "edifout_display_instance_names" "edifout_display_net_names"
    "edifout_external" "edifout_external_graphic_view_name"
    "edifout_external_netlist_view_name"
    "edifout_external_schematic_view_name" "edifout_ground_name"
    "edifout_ground_net_name" "edifout_ground_net_property_name"
    "edifout_ground_net_property_value" "edifout_ground_pin_name"
    "edifout_ground_port_name" "edifout_instance_property_name"
    "edifout_instantiate_ports" "edifout_library_graphic_view_name"
    "edifout_library_netlist_view_name"
    "edifout_library_schematic_view_name" "edifout_merge_libraries"
    "edifout_multidimension_arrays"
    "edifout_name_oscs_different_from_ports"
    "edifout_name_rippers_same_as_wires" "edifout_netlist_only"
    "edifout_no_array" "edifout_numerical_array_members"
    "edifout_pin_direction_in_value" "edifout_pin_direction_inout_value"
    "edifout_pin_direction_out_value"
    "edifout_pin_direction_property_name" "edifout_pin_name_property_name"
    "edifout_portinstance_disabled_property_name"
    "edifout_portinstance_disabled_property_value"
    "edifout_portinstance_property_name"
    "edifout_power_and_ground_representation" "edifout_power_name"
    "edifout_power_net_name" "edifout_power_net_property_name"
    "edifout_power_net_property_value" "edifout_power_pin_name"
    "edifout_power_port_name" "edifout_skip_port_implementations"
    "edifout_target_system" "edifout_top_level_symbol"
    "edifout_translate_origin" "edifout_unused_property_value"
    "edifout_write_attributes" "edifout_write_constraints"
    "edifout_write_design_name" "edifout_write_properties_list"
    "enable_cell_based_verilog_reader" "enable_cell_based_verilog_writer"
    "enable_instances_in_report_net" "enable_page_mode"
    "enable_recovery_removal_arcs" "enable_slew_degradation"
    "enable_verilog_netlist_reader" "equationout_and_sign"
    "equationout_or_sign" "equationout_postfix_negation" "err"
    "estimate_io_latency" "exit_delete_command_log_file"
    "exit_delete_filename_log_file" "filename_log_file"
    "find_allow_only_non_hier_ports" "find_converts_name_lists"
    "find_ignore_case" "found_arch_apollo"
    "found_x11_vendor_string_apollo" "fpga_addsub_map_to_dsp"
    "fpga_auto_ungroup_num_cells" "fpga_block_rom_max_addr_width"
    "fpga_block_rom_min_addr_width" "fpga_block_rom_min_total_bits"
    "fpga_boundary_optimization" "fpga_buffer_clk_min_fanout"
    "fpga_compile_cpu_limit" "fpga_compile_force_ungroup_num_cells"
    "fpga_duplicate_iob_registers" "fpga_enable_mega_func_timing"
    "fpga_enable_merge_register" "fpga_fast_seqmap"
    "fpga_feed_through_opt" "fpga_infer_distributed_rom"
    "fpga_iob_mapping" "fpga_map_wide_gates" "fpga_patch_luts"
    "fpga_pre_auto_ungroup" "fpga_prefer_tmg" "fpga_seq_const_prop"
    "fpga_seq_inverter_move_back"
    "fpga_seq_inverter_move_back_with_reg_dup" "fsm_auto_inferring"
    "fsm_enable_state_minimization" "fsm_export_formality_state_info"
    "ft_array" "gen_bussing_exact_implicit" "gen_cell_pin_name_separator"
    "gen_create_netlist_busses" "gen_dont_show_single_bit_busses"
    "gen_match_ripper_wire_widths" "gen_max_compound_name_length"
    "gen_max_ports_on_symbol_side" "gen_open_name_postfix"
    "gen_open_name_prefix" "gen_show_created_busses"
    "gen_show_created_symbols" "gen_single_osc_per_name"
    "generic_symbol_library" "hdl_keep_licenses" "hdl_preferred_license"
    "hdlin_allow_4state_parameters" "hdlin_auto_full_case"
    "hdlin_auto_netlist_reader" "hdlin_auto_parallel_case_early"
    "hdlin_auto_save_templates" "hdlin_bdd_depth_limit"
    "hdlin_bdd_memory_limit" "hdlin_black_box_pin_hdlc_style"
    "hdlin_build_selectop_for_var_index" "hdlin_check_no_latch"
    "hdlin_check_user_full_case" "hdlin_check_user_parallel_case"
    "hdlin_compare_const_with_gates" "hdlin_compare_eq_with_gates"
    "hdlin_constmult_optimization" "hdlin_decoder_max_input_width"
    "hdlin_decoder_min_input_width" "hdlin_decoder_min_use_percentage"
    "hdlin_dont_check_param_width"
    "hdlin_dont_infer_mux_for_resource_sharing" "hdlin_elab_errors_deep"
    "hdlin_enable_analysis_info" "hdlin_enable_configurations"
    "hdlin_enable_presto" "hdlin_enable_presto_for_vhdl"
    "hdlin_enable_rtldrc_info" "hdlin_enable_vpp"
    "hdlin_ff_always_async_set_reset" "hdlin_ff_always_sync_set_reset"
    "hdlin_field_naming_style" "hdlin_fsm_encoding_threshold"
    "hdlin_fsm_subset_level" "hdlin_generate_naming_style"
    "hdlin_generate_separator_style" "hdlin_group_selectors"
    "hdlin_hide_resource_line_numbers" "hdlin_infer_block_local_latches"
    "hdlin_infer_comparators" "hdlin_infer_complex_enable"
    "hdlin_infer_complex_set_reset" "hdlin_infer_counter"
    "hdlin_infer_enumerated_types" "hdlin_infer_function_local_latches"
    "hdlin_infer_multibit" "hdlin_infer_mux" "hdlin_infer_ram"
    "hdlin_keep_feedback" "hdlin_keep_inv_feedback"
    "hdlin_keep_signal_name" "hdlin_latch_always_async_set_reset"
    "hdlin_loop_invariant_code_motion" "hdlin_map_to_entity"
    "hdlin_map_to_module" "hdlin_map_to_operator"
    "hdlin_merge_nested_conditional_statements"
    "hdlin_module_arch_name_splitting" "hdlin_module_name_limit"
    "hdlin_mux_oversize_ratio" "hdlin_mux_size_limit" "hdlin_mux_size_min"
    "hdlin_no_adder_feedthroughs" "hdlin_no_group_register"
    "hdlin_no_sequential_mapping" "hdlin_one_hot_one_cold_on"
    "hdlin_optimize_array_references" "hdlin_optimize_case_default"
    "hdlin_optimize_enum_types" "hdlin_optimize_shift_expressions"
    "hdlin_optimize_slice_op" "hdlin_out_of_bounds_error_level"
    "hdlin_pla_mode" "hdlin_preserve_sequential"
    "hdlin_preserve_sequential_loop_variables" "hdlin_preserve_vpp_files"
    "hdlin_presto_cell_name_prefix" "hdlin_presto_net_name_prefix"
    "hdlin_ram_min_address_width" "hdlin_ram_min_total_bits"
    "hdlin_redundancy_elimination" "hdlin_reg_report_length"
    "hdlin_register_report_depth" "hdlin_report_carry_in"
    "hdlin_report_case_analysis" "hdlin_report_enumerated_types"
    "hdlin_report_floating_net_to_ground" "hdlin_report_fsm"
    "hdlin_report_inferred_modules" "hdlin_report_mux_op"
    "hdlin_report_syn_cell" "hdlin_report_tri_state"
    "hdlin_selector_simplify_effort" "hdlin_seqmap_async_search_depth"
    "hdlin_seqmap_sync_search_depth" "hdlin_share_all_operators"
    "hdlin_shorten_long_module_name" "hdlin_signed_division_use_shift"
    "hdlin_subprogram_default_values" "hdlin_support_subprogram_var_init"
    "hdlin_translate_off_on" "hdlin_translate_off_skip_text"
    "hdlin_unsigned_rem" "hdlin_upcase_names" "hdlin_use_carry_in"
    "hdlin_use_syn_shifter" "hdlin_verbose_cell_naming" "hdlin_vhdl_87"
    "hdlin_vpp_temporary_directory" "hdlin_vrlg_std"
    "hdlin_warn_array_bound" "hdlin_warn_implicit_wires"
    "hdlin_warn_sens_list" "hdlin_while_loop_iterations"
    "hdlout_internal_busses" "hier_dont_trace_ungroup"
    "high_fanout_net_pin_capacitance" "high_fanout_net_threshold"
    "hlo_datapath_duplicate_cse" "hlo_disable_datapath_optimization"
    "hlo_resource_allocation" "hlo_resource_implementation"
    "inherit_parent_dont_touch" "init_path" "initial_target_library"
    "insert_test_design_naming_style" "jtag_manufacturer_id"
    "jtag_part_number" "jtag_port_drive_limit"
    "jtag_test_clock_port_naming_style"
    "jtag_test_data_in_port_naming_style"
    "jtag_test_data_out_port_naming_style"
    "jtag_test_mode_select_port_naming_style"
    "jtag_test_reset_port_naming_style" "jtag_version_number"
    "lbo_cells_in_regions" "lib_thresholds_per_lib"
    "lib_use_thresholds_per_pin" "libgen_max_differences"
    "link_force_case" "link_library" "link_path" "list_of_file_types"
    "lsiin_net_name_prefix" "lsiout_inverter_cell" "lsiout_upcase"
    "ltl_obstruction_type" "mentor_bidirect_value" "mentor_do_path"
    "mentor_input_output_property_name" "mentor_input_value"
    "mentor_logic_one_value" "mentor_logic_zero_one_property_name"
    "mentor_logic_zero_value" "mentor_output_value"
    "mentor_primitive_property_name" "mentor_primitive_property_value"
    "mentor_reference_property_name" "mentor_search_path"
    "mentor_write_symbols" "multi_pass_test_generation"
    "mux_auto_inferring_effort" "mw_cel_as_escaped" "mw_cell_name"
    "mw_convert_tf" "mw_create_netlist_from_CEL" "mw_current_design"
    "mw_design_library" "mw_disable_escape_char" "mw_enable_net_bus"
    "mw_enable_netl_view" "mw_ground_port" "mw_hdl_allow_dirty_netlist"
    "mw_hdl_bus_dir_for_undef_cell" "mw_hdl_create_multi_pg_net"
    "mw_hdl_expand_cell_with_no_instance" "mw_hdl_stop_at_FRAM_cells"
    "mw_hdl_top_module_list" "mw_hvo_core_filler_cells"
    "mw_hvo_corner_pad_cells" "mw_hvo_diode_ports"
    "mw_hvo_dump_master_names" "mw_hvo_empty_cell_definition"
    "mw_hvo_generate_macro_definition" "mw_hvo_must_not_dump_master_names"
    "mw_hvo_output_onezero_for_pg" "mw_hvo_output_wire_declaration"
    "mw_hvo_pad_filler_cells" "mw_hvo_pg_nets" "mw_hvo_pg_ports"
    "mw_hvo_split_bus" "mw_hvo_strip_backslash_before_hiersep"
    "mw_hvo_unconnected_cells" "mw_hvo_unconnected_ports" "mw_logic0_net"
    "mw_logic1_net" "mw_pgconn_cell_inst" "mw_pgconn_cell_master"
    "mw_power_port" "mw_read_ignore_corner_cell"
    "mw_read_ignore_filler_cell" "mw_read_ignore_pad_cell"
    "mw_read_ignore_unconnected_cell" "mw_reference_library" "mw_tech_pdb"
    "mw_use_pdb_lib_format" "new_idn_switch" "pass_dependent_file_types"
    "pdefout_diff_original" "physical_library" "pla_read_create_flip_flop"
    "plot_box" "plot_command" "plot_orientation" "plot_scale_factor"
    "plotter_maxx" "plotter_maxy" "plotter_minx" "plotter_miny" "pn"
    "port_complement_naming_style" "power_cg_all_registers"
    "power_cg_cell_naming_style" "power_cg_derive_related_clock"
    "power_cg_designware" "power_cg_ext_feedback_loop" "power_cg_flatten"
    "power_cg_gated_clock_net_naming_style"
    "power_cg_ignore_setup_condition" "power_cg_module_naming_style"
    "power_cg_print_enable_conditions"
    "power_cg_print_enable_conditions_max_terms"
    "power_default_static_probability" "power_default_toggle_rate"
    "power_default_toggle_rate_type" "power_do_not_size_icg_cells"
    "power_enable_power_gating" "power_fix_sdpd_annotation"
    "power_fix_sdpd_annotation_verbose" "power_hdlc_do_not_split_cg_cells"
    "power_keep_license_after_power_commands"
    "power_lib2saif_rise_fall_pd" "power_min_internal_power_threshold"
    "power_model_preference" "power_opto_extra_high_dynamic_power_effort"
    "power_preserve_rtl_hier_names"
    "power_rclock_inputs_use_clocks_fanout"
    "power_rclock_unrelated_use_fastest" "power_rclock_use_asynch_inputs"
    "power_remove_redundant_clock_gates" "power_rtl_saif_file"
    "power_sa_propagation_effort" "power_sa_propagation_verbose"
    "power_sdpd_message_tolerance" "power_sdpd_saif_file"
    "power_use_multi_vt_swap_opto" "product_build_date" "product_version"
    "prot_update" "rc_compute_delta_delay" "rc_compute_delta_slew"
    "rc_debug_noise" "rc_delta_delay_min" "rc_driver_model_mode"
    "rc_input_threshold_pct_fall" "rc_input_threshold_pct_rise"
    "rc_noise_model_mode" "rc_output_threshold_pct_fall"
    "rc_output_threshold_pct_rise" "rc_pt_driver_model"
    "rc_receiver_model_mode" "rc_slew_derate_from_library"
    "rc_slew_lower_threshold_pct_fall" "rc_slew_lower_threshold_pct_rise"
    "rc_slew_upper_threshold_pct_fall" "rc_slew_upper_threshold_pct_rise"
    "read_db_lib_warnings" "read_name_mapping_nowarn_libraries"
    "read_translate_msff" "register_duplicate"
    "reoptimize_design_changed_list_file_name"
    "report_default_significant_digits"
    "restrict_commands_for_embedded_scripts" "rom_auto_inferring"
    "rtl_load_resistance_factor" "sdc_version"
    "sdc_write_unambiguous_names" "sdf_enable_cond_start_end"
    "sdfout_allow_non_positive_constraints" "sdfout_min_fall_cell_delay"
    "sdfout_min_fall_net_delay" "sdfout_min_rise_cell_delay"
    "sdfout_min_rise_net_delay" "sdfout_time_scale"
    "sdfout_top_instance_name" "sdfout_write_to_output" "search_path"
    "set_isolate_ports" "sh_arch" "sh_command_abbrev_mode"
    "sh_command_log_file" "sh_continue_on_error" "sh_dev_null"
    "sh_enable_page_mode" "sh_line_editing_mode" "sh_new_variable_message"
    "sh_new_variable_message_in_proc" "sh_new_variable_message_in_script"
    "sh_product_version" "sh_script_stop_severity"
    "sh_source_emits_line_numbers" "sh_source_logging"
    "sh_source_uses_search_path" "sh_tcllib_app_dirname"
    "sh_user_man_path" "si_filter_accum_aggr_noise_peak_ratio"
    "si_filter_per_aggr_noise_peak_ratio" "si_xtalk_analysis_fast_model"
    "single_group_per_sheet" "site_info_file" "sort_outputs"
    "suppress_errors" "symbol_library" "synlib_abort_wo_dw_license"
    "synlib_disable_limited_licenses" "synlib_dont_get_license"
    "synlib_dwgen_smart_generation" "synlib_hiis_force_on_cells"
    "synlib_iis_use_netlist" "synlib_model_map_effort"
    "synlib_optimize_non_cache_elements" "synlib_replace_synthetic_oani"
    "synlib_sequential_module" "synlib_wait_for_design_license"
    "synopsys_exec" "synopsys_program_name" "synopsys_root"
    "syntax_check_status" "synthetic_library" "target_library"
    "tcl_interactive" "tcl_library" "tcl_patchLevel" "tcl_pkgPath"
    "tcl_platform" "tcl_version" "tdlout_upcase" "template_naming_style"
    "template_parameter_style" "template_separator_style"
    "test_allow_clock_reconvergence" "test_bsd_allow_tolerable_violations"
    "test_bsd_control_cell_drive_limit" "test_bsd_manufacturer_id"
    "test_bsd_optimize_control_cell" "test_bsd_part_number"
    "test_bsd_version_number" "test_bsdl_default_suffix_name"
    "test_bsdl_max_line_length" "test_capture_clock_skew"
    "test_cc_ir_masked_bits" "test_cc_ir_value_of_masked_bits"
    "test_check_port_changes_in_capture" "test_clock_port_naming_style"
    "test_dedicated_subdesign_scan_outs" "test_default_bidir_delay"
    "test_default_delay" "test_default_min_fault_coverage"
    "test_default_period" "test_default_strobe"
    "test_default_strobe_width" "test_design_analyzer_uses_insert_scan"
    "test_dft_drc_ungate_clocks" "test_dft_drc_ungate_internal_clocks"
    "test_disable_find_best_scan_out" "test_disconnect_non_functional_so"
    "test_dont_fix_constraint_violations" "test_enable_capture_checks"
    "test_infer_slave_clock_pulse_after_capture"
    "test_isolate_hier_scan_out" "test_mode_port_inverted_naming_style"
    "test_mode_port_naming_style" "test_mux_constant_si"
    "test_non_scan_clock_port_naming_style"
    "test_preview_scan_shows_cell_types" "test_rtldrc_latch_check_style"
    "test_scan_clock_a_port_naming_style"
    "test_scan_clock_b_port_naming_style"
    "test_scan_clock_port_naming_style"
    "test_scan_enable_inverted_port_naming_style"
    "test_scan_enable_port_naming_style" "test_scan_in_port_naming_style"
    "test_scan_link_so_lockup_key" "test_scan_link_wire_key"
    "test_scan_out_port_naming_style" "test_scan_segment_key"
    "test_scan_true_key" "test_setup_additional_clock_pulse"
    "test_simulation_library" "test_stil_max_line_length"
    "test_user_defined_instruction_naming_style"
    "test_user_test_data_register_naming_style" "tested_technology"
    "testsim_print_stats_file" "text_editor_command" "text_print_command"
    "timing_crpr_grouping_mode" "timing_crpr_remove_clock_to_data_crp"
    "timing_crpr_threshold_ps" "timing_disable_data_checks"
    "timing_enable_multiple_clocks_per_reg"
    "timing_input_port_clock_shift_one_cycle"
    "timing_input_port_default_clock"
    "timing_non_unate_clock_compatibility"
    "timing_remove_clock_reconvergence_pessimism"
    "timing_report_attributes" "timing_self_loops_no_skew"
    "timing_use_enhanced_capacitance_modeling" "tlu_plus_library"
    "tlu_plus_tf_layer_id" "ungroup_keep_original_design"
    "uniquify_keep_original_design" "uniquify_naming_style"
    "use_ccs_in_sdn" "use_port_name_for_oscs" "var_mux_mbm"
    "verbose_messages" "verilogout_equation"
    "verilogout_higher_designs_first" "verilogout_ignore_case"
    "verilogout_include_files" "verilogout_no_negative_index"
    "verilogout_no_tri" "verilogout_show_unconnected_pins"
    "verilogout_single_bit" "verilogout_unconnected_prefix"
    "vhdllib_architecture" "vhdllib_glitch_handle" "vhdllib_logic_system"
    "vhdllib_logical_name" "vhdllib_negative_constraint"
    "vhdllib_pulse_handle" "vhdllib_sdf_edge" "vhdllib_tb_compare"
    "vhdllib_tb_x_eq_dontcare" "vhdllib_timing_checks"
    "vhdllib_timing_mesg" "vhdllib_timing_xgen" "vhdllib_vital_99"
    "vhdlout_architecture_name" "vhdlout_bit_type"
    "vhdlout_bit_type_resolved" "vhdlout_bit_vector_type"
    "vhdlout_conversion_functions" "vhdlout_dont_create_dummy_nets"
    "vhdlout_dont_write_types" "vhdlout_equations"
    "vhdlout_follow_vector_direction" "vhdlout_lower_design_vector"
    "vhdlout_one_name" "vhdlout_package_naming_style"
    "vhdlout_preserve_hierarchical_types" "vhdlout_separate_scan_in"
    "vhdlout_single_bit" "vhdlout_synthesis_off"
    "vhdlout_target_simulator" "vhdlout_three_state_name"
    "vhdlout_three_state_res_func" "vhdlout_time_scale"
    "vhdlout_top_configuration_arch_name"
    "vhdlout_top_configuration_entity_name"
    "vhdlout_top_configuration_name" "vhdlout_top_design_vector"
    "vhdlout_unconnected_pin_prefix" "vhdlout_unknown_name"
    "vhdlout_upcase" "vhdlout_use_packages" "vhdlout_wired_and_res_func"
    "vhdlout_wired_or_res_func" "vhdlout_write_architecture"
    "vhdlout_write_attributes" "vhdlout_write_components"
    "vhdlout_write_entity" "vhdlout_write_top_configuration"
    "vhdlout_zero_name" "view_analyze_file_suffix" "view_arch_types"
    "view_background" "view_cache_images" "view_command_log_file"
    "view_command_win_max_lines" "view_dialogs_modal"
    "view_disable_cursor_warping" "view_disable_error_windows"
    "view_disable_output" "view_error_window_count"
    "view_execute_script_suffix" "view_info_search_cmd" "view_log_file"
    "view_on_line_doc_cmd" "view_read_file_suffix"
    "view_script_submenu_items" "view_tools_menu_items"
    "view_use_small_cursor" "view_use_x_routines" "view_write_file_suffix"
    "when_analysis_permitted" "when_analysis_without_case_analysis"
    "write_name_mapping_nowarn_libraries" "write_name_nets_same_as_ports"
    "write_sdc_output_lumped_net_capacitance"
    "write_sdc_output_net_resistance" "write_test_formats"
    "write_test_include_scan_cell_info" "write_test_input_dont_care_value"
    "write_test_max_cycles" "write_test_max_scan_patterns"
    "write_test_pattern_set_naming_style" "write_test_round_timing_values"
    "write_test_scan_check_file_naming_style"
    "write_test_vector_file_naming_style" "write_test_vhdlout"
    "x11_set_cursor_background" "x11_set_cursor_foreground"
    "x11_set_cursor_number" "xnfin_dff_clock_enable_pin_name"
    "xnfin_dff_clock_pin_name" "xnfin_dff_data_pin_name"
    "xnfin_dff_q_pin_name" "xnfin_dff_reset_pin_name"
    "xnfin_dff_set_pin_name" "xnfin_family" "xnfin_ignore_pins"
    "xnfout_clock_attribute_style" "xnfout_constraints_per_endpoint"
    "xnfout_default_time_constraints" "xnfout_library_version"
    "xterm_executable"
    )
  "List of DC variables (2006.06 release).")

(defconst tcl-dc-attributes
  '(
    "actual_max_net_capacitance" "actual_min_net_capacitance" "area"
    "ba_net_resistance" "boundary_optimization" "default_flip_flop_type"
    "default_flip_flop_type_exact" "default_latch_type" "default_values"
    "design_type" "disable_timing" "dont_touch" "dont_touch_network"
    "dont_use" "driven_by_logic_one" "driven_by_logic_zero"
    "driving_cell_dont_scale" "driving_cell_fall"
    "driving_cell_from_pin_fall" "driving_cell_from_pin_rise"
    "driving_cell_library_fall" "driving_cell_library_rise"
    "driving_cell_multiplier" "driving_cell_pin_fall"
    "driving_cell_pin_rise" "driving_cell_rise" "fall_delay" "fall_drive"
    "fanout_load" "fix_hold" "flatten" "flatten_effort" "flatten_minimize"
    "flatten_phase" "flip_flop_type" "flip_flop_type_exact" "is_black_box"
    "is_combinational" "is_hierarchical" "is_mapped" "is_sequential"
    "is_test_circuitry" "is_unmapped" "k_process_values" "k_temp_values"
    "k_volt_values" "latch_type" "latch_type_exact" "load"
    "local_link_library" "max_capacitance" "max_fanout" "max_time_borrow"
    "max_transition" "min_capacitance" "minus_uncertainty" "nom_process"
    "nom_temperature" "nom_voltage" "output_not_used" "pad_location"
    "part" "period" "pin_direction" "plus_uncertainty" "port_direction"
    "port_is_pad" "preferred" "propagated_clock" "ref_name" "rise_delay"
    "rise_drive" "structure" "subtract_pin_load" "ungroup" "wired_and"
    "wired_logic_disable" "wired_or" "xnf_init" "xnf_loc"
    )
  "List of DC attributes (2006.06 release).")

;; `regexp-opt' undefined (`xemacs-devel' not installed)
(unless (fboundp 'regexp-opt)
  (defun regexp-opt (strings &optional paren)
    (let ((open (if paren "\\(" "")) (close (if paren "\\)" "")))
      (concat open (mapconcat 'regexp-quote strings "\\|") close))))

(defconst tcl-dc-keywords-regexp
  (concat "\\<\\(" (regexp-opt tcl-dc-keywords) "\\)\\>")
  "Regexp for Tcl keywords.")

(defconst tcl-dc-commands-regexp
  (concat "\\<\\(" (regexp-opt (append tcl-dc-commands
				       (unless tcl-dc-extra-highlight
					 tcl-dc-extra-commands))) "\\)\\>")
  "Regexp for DC commands.")

(defconst tcl-dc-variables-regexp
  (concat "\\<\\(" (regexp-opt (append tcl-dc-variables
				       (unless tcl-dc-extra-highlight
					 tcl-dc-extra-variables))) "\\)\\>")
  "Regexp for DC variables.")

(defconst tcl-dc-attributes-regexp
  (concat "\\<\\(" (regexp-opt (append tcl-dc-attributes
				       (unless tcl-dc-extra-highlight
					 tcl-dc-extra-attributes))) "\\)\\>")
  "Regexp for DC attributes.")

(defconst tcl-dc-extra-commands-regexp
  (concat "\\<\\(" (regexp-opt (if tcl-dc-extra-highlight
				   (append tcl-dc-extra-commands
					   tcl-dc-extra-attributes)
				 '(""))) "\\)\\>")
  "Regexp for extra commands.")

(defconst tcl-dc-extra-variables-regexp
  (concat "\\<\\(" (regexp-opt (if tcl-dc-extra-highlight
				   tcl-dc-extra-variables
				 '(""))) "\\)\\>")
  "Regexp for extra variables.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fontification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar tcl-dc-font-lock-keywords
  (list
   ;; highlight predefined DC variables
   (list (concat "\\(\${?\\|\\<\\(set\\|global\\|variable\\)\\s-+\\)"
		 tcl-dc-variables-regexp)
	 3 'font-lock-type-face)
   ;; highlight extra variables
   (list (concat "\\(\${?\\|\\<\\(set\\|global\\|variable\\)\\s-+\\)"
		 tcl-dc-extra-variables-regexp)
	 3 'font-lock-warning-face)
   ;; highlight Tcl variables
   '("\\(\${?\\|\\<\\(set\\|global\\|variable\\)\\s-+\\)\\(\\w+\\)"
     3 'font-lock-variable-name-face)
   ;; highlight DC command options
   '("\\s-\\(-[a-z][_0-9a-z]*\\)\\>" 1 'font-lock-builtin-face)
   ;; highlight Tcl keywords
   (list tcl-dc-keywords-regexp 1 'font-lock-keyword-face)
   ;; highlight DC commands
   (list tcl-dc-commands-regexp 1 'font-lock-keyword-face)
   ;; highlight extra commands
   (list tcl-dc-extra-commands-regexp 1 'font-lock-warning-face)
   ;; highlight predefined DC attributes
   (list tcl-dc-attributes-regexp 1 'font-lock-constant-face)
   ;; highlight function names
   '("\\<proc\\s-+\\(\\w+\\)" 1 'font-lock-function-name-face)
  )
  "Regular expressions to highlight.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Electrification

(defun tcl-dc-electric-tab (&optional prefix-arg)
  "If preceeding character is part of a word or a paren then hippie-expand,
else if right of non whitespace on line then tab-to-tab-stop,
else if last command was a tab or return then dedent one step or if a comment
toggle between normal indent and inline comment indent,
else indent `correctly'.
If `tcl-dc-intelligent-tab' is nil, always indent line."
  (interactive "*P")
  (if tcl-dc-intelligent-tab
      (progn
	(cond
	 ;; indent region if region is active
	 ((and (not (featurep 'xemacs)) transient-mark-mode mark-active)
	  (indent-region (region-beginning) (region-end) nil))
	 ;; expand word
	 ((memq (char-syntax (preceding-char)) '(?w ?_))
	  (let ((case-fold-search t)
		(case-replace nil)
		(current-syntax-table (syntax-table))
		(hippie-expand-only-buffers
		 (or (and (boundp 'hippie-expand-only-buffers)
			  hippie-expand-only-buffers)
		     '(tcl-dc-mode))))
	    (set-syntax-table tcl-dc-he-syntax-table)
	    (tcl-dc-expand-abbrev prefix-arg)
	    (set-syntax-table current-syntax-table)))
	 ;; insert tab
	 ((> (current-column) (current-indentation))
	  (tab-to-tab-stop))
	 ;; dedent
	 ((and (eq last-command 'tcl-dc-electric-tab)
	       (/= 0 (current-indentation)))
	  (backward-delete-char-untabify tcl-indent-level nil))
	 ;; indent
	 (t (indent-according-to-mode)))
	(setq this-command 'tcl-dc-electric-tab))
    (indent-according-to-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hippie expand customization (for expansion of DC commands)

(defun tcl-dc-list-to-alist (list)
  "Convert list to alist."
  (let (alist)
    (while list
      (setq alist (cons (list (car list)) alist))
      (setq list (cdr list)))
    (nreverse alist)))

(defvar tcl-dc-commands-list
  (sort (append tcl-dc-keywords
		tcl-dc-commands
		(copy-sequence tcl-dc-extra-commands)
		tcl-dc-attributes
		(copy-sequence tcl-dc-extra-attributes)) 'string<)
  "List of Tcl keywords and DC commands.")

(defvar tcl-dc-variables-list
  (sort (append tcl-dc-variables
		(copy-sequence tcl-dc-extra-variables)) 'string<)
  "List of DC variables.")

(defvar tcl-dc-commands-alist (tcl-dc-list-to-alist tcl-dc-commands-list)
  "Alist of `tcl-dc-commands-list'.")

(defvar tcl-dc-variables-alist (tcl-dc-list-to-alist tcl-dc-variables-list)
  "Alist of `tcl-dc-variables-list'.")

(defvar tcl-dc-abbrev-list)

(defvar tcl-dc-abbrev-alist)

(eval-when-compile (require 'hippie-exp))

(defun tcl-dc-try-expand-abbrev (old)
  "Try expanding abbreviations from `tcl-dc-commands-list' and
`tcl-dc-variables-list'."
  (let ((current-syntax-table (syntax-table))
	common)
    (unless old
      ;; find expansion string
      (he-init-string (he-dabbrev-beg) (point))
      ;; if preceded by "$" or "set " expand variables
      (if (or (save-excursion (skip-syntax-backward "w")
			      (= (preceding-char) ?$))
	      (save-excursion (skip-syntax-backward "w")
			      (skip-syntax-backward " ")
			      (skip-syntax-backward "w")
			      (looking-at "\\<set\\>")))
	  (setq tcl-dc-abbrev-list tcl-dc-variables-list
		tcl-dc-abbrev-alist tcl-dc-variables-alist)
	;; else expand commands
	(setq tcl-dc-abbrev-list tcl-dc-commands-list
	      tcl-dc-abbrev-alist tcl-dc-commands-alist))
      ;; find common substring
      (setq common (try-completion he-search-string tcl-dc-abbrev-alist))
      ;; determine expansion list
      (setq he-expand-list
	    (let ((abbrev-list tcl-dc-abbrev-list)
		  (sel-abbrev-list (when (and common (not (eq common t)))
				     (list common))))
	      (while abbrev-list
		(when (string-match
		       (concat "^" he-search-string) (car abbrev-list))
		  (setq sel-abbrev-list
			(cons (car abbrev-list) sel-abbrev-list)))
		(setq abbrev-list (cdr abbrev-list)))
	      (nreverse sel-abbrev-list))))
    ;; find next in expansion list
    (while (and he-expand-list
		(he-string-member (car he-expand-list) he-tried-table t))
      (setq he-expand-list (cdr he-expand-list)))
    ;; show expansion list in minibuffer
    (when (and common (> (length he-expand-list) 2))
      (message "%s" (cdr he-expand-list)))
    ;; expand
    (if (null he-expand-list)
	(progn (when old (he-reset-string))
	       nil)
      (he-substitute-string (car he-expand-list) t)
      (setq he-expand-list (cdr he-expand-list))
      t)))

;; function for expanding abbrevs and dabbrevs
(defun tcl-dc-expand-abbrev (arg))
(fset 'tcl-dc-expand-abbrev (make-hippie-expand-function
			     '(try-expand-dabbrev
			       try-expand-dabbrev-all-buffers
			       tcl-dc-try-expand-abbrev)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comments

(defun tcl-dc-comment-uncomment-region (beg end &optional arg)
  "Comment region if not commented, uncomment region if already commented."
  (interactive "r\nP")
  (goto-char beg)
  (if (looking-at (regexp-quote comment-start))
      (comment-region beg end '(4))
    (comment-region beg end)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tcl-dc-doc-mode ()
  "Display Tcl DC Mode documentation in *Help* buffer."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ "Tcl DC Mode:\n")
    (princ (documentation 'tcl-dc-mode))
    (unless (string-match "XEmacs" emacs-version)
      (help-setup-xref (list #'tcl-dc-doc-mode) (interactive-p)))
    (save-excursion
      (set-buffer standard-output)
      (help-mode))
    (print-help-return-message)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'tcl-dc-mode)

;;; tcl-dc-mode.el ends here
