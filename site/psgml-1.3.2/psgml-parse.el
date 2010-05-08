;;;; psgml-parse.el --- Parser for SGML-editing mode with parsing support
;; $Id: psgml-parse.el,v 2.99 2005/02/27 17:13:07 lenst Exp $

;; Copyright (C) 1994, 1995, 1996, 1997, 1998 Lennart Staflin

;; Author: Lennart Staflin <lenst@lysator.liu.se>
;; Acknowledgment:
;;   The catalog and XML parsing code was contributed by
;;      David Megginson <david@megginson.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;;; Commentary:

;; Part of major mode for editing the SGML document-markup language.


;;;; Code:

(require 'psgml)
(require 'psgml-sysdep)
(require 'psgml-ids)			; just for sgml-add-id


;;; Interface to psgml-dtd
(eval-and-compile
  (autoload 'sgml-do-usemap-element  "psgml-dtd")
  (autoload 'sgml-write-dtd	     "psgml-dtd")
  (autoload 'sgml-check-dtd-subset   "psgml-dtd") )

(eval-when-compile (require 'cl))


;;;; Advise to do-auto-fill

(defvar sgml-auto-fill-inhibit-function nil
  "If non-nil, it should be a function of no arguments.
The functions is evaluated before the standard auto-fill function,
`do-auto-fill', tries to fill a line.  If the function returns a true
value the auto-fill is inhibited.")

;;(defadvice do-auto-fill (around disable-auto-fill-hook activate)
;;  (or (and sgml-auto-fill-inhibit-function
;;	   (funcall sgml-auto-fill-inhibit-function))
;;      ad-do-it))


;;;; Variables

(defvar sgml-psgml-pi-enable-outside-dtd nil)


;;; Hooks

(defvar sgml-open-element-hook nil
  "The hook run by `sgml-open-element'.
Theses functions are called with two arguments, the first argument is
the opened element and the second argument is the attribute specification
list.  It is probably best not to refer to the content or the end-tag of
the element.")

(defvar sgml-close-element-hook nil
  "The hook run by `sgml-close-element'.
These functions are invoked with `sgml-current-tree' bound to the
element just parsed.")

(defvar sgml-doctype-parsed-hook nil
  "This hook is called after the doctype has been parsed.
It can be used to load any additional information into the DTD structure.")

(defvar sgml-sysid-resolve-functions nil
  "A list of functions for resolving sysids.
Each function should take one argument, the system identifier of an entity.
If the function can handle that identifier, it should insert the text
of the entity into the current buffer at point and return t.  If the
system identifier is not handled the function should return nil.")


;;; Internal variables

(defconst sgml-pcdata-token (intern "#PCDATA"))

(defvar sgml-computed-map nil
  "Internal representation of entity search map.")

(defvar sgml-used-entity-map nil
  "Value of `sgml-current-entity-map' used to compute the map in `sgml-compute-map'.")

(defvar sgml-last-element nil
  "Used to keep information about position in element structure between commands.")

(defconst sgml-users-of-last-element
  '(sgml-beginning-of-element
    sgml-end-of-element
    sgml-up-element
    sgml-backward-up-element
    sgml-backward-element
    sgml-forward-element
    sgml-down-element
    sgml-show-context
    sgml-next-data-field
    )
  "List of commands that set the variable `sgml-last-element'.")

(defvar sgml-parser-syntax nil
  "Syntax table used during parsing.")

(defvar sgml-ecat-assoc nil
  "Assoc list caching parsed ecats.")

(defvar sgml-catalog-assoc nil
  "Assoc list caching parsed catalogs.")


;;; Variables dynamically bound to affect parsing

(defvar sgml-throw-on-warning nil
  "Set to a symbol other than nil to make `sgml-log-warning' throw to that symbol.")

(defvar sgml-throw-on-error nil
  "Set to a symbol other than nil to make `sgml-error' throw to that symbol.")

(defvar sgml-show-warnings nil
  "Set to t to show warnings.")

(defvar sgml-close-element-trap nil
  "Can be nil for no trap, an element or t for any element.
Tested by `sgml-close-element' to see if the parse should be ended.")

(defvar sgml-goal 0
  "Point in buffer to parse up to.")

(defvar sgml-shortref-handler (function sgml-handle-shortref)
  "Function called by parser to handle a short reference.
Called with the entity as argument.  The start and end of the
short reference is `sgml-markup-start' and point.")

(defvar sgml-data-function nil
  "Function called with parsed data.")

(defvar sgml-entity-function nil
  "Function called with entity referenced at current point in parse.")

(defvar sgml-pi-function nil
  "Function called with parsed processing instruction.")

(defvar sgml-signal-data-function nil
  "Called when some data characters are conceptually parsed.
E.g. a data entity reference.")

(defvar sgml-throw-on-element-change nil
  "Throw tag.")

;;; Global variables active during parsing

(defvar sgml-parsing-dtd nil
  "This variable is bound to t while parsing a DTD (subset).")

(defvar sgml-rs-ignore-pos nil
  "Set to position of last parsing start in current buffer.")
(make-variable-buffer-local 'sgml-rs-ignore-pos)

(defvar sgml-dtd-info nil
  "Holds the `sgml-dtd' structure describing the current DTD.")

(defvar sgml-current-namecase-general t
  "Value of `sgml-namecase-general' in main buffer.  Valid during parsing.")

(defvar sgml-current-omittag nil
  "Value of `sgml-omittag' in main buffer.  Valid during parsing.")

(defvar sgml-current-shorttag nil
  "Value of `sgml-shorttag' in main buffer.  Valid during parsing.")

(defvar sgml-current-localcat nil
  "Value of `sgml-local-catalogs' in main buffer.  Valid during parsing.")

(defvar sgml-current-local-ecat nil
  "Value of `sgml-local-ecat-files' in main buffer.  Valid during parsing.")

(defvar sgml-current-top-buffer nil
  "The buffer of the document entity, the main buffer.
Valid during parsing.  This is used to find current directory for
catalogs.")

(defvar sgml-current-state nil
  "Current state in content model or model type if CDATA, RCDATA or ANY.")

(defvar sgml-current-shortmap nil
  "The current active short reference map.")

(defvar sgml-current-tree nil
  "Current parse tree node, identifies open element.")

(defvar sgml-previous-tree nil
  "Previous tree node in current tree.
This is nil if no previous node.")

(defvar sgml-last-buffer nil
  "Buffer where last parse was ended.
Used for restarting parser at the point where it left of.")

(defvar sgml-markup-type nil
"Contains the type of markup parsed last.
The value is a symbol:
nil	- pcdata or space
CDATA	- CDATA or RCDATA
comment	- comment declaration
doctype	- doctype declaration
end-tag
ignored	- ignored marked section
ms-end	- marked section start, if not ignored
ms-start - marked section end, if not ignored
pi	- processing instruction
sgml	- SGML declaration
start-tag
entity  - general entity reference
param   - parameter reference
shortref- short reference
mdecl   - markup declaration")

(defvar sgml-top-tree nil
  "Root node of parse tree during parsing.")

(defvar sgml-markup-tree nil
  "Tree node of markup parsed.
In case markup closed element this is different from `sgml-current-tree'.
Only valid after `sgml-parse-to'.")

(defvar sgml-markup-start nil
  "Start point of markup being parsed.")

(defvar sgml-conref-flag nil
  "Set by `sgml-parse-attribute-specification-list' if a CONREF attribute is parsed.")

(defvar sgml-no-elements nil
  "Number of declared elements.")

;;; Vars used in *param* buffers

(defvar sgml-previous-buffer nil)

(defvar sgml-current-eref nil
  "This is the entity reference used to enter current entity.
If this is nil, then current entity is main buffer.")

(defvar sgml-current-file nil
  "This is the file name of the current entity.")

(defvar sgml-scratch-buffer nil
  "The global value of this variable is the first scratch buffer for entities.
The entity buffers can have a buffer local value for this variable
to point to the next scratch buffer.")

(defvar sgml-last-entity-buffer nil)

;;; For loading DTD

(eval-and-compile
  (defconst sgml-max-single-octet-number 250
    "Octets greater than this is the first of a two octet coding."))

(defvar sgml-read-token-vector nil)	; Vector of symbols used to decode
					; token numbers.
(defvar sgml-read-nodes nil)		; Vector of nodes used when reading
					; a finite automaton.

;; Buffer local variables

(defvar sgml-loaded-dtd nil
  "File name corresponding to current DTD.")
(make-variable-buffer-local 'sgml-loaded-dtd)

(defvar sgml-current-element-name nil
  "Name of current element for mode line display.")
(make-variable-buffer-local 'sgml-current-element-name)

(defvar sgml-dtd-less nil
  "Non-nil means the document doesn't have a DTD.
Applicable to XML.")
(make-variable-buffer-local 'sgml-dtd-less)

;;;; Build parser syntax table

(setq sgml-parser-syntax (make-syntax-table))

(let ((i 0))
  (while (< i 256)
    (modify-syntax-entry i " " sgml-parser-syntax)
    (setq i (1+ i))))

(mapconcat (function (lambda (c)
	     (modify-syntax-entry c "w" sgml-parser-syntax)))
	   "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrtsuvwxyz" "")
(mapconcat (function (lambda (c)
		       (modify-syntax-entry c "_" sgml-parser-syntax)))
	   "-.0123456789" "")


;;(progn (set-syntax-table sgml-parser-syntax) (describe-syntax))

(defconst xml-parser-syntax
  (let ((tab (make-syntax-table)))
    (let ((i 0))
      (while (< i 128)
	(modify-syntax-entry i " " tab)
	(setq i (1+ i))))
    (mapconcat (function (lambda (c)
			   (modify-syntax-entry c "w" tab)))
	       "_:ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrtsuvwxyz" "")
    (mapconcat (function (lambda (c)
			   (modify-syntax-entry c "_" tab)))
	       ;; Fixme: what's the non-ASCII character doing here?  -- fx
	       "-.0123456789·" "")
    tab))

;;(progn (set-syntax-table xml-parser-syntax) (describe-syntax))

(defmacro sgml-with-parser-syntax (&rest body)
  (` (let ((normal-syntax-table (syntax-table))
           (cb (current-buffer)))
       (set-syntax-table (if sgml-xml-p xml-parser-syntax sgml-parser-syntax))
       (unwind-protect
	   (progn (,@ body))
         (setq sgml-last-buffer (current-buffer))
         (set-buffer cb)
	 (set-syntax-table normal-syntax-table)))))

(defmacro sgml-with-parser-syntax-ro (&rest body)
  ;; Should only be used for parsing ....
  (` (let ((normal-syntax-table (syntax-table))
           (cb (current-buffer))
           (buffer-modified (buffer-modified-p)))
       (set-syntax-table (if sgml-xml-p xml-parser-syntax sgml-parser-syntax))
       (unwind-protect
	   (progn (,@ body))
         (setq sgml-last-buffer (current-buffer))
         (set-buffer cb)
	 (set-syntax-table normal-syntax-table)
         (sgml-restore-buffer-modified-p buffer-modified)
         (sgml-debug "Restoring buffer mod: %s" buffer-modified)))))

(defun sgml-set-buffer-multibyte (flag)
  (cond ((featurep 'xemacs)
         flag)
        ((and (boundp 'emacs-major-version) (>= emacs-major-version 20))
         (set-buffer-multibyte
          (if (eq flag 'default)
              default-enable-multibyte-characters
            flag)))
	((boundp 'MULE)
         (set 'mc-flag flag))
        (t
         flag)))
;; Probably better.  -- fx
;; (eval-and-compile
;;   (if (fboundp 'set-buffer-multibyte)
;;       (defalias 'sgml-set-buffer-multibyte
;; 	(if (fboundp 'set-buffer-multibyte)
;; 	    'set-buffer-multibyte
;; 	  'identity))))


;;;; State machine

;; From the parsers POV a state is a mapping from tokens (in sgml it
;; is primitive state tokens) to states.  The pairs of the mapping is
;; called moves.

;; DFAs are always represented by the start state, which is a
;; normal state.  Normal states contain moves of two types:
;; 1. moves for required tokens, 2. moves for optional tokens.
;; By design these are keept in two different sets.
;; [Alt: they could perhaps have been keept in one set but
;; marked in different ways.]

;; The and-model groups creates too big state machines, therefor
;; there is a datastruture called and-node.

;; An and-node is a specification for a dfa that has not been computed.
;; It contains a set of dfas that all have to be traversed before going
;; to the next state.  The and-nodes are only stored in moves and are
;; not seen by the parser.  When a move is taken the and-node is converted
;; to an and-state.

;; An and-state keeps track of which dfas still need to be
;; traversed and the state of the current dfa.

;; move = <token, node>

;; node = normal-state | and-node

;; and-node = <dfas, next>
;; where: dfas is a set of normal-state
;;        next is a normal-state

;; State = normal-state | and-state
;; The parser only knows about the state type.

;; normal-state = <opts, reqs>
;; where: opts is a set of moves for optional tokens
;; 	  reqs is a set of moves for required tokens

;; and-state = <substate, dfas, next>
;; where: substate is a normal-state
;;        dfas is a set of states
;;        next is the next state

;; The and-state is only used during the parsing.
;; Primitiv functions to get data from parse state need
;; to know both normal-state and and-state.


;;; Representations:

;;move: (token . node)

(defmacro sgml-make-move (token node)
  (` (cons (, token) (, node))))

(defmacro sgml-move-token (x)
  (` (car (, x))))

(defmacro sgml-move-dest (x)
  (` (cdr (, x))))

;; set of moves: list of moves

(defmacro sgml-add-move-to-set (token node set)
  (`(cons (cons (, token) (, node)) (, set))))

(defmacro sgml-moves-lookup (token set)
  (` (assq (, token) (, set))))

;; normal-state: ('normal-state opts . reqs)

(defsubst sgml-make-state ()
  (cons 'normal-state (cons nil nil)))

(defmacro sgml-normal-state-p (s)
  (` (eq (car (, s)) 'normal-state)))

(defmacro sgml-state-opts (s)
  (` (cadr (, s))))

(defmacro sgml-state-reqs (s)
  (` (cddr (, s))))

(defmacro sgml-state-final-p (s)
  (`(null (sgml-state-reqs (, s)))))

;; adding moves
;; *** Should these functions check for ambiguity?
;; What if adding a optional move for a token that has a
;;  required move?
;; What about the other way?

(defsubst sgml-add-opt-move (s token dest)
  (or (sgml-moves-lookup token (sgml-state-opts s))
      (setf (sgml-state-opts s)
	    (sgml-add-move-to-set token dest (sgml-state-opts s)))))

(defsubst sgml-add-req-move (s token dest)
  (or (sgml-moves-lookup token (sgml-state-reqs s))
      (setf (sgml-state-reqs s)
	    (sgml-add-move-to-set token dest (sgml-state-reqs s)))))

(defsubst sgml-make-primitive-content-token (token)
  (let ((s1 (sgml-make-state))
	(s2 (sgml-make-state)))
    (sgml-add-req-move s1 token s2)
    s1))

;;and-state: (state next . dfas)

(defsubst sgml-make-and-state (state dfas next)
  (cons state (cons next dfas)))

(defsubst sgml-step-and-state (state and-state)
  (cons state (cdr and-state)))

(defsubst sgml-and-state-substate (s)
  (car s))

(defsubst sgml-and-state-dfas (s)
  (cddr s))

(defsubst sgml-and-state-next (s)
  (cadr s))


;;and-node:  (next . dfas)

(defsubst sgml-make-and-node (dfas next)
  (cons next dfas))

(defmacro sgml-and-node-next (n)
  (` (car (, n))))

(defmacro sgml-and-node-dfas (n)
  (` (cdr (, n))))


;;; Using states

(defsubst sgml-final (state)
  (if (sgml-normal-state-p state)
      (sgml-state-final-p state)
    (sgml-final-and state)))

(defun sgml-final-and (state)
  (and (sgml-final (sgml-and-state-substate state))
       (loop for s in (sgml-and-state-dfas state)
	     always (sgml-state-final-p s))
       (sgml-state-final-p (sgml-and-state-next state))))


;; get-move: State x Token --> State|nil

(defsubst sgml-get-move (state token)
  "Return a new state or nil, after traversing TOKEN from STATE."
  (cond
   ((symbolp state) nil)                ;if EMPTY slips thru...
   ((sgml-normal-state-p state)
    (let ((c (or (sgml-moves-lookup token (sgml-state-opts state))
		 (sgml-moves-lookup token (sgml-state-reqs state)))))
      (if c
	  (let ((dest (sgml-move-dest c)))
	    (if (sgml-normal-state-p dest)
		dest
	      ;; dest is a and-node
	      (sgml-next-sub-and (sgml-and-node-dfas dest)
				 token
				 (sgml-and-node-next dest)))))))
   (t					;state is a and-state
    (sgml-get-and-move state token))))

(defun sgml-get-and-move (state token)
  ;; state is a and-state
  (let ((m (sgml-get-move (sgml-and-state-substate state) token)))
    (cond (m (cons m (cdr state)))
	  ((sgml-final (sgml-and-state-substate state))
	   (sgml-next-sub-and (sgml-and-state-dfas state)
			      token
			      (sgml-and-state-next state))))))

(defun sgml-next-sub-and (dfas token next)
  "Compute the next state, choosing from DFAS and moving by TOKEN.
If this is not possible, but all DFAS are final, move by TOKEN in NEXT."
  (let ((allfinal t)
	(l dfas)
	(res nil)
	s1 s2)
    (while (and l (not res))
      (setq s1 (car l)
	    allfinal (and allfinal (sgml-state-final-p s1))
	    s2 (sgml-get-move s1 token)
	    res (and s2 (sgml-make-and-state s2 (remq s1 dfas) next))
	    l (cdr l)))
    (cond (res)
	  (allfinal (sgml-get-move next token)))))

(defsubst sgml-tokens-of-moves (moves)
  (mapcar (function (lambda (m) (sgml-move-token m)))
	  moves))

(defun sgml-required-tokens (state)
  (if (sgml-normal-state-p state)
      (sgml-tokens-of-moves (sgml-state-reqs state))
    (or (sgml-required-tokens (sgml-and-state-substate state))
        (loop for s in (sgml-and-state-dfas state)
              nconc (sgml-tokens-of-moves (sgml-state-reqs s)))
        (sgml-tokens-of-moves (sgml-state-reqs (sgml-and-state-next state))))))

(defun sgml-optional-tokens (state)
  (if (sgml-normal-state-p state)
      (sgml-tokens-of-moves (sgml-state-opts state))
    (nconc
     (sgml-optional-tokens (sgml-and-state-substate state))
     (if (sgml-final (sgml-and-state-substate state))
	 (loop for s in (sgml-and-state-dfas state)
	       nconc (sgml-tokens-of-moves (sgml-state-opts s))))
     (if (loop for s in (sgml-and-state-dfas state)
               always (sgml-state-final-p s))
	 (sgml-tokens-of-moves
	  (sgml-state-opts (sgml-and-state-next state)))))))


;;;; Attribute Types

;;; Basic Types
;; name = string	attribute names are lisp strings
;; attval = string	attribute values are lisp strings

;;; Attribute Declaration Type
;; attdecl = <name, declared-value, default-value>

;; This is the result of the ATTLIST declarations in the DTD.
;; All attribute declarations for an element is the elements
;; attlist.

;;; Attribute Declaration Operations
;; sgml-make-attdecl: name declared-value default-value -> attdecl
;; sgml-attdecl-name: attdecl -> name
;; sgml-attdecl-declared-value: attdecl -> declared-value
;; sgml-attdecl-default-value: attdecl -> default-value

;;; Attribute Declaration List Type
;; attlist = attdecl*

;;; Attribute Declaration List Operations
;; sgml-lookup-attdecl: name x attlist -> attdecl

;;; Declared Value Type
;; declared-value = (token-group | notation | simpel)
;; token-group = nametoken+
;; notation = nametoken+
;; simple = symbol		lisp symbol corresponding to SGML type

;;; Declared Value Operations
;; sgml-declared-value-token-group: declared-value -> list of symbols
;; sgml-declared-value-notation: declared-value -> list of symbols
;; (empty list if not token-group/notation)

;;; Default Value Type
;; default-value = (required | implied | conref | specified )
;; implied, conref = constant symbol
;; specified = (fixed | normal)
;; fixed, normal = attval

;;; Default Value Operations
;; sgml-default-value-attval: default-value -> (attval | nil)
;; sgml-default-value-type-p: type x default-value -> cond

;;; Attribute Specification Type
;; attspec = <name, attval>

;; This is the result of parsing an attribute specification.

;; sgml-make-attspec: name x attval -> attspec
;; sgml-attspec-name: attspec -> name
;; sgml-attspec-attval: attspec -> attval


;;; Attribute Specification List Type
;; asl = attspec*

;; aka. attribute value list


;;; Code

;;; attdecl representation = (name declared-value default-value)

(defun sgml-make-attdecl (name dcl-value default-value)
  (list name dcl-value default-value))

(defun sgml-attdecl-name (attdecl)
  (car attdecl))

(defun sgml-attdecl-declared-value (attdecl)
  "The declared value of ATTDECL.
It may be a symbol or (name-token-group (NAME1 ... NAMEn))
or (notation  (NOT1 ... NOTn))"
  (cadr attdecl))

(defun sgml-attdecl-default-value (attdecl)
  "The default value of ATTDECL.
The default value is either a symbol (REQUIRED | IMPLIED | CURRENT |
CONREF) or a list with first element nil or symbol `FIXED' and second
element the value."
  (car (cddr attdecl)))


;;; attlist representation = (attspec*)

(defun sgml-lookup-attdecl (name attlist)
  "Return the attribute declaration for NAME in ATTLIST."
  (assoc name attlist))

(defun sgml-attribute-with-declared-value (attlist declared-value)
  "Find the first attribute in ATTLIST that has DECLARED-VALUE."
  (let ((found nil))
    (while (and attlist (not found))
      (when (equal declared-value
		   (sgml-attdecl-declared-value (car attlist)))
	(setq found (car attlist)))
      (setq attlist (cdr attlist)))
    found))


;;; declared-value representation
;; token-group = (name-token (symbol+))
;; notation = (notation (symbol+))
;; simple = symbol		lisp symbol correspoinding to SGML type

(defun sgml-make-declared-value (type &optional names)
  "Make a declared-value of TYPE.
TYPE should be a symbol.  If TYPE is name-token-group or notation
NAMES should be a list of symbols."
  (if (consp names)
      (list type names)
    type))

(defun sgml-declared-value-token-group (declared-value)
  "Return the name token group for the DECLARED-VALUE.
This applies to name token groups.  For other declared values nil is
returned."
  (and (consp declared-value)
       (eq 'name-token-group (car declared-value))
       (cadr declared-value)))

(defun sgml-declared-value-notation (declared-value)
  "Return the list of notation names for the DECLARED-VALUE.
This applies to notation declared value.  For other declared values
nil is returned."
  (and (consp declared-value)
       (eq 'NOTATION (car declared-value))
       (cadr declared-value)))

;;; default-value representation = symbol | ((nil | 'fixed) attval)

(defun sgml-make-default-value (type &optional attval)
  (if attval
      (list type attval)
    type))

(defun sgml-default-value-attval (default-value)
  "Return the actual default value of the declared DEFAULT-VALUE.
The actual value is a string.  Return nil if no actual value."
  (and (consp default-value)
       (cadr default-value)))

(defun sgml-default-value-type-p (type default-value)
  "Return true if DEFAULT-VALUE is of TYPE.
Where TYPE is a symbol, one of REQUIRED, IMPLIED, CONREF, or FIXED."
  (or (eq type default-value)
      (and (consp default-value)
	   (eq type (car default-value)))))


;;; attspec representation = (symbol . string)

(defun sgml-make-attspec (name attval)
  "Create an attspec from NAME and ATTVAL.
Special case, if ATTVAL is nil this is an implied attribute."
  (cons name attval))

;; sgml-attspec-name: attspec -> name
(defun sgml-attspec-name (attspec)
  (car attspec))

;; sgml-attspec-attval: attspec -> attval
(defun sgml-attspec-attval (attspec)
  "Return the value of attribute specification ATTSPEC.
If ATTSPEC is nil, nil is returned."
  (cdr attspec))

;;; asl representaion = (attspec*)

(defun sgml-lookup-attspec (name asl)
  (assoc name asl))


;;;; Element content types

;; The content of an element is defined as
;;	 (125 declared content | 126 content model),
;; 125  declared content = "CDATA" | "RCDATA" | "EMPTY"
;; 126  content model    = (127 model group | "ANY"),
;;			 (65 ps+, 138 exceptions)?

;; I represent a model group with the first state of a corresponding finite
;; automaton (this is a cons).  Exceptions are handled separately.
;; The other content types are represented by symbols.

(defsubst sgml-model-group-p (model)
  (consp model))

(defconst sgml-cdata 'CDATA)
(defconst sgml-rcdata 'RCDATA)
(defconst sgml-empty 'EMPTY)
(defconst sgml-any 'ANY)


;;;; External identifier
;; extid = (pubid? sysid? dir)
;; Representation as (pubid  sysid . dir)
;; where pubid = nil | string
;;       sysid = nil | string
;;       dir   = string

(defun sgml-make-extid (pubid sysid &optional pubid-ok)
  (and sgml-xml-p (not pubid-ok) pubid (not sysid)
    (sgml-error "XML requires a system ID after a public ID"))
  (cons pubid (cons sysid default-directory)))

(defun sgml-extid-pubid (extid)
  (car extid))

(defun sgml-extid-sysid (extid)
  (if (consp (cdr extid))
      (cadr extid)
    (cdr extid)))

(defun sgml-extid-dir (extid)
  "Directory where EXTID was declared."
  (if (consp (cdr extid))
      (cddr extid)
    nil))

(defun sgml-extid-expand (file extid)
  "Expand file name FILE in the context of EXTID."
  (let ((sgml-system-path (cons (sgml-extid-dir extid)
				sgml-system-path)))
    (or (sgml-extid-expand-2 file sgml-system-path)
	(expand-file-name file (sgml-extid-dir extid)))))

(defun sgml-extid-expand-2 (file directories)
  (cond ((null directories) nil)
	(t
	 (let ((f (expand-file-name file (car directories))))
	   (if (file-exists-p f)
	       f
	     (sgml-extid-expand-2 file (cdr directories)))))))
	       


;;;; DTD

;; DTD = (doctype, eltypes, parameters, entities, shortmaps,
;;	 notations, dependencies, merged)
;; DTDsubset ~=~ DTD, but doctype is unused
;;
;; doctype = name
;; eltypes = oblist
;; parameters = entity*
;; entities = entity*
;; shortmaps = (name, shortmap)*
;; dependencies = file*
;; merged = Compiled-DTD?  where  Compiled-DTD = (file, DTD)

(defstruct (sgml-dtd
	    (:type vector)
	    (:constructor sgml-make-dtd  (doctype)))
  doctype				; STRING, name of doctype
  (eltypes				; OBLIST, element types defined
   (sgml-make-eltype-table))
  (parameters				; ALIST
   (sgml-make-entity-table))
  (entities				; ALIST
   (sgml-make-entity-table))
  (shortmaps				; ALIST
   (sgml-make-shortref-table))
  (notations				; ??
   nil)
  (dependencies				; LIST
   nil)
  (merged				; (file . DTD)
   nil)
  (undef-entities			; LIST of entity names
   nil))


;;;; Element type objects

;; An element type object contains the information about an element type
;; obtained from parsing the DTD.

;; An element type object is represented by a symbol in a special oblist.
;; A table of element type objects is represented by a oblist.


;;; Element type objects

(defsubst sgml-eltype-name (et)
  (symbol-name et))

(defsubst sgml-eltype-defined (et)
  (fboundp et))

(defsubst sgml-eltype-token (et)
  "Return a token for the element type."
  et)

(defsubst sgml-token-eltype (token)
  "Return the element type corresponding to TOKEN."
  token)

(defmacro sgml-prop-fields (&rest names)
  (cons
   'progn
   (loop for n in names collect
	 (`(defmacro (, (intern (format "sgml-eltype-%s" n))) (et)
	     (list 'get et ''(, n)))))))

(sgml-prop-fields
 ;;flags			; optional tags and mixed
					; (perhaps in value field)
 ;;model					; Content type
					; (perhaps in function field)
 attlist				; List of defined attributes
 includes				; List of included elements
 excludes				; List of excluded elements
 shortmap				; Associated shortref map
					; nil if none and 'empty if #empty
 )

(defmacro sgml-eltype-flags (et)
  (` (symbol-value (, et))))

(defun sgml-eltype-model (et)
  (if (fboundp et)
      (symbol-function et)
    sgml-any))

(defsetf sgml-eltype-model fset)


(defun sgml-eltype-stag-optional (et)
  (= 1 (logand (sgml-eltype-flags et) 1)))

(defun sgml-eltype-etag-optional (et)
  (/= 0 (logand 2 (sgml-eltype-flags et))))

(defsubst sgml-eltype-mixed (et)
  (< 3 (sgml-eltype-flags et)))

(defsetf sgml-eltype-stag-optional (et) (f)
  (list 'sgml-set-eltype-flag et 1 f))
(defsetf sgml-eltype-etag-optional (et) (f)
  (list 'sgml-set-eltype-flag et 2 f))
(defsetf sgml-eltype-mixed (et) (f)
  (list 'sgml-set-eltype-flag et 4 f))

(defun sgml-set-eltype-flag (et mask f)
  (setf (sgml-eltype-flags et)
	(logior (logand (if (boundp et)
			    (sgml-eltype-flags et)
			  0)
			(lognot mask))
	       (if f mask 0))))

(defun sgml-maybe-put (sym prop val)
  (when val (put sym prop val)))

(defsetf sgml-eltype-includes (et) (l)
  (list 'sgml-maybe-put et ''includes l))

(defsetf sgml-eltype-excludes (et) (l)
  (list 'sgml-maybe-put et ''excludes l))

(defmacro sgml-eltype-appdata (et prop)
  "Get application data from element type ET with name PROP.
PROP should be a symbol, reserved names are: flags, model, attlist,
includes, excludes, conref-regexp, mixed, stag-optional, etag-optional."
  (` (get (, et) (, prop))))

(defun sgml-eltype-all-miscdata (et)
  (loop for p on (symbol-plist et) by (function cddr)
	unless (memq (car p) '(model flags includes excludes))
	nconc (list (car p) (cadr p))))

(defun sgml-eltype-set-all-miscdata (et miscdata)
  (setf (symbol-plist et)
	(nconc (symbol-plist et) miscdata)))

(defun sgml-make-eltype (name)
  (let ((et (make-symbol name)))
    (setf (sgml-eltype-flags et) 0)
    et))


;;; Element type tables

(defun sgml-make-eltype-table ()
  "Make an empty table of element types."
  (make-vector 73 0))

(defun sgml-eltype-table-empty (eltype-table)
  (loop for x across eltype-table always (eq x 0)))

(defun sgml-merge-eltypes (eltypes1 eltypes2)
  "Return the merge of two element type tables ELTYPES1 and ELTYPES2.
This may change ELTYPES1, ELTYPES2 is unchanged.  Returns the new table."
  (if (sgml-eltype-table-empty eltypes1)
      eltypes2
    (progn
      (mapatoms
       (function (lambda (sym)
		   (let ((et (intern (symbol-name sym) eltypes1)))
		     (unless (fboundp et) ; not yet defined by <!element
		       (when (fboundp sym)
			 (fset et (symbol-function sym)))
		       (when (boundp sym)
			 (set et (symbol-value sym))))
		     (setf (symbol-plist et)
			   (nconc (symbol-plist et)
				  (copy-sequence (symbol-plist sym)))))))
       eltypes2)      
      eltypes1)))

(defun sgml-lookup-eltype (name &optional dtd)
  "Lookup the element definition for NAME (string)."
  (intern name (sgml-dtd-eltypes (or dtd sgml-dtd-info))))

(defun sgml-eltype-completion-table (eltypes)
  "Make a completion table from a list, ELTYPES, of element types."
  (loop for et in eltypes as name = (sgml-eltype-name et)
	if (boundp et)
	collect (cons name name)))

(defun sgml-read-element-type (prompt dtd &optional default)
  "Read an element type name.
PROMPT is displayed as a prompt and DTD should be the dtd to get the
element types from.  Optional argument DEFAULT (string) will be used as
a default for the element type name."
  (let ((name
	 (let ((completion-ignore-case sgml-namecase-general))
           (completing-read prompt
                            (sgml-dtd-eltypes dtd)
                            (if sgml-dtd-less
				#'symbolp
			      #'fboundp)
                            (not sgml-dtd-less)))))
    (when (equal name "")
      (setq name (or default (error "Aborted"))))
    (sgml-lookup-eltype name dtd)))

(defun sgml-map-eltypes (fn dtd &optional collect all)
  (let ((*res* nil))
    (mapatoms
     (cond ((and collect all)
	    (function (lambda (a) (push (funcall fn a) *res*))))
	   (collect
	    (function (lambda (a) (when (boundp a)
				    (push (funcall fn a) *res*)))))
	   (all
	    fn)
	   (t
	    (function (lambda (a) (when (boundp a) (funcall fn a))))))
     (sgml-dtd-eltypes dtd))
    (nreverse *res*)))

;;;; Load a saved dtd

;;; Wing addition
(defmacro sgml-char-int (ch)
  (if (fboundp 'char-int)
      (` (char-int (, ch)))
    ch))

(defsubst sgml-read-octet ()
  ;; Wing change
  (prog1 (sgml-char-int (following-char))
    (forward-char)))

(defsubst sgml-read-peek ()
  (sgml-char-int (following-char)))

(defsubst sgml-read-number ()
  "Read a number.
A number is 1: an octet [0--sgml-max-single-octet-number]
or 2: two octets (n,m) interpreted as  (n-t-1)*256+m+t."
  (if (> (sgml-read-peek) sgml-max-single-octet-number)
      (+ (* (- (sgml-read-octet) (eval-when-compile
				   (1+ sgml-max-single-octet-number)))
	    256)
	 (sgml-read-octet)
	 sgml-max-single-octet-number)
    (sgml-read-octet)))


(defun sgml-read-sexp ()
  (prog1
      (let ((standard-input (current-buffer)))
	(read))
    (skip-chars-forward " \t")
    (forward-char 1)))

(defsubst sgml-read-token ()
  (aref sgml-read-token-vector (sgml-read-number)))

(defsubst sgml-read-node-ref ()
  (aref sgml-read-nodes (sgml-read-octet)))

(defun sgml-read-model-seq ()
  (loop repeat (sgml-read-number) collect (sgml-read-model)))

(defun sgml-read-token-seq ()
  (loop repeat (sgml-read-number) collect (sgml-read-token)))

(defun sgml-read-moves ()
  (loop repeat (sgml-read-number)
	collect (sgml-make-move (sgml-read-token) (sgml-read-node-ref))))

(defun sgml-read-model ()
  (let* ((n (sgml-read-number))
	 (sgml-read-nodes (make-vector n nil)))
    (loop for i below n do (aset sgml-read-nodes i (sgml-make-state)))
    (loop for e across sgml-read-nodes do
	  (cond ((eq 255 (sgml-read-peek))	; a and-node
		 (sgml-read-octet)		; skip
		 (setf (sgml-and-node-next e) (sgml-read-node-ref))
		 (setf (sgml-and-node-dfas e) (sgml-read-model-seq)))
		(t			; a normal-state
		 (setf (sgml-state-opts e) (sgml-read-moves))
		 (setf (sgml-state-reqs e) (sgml-read-moves)))))
    (aref sgml-read-nodes 0)))

(defun sgml-read-content ()
  (let ((c (sgml-read-octet)))
    (cond ((eq c 0) sgml-cdata)
	  ((eq c 1) sgml-rcdata)
	  ((eq c 2) sgml-empty)
	  ((eq c 3) sgml-any)
	  ((eq c 4) nil)
	  ((eq c 128)
	   (sgml-read-model)))))

(defun sgml-read-decode-flag (flag mask)
  (not (zerop (logand flag mask))))

(defun sgml-read-element (et)
  (sgml-eltype-set-all-miscdata et (sgml-read-sexp))
  (let ((flags (sgml-read-octet)))
    (unless (= flags 128)
      (setf (sgml-eltype-flags et) flags
	    (sgml-eltype-model et) (sgml-read-content)
	    (sgml-eltype-includes et) (sgml-read-token-seq)
	    (sgml-eltype-excludes et) (sgml-read-token-seq)))))

(defun sgml-read-dtd ()
  "Decode the saved DTD in current buffer, return the DTD."
  (let ((gc-cons-threshold (max gc-cons-threshold 500000))
        (file-version (sgml-read-sexp))
        dtd)
    (cond
     ((equal file-version '(sgml-saved-dtd-version 7))
      (setq dtd (sgml-bdtd-read-dtd)))
     ;; Something else
     (t
      (error "Unknown file format for saved DTD: %s" file-version)))
    dtd))

(defun sgml-load-dtd (file)
  "Load a saved DTD from FILE."
  (interactive
   (let ((tem (expand-file-name
	       (or sgml-default-dtd-file
		   (sgml-default-dtd-file)))))
     (list (read-file-name "Load DTD from: "
			   (file-name-directory tem)
			   tem
			   t
			   (file-name-nondirectory tem)))))
  (setq sgml-loaded-dtd nil)		; Allow reloading of DTD
  ;; Search for 'file' on the sgml-system-path [ndw]
  (let ((real-file (car (apply 'nconc
			       (mapcar (lambda (dir)
					 (let ((f (expand-file-name file dir)))
					   (if (file-exists-p f)
					       (list f))))
				       (cons "." sgml-system-path))))))
    (or real-file
	(error "Saved DTD file %s not found" file))
    (let ((cb (current-buffer))
	  (tem nil)
	  (dtd nil)
	  (l (buffer-list))
	  (find-file-type		; Allways binary
	   (function (lambda (fname) 1))))
      ;; Search loaded buffer for a already loaded DTD
      (while (and l (null tem))
	(set-buffer (car l))
	(if (equal sgml-loaded-dtd real-file)
	    (setq tem (current-buffer)))
	(setq l (cdr l)))
      (cond
       (tem				; loaded DTD found
	(setq dtd (sgml-pstate-dtd sgml-buffer-parse-state)))
       (t				; load DTD from file
	(set-buffer cb)
	(sgml-push-to-entity real-file)
	(message "Loading DTD from %s..." real-file)
	(setq dtd (sgml-read-dtd))
	(message "Loading DTD from %s...done" real-file)
	(sgml-pop-entity)))
      (set-buffer cb)
      (sgml-set-initial-state dtd)
      (setq sgml-default-dtd-file file)
      (setq sgml-loaded-dtd real-file))))

;;;; Binary coded DTD module
;;; Works on the binary coded compiled DTD (bdtd)

;;; bdtd-load: cfile dtdfile ents -> bdtd
;;; bdtd-merge: bdtd dtd -> dtd?
;;; bdtd-read-dtd: bdtd -> dtd

;;; Implement by letting bdtd be implicitly the current buffer and
;;; dtd implicit in sgml-dtd-info.

(defun sgml-bdtd-load (cfile dtdfile ents)
  "Load the compiled dtd from CFILE into the current buffer.
If this file does not exist, is of an old version or out of date, a
new compiled dtd will be created from file DTDFILE and parameter entity
settings in ENTS."
  ;;(Assume the current buffer is a scratch buffer and is empty)
  (sgml-debug "Trying to load compiled DTD from %s..." cfile)
  (sgml-set-buffer-multibyte nil)
  (or (and (file-readable-p cfile)
	   (let ((find-file-type	; Always binary
		  (function (lambda (fname) 1)))
		 (coding-system-for-read 'binary))
	     ;; fifth arg to insert-file-contents is not available in early
	     ;; v19.
	     (insert-file-contents cfile nil nil nil))
	   (equal '(sgml-saved-dtd-version 7) (sgml-read-sexp))
	   (or (sgml-up-to-date-p cfile (sgml-read-sexp))
	       (if (eq 'ask sgml-recompile-out-of-date-cdtd)
		   (not (y-or-n-p
			 "Compiled DTD is out of date, recompile? "))
		 (not sgml-recompile-out-of-date-cdtd))))
      (sgml-compile-dtd dtdfile cfile ents)))

(defun sgml-up-to-date-p (file dependencies)
  "Check if FILE is newer than all files in the list DEPENDENCIES.
If DEPENDENCIES contains the symbol t, FILE is not considered newer."
  (if (memq t dependencies)
      nil
    (loop for f in dependencies
	  always (file-newer-than-file-p file f))))

(defun sgml-compile-dtd (dtd-file to-file ents)
  "Construct a binary code compiled dtd from DTD-FILE and write it to TO-FILE.
The dtd will be constructed with the parameter entities set according
to ENTS.  The bdtd will be left in the current buffer.  The current
buffer is assumed to be empty to start with."
  (message "Recompiling DTD file %s..." dtd-file)
  (let* ((sgml-dtd-info (sgml-make-dtd nil))
	 (parameters (sgml-dtd-parameters sgml-dtd-info))
	 (sgml-parsing-dtd t))
    (push dtd-file
	  (sgml-dtd-dependencies sgml-dtd-info))
    (loop for (name . val) in ents
	  do (sgml-entity-declare name parameters 'text val))
    (sgml-push-to-entity dtd-file)
    (sgml-check-dtd-subset)
    (sgml-debug "sgml-compile-dtd: poping entity")
    (sgml-pop-entity)
    (erase-buffer)
    (sgml-write-dtd sgml-dtd-info to-file)
    t))

(defun sgml-check-entities (params1 params2)
  "Check that PARAMS1 is compatible with PARAMS2."
  (block check-entities
    (sgml-map-entities
     (function (lambda (entity)
		 (let ((other
			(sgml-lookup-entity (sgml-entity-name entity)
					    params2)))
		   (unless (or (null other)
			       (equal entity other))
		     (message
		      "Parameter %s in compiled DTD has wrong value;\
 is '%s' should be '%s'"
		      (sgml-entity-name entity)
		      (sgml-entity-text other)
		      (sgml-entity-text entity))
		     (return-from check-entities nil)))))
     params1)
    t))

(defun sgml-bdtd-merge ()
  "Merge the binary coded dtd in the current buffer with the current dtd.
The current dtd is the variable `sgml-dtd-info'.  Return t if the merge
was successful or nil if failed."
  (goto-char (point-min))
  (sgml-read-sexp)			; skip filev
  (let ((dependencies (sgml-read-sexp))
	(parameters (sgml-read-sexp))
	(gc-cons-threshold (max gc-cons-threshold 500000))
	temp)
    ;; Check compatibility of parameters
    (and (sgml-check-entities (sgml-dtd-parameters sgml-dtd-info)
			      parameters)
	 (progn
	   ;; Do the merger
	   (sgml-message "Reading compiled DTD...")
	   (sgml-merge-entity-tables (sgml-dtd-parameters sgml-dtd-info)
				     parameters)
	   (setf (sgml-dtd-dependencies sgml-dtd-info)
		 (nconc (sgml-dtd-dependencies sgml-dtd-info)
			dependencies))
	   ;; Doctype
	   (setq temp (sgml-read-sexp))
	   (when (and temp (null (sgml-dtd-doctype sgml-dtd-info)))
	     (setf (sgml-dtd-doctype sgml-dtd-info) temp))

	   ;; Element type names -- read and create token vector
	   (setq temp (sgml-read-number)) ; # eltypes
	   (setq sgml-read-token-vector (make-vector (1+ temp) nil))
	   (aset sgml-read-token-vector 0 sgml-pcdata-token)
	   (loop for i from 1 to temp do
		 (aset sgml-read-token-vector i
		       (sgml-lookup-eltype (sgml-read-sexp))))
	   ;; Element type descriptions
	   (loop for i from 1 to (sgml-read-number) do
		 (sgml-read-element (aref sgml-read-token-vector i)))
	   (sgml-merge-entity-tables (sgml-dtd-entities sgml-dtd-info)
				     (sgml-read-sexp))
	   (sgml-merge-shortmaps (sgml-dtd-shortmaps sgml-dtd-info)
				 (sgml-read-sexp))
	   (setf (sgml-dtd-notations sgml-dtd-info) (sgml-read-sexp))
	   t))))

(defun sgml-bdtd-read-dtd ()
  "Create and return a dtd from the binary coded dtd in the current buffer."
  (let ((sgml-dtd-info (sgml-make-dtd nil)))
    (sgml-bdtd-merge)
    sgml-dtd-info))

;;;; Set markup type

(defsubst sgml-set-markup-type (type)
  "Set the type of the markup parsed to TYPE.
The markup starts at position given by variable `sgml-markup-start' and
ends at point."
  (when (and sgml-set-face
	     (null sgml-current-eref))
    (sgml-set-face-for sgml-markup-start (point) type))
  (setq sgml-markup-type type))


;;;; Parsing delimiters

(eval-and-compile
  (defconst sgml-delimiters
    '("AND"   "&"
      "COM"   "--"
      "CRO"   "&#"
      "DSC"   "]"
      "DSO"   "["
      "DTGC"  "]"
      "DTGO"  "["
      "ERO"   "&"
      "ETAGO" "</"
      "GRPC"  ")"
      "GRPO"  "("
      "LIT"   "\""
      "LITA"  "'"
      "MDC"   ">"
      "MDO"   "<!"
      "MINUS" "-"
      "MSC"   "]]"
      "NESTC" "/"
      "NET"   "/"
      "OPT"   "?"
      "OR"    "|"
      "PERO"  "%"
      "PIC"   ">"
      "PIO"   "<?"
      "PLUS"  "+"
      "REFC"  ";"
      "REP"   "*"
      "RNI"   "#"
      "SEQ"   ","
      "STAGO" "<"
      "TAGC"  ">"
      "VI"    "="
      ;; Some combinations
      "MS-START" "<!["			; MDO DSO
      "MS-END"   "]]>"			; MSC MDC
      ;; XML stuff
      "XML-ECOM"   "-->"		; end an XML comment
      "XML-PIC"    "?>"			; end an XML processing instruction
      "XML-SCOM"   "<!--"		; start an XML comment
      "XML-TAGCE"  "/>"			; end an XML empty element
      ;; Pseudo
      "NULL"  ""
      )))

(eval-and-compile
(defun sgml-get-delim-string (drole)
  (car (cdr (member drole sgml-delimiters)))))

(defmacro sgml-delim (drole)
  (if (stringp drole)
      (sgml-get-delim-string (upcase drole))
    `(sgml-get-delim-string ,drole)))


(defmacro sgml-is-delim (delim &optional context move offset)
  "Macro for matching delimiters.
Syntax: DELIM &optional CONTEXT MOVE
where DELIM is the delimiter name (string or symbol),
CONTEXT the contextual constraint, and
MOVE is `nil', `move' or `check'.

Test if the text following point in current buffer matches the SGML
delimiter DELIM.  Also check the characters after the delimiter for
CONTEXT.  Applicable values for CONTEXT is
`gi' -- name start or TAGC if SHORTTAG YES,
`com' -- if COM or MDC,
`nmstart' -- name start character,
`stagc' -- TAGC if SHORTTAG YES,
`digit' -- any Digit,
string -- delimiter with that name,
list -- any of the contextual constraints in the list."

  (or offset (setq offset 0))
  (setq delim (upcase (format "%s" delim)))
  (let ((ds (sgml-get-delim-string delim)))
    (assert ds)
    (cond ((eq context 'gi)
	   (setq context '(nmstart stagc)))
	  ((eq context 'com)
	   (setq context '("COM" "MDC")))
	  ((null context)
	   (setq context '(t)))
	  ((not (listp context))
	   (setq context (list context))))
    (`(if (and				; This and checks that characters
					; of the delimiter
	   (,@(loop for i from 0 below (length ds) collect
		    (` (eq (, (aref ds i))
			   (sgml-following-char (, (+ i offset)))))))
	   (or
	    (,@(loop
		for c in context collect ; context check
		(cond
		 ((eq c 'nmstart)	; name start character
		  (`(sgml-startnm-char
		     (or (sgml-following-char (, (length ds))) 0))))
		 ((eq c 'stagc)
		  (`(and sgml-current-shorttag
			 (sgml-is-delim "TAGC" nil nil (, (length ds))))))
		 ((eq c 'digit)
		  (`(memq (sgml-following-char (, (length ds)))
			  '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))))
		 ((stringp c)
		  (`(sgml-is-delim (, c) nil nil (, (length ds)))))
		 ((eq c t))
		 (t (error "Context type: %s" c))))
	       )))
	  
	  (progn			; Do operations if delimiter found
	    (,@ (if move (`((forward-char (, (length ds)))))))
	    (,@ (if (not (eq move 'check))
		    '(t))))
	(,@ (if (eq move 'check)
		(`((sgml-delimiter-parse-error (, delim))))))))))

(defmacro sgml-following-char (n)
  (cond ((zerop n)  '(following-char))
	((= n 1)    '(char-after (1+ (point))))
	(t          (` (char-after (+ (, n) (point)))))))

(defun sgml-delimiter-parse-error (delim)
  (sgml-parse-error "Delimiter %s (%s) expected"
		    delim (sgml-get-delim-string delim)))

(defmacro sgml-parse-delim (delim &optional context)
  (`(sgml-is-delim (, delim) (, context) move)))

(defmacro sgml-check-delim (delim &optional context)
  (`(sgml-is-delim (, delim) (, context) check)))

(defmacro sgml-skip-upto (delim)
  "Skip until the delimiter or first char of one of the delimiters.
If DELIM is a string/symbol this is should be a delimiter role.
Characters are skipped until the delimiter is recognized.
If DELIM is a list of delimiters, skip until a character that is first
in any of them."
  (cond
   ((consp delim)
    (list 'skip-chars-forward
	  (concat "^"
		  (loop for d in delim
			concat (let ((ds (member (upcase (format "%s" d))
						 sgml-delimiters)))
				 (assert ds)
				 (let ((s (substring (cadr ds) 0 1)))
				   (if (member s '("-" "\\"))
				       (concat "\\" s)
				     s)))))))
   (t
    (let ((ds (sgml-get-delim-string (upcase (format "%s" delim)))))
      (if (= 1 (length ds))
	  (list 'skip-chars-forward (concat "^" ds))
	(`(and (search-forward (, ds) nil t)
	       (backward-char (, (length ds))))))))))


;;(macroexpand '(sgml-is-delim mdo))
;;(macroexpand '(sgml-parse-delim mdo))
;;(macroexpand '(sgml-check-delim mdo))


;;;; General lexical functions
;;; Naming conventions
;;; sgml-parse-xx  try to parse xx, return nil if can't else return
;;;		   some propriate non-nil value.
;;;                Except: for name/nametoken parsing, return 0 if can't.
;;; sgml-check-xx  require xx, report error if can't parse.  Return
;;;                aproporiate value.

(defmacro sgml-parse-char (char)
  (` (cond ((eq (, char) (following-char))
	    (forward-char 1)
	    t))))

(defmacro sgml-parse-chars (char1 char2 &optional char3)
  "Parse two or three chars; return nil if can't."
  (if (null char3)
      (` (cond ((and (eq (, char1) (following-char))
		 (eq (, char2) (char-after (1+ (point)))))
	    (forward-char 2)
	    t)))
    (` (cond ((and (eq (, char1) (following-char))
		 (eq (, char2) (char-after (1+ (point))))
		 (eq (, char3) (char-after (1+ (1+ (point))))))
	    (forward-char 3)
	    t)))))

(defun sgml-check-char (char)
  (cond ((not (sgml-parse-char char))
	 (sgml-parse-error "Expecting %c" char))))

(defun sgml-parse-RE ()
  (or (sgml-parse-char ?\n)
      (sgml-parse-char ?\r)))

(defmacro sgml-startnm-char (c)
  (` (eq ?w (char-syntax (, c)))))

(defsubst sgml-startnm-char-next ()
  (and (not (eobp))
       (sgml-startnm-char (following-char))))

(defun sgml-name-char (c)
  (and c
       (or (sgml-startnm-char c)
	   (eq ?_ (char-syntax c)))))

(defun sgml-is-end-tag ()
  (sgml-is-delim "ETAGO" gi))

(defsubst sgml-is-enabled-net ()
  (and (sgml-is-delim "NET")
       sgml-current-shorttag
       (sgml-tree-net-enabled sgml-current-tree)))

(defun sgml-is-start-tag ()
  (sgml-is-delim "STAGO" gi))

(defsubst sgml-parse-s (&optional shortmap)
  (if shortmap
      (or (/= 0 (skip-chars-forward " "))
	  (/= 0 (skip-chars-forward "\t"))
	  (sgml-parse-char ?\n)
	  (sgml-parse-char ?\r))
    (/= 0 (skip-chars-forward " \t\n\r"))))

(defsubst sgml-parse-processing-instruction (&optional in-declaration)
  (if (sgml-parse-delim "PIO")
      (sgml-do-processing-instruction in-declaration)))


(defmacro sgml-entity-case (string)   string)

(defun sgml-general-case (string)
  (if sgml-current-namecase-general
      (upcase string)
    string))

(defun sgml-parse-set-appflag (flagsym)
  (loop for name = (sgml-parse-name)
        while name
        for et = (sgml-lookup-eltype name)
        do (setf (sgml-eltype-appdata et flagsym) t)
        (message "Defining element %s as %s" name flagsym)
        (sgml-parse-s)))

(defun sgml-do-processing-instruction (in-declaration)
  (let ((start (point))
        (psgml-pi (and (eq ?P (following-char))
                       (looking-at "PSGML +\\(\\sw+\\) *"))))
    (if sgml-xml-p
	(sgml-skip-upto "XML-PIC")
      (sgml-skip-upto "PIC"))
    (let ((end (point)))
      (if sgml-xml-p
          (sgml-check-delim "XML-PIC")
        (sgml-check-delim "PIC"))
      (let ((next (point)))
        (cond (psgml-pi
               (goto-char start)
               (sgml--pi-psgml-handler in-declaration end))
              (sgml-pi-function
               (funcall sgml-pi-function
                        (buffer-substring-no-properties start end))))
        (goto-char next))))
  (unless in-declaration
    (sgml-set-markup-type 'pi))
  t)


(defun sgml--pi-psgml-handler (in-declaration end)
  (when (or in-declaration
            sgml-psgml-pi-enable-outside-dtd)
    (save-restriction
      (narrow-to-region (point) end)
      (let* ((command (downcase (match-string 1)))
             (flag-command (assoc command
                                  '(("nofill"      . nofill)
                                    ("breakafter"  . break-after-stag)
                                    ("breakbefore" . break-before-stag)
                                    ("structure"   . structure)))))
        (goto-char (match-end 0))
        (cond (flag-command
               (sgml-parse-set-appflag (cdr flag-command)))
              ((equal command "element")
               (sgml--pi-element-handler))
              (t
               (sgml-log-warning "Unknown processing instruction for PSGML: %s"
                                 command)))))))


(defun sgml--pi-element-handler ()
  (sgml-parse-s)
  (let ((eltype (sgml-lookup-eltype (sgml-parse-name)))
        name value)
    (sgml-parse-s)
    (while (setq name (sgml-parse-name))
      ;; FIXME: check name not reserved
      (sgml-parse-s)
      (cond ((sgml-parse-delim "VI")
             (sgml-parse-s)
             (setq value
                   (if (looking-at "['\"]")
                       (sgml-parse-literal)
                     (read (current-buffer)))))
            (t
             (setq value t)))
      (message "%s = %S" name value)
      (setf (sgml-eltype-appdata eltype (intern (downcase name))) value)
      (sgml-parse-s))))


;;[lenst/1998-03-09 19:52:08]  Perhaps not the right place
(defun sgml-general-insert-case (text)
  (if sgml-namecase-general
      (case sgml-general-insert-case
        (upper (upcase text))
        (lower (downcase text))
        (t text))
    text))

(defun sgml-entity-insert-case  (text)
  (if sgml-namecase-entity
      (case sgml-entity-insert-case
        (upper (upcase text))
        (lower (downcase text))
        (t text))
    text))


(defun sgml-parse-name (&optional entity-name)
  (if (sgml-startnm-char-next)
      (let ((name (buffer-substring-no-properties
		   (point)
		   (progn (skip-syntax-forward "w_")
			  (point)))))
	(if entity-name
	    (sgml-entity-case name)
	  (sgml-general-case name)))))

(define-compiler-macro sgml-parse-name (&whole form &optional entity-name)
  (cond
   ((memq entity-name '(nil t))
    (` (if (sgml-startnm-char-next)
	   ((, (if entity-name 'sgml-entity-case 'sgml-general-case))
	    (buffer-substring-no-properties (point)
					    (progn (skip-syntax-forward "w_")
						   (point)))))))
   (t
    form)))


(defsubst sgml-check-name (&optional entity-name)
  (or (sgml-parse-name entity-name)
      (sgml-parse-error "Name expected")))

(defun sgml-parse-nametoken (&optional entity-name)
  "Parses a name token and returns a string or nil if no nametoken."
  (if (sgml-name-char (following-char))
      (let ((name (buffer-substring-no-properties
		   (point)
		   (progn (skip-syntax-forward "w_")
			  (point)))))
	(if entity-name
	    (sgml-entity-case name)
	  (sgml-general-case name)))))

(defun sgml-check-nametoken ()
  (or (sgml-parse-nametoken)
      (sgml-parse-error "Name token expected")))

(defsubst sgml-parse-general-entity-ref ()
  (if (sgml-parse-delim "ERO" nmstart)
      (sgml-do-general-entity-ref)))

(defun sgml-do-general-entity-ref ()
  (sgml-do-entity-ref
   (prog1 (sgml-parse-name t)
     (or (sgml-parse-delim "REFC")
	 (sgml-parse-RE))
     (sgml-set-markup-type 'entity)))
  t)

(defun sgml-do-entity-ref (name)
  (let ((entity
	 (sgml-lookup-entity name
			     (sgml-dtd-entities sgml-dtd-info))))
    (cond ((null entity)
           (when sgml-warn-about-undefined-entities
             (sgml-log-warning "Undefined entity %s" name)))
	  ((sgml-entity-data-p entity)
	   (when sgml-xml-p
	     (sgml-error
	      "XML forbids data-entity references in data or DTD (%s)"
	      name))
	   (when sgml-signal-data-function
	     (funcall sgml-signal-data-function))
	   (cond
	    (sgml-entity-function
	     (funcall sgml-entity-function entity))
	    (sgml-data-function
	     (sgml-push-to-entity entity sgml-markup-start)
	     (funcall sgml-data-function (buffer-string))
	     (sgml-pop-entity))))
	  (t
	   (sgml-push-to-entity entity sgml-markup-start)))))

(defsubst sgml-parse-parameter-entity-ref ()
  "Parse and push to a parameter entity, return nil if no ref here."
  ;;(setq sgml-markup-start (point))
  (if (sgml-parse-delim "PERO" nmstart)
      (sgml-do-parameter-entity-ref)))

(defun sgml-do-parameter-entity-ref ()
  (let* ((name (sgml-parse-name t))
         (ent (sgml-lookup-entity name
                                  (sgml-dtd-parameters sgml-dtd-info))))
	(or (sgml-parse-delim "REFC")
	    (sgml-parse-char ?\n))
	;;(sgml-set-markup-type 'param)
	(cond (ent
	       (sgml-push-to-entity ent sgml-markup-start 'param))
	      (t
	       (sgml-parse-warning
		"Undefined parameter entity %s" name)))
	t))

(defun sgml-parse-comment ()
  (if (sgml-parse-delim "COM")
      (progn
        (if sgml-xml-p
            (sgml-parse-warning "XML forbids nested comments."))
        (sgml-skip-upto "COM")
        (sgml-check-delim "COM")
        t)))

(defun sgml-parse-xml-comment ()
  (if (sgml-parse-delim "XML-SCOM")
      (progn (sgml-skip-upto "XML-ECOM")
	     (sgml-check-delim "XML-ECOM")
             (sgml-set-markup-type 'comment)
	     t)))

(defun sgml-skip-cs ()
  "Skip over the separator used in the catalog.
Return true if not at the end of the buffer."
  (while (or (sgml-parse-s)
	     (sgml-parse-comment)))
  (not (eobp)))

(defsubst sgml-skip-ps ()
  "Move point forward stopping before a char that isn't a parameter separator."
  (while
      (or (sgml-parse-s)
	  (if (eobp) (sgml-pop-entity))
	  (sgml-parse-parameter-entity-ref)
	  (sgml-parse-comment))))

(defsubst sgml-parse-ds ()
;71  ds   = 5 s | EE | 60+ parameter entity reference
;         | 91 comment declaration
;         | 44 processing instruction
;         | 93 marked section declaration ***
  (or (and (eobp) (sgml-pop-entity))	;EE
      (sgml-parse-s)			;5 s
      ;;(sgml-parse-comment-declaration)	;91 comment declaration
      (sgml-parse-parameter-entity-ref)
      (sgml-parse-processing-instruction 'in-declaration)))

(defun sgml-skip-ds ()
  (while (sgml-parse-ds)))

(defmacro sgml-parse-rni (&optional name)
  "Parse a RNI (#) return nil if none.
With optional NAME, RNI must be followed by NAME."
  (cond
   (name
    (` (if (sgml-parse-delim "RNI")
	   (sgml-check-token (, name)))))
   (t '(sgml-parse-delim "RNI"))))

(defun sgml-check-token (name)
  (or (equal (sgml-check-case (sgml-check-name)) name)
      (sgml-parse-error "Reserved name not expected (expecting %s)"
                        name)))

(defun sgml-check-case (name)
  "Check that NAME is in upper case.
If `sgml-namecase-general' is nil, then signal an error if the argument
is not already in upper case."
  ;; FIXME: Perhaps only warn and upcase
  (or sgml-current-namecase-general
      (equal name (upcase name))
      (sgml-parse-error "Uppercase keyword expected (found %s)" name))
  name)

(defun sgml-parse-literal ()
  "Parse a literal and return a string, if no literal return nil."
  (let (lita start value)
    (cond ((or (sgml-parse-delim "LIT")
	       (setq lita (sgml-parse-delim "LITA")))
	   (setq start (point))
	   (if lita
	       (sgml-skip-upto "LITA")
	     (sgml-skip-upto "LIT"))
	   (setq value (buffer-substring-no-properties start (point)))
	   (if lita
	       (sgml-check-delim "LITA")
	     (sgml-check-delim "LIT"))
	   value))))

(defun sgml-check-literal ()
  (or (sgml-parse-literal)
      (sgml-parse-error "A literal expected")))

(defun sgml-parse-minimum-literal ()
  "Parse a quoted SGML string and return it, if no string return nil."
  (cond
   ((memq (following-char) '(?\" ?\'))
    (let* ((qchar (following-char))
	   (blanks " \t\r\n")
	   (qskip (format "^%s%c" blanks qchar))
	   (start (point))
	   (value			; accumulates the literal value
	    "")
	   (spaced ""))
      (forward-char 1)
      (skip-chars-forward blanks)
      (while (not (sgml-parse-char qchar))
	(cond ((eobp)
	       (goto-char start)
	       (sgml-parse-error "Unterminated literal"))
	      ((sgml-parse-s)
	       (setq spaced " "))
	      (t
	       (setq value
		     (concat value spaced
			     (buffer-substring-no-properties
			      (point)
			      (progn (skip-chars-forward qskip)
				     (point))))
		     spaced ""))))
      value))))

(defun sgml-check-minimum-literal ()
  (or (sgml-parse-minimum-literal)
      (sgml-parse-error "A minimum literal expected")))

(defun sgml-parse-external (&optional pubid-ok)
  "Leaves nil if no external id, or (pubid . sysid)."
  (sgml-skip-ps)
  (let* ((p (point))
	 (token (sgml-parse-nametoken)))
    (cond
     (token
      (sgml-skip-ps)
      (cond ((member (sgml-check-case token) '("PUBLIC" "SYSTEM"))
	     (let* ((pubid		; the public id
		     (if (string-equal token "PUBLIC")
			 (or (sgml-parse-minimum-literal)
			     (sgml-parse-error "Public identifier expected"))))
		    (sysid		; the system id
		     (progn (sgml-skip-ps)
			    (sgml-parse-literal))))
	       (sgml-make-extid pubid sysid pubid-ok)))
	    (t
	     (goto-char p)
	     nil))))))

(defun sgml-skip-tag ()
  (when (sgml-parse-char ?<)
    (sgml-parse-char ?/)
    (unless (search-forward-regexp
	       "\\([^\"'<>/]\\|\"[^\"]*\"\\|'[^']*'\\)*"
	       nil t)
      (sgml-error "Invalid tag"))
    (or (sgml-parse-char ?>)
	(sgml-parse-char ?/))))


;;;; Entity Manager

(defstruct (sgml-entity
	    (:type list)
	    (:constructor sgml-make-entity (name type text &optional notation)))
  name					; Name of entity (string)
  type					; Type of entity CDATA NDATA PI SDATA
  text					; string or external
  notation                              ; Notation of external entity or nil
  ;; Last cdr is undefined flag
  )

(defun sgml-entity-data-p (entity)
  "True if ENTITY is a data entity, that is not a text entity."
  (not (eq (sgml-entity-type entity) 'text)))

(defun sgml-entity-marked-undefined-p (entity)
  (nthcdr 4 entity))

(defsetf sgml-entity-marked-undefined-p (entity) (val)
  ;; `(setf (nthcdr 4 ,entity) ,val)
  `(progn (setcdr (nthcdr 3 ,entity) ,val)
	  ,entity))



;;; Entity tables
;; Represented by a cons-cell whose car is the default entity (or nil)
;; and whose cdr is as an association list.

(defun sgml-make-entity-table ()
  (list nil))

(defun sgml-lookup-entity (name entity-table)
  (or (assoc name (cdr entity-table))
      (car entity-table)))

(defun sgml-entity-declare (name entity-table type text &optional notation)
  "Declare an entity with name NAME in table ENTITY-TABLE.
TYPE should be the type of the entity (text|CDATA|NDATA|SDATA...).
TEXT is the text of the entity, a string or an external identifier.
NOTATION is the notation of an external entity, if present.
If NAME is nil, this defines the default entity."
  (cond
   (name
    (unless (sgml-lookup-entity name entity-table)
      (sgml-debug "Declare entity %s %s as %S" name type text)
      (nconc entity-table
	     (list (sgml-make-entity name type text notation)))))
   (t
    (unless (car entity-table)
      (sgml-debug "Declare default entity %s as %S" type text)
      (setcar entity-table (sgml-make-entity name type text))))))

(defun sgml-entity-completion-table (entity-table)
  "Make a completion table from the ENTITY-TABLE."
  (cdr entity-table))

(defun sgml-map-entities (fn entity-table &optional collect)
  (if collect
      (mapcar fn (cdr entity-table))
    (loop for e in (cdr entity-table) do (funcall fn e))))

(defun sgml-merge-entity-tables (tab1 tab2)
  "Merge entity table TAB2 into TAB1.  TAB1 is modified."
  (nconc tab1 (cdr tab2))
  (setcar tab1 (or (car tab1) (car tab2))))


(defun sgml-entity-insert-text (entity &optional ptype)
  "Insert the text of ENTITY.
PTYPE can be 'param if this is a parameter entity."
  (let ((text (sgml-entity-text entity)))
    (cond
     ((stringp text)
      (insert text))
     (t
      (sgml-insert-external-entity text
				   (or ptype
				       (sgml-entity-type entity))
				   (sgml-entity-name entity))))))

;;;; External identifier resolve

(defun sgml-cache-catalog (file cache-var parser-fun
				&optional default-dir)
  "Return parsed catalog.
FILE is the file containing the catalog.  Maintains a cache of parsed
catalog files in variable CACHE-VAR. The parsing is done by function
PARSER-FUN that should parse the current buffer and return the parsed
representation of the catalog."
  (setq file (expand-file-name file default-dir))
  (and
   (file-readable-p file)
   (let ((c (assoc file (symbol-value cache-var)))
	 (modtime (elt (file-attributes (file-truename file)) 5)))
     (if (and c (equal (second c) modtime))
	 (cddr c)
       (when c (set cache-var (delq c (symbol-value cache-var))))
       (let (new)
	 (message "Loading %s ..." file)
         (save-excursion
           (sgml-push-to-entity file)
           (setq default-directory (file-name-directory file))
           (setq new (funcall parser-fun)))
	 (push (cons file (cons modtime new)) (symbol-value cache-var))
	 (message "Loading %s ... done" file)
	 new)))))

(defun sgml-main-directory ()
  "Directory of the document entity."
  (let ((cb (current-buffer)))
    (set-buffer sgml-current-top-buffer)
    (prog1 default-directory
      (set-buffer cb))))

(defun sgml-trace-lookup (&rest args)
  "Log a message like `sgml-log-message', but only if `sgml-trace-entity-lookup' is set."
  (when sgml-trace-entity-lookup
    (apply (function sgml-log-message) args)))


(defun sgml-catalog-lookup (files pubid type name)
  "Look up the public identifier/entity name in catalogs.
The result is a file name or nil.  FILES is a list of catalogs to use.
PUBID is the public identifier \(if any). TYPE is the entity type and
NAME is the entity name."
  (cond ((eq type 'param)
	 (setq name (format "%%%s" name)
	       type 'entity))
	((eq type 'dtd)
	 (setq type 'doctype)))
  ;;(sgml-trace-lookup "  [pubid='%s' type=%s name='%s']" pubid type name)
  (let ((remaining files)
	(file nil))
    (while (and remaining (null file))
      (let ((additional nil)		; Extra catalogs to search
	    (cat (sgml-cache-catalog (car remaining) 'sgml-catalog-assoc
				     (function sgml-parse-catalog-buffer)
				     (sgml-main-directory))))
	(sgml-trace-lookup "  catalog: %s %s"
			   (expand-file-name (car remaining)
					     (sgml-main-directory))
			   (if (null cat) "empty/non existent" "exists"))
	(when pubid
	  ;; Giv PUBLIC entries priority over ENTITY and DOCTYPE
	  (loop for (key cname cfile) in cat
		while (not file) do
		(when (and (eq 'public key)
			   (string= pubid cname))
		  (when (file-readable-p cfile) (setq file cfile))
		  (sgml-trace-lookup "  >> %s [by pubid]%s"
				     cfile (if file "" " !unreadable")))))
	(loop for (key cname cfile) in cat
	      while (not file) do
	      (when (eq 'catalog key)
		(push cfile additional))
	      (when (and (eq type key)
			 (or (null cname)
			     (string= name cname)))
		(when (file-readable-p cfile) (setq file cfile))
		(sgml-trace-lookup "  >> %s [by %s %s]%s"
				   cfile key cname
				   (if file "" " !unreadable"))))
	(setq remaining
	      (nconc (nreverse additional) (cdr remaining)))))
    file))


(defun sgml-path-lookup (extid type name)
  (let* ((pubid (sgml-extid-pubid extid))
	 (sysid (sgml-extid-sysid extid))
	 (subst (list '(?% ?%))))
    (when pubid
      (nconc subst (list (cons ?p (sgml-transliterate-file pubid)))
	     (sgml-pubid-parts pubid))
      (setq pubid (sgml-canonize-pubid pubid)))
    (when sysid (nconc subst (list (cons ?s sysid))))
    (when name  (nconc subst (list (cons ?n name))))
    (when type  (nconc subst (list (cons ?y (cond ((eq type 'dtd) "dtd")
						  ((eq type 'text) "text")
						  ((eq type 'param) "parm")
						  (t "sgml"))))))
    (sgml-debug "Ext. file subst. = %S" subst)
    (loop for cand in sgml-public-map
	  thereis
	  (and (setq cand (sgml-subst-expand cand subst))
	       (file-readable-p
		(setq cand
		      (sgml-extid-expand (substitute-in-file-name cand)
					 extid)))
	       (not (file-directory-p cand))
	       cand))))

(defun sgml-external-file (extid &optional type name)
  "Return file name for entity with external identifier EXTID.
Optional argument TYPE should be the type of entity and NAME should be
the entity name."
  ;; extid is (pubid . sysid)
  (let ((pubid (sgml-extid-pubid extid)))
    (when pubid (setq pubid (sgml-canonize-pubid pubid)))
    (sgml-trace-lookup "Start looking for %s entity %s public %s system %s"
		       (or type "-")
		       (or name "?")
		       pubid
		       (sgml-extid-sysid extid))
    (or (if (and sgml-system-identifiers-are-preferred
		 (sgml-extid-sysid extid))
	    (or (sgml-lookup-sysid-as-file extid)
		(sgml-path-lookup  ;Try the path also, but only using sysid
		 (sgml-make-extid nil (sgml-extid-sysid extid))
		 nil nil)))
	(sgml-catalog-lookup sgml-current-localcat pubid type name)
	(sgml-catalog-lookup sgml-catalog-files pubid type name)
	(if (not sgml-system-identifiers-are-preferred)
	    (sgml-lookup-sysid-as-file extid))
	(sgml-path-lookup extid type name))))

(defun sgml-lookup-sysid-as-file (extid)
  (let ((sysid (sgml-extid-sysid extid)))
    (and sysid
	 (loop for pat in sgml-public-map
	       never (string-match "%[Ss]" pat))
	 (file-readable-p (setq sysid (sgml-extid-expand sysid extid)))
	 sysid)))

(defun sgml-insert-external-entity (extid &optional type name)
  "Insert the contents of an external entity at point.
EXTID is the external identifier of the entity.  Optional arguments TYPE
is the entity type and NAME is the entity name, used to find the entity.
Returns nil if entity is not found."
  (let* ((pubid (sgml-extid-pubid extid))
	 (sysid (sgml-extid-sysid extid)))
    (or (if sysid
	    (loop for fn in sgml-sysid-resolve-functions
		  thereis (funcall fn sysid)))
	(let ((file (sgml-external-file extid type name)))
	  (and file (insert-file-contents file)))
	(progn
          (sgml-warn-external-entity-not-found name pubid sysid)
	  nil))))

(defun sgml-warn-external-entity-not-found (name pubid sysid)
  (sgml-log-warning "External entity not found: %s%s%s"
                    name                    
                    (if pubid
                        (format " PUBLIC \"%s\"" pubid)
                      "")
                    (if sysid
                        (format " SYSTEM \"%s\"" sysid)
                      "")))


;; Parse a buffer full of catalogue entries.
(defun sgml-parse-catalog-buffer ()
  "Parse all entries in a catalogue."
  (let ((sgml-xml-p nil))
    (sgml-trace-lookup "  (Parsing catalog)")
    (loop
     while (sgml-skip-cs)
     for type = (downcase (sgml-check-cat-literal))
     for class = (cdr (assoc type '(("public" . public) ("dtddecl" . public)
                                    ("entity" . name)   ("linktype" . name)
                                    ("doctype" . name)  ("sgmldecl" . noname)
                                    ("document" . noname)
                                    ("catalog"  . noname))))
     when (not (null class))
     collect
     (let* ((name
             (cond ((eq class 'public)
                    (sgml-skip-cs)
                    (sgml-canonize-pubid (sgml-check-minimum-literal)))
                   ((string= type "doctype")
                    (sgml-general-case (sgml-check-cat-literal)))
                   ((eq class 'name)
                    (sgml-entity-case (sgml-check-cat-literal)))))
            (file
             (expand-file-name (sgml-check-cat-literal))))
       (list (intern type) name file)))))


(defun sgml-check-cat-literal ()
  "Read the next catalog token.
Skips any leading spaces/comments."
  (sgml-skip-cs)
  (or (sgml-parse-literal)
      (buffer-substring-no-properties
       (point)
       (progn (skip-chars-forward "^ \r\n\t")
	      (point)))))

(defconst sgml-formal-pubid-regexp
  (concat
   "^\\(+//\\|-//\\|\\)"		; Registered indicator  [1]
   "\\(\\([^/]\\|/[^/]\\)+\\)"		; Owner                 [2]
   "//"
   "\\([^ ]+\\)"			; Text class            [4]
   " "
   "\\(\\([^/]\\|/[^/]\\)*\\)"		; Text description      [5]
   "//"
   "\\(\\([^/]\\|/[^/]\\)*\\)"		; Language              [7]
   "\\(//"				;   		        [9]
   "\\(\\([^/]\\|/[^/]\\)*\\)"		; Version	        [10]
   "\\)?"))

(defun sgml-pubid-parts (pubid)
  (nconc
   (if (string-match sgml-formal-pubid-regexp pubid)
       (nconc
	(list
	 (cons ?o (sgml-transliterate-file (sgml-matched-string pubid 2)))
	 (cons ?c (downcase (sgml-matched-string pubid 4)))
	 (cons ?d (sgml-transliterate-file (sgml-matched-string pubid 5)))
	 ;; t alias for d  (%T used by sgmls)
	 (cons ?t (sgml-transliterate-file (sgml-matched-string pubid 5)))
	 (cons ?l (downcase (sgml-matched-string pubid 7))))
	(if (match-beginning 9)
	    (list (cons ?v (sgml-transliterate-file
			    (sgml-matched-string pubid 10)))))))))

(defun sgml-canonize-pubid (pubid)
  (if (string-match sgml-formal-pubid-regexp pubid)
      (concat
       (sgml-matched-string pubid 1)	; registered indicator
       (sgml-matched-string pubid 2)	; Owner
       "//"
       (upcase (sgml-matched-string pubid 4)) ; class
       " "
       (sgml-matched-string pubid 5)	; Text description
       "//"
       (upcase (sgml-matched-string pubid 7)) ; Language
       "//"
       (if (match-beginning 9)
	   (sgml-matched-string pubid 10) ""))))

(defun sgml-transliterate-file (string)
  (mapconcat (function (lambda (c)
			 (char-to-string
			  (or (cdr-safe (assq c sgml-public-transliterations))
			      c))))
	     string ""))

(defun sgml-subst-expand-char (c parts)
  (cdr-safe (assq (downcase c) parts)))

(defun sgml-subst-expand (s parts)
  (loop for i from 0 to (1- (length s))
	as c = (aref s i)
	concat (if (eq c ?%)
		   (or (sgml-subst-expand-char (aref s (incf i)) parts)
		       (return nil))
		 (char-to-string (aref s i)))))

(defun sgml-matched-string (string n &optional regexp noerror)
  (let ((res (if regexp
		 (or (string-match regexp string)
		     noerror
		     (error "String match fail")))))
    (if (or (null regexp)
	    (numberp res))
	(substring string (match-beginning n)
		   (match-end n)))))

;;;; Files for SGML declaration and DOCTYPE declaration

(defun sgml-declaration ()
  (or sgml-declaration
      (if sgml-doctype
	  (sgml-in-file-eval sgml-doctype
			     '(sgml-declaration)))
      (if sgml-parent-document
	  (sgml-in-file-eval (car sgml-parent-document)
			     '(sgml-declaration)))
      ;; *** check for sgmldecl comment
      (sgml-external-file nil 'sgmldecl)))

(defun sgml-in-file-eval (file expr)
  (let ((cb (current-buffer)))
    (set-buffer (find-file-noselect file))
    (prog1 (eval expr)
      (set-buffer cb))))


;;;; Entity references and positions

(defstruct (sgml-eref
	    (:constructor sgml-make-eref (entity start end))
	    (:type list))
  entity
  start					; type: epos
  end)

(defun sgml-make-epos (eref pos)
  (cons eref pos))

(defun sgml-epos-eref (epos)
  (if (consp epos)
      (car epos)))

(defun sgml-epos-pos (epos)
  "The buffer position of EPOS within its entity."
  (if (consp epos)
      (cdr epos)
    epos))

(defun sgml-bpos-p (epos)
  "True if EPOS is a position in the main buffer."
  (numberp epos))

(defun sgml-strict-epos-p (epos)
  "True if EPOS is a position in an entity other then the main buffer."
  (consp epos))

(defun sgml-epos (pos)
  "Convert a buffer position POS into an epos."
  (if sgml-current-eref
      (sgml-make-epos sgml-current-eref pos)
    pos))

(defun sgml-epos-before (epos)
  "The last position in buffer not after EPOS.
If EPOS is a buffer position this is the same.  If EPOS is in an entity
this is the buffer position before the entity reference."
  (while (consp epos)
    (setq epos (sgml-eref-start (sgml-epos-eref epos))))
  epos)

(defun sgml-epos-after (epos)
  "The first position in buffer after EPOS.
If EPOS is in an other entity, buffer position is after
entity reference leading to EPOS."
  (while (consp epos)
    (setq epos (sgml-eref-end (sgml-epos-eref epos))))
  epos)

(defun sgml-epos-promote (epos)
  "Convert position in entity structure EPOS to a buffer position.
If EPOS is in an entity, the buffer position will be the position
before the entity reference if EPOS is first character in entity
text.  Otherwise buffer position will be after entity reference."
  (while (and (consp epos)
	      (= (cdr epos) 1))
    (setq epos (sgml-eref-start (car epos))))
  (sgml-epos-after epos))


;;;; DTD repository
;;compiled-dtd: extid -> Compiled-DTD?
;;extid-cdtd-name: extid -> file?
;;up-to-date-p: (file, dependencies) -> cond

;; Emacs Catalogues:
;; Syntax:
;;  ecat ::= (cs | ecat-entry)*
;;  cs ::= (s | comment)
;;  ecat-entry ::= (pub-entry | file-entry)
;;  pub-entry ::= ("PUBLIC", minimal literal, ent-spec?, cat literal)
;;  pub-entry ::= ("FILE", literal, ent-spec?, cat literal)
;;  ent-spec ::= ("[", (name, literal)*, "]")

;; Parsed ecat = (eent*)
;; eent = (type ...)
;;      = ('public pubid cfile . ents)
;;      = ('file file cfile . ents)

(defun sgml-load-ecat (file)
  "Return ecat for FILE."
  (sgml-cache-catalog
   file 'sgml-ecat-assoc
   (function
    (lambda ()
      (let ((sgml-xml-p nil)
            new type ents from to name val)
	(while (progn (sgml-skip-cs)
		      (setq type (sgml-parse-name)))
	  (setq type (intern (downcase type)))
	  (setq ents nil from nil)
	  (sgml-skip-cs)
	  (cond
	   ((eq type 'public)
	    (setq from (sgml-canonize-pubid (sgml-check-minimum-literal))))
	   ((eq type 'file)
	    (setq from (expand-file-name (sgml-check-cat-literal)))))
	  (cond
	   ((null from)
	    (error "Syntax error in ECAT: %s" file))
	   (t
	    (sgml-skip-cs)
	    (when (sgml-parse-char ?\[)
	      (while (progn (sgml-skip-cs)
			    (setq name (sgml-parse-name t)))
		(sgml-skip-cs)
		(setq val (sgml-check-literal))
		(push (cons name val) ents))
	      (sgml-check-char ?\])
	      (sgml-skip-cs))
	    (setq to (expand-file-name (sgml-check-cat-literal)))
	    (push (cons type (cons from (cons to ents)))
		  new))))
	(nreverse new))))))

(defun sgml-ecat-lookup (files pubid file)
  "Return (file . ents) or nil."
  (let ((params (sgml-dtd-parameters sgml-dtd-info)))
    (loop
     for f in files
     do (sgml-debug "Search ECAT %s" f)
     thereis
     (loop
      for (type name cfile . ents) in (sgml-load-ecat f)
      thereis
      (if (and (cond ((eq type 'public) (equal name pubid))
		     ((eq type 'file)   (equal name file)))
	       (loop for (name . val) in ents
		     for entity = (sgml-lookup-entity name params)
		     always (and entity
				 (equal val (sgml-entity-text entity)))))
	  (cons cfile ents))))))

;;(let ((sgml-dtd-info (sgml-make-dtd nil)))
;;  (sgml-ecat-lookup sgml-ecat-files
;;		    "-//lenst//DTD My DTD//EN//"
;;		    "/home/u5/lenst/src/psgml/bar.dtd"))


;;;; Merge compiled dtd

(defun sgml-try-merge-compiled-dtd (pubid file)
  (when pubid (setq pubid (sgml-canonize-pubid pubid)))
  (when file (setq file (expand-file-name file)))
  (sgml-debug "Find compiled dtd for %s %s" pubid file)
  (let ((ce (or (sgml-ecat-lookup sgml-current-local-ecat pubid file)
		(sgml-ecat-lookup sgml-ecat-files pubid file))))
    (and ce
	 (let ((cfile (car ce))
	       (ents  (cdr ce)))
	   (sgml-debug "Found %s" cfile)
	   (if (sgml-use-special-case)
	       (sgml-try-merge-special-case pubid file cfile ents)
	     (and (sgml-bdtd-load cfile file ents)
		  (sgml-bdtd-merge)))))))

(defun sgml-use-special-case ()
  (and (null (sgml-dtd-merged sgml-dtd-info))
       (sgml-eltype-table-empty (sgml-dtd-eltypes sgml-dtd-info))
       (eq 'dtd (sgml-entity-type (sgml-eref-entity sgml-current-eref)))))

(defun sgml-try-merge-special-case (pubid file cfile ents)
  (let (cdtd)
    (sgml-debug "Merging special case")
    ;; Look for a compiled dtd in some other buffer
    (let ((cb (current-buffer)))
      (loop for b in (buffer-list)
	    until
	    (progn (set-buffer b)
		   (and sgml-buffer-parse-state
			(let ((m (sgml-dtd-merged
				  (sgml-pstate-dtd sgml-buffer-parse-state))))
			  (and m
			       (string-equal cfile (car m))
			       (setq cdtd (cdr m)))))))
      (set-buffer cb))
    ;; Load a new compiled dtd
    (unless cdtd
      (and (sgml-bdtd-load cfile file ents)
	   (setq cdtd (sgml-bdtd-read-dtd))))
    ;; Do the merger
    (cond
     ((and cdtd
	   (sgml-check-entities (sgml-dtd-parameters sgml-dtd-info)
				(sgml-dtd-parameters cdtd)))
      (setf (sgml-dtd-eltypes sgml-dtd-info)
	    (sgml-dtd-eltypes cdtd))
      (sgml-merge-entity-tables (sgml-dtd-entities sgml-dtd-info)
				(sgml-dtd-entities cdtd))
      (sgml-merge-entity-tables (sgml-dtd-parameters sgml-dtd-info)
				(sgml-dtd-parameters cdtd))
      (sgml-merge-shortmaps (sgml-dtd-shortmaps sgml-dtd-info)
			    (sgml-dtd-shortmaps cdtd))
      (setf (sgml-dtd-dependencies sgml-dtd-info)
	    (nconc (sgml-dtd-dependencies sgml-dtd-info)
		   (sgml-dtd-dependencies cdtd)))
      (setf (sgml-dtd-merged sgml-dtd-info) (cons cfile cdtd))))))


;;;; Pushing and popping entities

(defun sgml-push-to-entity (entity &optional ref-start type)
  "Set current buffer to a buffer containing the entity ENTITY.
ENTITY can also be a file name.  Optional argument REF-START should be
the start point of the entity reference.  Optional argument TYPE,
overrides the entity type in entity look up."
  (when ref-start
    ;; don't consider a RS shortref here again
    (setq sgml-rs-ignore-pos ref-start))
  (unless (and sgml-scratch-buffer
	       (buffer-name sgml-scratch-buffer)
	       ;; An existing buffer may have been left unibyte by
	       ;; processing a cdtd.
               ;; FIXME: looks strange, we haven't changed bufferw yet
	       (sgml-set-buffer-multibyte t))
    (setq sgml-scratch-buffer (generate-new-buffer " *entity*")))
  (let ((cb (current-buffer))
	(dd default-directory)
        (syntax-table (syntax-table))
        (xml-p sgml-xml-p)
	;;*** should eref be argument ?
	(eref (sgml-make-eref (if (stringp entity)
				  (sgml-make-entity entity nil nil)
				entity)
			      (sgml-epos (or ref-start (point)))
			      (sgml-epos (point)))))
    (set-buffer sgml-scratch-buffer)
    (when (eq sgml-scratch-buffer (default-value 'sgml-scratch-buffer))
      (make-local-variable 'sgml-scratch-buffer)
      (setq sgml-scratch-buffer nil))
    (setq sgml-last-entity-buffer (current-buffer))
    (erase-buffer)
    (sgml-set-buffer-multibyte 'default)
    (setq default-directory dd)
    (set-visited-file-name nil t)
    (set (make-local-variable 'sgml-current-file) nil)
    (make-local-variable 'sgml-current-eref)
    (setq sgml-current-eref eref)
    (set-syntax-table syntax-table)
    (make-local-variable 'sgml-previous-buffer)
    (setq sgml-previous-buffer cb)
    (setq sgml-xml-p xml-p)
    (setq sgml-rs-ignore-pos		; don't interpret beginning of buffer
					; as #RS if internal entity.
	  (if (or (stringp entity)
		  (stringp (sgml-entity-text entity)))
	      (point)
	    0))
    (when sgml-buffer-parse-state
      (sgml-debug "-- pstate set in scratch buffer")
      (setq sgml-buffer-parse-state nil))
    (cond
     ((stringp entity)			; a file name
      ;;(save-excursion ) test remove [lenst/1998-06-19 12:49:47]
      (sgml-debug "Push to %s: FILE %s"
                  (current-buffer) entity)

      (insert-file-contents entity)
      (setq sgml-current-file entity)
      ;; (goto-char (point-min)) ??
      (setq default-directory (file-name-directory entity)))
     ((consp (sgml-entity-text entity)) ; external id?
      (let* ((extid (sgml-entity-text entity))
	     (file
	      (sgml-external-file extid
				  (or type (sgml-entity-type entity))
				  (sgml-entity-name entity))))
	(when sgml-parsing-dtd
	  (push (or file t)
		(sgml-dtd-dependencies sgml-dtd-info)))
	(sgml-debug "Push to %s: %s = %s" (current-buffer) extid file)
	(cond
	 ((and file sgml-parsing-dtd
	       (sgml-try-merge-compiled-dtd (sgml-extid-pubid extid)
					    file))
	  (goto-char (point-max)))
	 (file
	  ;; fifth arg not available in early v19
	  (erase-buffer)
	  (insert-file-contents file nil nil nil)
          (setq sgml-current-file file)
	  (setq default-directory (file-name-directory file))
	  (goto-char (point-min)))
	 (t ;; No file for entity
	  (save-excursion
	    (let* ((pubid (sgml-extid-pubid extid))
		   (sysid (sgml-extid-sysid extid)))
	      (or (if sysid		; try the sysid hooks
		      (loop for fn in sgml-sysid-resolve-functions
			    thereis (funcall fn sysid)))
		  (progn
		    ;; Mark entity as not found
                    (setf (sgml-entity-marked-undefined-p entity) t)
		    (if sgml-warn-about-undefined-entities
			(sgml-warn-external-entity-not-found
                         (sgml-entity-name entity) pubid sysid))
		    nil))))))))
     (t ;; internal entity
      (sgml-debug "Push to %s: string '%s'"
                  (current-buffer) (sgml-entity-text entity))
      (save-excursion
	(insert (sgml-entity-text entity)))))))



(defun sgml-pop-entity ()
  (cond ((and (boundp 'sgml-previous-buffer)
	      (bufferp sgml-previous-buffer))
	 (sgml-debug "Exit entity %s => %s"
                     (current-buffer) sgml-previous-buffer)
	 (setq sgml-last-entity-buffer sgml-previous-buffer)
	 (set-buffer sgml-previous-buffer)
	 t)))

(defun sgml-mainbuf-point ()
  "Value of point in main buffer."
  (if sgml-current-eref
      (sgml-eref-end sgml-current-eref)
    (point)))

(defun sgml-goto-epos (epos)
  "Goto a position in an entity given by EPOS."
  (assert epos)
  (cond ((sgml-bpos-p epos)
	 (goto-char epos))
	(t
	 (let ((eref (sgml-epos-eref epos)))
	   (sgml-cleanup-entities)
	   (sgml-goto-epos (sgml-eref-end eref))
	   (sgml-push-to-entity (sgml-eref-entity eref)
				(sgml-epos-pos (sgml-eref-start eref))))
	 (goto-char (sgml-epos-pos epos)))))

(defun sgml-pop-all-entities ()
  (while (sgml-pop-entity)))

(defun sgml-cleanup-entities ()
  (let ((cb (current-buffer))
	(n 0))
    (while (and sgml-scratch-buffer (buffer-name sgml-scratch-buffer))
      (set-buffer sgml-scratch-buffer)
      (assert (not (eq sgml-scratch-buffer
		       (default-value 'sgml-scratch-buffer))))
      (incf n))
    (while (> n 10)
      (set-buffer (prog1 sgml-previous-buffer
		    (kill-buffer (current-buffer))))
      (decf n))
    (set-buffer cb)))

(defun sgml-any-open-param/file ()
  "Return true if there currently is a parameter or file open."
  (and (boundp 'sgml-previous-buffer)
       sgml-previous-buffer))


;;;; Parse tree

(defstruct (sgml-tree
	    (:type vector)
	    (:constructor sgml-make-tree
			  (eltype stag-epos stag-len  parent level
				  excludes includes pstate net-enabled
				  conref &optional shortmap pshortmap asl)))
  eltype				; element object
  ;;start				; start point in buffer
  ;;end					; end point in buffer
  stag-epos				; start-tag entity position
  etag-epos				; end-tag entity position
  stag-len				; length of start-tag
  etag-len				; length of end-tag
  parent				; parent tree
  level					; depth of this node
  excludes				; current excluded elements
  includes				; current included elements
  pstate				; state in parent
  next					; next sibling tree
  content				; child trees
  net-enabled				; if NET enabled (t this element,
					;  other non-nil, some parent)
  conref				; if conref attribute used
  shortmap				; shortmap at start of element
  pshortmap				; parents shortmap
  asl					; attribute specification list
)


(defun sgml-tree-end (tree)
  "Buffer position after end of TREE."
  (let ((epos (sgml-tree-etag-epos tree))
	(len (sgml-tree-etag-len tree)))
    (cond ((sgml-bpos-p epos)
	   (+ epos len))
	  ((zerop len)
	   (sgml-epos-promote epos))
	  (t
	   (sgml-epos-after epos)))))


(defun sgml-max-pos-in-tree (tree)
  (let ((epos (sgml-tree-etag-epos tree)))
    (while (not epos)
      (let ((children (sgml-tree-content tree)))
        (if children
            (while children
              (setq tree children
                    children (sgml-tree-next children)))
          (setq epos (sgml-tree-stag-epos tree)))))
    (sgml-epos-after epos)))


;;;; (text) Element view of parse tree

(defmacro sgml-alias-fields (orig dest &rest fields)
  (let ((macs nil))
    (while fields
      (push
       (` (defmacro (, (intern (format "%s-%s" dest (car fields)))) (element)
	    (, (format "Return %s field of ELEMENT." (car fields)))
	    (list
	     '(, (intern (format "%s-%s" orig (car fields))))
	     element)))
       macs)
      (setq fields (cdr fields)))
    (cons 'progn macs)))

(sgml-alias-fields sgml-tree sgml-element
  eltype				; element object
  ;;  start					; start point in buffer
  stag-epos
  etag-epos
  stag-len				; length of start-tag
  etag-len				; length of end-tag
  parent				; parent tree
  level					; depth of this node
  excludes				; current excluded elements
  includes				; current included elements
  pstate				; state in parent
  net-enabled				; if NET enabled
  )

(defun sgml-element-model (element)
  "Declared content or content model of ELEMENT."
  (sgml-eltype-model (sgml-tree-eltype element)))

(defun sgml-element-name (element)
  "Return name (symbol) of ELEMENT."
  (sgml-tree-eltype element))

(defun sgml-element-gi (element)
  "Return general identifier (string) of ELEMENT."
  (sgml-eltype-name (sgml-tree-eltype element)))

(defun sgml-element-appdata (element prop)
  "Return the application data named PROP associated with the type of ELEMENT."
  (sgml-eltype-appdata (sgml-tree-eltype element) prop))

(defmacro sgml-element-stag-optional (element)
  "True if start-tag of ELEMENT is omissible."
  (`(sgml-eltype-stag-optional (sgml-tree-eltype (, element)))))

(defsubst sgml-element-etag-optional (element)
  "True if end-tag of ELEMENT is omissible."
  (sgml-eltype-etag-optional (sgml-tree-eltype element)))

(defun sgml-element-attlist (element)
  "Return the attribute specification list of ELEMENT."
  (sgml-eltype-attlist (sgml-tree-eltype element)))

(defsubst sgml-element-mixed (element)
  "True if ELEMENT has mixed content."
  (sgml-eltype-mixed (sgml-tree-eltype element)))

(defun sgml-element-start (element)
  "Position before start of ELEMENT."
  (sgml-epos-promote (sgml-tree-stag-epos element)))

(defun sgml-element-stag-end (element)
  "Position after start-tag of ELEMENT."
  (let ((epos (sgml-tree-stag-epos element))
	(len (sgml-tree-stag-len element)))
    (cond ((sgml-bpos-p epos)
	   (+ epos len))
	  ((zerop len)
	   (sgml-epos-promote epos))
	  (t
	   (sgml-epos-after epos)))))

(defun sgml-element-empty (element)
  "True if ELEMENT is empty."
  (or (sgml-tree-conref element)
      (and (not sgml-xml-p)
           (eq sgml-empty (sgml-element-model element)))))

(defun sgml-check-empty (name)
  "True if element with NAME is empty."
  (let ((eltype (if (symbolp name) name (sgml-lookup-eltype name))))
    (eq sgml-empty (sgml-eltype-model eltype))))

(defun sgml-element-data-p (element)
  "True if ELEMENT can have data characters in its content."
  (or (sgml-element-mixed element)
      (eq sgml-cdata (sgml-element-model element))
      (eq sgml-rcdata (sgml-element-model element))))

(defun sgml-element-context-string (element)
  "Return string describing context of ELEMENT."
  (if (eq element sgml-top-tree)
      ""
    (format "in %s %s"
            (sgml-element-gi element)
            (sgml-element-context-string (sgml-tree-parent element)))))


;;;; Display and Mode-line

(defun sgml-update-display ()
  (when (not (eq this-command 'keyboard-quit))
    ;; Don't let point be inside an invisible region
    (when (and (get-text-property (point) 'invisible)
	       (eq (get-text-property (point) 'invisible)
		   (get-text-property (1- (point)) 'invisible)))
      (setq sgml-last-element nil)	; May not be valid after point moved
      (if (memq this-command '(backward-char previous-line backward-word))
	  (goto-char (or (previous-single-property-change (point) 'invisible)
			 (point-min)))
	(goto-char (or (next-single-property-change (point) 'invisible)
		       (point-max)))))
    (when (and (not executing-macro)
	       (or sgml-live-element-indicator
		   sgml-set-face)
	       (not (null sgml-buffer-parse-state))
	       (sit-for 0))
      (let ((deactivate-mark nil))
	(sgml-need-dtd)
	(let ((eol-pos (save-excursion (end-of-line 1) (point))))
	  (let ((quiet (< (- (point) (sgml-max-pos-in-tree sgml-top-tree))
                          500)))
	    (when (if quiet
		      t
		    (setq sgml-current-element-name "?")
		    (sit-for 1))

	      ;; Find current element
	      (cond ((and (memq this-command sgml-users-of-last-element)
			  sgml-last-element)
		     (setq sgml-current-element-name
			   (sgml-element-gi sgml-last-element)))
		    (sgml-live-element-indicator
		     (save-excursion
		       (condition-case nil
			   (sgml-parse-to
			    (point) (function input-pending-p) quiet)
			 (error
			  (setq sgml-current-element-name "*error*")))
		       (unless (input-pending-p)
			 (setq sgml-current-element-name
			       (sgml-element-gi sgml-current-tree))
			 (force-mode-line-update)))))
	      ;; Set face on current line
	      (when (and sgml-set-face (not (input-pending-p)))
		(save-excursion
		  (condition-case nil
		      (sgml-parse-to
		       eol-pos (function input-pending-p) quiet)
		    (error nil)))))))
	;; Set face in rest of buffer
	(sgml-fontify-buffer 6)		;FIXME: make option for delay
	))))

(defun sgml-fontify-buffer (delay)
  (and
   sgml-set-face
   (null (sgml-tree-etag-epos
	  (sgml-pstate-top-tree sgml-buffer-parse-state)))
   (sit-for delay)
   (condition-case nil
       (save-excursion
	 (message "Fontifying...")
	 (sgml-parse-until-end-of nil nil
				  (function input-pending-p)
				  t)
	 (message "Fontifying...done"))
     (error nil))))

(defun sgml-set-active-dtd-indicator (name)
  ;; At least when using the which-func machinery, don't show anything
  ;; unless `sgml-live-element-indicator' is non-nil.
  (set (make-local-variable 'which-func-mode) sgml-live-element-indicator)
  (set (make-local-variable 'sgml-active-dtd-indicator)
       (list (format " [%s" name)
	     '(sgml-live-element-indicator ("/" sgml-current-element-name))
	     "]"))
  (force-mode-line-update))

;;;; Parser state

(defstruct (sgml-pstate
	    (:constructor sgml-make-pstate (dtd top-tree)))
  dtd
  top-tree)

;(defsubst sgml-excludes ()
;  (sgml-tree-excludes sgml-current-tree))

;(defsubst sgml-includes ()
;  (sgml-tree-includes sgml-current-tree))

(defsubst sgml-current-mixed-p ()
  (sgml-element-mixed sgml-current-tree))

(defun sgml-set-initial-state (dtd)
  "Set initial state of parsing."
  (make-local-hook 'before-change-functions)
  (make-local-hook 'after-change-functions)
  (add-hook 'before-change-functions 'sgml-note-change-at nil 'local)
  (add-hook 'after-change-functions 'sgml-set-face-after-change nil 'local)
  (sgml-set-active-dtd-indicator (sgml-dtd-doctype dtd))
  (let ((top-type			; Fake element type for the top
					; node of the parse tree
	 (sgml-make-eltype "#DOC")	; was "Document (no element)"
	 ))
    (setf (sgml-eltype-model top-type)
	  (sgml-make-primitive-content-token
	   (sgml-eltype-token
	    (sgml-lookup-eltype (sgml-dtd-doctype dtd) dtd))))
    (setq sgml-buffer-parse-state
	  (sgml-make-pstate dtd
			    (sgml-make-tree top-type
					    0 0 nil 0 nil nil nil nil nil)))))

(defun sgml-set-parse-state (tree where)
  "Set parse state from TREE.
Either from start of TREE if WHERE is start or from after TREE if
WHERE is `after'."
  (setq sgml-current-tree tree
	sgml-markup-tree tree
	sgml-rs-ignore-pos 0 )
  (let ((empty
	 (sgml-element-empty tree)))
    (cond ((and (eq where 'start)
		(not empty))
	   (setq sgml-current-state (sgml-element-model sgml-current-tree)
		 sgml-current-shortmap (sgml-tree-shortmap sgml-current-tree)
		 sgml-previous-tree nil)
	   (setq sgml-markup-type
		 (if (and (not (zerop (sgml-tree-stag-len tree)))
			  (sgml-bpos-p (sgml-tree-stag-epos tree)))
		     'start-tag)
		 sgml-markup-start (sgml-element-start sgml-current-tree))
	   (sgml-goto-epos (sgml-tree-stag-epos sgml-current-tree))
	   (forward-char (sgml-tree-stag-len sgml-current-tree)))
	  (t
	   (setq sgml-current-state (sgml-tree-pstate sgml-current-tree)
		 sgml-current-shortmap (sgml-tree-pshortmap sgml-current-tree)
		 sgml-previous-tree sgml-current-tree)
	   (sgml-goto-epos (sgml-tree-etag-epos sgml-current-tree))
	   (forward-char (sgml-tree-etag-len sgml-current-tree))
	   (setq sgml-markup-type (if empty 'start-tag 'end-tag)
		 sgml-markup-start (- (point)
				      (sgml-tree-etag-len sgml-current-tree)))
	   (setq sgml-current-tree (sgml-tree-parent sgml-current-tree))))
    (assert sgml-current-state)))

(defsubst sgml-final-p (state)
  ;; Test if a state/model can be ended
  (or (not (sgml-model-group-p state))
      (sgml-final state)))

;(defun sgml-current-element-contains-data ()
;  "Retrun true if the current open element is either mixed or is (r)cdata."
;  (or (eq sgml-cdata sgml-current-state)
;      (eq sgml-rcdata sgml-current-state)
;      (sgml-current-mixed-p)))

;(defun sgml-current-element-content-class ()
;  "Return a string describing the type of content in the current element.
;The type can be CDATA, RCDATA, ANY, #PCDATA or none."
;  (cond ((eq sgml-cdata sgml-current-state)
;	 "CDATA")
;	((eq sgml-rcdata sgml-current-state)
;	 "RCDATA")
;	((eq sgml-any sgml-current-state)
;	 "ANY")
;	((sgml-current-mixed-p)
;	 "#PCDATA")
;	(t "")))

(defun sgml-promoted-epos (start end)
  "Return an entity position for start of region START END.
If region is empty, choose return an epos as high in the
entity hierarchy as possible."
;; This does not work if the entity is entered by a shortref that
;; only is active in the current element.
  (let ((epos (sgml-epos start)))
    (when (= start end)
      (while (and (sgml-strict-epos-p epos)
		  (= 1 (sgml-epos-pos epos)))
	(setq epos (sgml-eref-start (sgml-epos-eref epos)))))
    epos))

(defun sgml-open-element (eltype conref before-tag after-tag
                                 &optional asl net-enabled)
  (unless (sgml-eltype-defined eltype)
    (setf (sgml-eltype-mixed eltype) t)
    (setf (sgml-eltype-etag-optional eltype) t)
    (when sgml-warn-about-undefined-elements
      (sgml-log-warning
       "Start-tag of undefined element %s; assume O O ANY"
       (sgml-eltype-name eltype))))
  (let* ((emap (sgml-eltype-shortmap eltype))
	 (newmap (if emap
		     (if (eq 'empty emap)
			 nil
		       (sgml-lookup-shortref-map
			(sgml-dtd-shortmaps sgml-dtd-info)
			emap))
		   sgml-current-shortmap))
	 (nt (sgml-make-tree
	      eltype
	      (sgml-promoted-epos before-tag after-tag) ; stag-epos
	      (- after-tag before-tag)	; stag-len
	      sgml-current-tree		; parent
	      (1+ (sgml-tree-level sgml-current-tree)) ; level
	      (append (sgml-eltype-excludes eltype)
		      (sgml-tree-excludes sgml-current-tree))
	      (append (sgml-eltype-includes eltype)
		      (sgml-tree-includes sgml-current-tree))
	      sgml-current-state
              (or net-enabled
                  (if (sgml-tree-net-enabled sgml-current-tree) 1))
	      conref
	      newmap
	      sgml-current-shortmap
	      asl)))
;; (let ((u (sgml-tree-content sgml-current-tree)))
;;      (cond ((and u (> before-tag (sgml-element-start u)))
;;	     (while (and (sgml-tree-next u)
;;			 (> before-tag
;;			    (sgml-element-start (sgml-tree-next u))))
;;	       (setq u (sgml-tree-next u)))
;;	     (setf (sgml-tree-next u) nt))
;;	    (t
;;	     (setf (sgml-tree-content sgml-current-tree) nt))))
    ;; Install new node in tree
    (cond (sgml-previous-tree
	   (sgml-debug "Open element %s: after %s"
		       eltype (sgml-tree-eltype sgml-previous-tree))
	   (setf (sgml-tree-next sgml-previous-tree) nt))
	  (t
	   (sgml-debug "Open element %s: first in %s"
		       eltype (sgml-tree-eltype sgml-current-tree))
	   (setf (sgml-tree-content sgml-current-tree) nt)))
    ;; Prune tree
    ;; *** all the way up?  tree-end = nil?
    (setf (sgml-tree-next sgml-current-tree) nil)
    ;; Set new state
    (setq sgml-current-state (sgml-eltype-model eltype)
	  sgml-current-shortmap newmap
	  sgml-current-tree nt
	  sgml-previous-tree nil)
    (assert sgml-current-state)
    (setq sgml-markup-tree sgml-current-tree)
    (run-hook-with-args 'sgml-open-element-hook sgml-current-tree asl)
    (when (sgml-element-empty sgml-current-tree)
      (sgml-close-element after-tag after-tag))))

(defun sgml-fake-open-element (tree el &optional state)
  (sgml-make-tree
   el 0 0
   tree
   0
   (append (sgml-eltype-excludes el) (sgml-tree-excludes tree))
   (append (sgml-eltype-includes el) (sgml-tree-includes tree))
   state
   nil
   nil))

(defun sgml-close-element (before-tag after-tag)
  (when (or (eq sgml-close-element-trap t)
	    (eq sgml-close-element-trap sgml-current-tree))
    (setq sgml-goal (point)))
  (when sgml-throw-on-element-change
    (throw sgml-throw-on-element-change 'end))
  (sgml-debug "Close element %s" (sgml-tree-eltype sgml-current-tree))
  (setf (sgml-tree-etag-epos sgml-current-tree)
	;;(sgml-promoted-epos before-tag after-tag)
	(sgml-epos before-tag))
  (setf (sgml-tree-etag-len sgml-current-tree) (- after-tag before-tag))
  (run-hooks 'sgml-close-element-hook)
  (setq sgml-markup-tree sgml-current-tree)
  (cond ((eq sgml-current-tree sgml-top-tree)
	 (unless (eobp)
	   (sgml-error "Parse ended")))
	(t
	 (setq sgml-previous-tree sgml-current-tree
	       sgml-current-state (sgml-tree-pstate sgml-current-tree)
	       sgml-current-shortmap (sgml-tree-pshortmap sgml-current-tree)
	       sgml-current-tree (sgml-tree-parent sgml-current-tree))
	 (assert sgml-current-state))))

(defun sgml-fake-close-element (tree)
  (sgml-tree-parent tree))

(defun sgml-note-change-at (at &optional end)
  ;; Inform the cache that there have been some changes after AT
  (when sgml-buffer-parse-state
    (sgml-debug "sgml-note-change-at %s" at)
    (let ((u (sgml-pstate-top-tree sgml-buffer-parse-state)))
      (when u
	;;(message "%d" at)
        (when (and sgml-xml-p (> at (point-min)))
          (when (eq ?/ (char-after (1- at)))
            (setq at (1- at))))
	(while
	    (cond
	     ((and (sgml-tree-next u)	; Change clearly in next element
		   (> at (sgml-element-stag-end (sgml-tree-next u))))
	      (setq u (sgml-tree-next u)))
	     (t				; 
	      (setf (sgml-tree-next u) nil) ; Forget next element
	      (cond
	       ;; If change after this element and it is ended by an end
	       ;; tag no pruning is done.  If the end of the element is
	       ;; implied changing the tag that implied it may change
	       ;; the extent of the element.
	       ((and (sgml-tree-etag-epos u)
		     (> at (sgml-tree-end u))
		     (or (> (sgml-tree-etag-len u) 0)
			 (sgml-element-empty u)))
		nil)
	       (t
		(setf (sgml-tree-etag-epos u) nil)
		(cond;; Enter into content if change is clearly in it
		 ((and (sgml-tree-content u)
		       (> at (sgml-element-stag-end (sgml-tree-content u))))
		  (setq u (sgml-tree-content u)))
		 ;; Check if element has no start tag,
		 ;; then it must be pruned as a change could create
		 ;; a valid start tag for the element.
		 ((and (zerop (sgml-tree-stag-len u))
		       (> at (sgml-element-start u)))
		  ;; restart from to with new position
		  ;; this can't loop forever as
		  ;; position allways gets smaller
		  (setq at (sgml-element-start u)
			u sgml-top-tree))
		 (t
		  (setf (sgml-tree-content u) nil))))))))))))

(defun sgml-list-implications (token type)
  "Return a list of the tags implied by a token TOKEN.
TOKEN is a token, and the list elements are either tokens or `t'.
Where the latter represents end-tags."
  (let ((state sgml-current-state)
	(tree sgml-current-tree)
	(temp nil)
	(imps nil))
    (while				; Until token accepted
	(cond
	 ;; Test if accepted in state
	 ((or (eq state sgml-any)
	      (and (sgml-model-group-p state)
		   (not (memq token (sgml-tree-excludes tree)))
		   (or (memq token (sgml-tree-includes tree))
		       (sgml-get-move state token))))
	  nil)
	 ;; Test if end tag implied
	 ((or (eq state sgml-empty)
	      (and (sgml-final-p state)
		   (not (eq tree sgml-top-tree))))
	  (unless (eq state sgml-empty)	; not really implied
	    (push t imps))
	  (setq state (sgml-tree-pstate tree)
		tree (sgml-fake-close-element tree))
	  t)
	 ;; Test if start-tag can be implied
	 ((and (setq temp (sgml-required-tokens state))
	       (null (cdr temp)))
	  (setq temp (car temp)
		tree (sgml-fake-open-element tree temp
					     (sgml-get-move state temp))
		state (sgml-element-model tree))
	  (push temp imps)
	  t)
	 ;; No implictions and not accepted
	 (t
	  (sgml-log-warning "Out of context %s" type)
	  (setq imps nil))))
    ;; Return the implications in correct order
    (nreverse imps)))


(defun sgml-eltypes-in-state (tree state)
  "Return list of element types (eltype) valid in STATE and TREE."
  (let* ((req				; Required tokens
	  (if (sgml-model-group-p state)
	      (sgml-required-tokens state)))
	 (elems				; Normally valid tokens
	  (if (sgml-model-group-p state)
	      (nconc req
		     (delq sgml-pcdata-token (sgml-optional-tokens state))))))
    ;; Modify for exceptions
    (loop for et in (sgml-tree-includes tree) ;*** Tokens or eltypes?
	  unless (memq et elems) do (push et elems))
    (loop for et in (sgml-tree-excludes tree)
	  do (setq elems (delq et elems)))
    ;; Check for omitable start-tags
    (when (and sgml-omittag-transparent
	       (not (sgml-final-p state))
	       req
	       (null (cdr req)))
      (let ((et (sgml-token-eltype (car req))))
	(when (sgml-eltype-stag-optional et)
	  (setq elems
		(nconc elems		; *** possibility of duplicates
		       (sgml-eltypes-in-state
			(sgml-fake-open-element tree et)
			(sgml-eltype-model et)))))))
    elems))

(defun sgml-current-list-of-valid-eltypes ()
  "Return a list of contextually valid element types (eltype)."
  (let ((elems (sgml-eltypes-in-state sgml-current-tree sgml-current-state))
	(tree sgml-current-tree)
	(state sgml-current-state))
    (when sgml-omittag-transparent
      (while (and tree
		  (sgml-final-p state)
		  (sgml-element-etag-optional tree))
	(setq state (sgml-tree-pstate tree)
	      tree (sgml-tree-parent tree))
	(loop for e in (sgml-eltypes-in-state tree state) do
	      (when (not (memq e elems))
		(setq elems (nconc elems (list e)))))))
    ;; FIXME: Filter out elements that are undefined?
    (sort elems (function string-lessp))))

(defun sgml-current-list-of-endable-eltypes ()
  "Return a list of the element types endable in current state."
  (let* ((elems nil)
	 (tree sgml-current-tree)
	 (state sgml-current-state))
    (while
	(and (sgml-final-p state)
	     (not (eq tree sgml-top-tree))
	     (progn
	       (setq elems
		     (nconc elems (list (sgml-tree-eltype tree))))
	       sgml-omittag)
	     (sgml-eltype-etag-optional (sgml-tree-eltype tree)))
      (setq state (sgml-tree-pstate tree)
	    tree (sgml-tree-parent tree)))
    elems))

;;;; Logging of warnings

(defconst sgml-log-buffer-name "*SGML LOG*")

(defvar sgml-log-last-size 0)

(defun sgml-display-log ()
  (let ((buf (get-buffer sgml-log-buffer-name)))
    (when buf
      (display-buffer buf)
      (setq sgml-log-last-size (save-excursion (set-buffer buf)
					       (point-max))))))

(defun sgml-log-message (format &rest things)
  (let ((mess (apply 'format format things))
	(buf (get-buffer-create sgml-log-buffer-name))
	(cb (current-buffer)))
    (set-buffer buf)
    (goto-char (point-max))
    (insert mess "\n")
    (when (get-buffer-window buf)
      (setq sgml-log-last-size  (point-max)))
    (set-buffer cb)))

(defun sgml-reset-log ()
  (let ((buf (get-buffer sgml-log-buffer-name)))
    (when buf
      (setq sgml-log-last-size
	    (save-excursion (set-buffer buf)
			    (point-max))))))

(defun sgml-clear-log ()
  (let ((b (get-buffer sgml-log-buffer-name)))
    (when b
      (delete-windows-on b)
      (kill-buffer b)
      (setq sgml-log-last-size 0))))

(defun sgml-show-or-clear-log ()
  "Show the *SGML LOG* buffer if it is not showing.
clear and remove it if it is showing."
  (interactive)
  (cond ((and (get-buffer sgml-log-buffer-name)
	      (null (get-buffer-window sgml-log-buffer-name)))
	 (sgml-display-log))
	(t
	 (sgml-clear-log))))



(defun sgml-log-entity-stack ()
  (save-excursion
    (loop
     do (sgml-log-message
         "%s line %s col %s %s"
         (or sgml-current-file (buffer-file-name) "-")
         (count-lines (point-min) (point))
         (current-column)
         (let ((entity (if sgml-current-eref
                           (sgml-eref-entity sgml-current-eref))))
           (if (and entity (sgml-entity-type entity))
               (format "entity %s" (sgml-entity-name entity))
             "")))
     while (and (boundp 'sgml-previous-buffer) sgml-previous-buffer)
     do (set-buffer sgml-previous-buffer))))



(defvar sgml-warning-message-flag nil
  "True if a warning message has been displayed.
To avoid clearing message with out showing previous warning.")


(defun sgml-log-warning (format &rest things)
  (when sgml-throw-on-warning
    (apply 'message format things)
    (throw sgml-throw-on-warning t))
  (when (or sgml-show-warnings sgml-parsing-dtd)
    (apply 'sgml-message format things)
    (setq sgml-warning-message-flag t)))


(defun sgml-error (format &rest things)
  (when sgml-throw-on-error
    (throw sgml-throw-on-error nil))
  (setq sgml-warning-message-flag nil)
  (error "%s%s" (apply 'format format things )
         (sgml-entity-stack)))


(defun sgml-entity-stack-1 ()
  (format
   "\n %s line %s col %s %s%s"
   (or sgml-current-file (buffer-file-name) "-")
   (count-lines (point-min) (point))
   (current-column)
   (let ((entity (if sgml-current-eref
                     (sgml-eref-entity sgml-current-eref))))
     (if (and entity (sgml-entity-type entity))
         (format "entity %s" (sgml-entity-name entity))
       ""))
   (if (and (boundp 'sgml-previous-buffer) sgml-previous-buffer)
       (progn (set-buffer sgml-previous-buffer)
              (sgml-entity-stack-1))
     "")))

(defun sgml-entity-stack ()
  (save-excursion (sgml-entity-stack-1)))


(defun sgml-parse-warning (format &rest things)
  (message "%s%s" (apply 'format format things) (sgml-entity-stack))
  (setq sgml-warning-message-flag t))

(defun sgml-parse-error (format &rest things)
  (apply 'sgml-error
	 (concat format "; at: %s")
	 (append things (list (buffer-substring-no-properties
			       (point)
			       (min (point-max) (+ (point) 12)))))))

(defun sgml-message (format &rest things)
  (unless (and (or (equal format "")
                 (string-match "\\.\\.done$" format))
             sgml-warning-message-flag)
    (apply 'message format things)
    (setq sgml-warning-message-flag nil)))



;;; This has noting to do with warnings...

(defvar sgml-lazy-time 0)

(defun sgml-lazy-message (&rest args)
  (unless (= sgml-lazy-time (second (current-time)))
    (apply 'message args)
    (setq sgml-lazy-time (second (current-time)))))


;;;; Shortref maps

(eval-and-compile
  (defconst sgml-shortref-list
    '(
      "\t"				;&#TAB
      "\n"				;&#RE;
      "\001"				;&#RS;
      "\001B"
      "\001\n"
      "\001B\n"
      "B\n"
      " "				;&#SPACE;
      "BB"
      "\""				;&#34;
      "#"
      "%"
      "'"
      "("
      ")"
      "*"
      "+"
      ","
      "-"
      "--"
      ":"
      ";"
      "="
      "@"
      "["
      "]"
      "^"
      "_"
      "{"
      "|"
      "}"
      "~")))

(eval-and-compile
  (defun sgml-shortref-index (string)
    (let ((pos (member string sgml-shortref-list))
	  (len (length sgml-shortref-list)))
      (and pos (- len (length pos))) )))

(defun sgml-make-shortmap (pairs)
  "Create a shortreference map from PAIRS.
Where PAIRS is a list of (delim . ename)."
  (let ((map
	 (make-vector (1+ (length sgml-shortref-list))
		      nil))
	index)
    (loop for p in pairs
	  for delim = (car p)
	  for name = (cdr p)
	  do
	  (setq index (sgml-shortref-index delim))
	  (cond ((null index)
		 (sgml-log-warning
		  "Illegal short reference delimiter '%s'" delim))
		(t
		 (aset map index name))))
    ;; Compute a suitable string for skip-chars-forward that
    ;; can be used to skip over pcdata
    (aset map
	  (eval-when-compile (length sgml-shortref-list))
	  (if (some (function
		     (lambda (r) (aref map (sgml-shortref-index r))))
		    '("\001B\n" "B\n" " " "BB"))
	      "^<]/& \n\t\"#%'()*+,\\-:;=@[]\\^_{|}~"
	    "^<]/&\n\t\"#%'()*+,\\-:;=@[]\\^_{|}~"))
    map))

(defun sgml-shortmap-skipstring (map)
  (if (bolp)
      ""
      (aref map (eval-when-compile (length sgml-shortref-list)))))


(defconst sgml-shortref-oneassq
  (loop for d in sgml-shortref-list
	for c = (aref d 0)
	when (and (= 1 (length d))
		  (/= 1 c) (/= 10 c))
	collect (cons c (sgml-shortref-index d))))

(defun sgml-parse-B ()
  (/= 0 (skip-chars-forward " \t")))

(defun sgml-deref-shortmap (map &optional nobol)
  "Identify shortref delimiter at point and return entity name.
Also move point.  Return nil, either if no shortref or undefined."

  (macrolet
      ((delim (x) (` (aref map (, (sgml-shortref-index x))))))
    (let ((i (if nobol 1 0)))
      (while (numberp i)
	(setq i
	      (cond
	       ((and (bolp) (zerop i)) ; Either "\001" "\001B"
					; "\001\n" "\001B\n"
		(cond ((sgml-parse-B)	; "\001B"
		       (if (eolp)
			   (delim "\001B\n")
			 (delim "\001B")))
		      ((sgml-parse-RE) (delim "\001\n"))
		      ((delim "\001"))
		      (t 1)))
	       ((cond ((sgml-parse-char ?\t) (setq i (delim "\t")) t)
		      ((sgml-parse-char ? )  (setq i (delim " "))  t))
		(cond ((sgml-parse-B) (setq i (delim "BB"))))
		(cond ((sgml-parse-char ?\n)
		       (delim "B\n"))
		      (t i)))
	       ((sgml-parse-RE) (delim "\n"))
	       ((sgml-parse-chars ?- ?-) (delim "--"))
	       ;; The other one character delimiters
	       ((setq i (assq (following-char) sgml-shortref-oneassq))
		(when i (forward-char 1))
		(aref map (cdr i))))))
      i)))

;;; Table of shortref maps

(defun sgml-make-shortref-table ()
  (list nil))

(defun sgml-add-shortref-map (table name map)
  (nconc table (list (cons name map))))

(defun sgml-lookup-shortref-map (table name)
  (cdr (assoc name (cdr table))))

(defun sgml-lookup-shortref-name (table map)
  (car (rassq map (cdr table))))

(defun sgml-merge-shortmaps (tab1 tab2)
  "Merge tables of short reference maps TAB2 into TAB1, modifying TAB1."
  (nconc tab1 (cdr tab2)))

;;;; Parse markup declarations

(defun sgml-skip-until-dsc ()
  (while (progn
	   (if sgml-xml-p
	       (sgml-skip-upto ("DSO" "DSC" "LITA" "LIT" "XML-SCOM" "COM"))
	     (sgml-skip-upto ("DSO" "DSC" "LITA" "LIT" "COM")))
	   (not (sgml-parse-delim "DSC")))
    (cond ((sgml-parse-literal))
	  ((sgml-parse-delim "DSO")
	   (sgml-skip-until-dsc))
	  ((and sgml-xml-p (sgml-parse-xml-comment)))
	  ((and (not sgml-xml-p) (sgml-parse-comment)))
	  (t (forward-char 1)))))

(defun sgml-skip-upto-mdc ()
  "Move point forward until end of current markup declaration.
Assumes starts with point inside a markup declaration."
  (while (progn
	   (sgml-skip-upto ("DSO" "MDC" "LIT" "LITA" "COM"))
	   (not (sgml-is-delim "MDC")))
    (cond ((sgml-parse-delim "DSO")
	   (sgml-skip-until-dsc))
	  ((sgml-parse-literal))
	  ((sgml-parse-comment))
	  (t (forward-char 1)))))

(defun sgml-do-sgml-declaration ()
  (sgml-skip-upto-mdc)
  (setq sgml-markup-type 'sgml))

(defun sgml-do-doctype ()
  (cond
   (sgml-dtd-info			; Has doctype already been defined
    (sgml-skip-upto-mdc))
   (t
    (let (sgml-markup-start)
      (message "Parsing doctype...")
      (sgml-setup-doctype (sgml-check-name)
			  (sgml-parse-external))
      (message "Parsing doctype...done"))))
  (setq sgml-markup-type 'doctype))

(defun sgml-check-end-of-entity (type)
  (unless (eobp)
    (sgml-parse-error "Illegal character '%c' in %s"
		      (following-char)
		      type)))

(defun sgml-setup-doctype (docname external)
  (let ((sgml-parsing-dtd t))
    (setq sgml-no-elements 0)
    (setq sgml-dtd-info (sgml-make-dtd docname))
    ;;(setq sgml-dtd-shortmaps nil)
    (sgml-skip-ps)
    (cond
     ((sgml-parse-delim "DSO")
      (let ((original-buffer (current-buffer)))
	(sgml-check-dtd-subset)
	(if (eq (current-buffer) original-buffer)
	    (sgml-check-delim "DSC")
	  (sgml-parse-error "Illegal character '%c' in doctype declaration"
			    (following-char))))))
    (cond (external
	   (sgml-push-to-entity (sgml-make-entity docname 'dtd external))
	   (sgml-check-dtd-subset)
	   (sgml-check-end-of-entity "DTD subset")
	   (sgml-pop-entity)))
    (when sgml-xml-p
      (let ((table (sgml-dtd-entities sgml-dtd-info)))
        (sgml-entity-declare "lt" table 'text "&#60;")
        (sgml-entity-declare "gt" table 'text ">")
        (sgml-entity-declare "amp" table 'text "&#38;")
        (sgml-entity-declare "apos" table 'text "'")
        (sgml-entity-declare "quot" table 'text "\"")))
    (sgml-set-initial-state sgml-dtd-info)
    (run-hooks 'sgml-doctype-parsed-hook)))

(defun sgml-do-data (type &optional marked-section)
  "Move point forward until there is an end-tag open after point."
  (let ((start (point))
	(done nil)
	(eref sgml-current-eref)
	sgml-signal-data-function)
    (while (not done)
      ;; FIXME: a lot of hardcoded knowledge about concrete delimiters
      (cond (marked-section
	     (skip-chars-forward (if (eq type sgml-cdata) "^]" "^&]"))
	     (when sgml-data-function
	       (funcall sgml-data-function (buffer-substring-no-properties
					    start (point))))
	     (setq done (sgml-parse-delim "MS-END")))
	    (t
	     (skip-chars-forward (if (eq type sgml-cdata) "^</" "^</&"))
	     (when sgml-data-function
	       (funcall sgml-data-function
                        (buffer-substring-no-properties start (point))))
	     (setq done (or (sgml-is-delim "ETAGO" gi)
			    (sgml-is-enabled-net)))))
      (setq start (point))
      (cond
       (done)
       ((eobp)
	(when (eq eref sgml-current-eref)
	  (sgml-error "Unterminated %s %s"
		      type (if marked-section "marked section")))
	(sgml-pop-entity)
	(setq start (point)))
       ((null sgml-data-function)
	(forward-char 1))
       ((sgml-parse-general-entity-ref)
	(setq start (point)))
       (t
	(forward-char 1))))))


(defun sgml-do-marked-section ()
  (let ((status nil))
    (while (progn (sgml-skip-ps)
		  (not (sgml-parse-char ?\[)))
      (push (sgml-check-name)
	    status))
    (cond
     ((member "IGNORE" status)
      (sgml-skip-marked-section)
      (sgml-set-markup-type 'ignored))
     ((or (member "CDATA" status)
	  (member "RCDATA" status))
      (when sgml-signal-data-function
	(funcall sgml-signal-data-function))
      (let ((type (if (member "CDATA" status) sgml-cdata sgml-rcdata)))
	(sgml-do-data type t)
      (sgml-set-markup-type type)))
     (t
      (sgml-set-markup-type 'ms-start)))))
  
(defun sgml-skip-marked-section ()
  (while (progn
	   (sgml-skip-upto ("MS-START" "MS-END"))
	   (when (eobp) (sgml-error "Marked section unterminated"))
	   (not (sgml-parse-delim "MS-END")))
    (cond ((sgml-parse-delim "MS-START")
	   ;;(search-forward "[")
	   (sgml-skip-marked-section))
	  (t (forward-char 1)))))

(defun sgml-do-usemap ()
  (let (mapname)
    ;;(setq sgml-markup-type 'usemap)
    (unless (sgml-parse-rni "EMPTY")
      (setq mapname (sgml-check-name)))
    (sgml-skip-ps)
    (cond
     ((sgml-is-delim "MDC")
      (sgml-debug "USEMAP %s" (if mapname mapname "#EMPTY"))
      (cond (sgml-dtd-info
	     (setq sgml-current-shortmap
		   (if mapname
		       (or (sgml-lookup-shortref-map
			    (sgml-dtd-shortmaps sgml-dtd-info)
			    mapname)
			   (sgml-error "Undefined shortref map %s" mapname)))))
	    ;; If in prolog
	    (t
	     (sgml-log-warning
	      "USEMAP without associated element type in prolog"))))
     (t
      ;; Should be handled by psgml-dtd
      (sgml-do-usemap-element mapname)))))

(defconst sgml-markup-declaration-table
  '(("SGML"     . sgml-do-sgml-declaration)
    ("DOCTYPE"  . sgml-do-doctype)
    ("ELEMENT"  . sgml-declare-element)
    ("ENTITY"   . sgml-declare-entity)
    ("USEMAP"   . sgml-do-usemap)
    ("SHORTREF" . sgml-declare-shortref)
    ("NOTATION" . sgml-declare-notation)
    ("ATTLIST"  . sgml-declare-attlist)
    ("USELINK"  . sgml-skip-upto-mdc)
    ("LINKTYPE" . sgml-skip-upto-mdc)
    ("LINK"     . sgml-skip-upto-mdc)
    ("IDLINK"   . sgml-skip-upto-mdc)))


(defun sgml-parse-markup-declaration (option)
  "Parse a markup declaration.
OPTION can be `prolog' if parsing the prolog or `dtd' if parsing the
dtd or `ignore' if the declaration is to be ignored."
  (cond
   ((and sgml-xml-p (sgml-parse-xml-comment)))
   ((sgml-parse-delim "MDO" (nmstart "COM" "MDC"))
    (cond
     ((sgml-startnm-char-next)
      (setq sgml-markup-type nil)
      (let* ((tok (sgml-parse-nametoken))
	     (rut (assoc (sgml-check-case tok) sgml-markup-declaration-table)))
	(when (and (not (memq option '(prolog ignore)))
		   (member tok '("SGML" "DOCTYPE")))
	  (sgml-error "%s declaration is only valid in prolog" tok))
	(when (and (not (memq option '(dtd ignore)))
		   (member tok '("ELEMENT" "ENTITY" "ATTLIST" "NOTATION"
				 "SHORTREF")))
	  (sgml-error "%s declaration is only valid in doctype" tok))
	(cond ((eq option 'ignore)
	       (sgml-skip-upto-mdc))
	      (rut (sgml-skip-ps)
		   (funcall (cdr rut)))
	      (t (sgml-parse-error
		  "Illegal markup declaration %s" tok)))))
     (t
      (setq sgml-markup-type 'comment)))
    (sgml-skip-ps)
    (sgml-check-delim "MDC")
    (unless (eq option 'ignore)		; Set the markup type given
      (when sgml-markup-type
	(sgml-set-markup-type sgml-markup-type)))
    t)
   ((sgml-parse-delim "MS-START")
    (sgml-do-marked-section))))

;;;; Parsing attribute values

(defun sgml-parse-attribute-specification-list (&optional eltype)
  "Parse an attribute specification list.
Optional argument ELTYPE, is used to resolve omitted name=.
Returns a list of attspec (attribute specification)."
  (setq sgml-conref-flag nil)
  (let ((attlist (if eltype (sgml-eltype-attlist eltype)))
	name val asl attdecl)
    (while (setq name (progn (sgml-parse-s)
			     (sgml-parse-nametoken)))
      (sgml-parse-s)
      (cond ((sgml-parse-delim "VI")
	     (sgml-parse-s)
	     (setq val (sgml-parse-attribute-value-specification 'warn))
	     (if (null val)
		 (setq attdecl nil)
	       (when eltype
		 (or (setq attdecl (sgml-lookup-attdecl name attlist))
		     sgml-dtd-less
		     (sgml-log-warning
		      "Attribute %s not declared for element %s"
		      name (sgml-eltype-name eltype))))))
	    ((null eltype)
	     (sgml-parse-error "Expecting a ="))
	    ((progn
	       (unless sgml-current-shorttag
		 (sgml-log-warning
		  "Must have attribute name when SHORTTAG NO"))
	       (setq attdecl
		     (sgml-find-attdecl-for-value (setq val name)
						  eltype))))
	    (t
	     (sgml-log-warning
	      "%s is not in any name group for element %s"
	      val
	      (sgml-eltype-name eltype))))
      ;; FIXME: What happens when eltype is nil ??
      (cond
       (attdecl
	;; JDF's addition 12/2001
	(if (eq (sgml-attdecl-declared-value attdecl) 'ID)
	    (sgml-add-id val))
	(push (sgml-make-attspec (sgml-attdecl-name attdecl) val)
	      asl)
	(when (sgml-default-value-type-p 'CONREF
					 (sgml-attdecl-default-value attdecl))
	  (setq sgml-conref-flag t)))
       (t                               ; No attdecl, record attribute any way
        (push (sgml-make-attspec name val) asl))))
    asl))

(defun sgml-check-attribute-value-specification ()
  (or (sgml-parse-literal)
      (prog1 (sgml-parse-nametoken t)	; Not really a nametoken, but an
	(when sgml-xml-p		; undelimited literal
	  (sgml-parse-warning "XML forbids undelimited literals.")))
      (sgml-parse-error "Expecting an attribute value: literal or token")))

(defun sgml-parse-attribute-value-specification (&optional warn)
  (or (sgml-parse-literal)
      (sgml-parse-nametoken t)		; Not really a nametoken, but an
					; undelimited literal
      (if warn
	  (progn
	    (sgml-log-warning "Expecting an attribute value: literal or token")
	    nil))))


(defun sgml-find-attdecl-for-value (value eltype)
  "Find the attribute declaration of ELTYPE that has VALUE in its name group.
VALUE is a string.  Returns nil or an attdecl."
  (let ((al (sgml-eltype-attlist eltype))
	dv)
    (while (and al
		(or (atom (setq dv (sgml-attdecl-declared-value (car al))))
		    (not (member value
				 (sgml-declared-value-token-group dv)))))
      (setq al (cdr al)))
    (if al (car al))))


;;;; Parser driver

;; The parser maintains a partial parse tree during the parse.  This tree
;; can be inspected to find information, and also be used to restart the
;; parse.  The parser also has a position in the current content model.
;; (Called a state.)  The parser is used for several things:
;; 1) To find the state the parser would be in at a point in the buffer.
;;    (Point in emacs sense, I.e. between chararacters).
;; 2) Identify the element containing a character.
;; 3) Find end of an element.
;; 4) Find the next element.
;; 5) To find the previous element.

;; These tasks are done by a combination of parsing and traversing
;; the partial parse tree.  The primitive parse operation is to parse
;; until a goal point in the buffer has been passed.  In addition to
;; this it is possible to "trap" closing of elements.  Either for a
;; specific element or for any element.  When the trap is sprung the
;; parse is ended.  This is used to extend the parse tree.  When the
;; trap is used the parser is usually called with the end of the
;; buffer as the goal point.

(defun sgml-need-dtd ()
  "Make sure that an eventual DTD is parsed or loaded."
  (sgml-pop-all-entities)
  (sgml-cleanup-entities)
  (when (null sgml-buffer-parse-state)	; first parse in this buffer
    ;;(sgml-set-initial-state)		; fall back DTD
    (add-hook 'pre-command-hook 'sgml-reset-log)
    (make-local-variable 'sgml-auto-fill-inhibit-function)
    (setq sgml-auto-fill-inhibit-function (function sgml-in-prolog-p))
    (if sgml-default-dtd-file
	(sgml-load-dtd sgml-default-dtd-file)
      (sgml-load-doctype)))
  (sgml-debug "Need dtd getting state from %s" (buffer-name))
  (setq sgml-dtd-info (sgml-pstate-dtd sgml-buffer-parse-state)
	sgml-top-tree (sgml-pstate-top-tree sgml-buffer-parse-state))
  (sgml-set-global))


(defun sgml-load-doctype ()
  "Load the documents DTD.
Either from parent document or by parsing the document prolog."
  (interactive)
  (cond
   ;; Case of doctype in another file
   ((or sgml-parent-document sgml-doctype)
    (let ((dtd
	   (save-excursion		; get DTD from parent document
	     (set-buffer (find-file-noselect
			  (if (consp sgml-parent-document)
			      (car sgml-parent-document)
			    (or sgml-doctype sgml-parent-document))))
	     (sgml-need-dtd)
	     (sgml-pstate-dtd sgml-buffer-parse-state))))
      (sgml-set-initial-state dtd)
      (when (consp sgml-parent-document) ; modify DTD for child documents
	(sgml-modify-dtd (cdr sgml-parent-document)))))
   
   ;; The doctype declaration should be in the current buffer
   (t
    (save-excursion (sgml-parse-prolog)))))


(defun sgml-modify-dtd (modifier)
  (setq sgml-dtd-info (sgml-pstate-dtd sgml-buffer-parse-state)
	sgml-top-tree (sgml-pstate-top-tree sgml-buffer-parse-state))
  (sgml-set-global)
  (setq sgml-current-tree sgml-top-tree)
  (while (stringp (cadr modifier))	; Loop thru the context elements
    (let ((et (sgml-lookup-eltype (sgml-general-case (car modifier)))))
      (sgml-open-element et nil (point-min) (point-min))
      (setq modifier (cdr modifier))))

  (unless (stringp (car modifier))
    (error "Wrong format of sgml-parent-document"))

  (let* ((doctypename (car modifier))
	 (et (sgml-lookup-eltype
	      (sgml-general-case (if (symbolp doctypename)
				     (symbol-name doctypename)
				   doctypename)))))
    
    (setq sgml-current-state
	  (sgml-make-primitive-content-token et))

    (when (consp (cdr modifier))	; There are "seen" elements
      (sgml-open-element et nil (point-min) (point-min))
      (loop for seenel in (cadr modifier)
	    do (let ((new-state (sgml-get-move sgml-current-state
				    (sgml-lookup-eltype
                                     (sgml-general-case seenel)))))
                 (unless new-state
                   (error
                    "Illegal has-seen-element in sgml-parent-document: %s"
                    seenel))
                 (setq sgml-current-state new-state)))))
  
  (let ((top (sgml-pstate-top-tree sgml-buffer-parse-state)))
    (setf (sgml-tree-includes top) (sgml-tree-includes sgml-current-tree))
    (setf (sgml-tree-excludes top) (sgml-tree-excludes sgml-current-tree))
    (setf (sgml-tree-shortmap top) sgml-current-shortmap)
    (setf (sgml-eltype-model (sgml-tree-eltype top))
	  sgml-current-state)
    (setf (sgml-tree-content top) nil)))


(defun sgml-set-global ()
  (setq sgml-current-namecase-general sgml-namecase-general
        sgml-current-omittag sgml-omittag
	sgml-current-shorttag sgml-shorttag
	sgml-current-localcat sgml-local-catalogs
	sgml-current-local-ecat sgml-local-ecat-files
	sgml-current-top-buffer (current-buffer)
        sgml-markup-start nil))

(defun sgml-parse-prolog ()
  "Parse the document prolog to learn the DTD."
  (interactive)
  (sgml-debug "Parse prolog in buffer %s" (buffer-name))
  (unless sgml-debug
    (sgml-clear-log))
  (message "Parsing prolog...")
  (sgml-cleanup-entities)
  (sgml-set-global)
  (setq	sgml-dtd-info nil)
  (goto-char (point-min))
  ;; Bind sgml-set-face to avoid fontification parsing past the prolog
  ;; and filling in sgml-dtd-info.
  (let ((sgml-set-face nil)
	;; Avoid a warning if we don't have a DTD.  Fixme: does this
	;; lose if we do have a DTD and the prolog's wrong?
	(sgml-warn-about-undefined-entities
	 (if sgml-xml-p
	     nil
             sgml-warn-about-undefined-entities)))
    (sgml-with-parser-syntax-ro
     (while (progn (setq sgml-markup-start (point))
		   (or (sgml-parse-s)
		       (sgml-parse-processing-instruction)
		       (and (sgml-parse-markup-declaration 'prolog)
			    (null sgml-dtd-info)))))
     (unless sgml-dtd-info		; Set up a default doctype
       (let ((docname (or (and sgml-default-doctype-name
                               (sgml-general-case sgml-default-doctype-name))
			  (if (sgml-parse-delim "STAGO" gi)
			      (sgml-parse-name)))))
	 (when docname
	   (sgml-setup-doctype docname '(nil)))))))
  ;; Unless we have a DTD, the element types vector will only contain
  ;; one element type after removing zeroes..
  ;; FIXME: this is too ugly.
  ;; Would it not be better to count the element types than to rely on
  ;; some properties of OBLISTs.
  (set (make-local-variable 'sgml-dtd-less)
       (or (null sgml-dtd-info)
           (= 1 (length (delq 0 (append (sgml-dtd-eltypes sgml-dtd-info)
                                        '()))))))
  (when (and sgml-xml-p sgml-dtd-less)
    (set (make-local-variable 'sgml-warn-about-undefined-elements) nil)
    (set (make-local-variable 'sgml-warn-about-undefined-entities) nil))
  (unless sgml-dtd-info
    (error "No document type defined by prolog"))
  (sgml-message "Parsing prolog...done"))


(defun sgml-parse-until-end-of (sgml-close-element-trap &optional
							cont extra-cond quiet)
  "Parse until the SGML-CLOSE-ELEMENT-TRAP has ended.
Or if it is t, any additional element has ended,
or if nil, until end of buffer."
  (sgml-debug "-> sgml-parse-until-end-of")
  (cond
   (cont (sgml-parse-continue (point-max)))
   (t    (sgml-parse-to (point-max) extra-cond quiet)))
  (when (eobp)				; End of buffer, can imply
					; end of any open element.
    (while (prog1 (not
		   (or (eq sgml-close-element-trap t)
		       (eq sgml-close-element-trap sgml-current-tree)
		       (eq sgml-current-tree sgml-top-tree)))
	     (sgml-implied-end-tag "buffer end" (point) (point)))))
  (sgml-debug "<- sgml-parse-until-end-of"))

(defun sgml-parse-to (sgml-goal &optional extra-cond quiet)
  "Parse until (at least) SGML-GOAL.
Optional argument EXTRA-COND should be a function.  This function is
called in the parser loop, and the loop is exited if the function returns t.
If third argument QUIET is non-nil, no \"Parsing...\" message will be displayed."
  (sgml-need-dtd)
  (sgml-with-parser-syntax-ro
   (sgml-goto-start-point (min sgml-goal (point-max)))
   (setq quiet (or quiet (< (- sgml-goal (sgml-mainbuf-point)) 500)))
  (unless quiet
    (sgml-message "Parsing..."))
   (sgml-parser-loop extra-cond)
  (unless quiet
    (sgml-message ""))))

(defun sgml-parse-continue (sgml-goal &optional extra-cond quiet)
  "Parse until (at least) SGML-GOAL."
  (assert sgml-current-tree)
  (unless quiet
    (sgml-message "Parsing..."))
  (sgml-debug "Parse continue")
  (sgml-with-parser-syntax-ro
   (set-buffer sgml-last-buffer)
   (sgml-parser-loop extra-cond))
  (unless quiet
    (sgml-message "")))

(defun sgml-reparse-buffer (shortref-fun)
  "Reparse the buffer and let SHORTREF-FUN take care of short references.
SHORTREF-FUN is called with the entity as argument and `sgml-markup-start'
pointing to start of short ref and point pointing to the end."
  (sgml-note-change-at (point-min))
  (let ((sgml-shortref-handler shortref-fun))
    (sgml-parse-until-end-of nil)))

(defsubst sgml-move-current-state (token)
  (setq sgml-current-state
	(or (sgml-get-move sgml-current-state token)
	    sgml-current-state)))

(defun sgml-execute-implied (imps type)
  (loop for token in imps do
	(if (eq t token)
	    (sgml-implied-end-tag type sgml-markup-start sgml-markup-start)
	  (sgml-move-current-state token)
	  (when sgml-throw-on-element-change
	    (throw sgml-throw-on-element-change 'start))
	  (sgml-open-element (sgml-token-eltype token)
			     (and sgml-xml-p
                                  (eq sgml-empty
                                      (sgml-eltype-model (sgml-token-eltype token))))
                             sgml-markup-start sgml-markup-start)
	  (unless (and sgml-current-omittag
		       (sgml-element-stag-optional sgml-current-tree))
	    (sgml-log-warning
	     "%s start-tag implied by %s; not minimizable"
	     (sgml-eltype-name (sgml-token-eltype token))
	     type)))))


(defun sgml-do-move (token type)
  (cond ((eq sgml-any sgml-current-state))
        (t
         (sgml-execute-implied (sgml-list-implications token type) type)
         (unless (eq sgml-any sgml-current-state)
           (sgml-move-current-state token)))))


(defun sgml-pcdata-move ()
  "Modify parser state to reflect parsed data."
  (sgml-do-move sgml-pcdata-token "data character"))

(defsubst sgml-parse-pcdata ()
  (/= 0
      (if sgml-current-shortmap
	  (skip-chars-forward (sgml-shortmap-skipstring sgml-current-shortmap))
	(skip-chars-forward "^<]/&"))))

(defsubst sgml-do-pcdata ()
  ;; Parse pcdata
  (sgml-pcdata-move)
  ;;*** assume sgml-markup-start = point
  ;;*** should perhaps handle &#nn;?
  (forward-char 1)
  (sgml-parse-pcdata)
  (when sgml-data-function
	(funcall sgml-data-function (buffer-substring-no-properties
				     sgml-markup-start
				     (point))))
  (sgml-set-markup-type nil))

(defvar sgml-parser-loop-hook nil)
(defun sgml-parser-loop (extra-cond)
  (let (tem
	(sgml-signal-data-function (function sgml-pcdata-move)))
    (while (and (eq sgml-current-tree sgml-top-tree)
		(or (< (point) sgml-goal) sgml-current-eref)
		(progn (setq sgml-markup-start (point)
			     sgml-markup-type nil)
		       (or (sgml-parse-s)
			   (sgml-parse-markup-declaration 'prolog)
			   (sgml-parse-processing-instruction)))))
    (while (and (or (< (point) sgml-goal) sgml-current-eref)
		(not (if extra-cond (funcall extra-cond))))
      (assert sgml-current-tree)
      (setq sgml-markup-start (point)
	    sgml-markup-type nil)
      (cond
       ((eobp) (sgml-pop-entity))
       ((and (or (eq sgml-current-state sgml-cdata)
		 (eq sgml-current-state sgml-rcdata)))
	(if (or (sgml-parse-delim "ETAGO" gi)
		(sgml-is-enabled-net))
	    (sgml-do-end-tag)
	  (sgml-do-data sgml-current-state)))
       ((and sgml-current-shortmap
	     (or (setq tem (sgml-deref-shortmap sgml-current-shortmap
						(eq (point)
						    sgml-rs-ignore-pos)))
		 ;; Restore position, to consider the delim for S+ or data
		 (progn (goto-char sgml-markup-start)
			nil)))
	(setq sgml-rs-ignore-pos sgml-markup-start) ; don't reconsider RS
	(funcall sgml-shortref-handler tem))
       ((and (not (sgml-current-mixed-p))
	     (sgml-parse-s sgml-current-shortmap)))
       ((or (sgml-parse-delim "ETAGO" gi)
	    (sgml-is-enabled-net))
	(sgml-do-end-tag))
       ((sgml-parse-delim "STAGO" gi)
	(sgml-do-start-tag))
       ((sgml-parse-general-entity-ref))
       ((sgml-parse-markup-declaration nil))
       ((sgml-parse-delim "MS-END")	; end of marked section
	(sgml-set-markup-type 'ms-end))
       ((sgml-parse-processing-instruction))
       ((and sgml-parser-loop-hook
             (run-hook-with-args-until-success 'sgml-parser-loop-hook)))
       (t
	(sgml-do-pcdata))))))

(defun sgml-handle-shortref (name)
  (sgml-set-markup-type 'shortref)
  (sgml-do-entity-ref name))

(defun sgml-do-start-tag ()
  ;; Assume point after STAGO
  (when sgml-throw-on-element-change
    (throw sgml-throw-on-element-change 'start))
  ;; Ugly? Let conref flag mean an empty element tag in XML mode,
  ;; then the node will be marked for special handling.
  (setq sgml-conref-flag nil)
  (let (net-enabled et asl)
    (setq et (if (sgml-is-delim "TAGC")	; empty start-tag
		 (sgml-do-empty-start-tag)
	       (sgml-lookup-eltype (sgml-check-name))))
    (unless (sgml-parse-delim "TAGC")	; optimize common case
      (setq asl (sgml-parse-attribute-specification-list et))
      (or
       (if (and (not sgml-xml-p) (sgml-parse-delim "NET"))
	   (prog1 (setq net-enabled t)
	     (or sgml-current-shorttag
		 (sgml-log-warning
		  "NET enabling start-tag is not allowed with SHORTTAG NO"))))
       (if (and sgml-xml-p (sgml-parse-delim "XML-TAGCE"))
           (setq sgml-conref-flag t))
       (sgml-check-tag-close)))
    (sgml-set-markup-type 'start-tag)
    (cond ((and sgml-ignore-undefined-elements
		(not (sgml-eltype-defined et)))
	   (when sgml-warn-about-undefined-elements
	     (sgml-log-warning
	      "Start-tag of undefined element %s; ignored"
	      (sgml-eltype-name et))))
	  (t
	   (sgml-do-move (sgml-eltype-token et)
			 (format "%s start-tag" (sgml-eltype-name et)))
	   (sgml-open-element et sgml-conref-flag
			      sgml-markup-start (point) asl net-enabled)))))


(defun sgml-do-empty-start-tag ()
  "Return eltype to use if empty start tag."
  (cond
   ;; Document element if no element is open
   ((eq sgml-current-tree sgml-top-tree)
    (sgml-lookup-eltype
     (sgml-dtd-doctype sgml-dtd-info)))
   ;; If omittag use current open element
   (sgml-current-omittag
    (sgml-tree-eltype sgml-current-tree))
   ;; Find the eltype of the last closed element.
   ;; If element has a left sibling then use that
   (sgml-previous-tree
    (sgml-tree-eltype sgml-previous-tree))
   ;; No sibling, last closed must be found in enclosing element
   (t
    (loop named outer
	  for current = sgml-current-tree then (sgml-tree-parent current)
	  for parent  = (sgml-tree-parent current)
	  do;; Search for a parent with a child before current
	  (when (eq parent sgml-top-tree)
		(sgml-error "No previously closed element"))
	  (unless (eq current (sgml-tree-content parent))
	    ;; Search content of u for element before current
	    (loop for c = (sgml-tree-content parent) then (sgml-tree-next c)
		  do (when (eq current (sgml-tree-next c))
		       (return-from outer (sgml-tree-eltype c)))))))))


(defun sgml-do-end-tag ()
  "Assume point after </ or at / in a NET."
  (let ((gi "Null")			; Name of element to end or "NET"
	et				; Element type of end tag
	(found				; Set to true when found element to end
	 t))
    (cond ((sgml-parse-delim "TAGC")	; empty end-tag
	   (setq et (sgml-tree-eltype sgml-current-tree)))
	  ((sgml-parse-delim "NET"))
	  (t
	   (setq et (sgml-lookup-eltype (sgml-check-name)))
	   (sgml-parse-s)
	   (sgml-check-tag-close)))
    (sgml-set-markup-type 'end-tag)	; This will create the overlay for
					; the end-tag before the element
					; is closed
    (when et
      (setq gi (sgml-eltype-name et))
      (setq found			; check if there is an open element
					; with the right eltype
	    (loop for u = sgml-current-tree then (sgml-tree-parent u)
		  while u
		  thereis (eq et (sgml-tree-eltype u))))
      (unless found
	(sgml-log-warning
	 "End-tag %s does not end any open element; ignored"
	 gi)))
    (when found
      (setq found nil)
      (while (not found)		; Loop until correct element to
					; end is found
	(unless (sgml-final-p sgml-current-state)
	  (sgml-log-warning
	   "%s element can't end here, need one of %s; %s end-tag out of context"
	   (sgml-element-gi sgml-current-tree)
	   (sgml-required-tokens sgml-current-state)
	   gi))
	(when (eq sgml-current-tree sgml-top-tree)
	  (sgml-error "%s end-tag ended document and parse" gi))
	(setq found
	      (or (eq et (sgml-tree-eltype sgml-current-tree))
		  (and (null et)	; Null end-tag
		       (eq t (sgml-tree-net-enabled sgml-current-tree)))))
	(unless found
	  (sgml-implied-end-tag (format "%s end-tag" gi)
				sgml-markup-start sgml-markup-start)))
      (sgml-close-element sgml-markup-start (point)))))

(defun sgml-is-goal-after-start (goal tree)
  (and tree
       (if (sgml-bpos-p (sgml-tree-stag-epos tree))
	   (> goal (sgml-tree-stag-epos tree))
	 (>= goal (sgml-epos-after (sgml-tree-stag-epos tree))))))

(defun sgml-goto-start-point (goal)
  (let ((u sgml-top-tree))
    (while
	(cond
	 ((sgml-is-goal-after-start goal (sgml-tree-next u))
	  (setq u (sgml-tree-next u)))
	 ((and (sgml-tree-etag-epos u)
	       (if (> (sgml-tree-etag-len u) 0) ; if threre is an end-tag
		   (>= goal (sgml-tree-end u))  ; precisely after is after
		 (> goal (sgml-tree-end u))))   ; else it could possibly
					; become part of the element
	  (sgml-set-parse-state u 'after)
	  nil)
	 ((sgml-is-goal-after-start goal (sgml-tree-content u))
	  (setq u (sgml-tree-content u)))
	 (t
	  (sgml-set-parse-state u 'start)
	  nil)))))


(defun sgml-check-tag-close ()
  (or
   (sgml-parse-delim "TAGC")
   (if (or (sgml-is-delim "STAGO" gi)
	   (sgml-is-delim "ETAGO" gi))
       (or sgml-current-shorttag
	   (sgml-log-warning
	    "Unclosed tag is not allowed with SHORTTAG NO")
	   t))
   (sgml-log-warning "Invalid character in markup %c"
		     (following-char))))

(defun sgml-implied-end-tag (type start end)
  (cond ((eq sgml-current-tree sgml-top-tree)
	 (unless (= start (point-max))
	   (sgml-error
	    "Document ended by %s" type)))
	((not
	  (and sgml-current-omittag
	       (sgml-element-etag-optional sgml-current-tree)))
	 (sgml-log-warning
	  "%s end-tag implied by %s; not minimizable"
	  (sgml-element-gi sgml-current-tree)
	  type)))
  (sgml-close-element start end))


;;;; Parsing tasks and extending the element view of the parse tree

(defun sgml-find-context-of (pos)
  "Find the parser context for POS, return the parse tree.
Also sets `sgml-current-tree' and `sgml-current-state'.  If POS is in
markup, `sgml-markup-type' will be a symbol identifying the markup
type.  It will be nil otherwise."
  (save-excursion
    (sgml-parse-to pos)
    (cond ((and (> (point) pos)
		sgml-markup-type)
	   ;;(setq sgml-current-state sgml-markup-type)
	   (cond ((memq sgml-markup-type '(start-tag end-tag))
		  (setq sgml-current-tree sgml-markup-tree))))
	  (t
	   (setq sgml-markup-type nil)))
    sgml-current-tree))

(defun sgml-parse-to-here ()
  "Find context of point.
See documentation of `sgml-find-context-of'."
  (sgml-find-context-of (point)))

(defun sgml-find-element-of (pos)
  "Find the element containing character at POS."
  (when (eq pos (point-max))
    (error "End of buffer"))
  (save-excursion
    (sgml-parse-to (1+ pos))		; Ensures that the element is
					; in the tree.
    ;;  Find p in u:
    ;;  assert p >= start(u)
    ;;  if next(u) and p >= start(next(u)): find p in next(u)
    ;;  else if end(u) and p >= end(u): in parent(u) unless u is top
    ;;  else if content:
    ;;    if p < start(content(u)): in u
    ;;    else find p in content(u)
    ;;  else: in u
    (let ((u sgml-top-tree))
      (while				; pos >= start(u)
	  (cond ((and (sgml-tree-next u)
		      (>= pos (sgml-element-start (sgml-tree-next u))))
		 (setq u (sgml-tree-next u))) ; continue searching next node
		((and (sgml-tree-etag-epos u)
		      (>= pos (sgml-tree-end u)))
		 (setq u (sgml-tree-parent u)) ; must be parent node
		 nil)
		((and (sgml-tree-content u)
		      (>= pos (sgml-element-start (sgml-tree-content u))))
		 (setq u (sgml-tree-content u))))) ; search content
      u)))

(defun sgml-find-previous-element (pos &optional in-element)
  "Find the element before POS and return it, error if non found.
If in IN-ELEMENT is given look for previous element in IN-ELEMENT else
look in current element.  If this element has no content elements but
end at POS, it will be returned as previous element."
  (save-excursion
    ;; Parse to point; now the previous element is in the parse tree
    (sgml-parse-to pos)
    ;; containing element may be given or obtained from parser
    (or in-element (setq in-element sgml-current-tree))
    ;; in-element is the containing element
    (let* ((c				; this is the content of the
					; containing element
	    (sgml-tree-content in-element)))
      (while
	  (cond
	   ((null c)			; If c = Nil: no previous element.
	    ;; But maybe the containing element ends at pos too.
	    (cond ((= pos (sgml-element-end in-element))
		   (setq c in-element))) ; Previous is parent!
	    nil)
	   ((<= pos (sgml-element-start c)) ; Pos before first content el
	    (setq c nil))		; No, previous element.
	   ((null (sgml-tree-next c)) nil) ; No next, c must be the prev el
	   ((>= (sgml-element-start (sgml-tree-next c)) pos)
	    nil)
	   (t
	    (setq c (sgml-tree-next c)))))
      (or c
	  (error "No previous element in %s element"
		 (sgml-element-gi in-element))))))

(defun sgml-find-element-after (pos &optional in-element)
  "Find the first element starting after POS.
Returns parse tree; error if no element after POS."
  (setq in-element (or in-element
		       (save-excursion (sgml-find-context-of pos))))
  (or
   ;; First try to find element after POS in IN-ELEMENT/current element
   (let ((c				; content of in-element
	  (sgml-element-content in-element)))
     (while (and c
		 (> pos (sgml-element-start c)))
       (setq c (sgml-element-next c)))
     c)
   ;; If there is no more elements IN-ELEMENT/current element try
   ;; to identify the element containing the character after POS.
   ;; If this element starts at POS, use it for element after POS.
   (let ((el (sgml-find-element-of pos)))
     (if (and el (= pos (sgml-element-start el)))
	 el))
   (progn
     (sgml-message "")			; force display of log buffer
     (error "No more elements in %s element"
	    (sgml-element-gi in-element)))))

(defun sgml-element-content (element)
  "First element in content of ELEMENT, or nil."
  (when (null (or (sgml-tree-content element)
		  (sgml-tree-etag-epos element)))
    (save-excursion (sgml-parse-until-end-of t)))
  (sgml-tree-content element))

(defun sgml-element-next (element)
  "Next sibling of ELEMENT."
  (unless (sgml-tree-etag-epos element)
    (save-excursion (sgml-parse-until-end-of element)))
  (unless (or (sgml-tree-next element)
	      (sgml-tree-etag-epos (sgml-tree-parent element)))
    (save-excursion (sgml-parse-until-end-of t)))
  (sgml-tree-next element))

(defun sgml-element-etag-start (element)
  "Last position in content of ELEMENT and start of end-tag, if any."
  (unless (sgml-tree-etag-epos element)
    (save-excursion
      (sgml-parse-until-end-of element)))
  (unless (sgml-tree-etag-epos element)
    (sgml-debug "Failed to define end of element %s"
                (sgml-element-gi element)))
  (assert (sgml-tree-etag-epos element))
  (sgml-epos-promote (sgml-tree-etag-epos element)))

(defun sgml-element-end (element)
  "First position after ELEMENT."
  (sgml-element-etag-start element)	; make end be defined
  (sgml-tree-end element))

(defun sgml-read-element-name (prompt)
  (sgml-parse-to-here)
  (cond (sgml-markup-type
	 (error "No elements allowed in markup"))
        ((or (eq sgml-current-state sgml-any)
	     sgml-dtd-less)
         (sgml-read-element-type prompt sgml-dtd-info))
	((and;;sgml-buffer-eltype-map
          (not (eq sgml-current-state sgml-any)))
	 (let ((tab
		(mapcar (function
                         (lambda (x)
                           (cons (sgml-general-insert-case (symbol-name x))
                                 nil)))
			(sgml-current-list-of-valid-eltypes))))
	   (cond ((null tab)
		  (error "No element valid at this point"))
		 (t
                  (let ((completion-ignore-case sgml-namecase-general))
                    (completing-read prompt tab nil t
                                     (and (null (cdr tab)) (caar tab))))))))
	(t
	 (read-from-minibuffer prompt))))

(defun sgml-element-attribute-specification-list (element)
  "Return the attribute specification list for ELEMENT.
This is a list of (attname value) lists."
;;;  (if (> (sgml-element-stag-len element) 2)
;;;      (save-excursion
;;;	(sgml-with-parser-syntax
;;;	 (sgml-goto-epos (sgml-element-stag-epos element))
;;;	 (sgml-check-delim "STAGO")
;;;	 (sgml-check-name)
;;;	 (prog1 (sgml-parse-attribute-specification-list
;;;		 (sgml-element-eltype element))
;;;	   (sgml-pop-all-entities)))))
  (sgml-tree-asl element))

(defun sgml-find-attribute-element ()
  "Return the element to which an attribute editing command should be applied."
  (let ((el (sgml-find-element-of (point))))
    (save-excursion
      (sgml-parse-to (point))
      ;; If after a start-tag of an empty element return that element
      ;; instead of current element
      (if (eq sgml-markup-type 'start-tag)
	  sgml-markup-tree		; the element of the start-tag
	el))))


(defun sgml-element-attval (element attribute)
  "Return the value of the ATTRIBUTE in ELEMENT, string or nil."
  (let ((asl (sgml-element-attribute-specification-list element))
	(def (sgml-attdecl-default-value
	      (sgml-lookup-attdecl attribute (sgml-element-attlist element)))))
    (or (sgml-attspec-attval (sgml-lookup-attspec attribute asl))
	(sgml-default-value-attval def))))


(defun sgml-cohere-name (x)
  "Convert X into a string where X can be a string, a symbol or an element."
  (cond ((stringp x) x)
	((symbolp x) (symbol-name x))
	(t (sgml-element-gi x))))

(defun sgml-start-tag-of (element)
  "Return the start-tag for ELEMENT."
  (if (and sgml-xml-p (sgml-check-empty (sgml-cohere-name element)))
      (format "<%s/>" (sgml-general-insert-case (sgml-cohere-name element)))
    (format "<%s>" (sgml-general-insert-case (sgml-cohere-name element)))))

(defun sgml-end-tag-of (element)
  "Return the end-tag for ELEMENT (token or element)."
  (format "</%s>" (sgml-general-insert-case (sgml-cohere-name element))))

(defun sgml-top-element ()
  "Return the document element."
  (sgml-element-content (sgml-find-context-of (point-min))))

(defun sgml-off-top-p (element)
  "True if ELEMENT is the pseudo element above the document element."
  (null (sgml-tree-parent element)))

(defun sgml-safe-context-of (pos)
  (let ((sgml-throw-on-error 'parse-error))
    (catch sgml-throw-on-error
      (sgml-find-context-of pos))))

(defun sgml-safe-element-at (pos)
  (let ((sgml-throw-on-error 'parse-error))
    (catch sgml-throw-on-error
      (if (= pos (point-max))
	  (sgml-find-context-of pos)
	(sgml-find-element-of pos)))))

(defun sgml-in-prolog-p ()
  (let ((el (sgml-safe-context-of (point))))
    (or (null el)
	(sgml-off-top-p el))))


;;;; Provide

(provide 'psgml-parse)

;; Local variables:
;; byte-compile-warnings:(free-vars unresolved callargs redefine)
;; End:
;;; psgml-parse.el ends here
