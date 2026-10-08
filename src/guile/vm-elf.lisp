; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-GUILE -*-

;;;; A reader of ELF64 images, for Guile's compiled code
;;;;
;;;; Guile writes its bytecode in ELF images: `.go' files, and the
;;;; bytevectors its assembler makes (docs/guile-vm.md).  This reads the
;;;; parts its VM needs (the header, the program and section tables, the
;;;; dynamic section and symbols) from an image in a byte vector.
;;;; Written from the ELF specification (System V ABI, elf(5)).

(in-package "PSEUDOSCHEME-GUILE")

(defstruct (elf (:constructor %make-elf))
  (bytes nil :type (simple-array (unsigned-byte 8) (*)))
  (little-endian t)
  type machine entry flags
  (segments '())			; elf-segment, in table order
  (sections '()))			; elf-section, in table order

(defstruct elf-segment type flags offset vaddr filesz memsz align)

(defstruct elf-section name type flags addr offset size link info addralign entsize)

(defconstant +pt-load+ 1)
(defconstant +pt-dynamic+ 2)
(defconstant +sht-symtab+ 2)
(defconstant +sht-strtab+ 3)
(defconstant +sht-dynamic+ 6)

(defun elf-unsigned (elf offset size)
  "The SIZE-byte unsigned integer at OFFSET, in ELF's byte order."
  (let ((bytes (elf-bytes elf)) (n 0))
    (if (elf-little-endian elf)
	(loop for i from (1- size) downto 0 do (setq n (logior (ash n 8) (aref bytes (+ offset i)))))
	(loop for i below size do (setq n (logior (ash n 8) (aref bytes (+ offset i))))))
    n))

(defun elf-signed (elf offset size)
  (let ((n (elf-unsigned elf offset size)))
    (if (logbitp (1- (* 8 size)) n) (- n (ash 1 (* 8 size))) n)))

(defun elf-error (message &rest args)
  (guile-error (ssym "misc-error") "load-thunk-from-memory" message args))

(defun parse-elf (bytes)
  "ELF's view of BYTES, an ELF64 image: its tables, checked."
  (unless (and (typep bytes '(simple-array (unsigned-byte 8) (*)))
	       (>= (length bytes) 64)
	       (= (aref bytes 0) #x7f) (= (aref bytes 1) 69) (= (aref bytes 2) 76) (= (aref bytes 3) 70))
    (elf-error "not an ELF file"))
  (unless (= (aref bytes 4) 2) (elf-error "ELF file does not have native word size"))
  (unless (member (aref bytes 5) '(1 2)) (elf-error "ELF file has a bad byte order"))
  (let ((elf (%make-elf :bytes bytes :little-endian (= (aref bytes 5) 1))))
    (flet ((u (offset size) (elf-unsigned elf offset size)))
      (setf (elf-type elf) (u 16 2) (elf-machine elf) (u 18 2) (elf-entry elf) (u 24 8)
	    (elf-flags elf) (u 48 4))
      (let ((phoff (u 32 8)) (shoff (u 40 8))
	    (phentsize (u 54 2)) (phnum (u 56 2))
	    (shentsize (u 58 2)) (shnum (u 60 2)) (shstrndx (u 62 2)))
	(setf (elf-segments elf)
	      (loop for i below phnum
		    for at = (+ phoff (* i phentsize))
		    collect (make-elf-segment :type (u at 4) :flags (u (+ at 4) 4) :offset (u (+ at 8) 8)
					      :vaddr (u (+ at 16) 8) :filesz (u (+ at 32) 8)
					      :memsz (u (+ at 40) 8) :align (u (+ at 48) 8))))
	(let ((sections
		(loop for i below shnum
		      for at = (+ shoff (* i shentsize))
		      collect (make-elf-section :name (u at 4) :type (u (+ at 4) 4) :flags (u (+ at 8) 8)
						:addr (u (+ at 16) 8) :offset (u (+ at 24) 8)
						:size (u (+ at 32) 8) :link (u (+ at 40) 4)
						:info (u (+ at 44) 4) :addralign (u (+ at 48) 8)
						:entsize (u (+ at 56) 8)))))
	  ;; names from the section-name string table
	  (when (< shstrndx (length sections))
	    (let ((strtab (nth shstrndx sections)))
	      (dolist (s sections)
		(setf (elf-section-name s) (elf-string elf (elf-section-offset strtab) (elf-section-name s))))))
	  (setf (elf-sections elf) sections))))
    elf))

(defun elf-string (elf table-offset index)
  "The NUL-terminated string at INDEX in the string table at TABLE-OFFSET."
  (let* ((bytes (elf-bytes elf))
	 (start (+ table-offset index))
	 (end (or (position 0 bytes :start start) (length bytes))))
    (sb-ext:octets-to-string bytes :start start :end end :external-format :utf-8)))

(defun elf-section-named (elf name)
  (find name (elf-sections elf) :key #'elf-section-name :test #'equal))

(defun elf-dynamic-entries (elf)
  "The dynamic section's (tag . value) entries, up to DT_NULL."
  (let ((dynamic (or (find +sht-dynamic+ (elf-sections elf) :key #'elf-section-type)
		     (elf-error "ELF file has no dynamic section"))))
    (loop for at from (elf-section-offset dynamic) by 16
	  repeat (floor (elf-section-size dynamic) 16)
	  for tag = (elf-signed elf at 8)
	  until (zerop tag)
	  collect (cons tag (elf-unsigned elf (+ at 8) 8)))))

(defun elf-symbols (elf)
  "The symbol table's (name address size) entries, of functions and
objects with a name."
  (let ((symtab (find +sht-symtab+ (elf-sections elf) :key #'elf-section-type)))
    (when symtab
      (let ((strtab (nth (elf-section-link symtab) (elf-sections elf))))
	(loop for at from (elf-section-offset symtab) by 24
	      repeat (floor (elf-section-size symtab) 24)
	      for name = (elf-unsigned elf at 4)
	      unless (zerop name)
		collect (list (elf-string elf (elf-section-offset strtab) name)
			      (elf-unsigned elf (+ at 8) 8)
			      (elf-unsigned elf (+ at 16) 8)))))))
