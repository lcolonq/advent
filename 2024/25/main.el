;;; -*- lexical-binding: t; byte-compile-warnings: (not suspicious); -*-
(require 'dash)
(require 'f)
(require 's)
(require 'ht)

(require 'c-c)

(add-to-list 'load-path "~/src/upsidedowncake")
(add-to-list 'load-path "~/src/upsidedowncake/src/")
(setq elisp-flymake-byte-compile-load-path load-path)
(require 'udc)

(defconst adv/inp (f-read-text "input.txt")) ;; let's at least pre-parse the thing :3
(defun adv/to-bits (xs) (if xs (logior (if (= (car xs) ?#) 1 0) (ash (adv/to-bits (cdr xs)) 1)) 0))
(defun adv/is-key (x) (= (logand x 1) 1))
(defconst adv/parsed ;; i really should learn these nice dash.el functions a little more
  (-group-by ;; i tend to fall into my ways, and use what i already know and am comfortable with
   #'adv/is-key ;; that's something i'd like to get better at in 2025
   (--map ;; let's be brave together, computer!
    (adv/to-bits (reverse (seq-into (s-replace "\n" "" it) 'list))) ;; let's howl like ravenous beasts!
    (s-split "\n\n" adv/inp)))) ;; awoooooooooooooooooooooooooooo
(defconst adv/locks (alist-get nil adv/parsed))
(defconst adv/keys (alist-get t adv/parsed))

(defun adv/compatible (x y) (= (logand x y) 0)) ;; well, it's certainly easy from here
(defconst adv/solution ;; but that's cheating...
  (length (-non-nil (-table-flat #'adv/compatible adv/locks adv/keys)))) ;; you do not belong in my world...
 
;;; step into nothing!
(defconst adv/syms (u/gba/make-symtab))
(u/symtab-add-section! adv/syms :header u/gba/rom-start)
(u/symtab-add-section! adv/syms :code (+ u/gba/rom-start #x1000))
(u/symtab-add-section! adv/syms :data (+ u/gba/rom-start #x20000))
(u/symtab-add-section! adv/syms :vars #x02000000)

(u/symtab-add! adv/syms :vars :var-test 'var 4)

(u/symtab-add!
 adv/syms
 :header :header 'const
 (u/gba/header
  (u/gba/make-header :entry :main :title "advent" :code "advc" :maker "lq")))

(u/symtab-add!
 adv/syms
 :code :interrupt-handler 'code
 (u/gba/toplevel
  (u/gba/claim! 'r2)
  (u/gba/emit! '(mov r0 1)) ;; vblank
  (u/gba/set16 adv/syms :reg-if 'r0)
  (u/gba/get16 adv/syms 'r1 :reg-ifbios)
  (u/gba/emit! '(orr r0 r0 r1))
  (u/gba/set16 adv/syms :reg-ifbios 'r0)
  (u/gba/emit! `(bx ,u/gba/lr))))

(u/symtab-add!
 adv/syms
 :code :main 'code
 (u/gba/toplevel
  (u/gba/claim! 'r2 'r3)
  (u/gba/set16 adv/syms :reg-dispcnt #b0000000001000000) ;; 1D object mapping, mode 0
  (u/gba/call adv/syms :enable-interrupts)
  (u/gba/emit! '(b :mainloop))))

(u/symtab-add!
 adv/syms
 :code :enable-interrupts 'code
 (u/gba/function
  (u/gba/set32 adv/syms :reg-intaddr (u/symtab-entry-addr (u/symtab-lookup adv/syms :interrupt-handler)))
  (u/gba/set16 adv/syms :reg-dispstat #b0000000000001000) ;; turn on vblank interrupt
  (u/gba/set16 adv/syms :reg-ie #b0000000000000001) ;; only enable vblank interrupt
  (u/gba/set32 adv/syms :reg-ime 1) ;; enable interrupts
  ))

(u/symtab-add!
 adv/syms
 :code :mainloop 'code
 (u/gba/toplevel
  (u/gba/call adv/syms :update)
  (u/gba/emit! `(swi ,(ash #x05 16))) ;; VBlankIntrWait BIOS function, remember to shift in ARM!
  (u/gba/emit! '(b :mainloop))))

(u/symtab-add!
 adv/syms
 :code :update 'code
 (u/gba/function
  (let ((r (u/gba/fresh!)))
    (u/gba/get32 adv/syms r :var-test)
    (u/gba/emit! `(add ,r ,r 1))
    (u/gba/set32 adv/syms :var-test r)
    )))

(setq
 c/c-gdb-symbols
 (--map
  (cons (format "%s" (car it)) (format "0x%x" (u/symtab-entry-addr (cdr it))))
  (ht->alist (u/symtab-symbols adv/syms))))

(defconst adv/linked (u/gba/link adv/syms u/gba/rom-start #x25000))
(defconst adv/rom (seq-mapcat #'byte-to-string adv/linked 'string))
(f-write-bytes adv/rom "advent.gba")
