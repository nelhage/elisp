;;; -*- Mode:Emacs-Lisp -*-

;;; The 213 area code (in LA) has been split into 213 and 310.  This code
;;; will map over your Insidious Big Brother Database and convert the area
;;; codes when approriate.  It then displays the records which it has changed.
;;;
;;; This is derived from a list posted by Paul Eggert <eggert@twinsun.com>
;;; on 3 Mar 92 18:24:04 GMT.

(require 'bbdb)

(defconst bbdb-310-exchanges
  '(201 202 203 204 205 206 207 208 209 210 212 214 215 216 217 218 219 220
    246 247 270 271 273 274 275 276 277 278 279 280 281 282 284 285 286 287
    288 289 297 301 302 305 306 312 313 314 315 316 317 318 319 320 322 323
    324 325 326 327 328 329 330 331 332 333 334 335 336 337 338 348 350 352
    354 355 363 364 370 371 372 373 374 375 376 377 378 379 390 391 392 393
    394 395 396 397 398 399 401 402 403 404 406 407 408 409 410 412 414 416
    417 419 420 421 422 423 424 425 426 427 428 429 430 431 432 433 434 435
    436 437 438 439 440 441 442 443 444 445 446 447 448 449 450 451 452 453
    454 455 456 457 458 459 470 471 472 473 474 475 476 477 478 479 490 491
    492 493 494 495 496 497 498 499 501 502 510 512 513 514 515 516 517 518
    519 521 524 525 526 527 528 529 530 531 532 533 534 535 536 537 538 539
    540 541 542 543 544 545 546 547 548 549 550 551 552 553 556 557 558 559
    568 570 571 572 573 574 575 576 577 578 590 591 592 593 594 595 596 597
    598 599 601 602 603 604 605 606 607 608 609 615 616 618 630 631 632 633
    634 635 637 638 639 640 641 642 643 644 645 646 647 648 649 652 657 659
    670 671 672 673 674 675 676 677 679 690 691 692 693 694 695 696 697 698
    699 708 715 719 761 762 763 764 767 768 769 781 782 783 784 785 787 788
    791 792 793 794 795 796 797 798 799 800 801 802 803 804 806 807 809 812
    813 814 815 816 819 820 821 822 823 824 825 826 827 828 829 830 831 832
    833 834 835 836 837 838 839 840 841 842 843 854 855 858 859 860 861 862
    863 864 865 866 867 868 869 885 886 898 899 902 903 904 905 906 907 908
    914 915 916 917 918 920 921 922 923 924 925 926 927 928 929 940 941 942
    943 944 945 946 947 948 949 967 970 973 978 980 981 982 983 984 985 986
    987 988 989)
  "Those exchanges which have moved from the 213 area code to the new 310.")

(defun bbdb-convert-213-to-310 ()
  "Convert phone numbers in the BBDB which are in the 213 area code to the
newly-created 310 area code if appropriate."
  (let ((records (bbdb-records))
	phones frobbed change-p)
    (while records
      (setq phones (bbdb-record-phones (car records))
	    change-p nil)
      (while phones
	(if (and (= (bbdb-phone-area (car phones)) 213)
		 (memq (bbdb-phone-exchange (car phones)) bbdb-310-exchanges))
	    (setq change-p (bbdb-phone-set-area (car phones) 310)))
	(setq phones (cdr phones)))
      (if change-p
	  (progn
	    (setq frobbed (cons (car records) frobbed))
	    (bbdb-change-record (car records) nil)))
      (setq records (cdr records)))
    (bbdb-display-records frobbed)))
