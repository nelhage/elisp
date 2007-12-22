;;; -*- Mode:Emacs-Lisp -*-

;;; The San Francisco Bay Area, which was formerly area code 415, has been
;;; split into two area codes: 415 for the west side of the bay, and 510
;;; for the east side.  This code will map over your Insidious Big Brother
;;; Database and convert the area codes when approriate.  It then displays
;;; the records which it has changed.

(require 'bbdb)

(defconst bbdb-415-exchanges
  '(206 219 221 227 239 241 243 244 252 255 257 258 259 266 267 282 285 289
    291 292 296 306 307 312 321 322 323 324 325 326 327 328 329 330 331 332
    333 334 335 336 337 338 340 341 342 343 344 345 346 347 348 349 354 355
    358 359 361 362 363 364 365 366 367 368 369 371 375 377 378 381 382 383
    386 387 388 389 391 392 393 394 395 396 397 398 399 403 404 406 421 424
    431 432 433 434 435 441 442 445 449 453 454 456 457 459 461 464 465 466
    467 468 469 472 473 474 476 477 478 479 485 488 491 492 493 494 495 496
    497 499 502 506 507 508 512 513 541 542 543 544 545 546 550 552 553 554
    556 557 558 561 563 564 565 566 567 570 571 572 573 574 576 578 579 583
    584 585 586 587 588 589 591 592 593 594 595 597 599 604 610 616 617 621
    622 626 627 637 641 647 648 661 662 663 664 665 666 668 669 673 677 681
    688 691 692 694 695 696 697 701 703 705 715 721 723 725 726 728 731 737
    738 739 742 744 747 749 750 751 752 753 755 756 759 761 764 765 768 771
    772 773 774 775 776 777 780 781 788 789 804 806 807 813 821 822 824 826
    851 852 853 854 855 856 857 858 859 861 863 864 868 871 872 873 875 876
    877 878 879 882 883 885 892 894 896 897 898 903 904 905 912 917 921 922
    923 924 925 926 927 928 929 931 940 941 948 949 951 952 953 954 955 956
    957 960 961 962 964 965 966 967 968 969 972 973 974 978 979 981 982 983
    984 985 986 989 991 992 993 994 995 997 998)
  "Those exchanges which are still in the 415 area code.")

(defconst bbdb-510-exchanges
  '(204 208 210 215 222 223 224 226 228 229 231 232 233 234 235 236 237 238
    245 246 248 251 253 254 256 261 262 263 268 271 272 273 275 276 277 278
    283 284 287 293 294 295 297 302 308 313 317 339 351 352 356 357 370 372
    373 374 376 420 422 423 425 426 427 428 429 430 436 437 438 439 443 444
    446 447 448 451 452 455 458 460 462 463 471 475 481 482 483 484 486 487
    489 490 498 515 516 521 522 523 524 525 526 527 528 529 530 531 532 533
    534 535 536 537 538 539 540 547 548 549 551 559 562 568 569 577 581 582
    596 598 601 602 603 606 609 613 614 618 620 623 624 625 631 632 633 634
    635 636 638 639 642 643 644 645 646 649 651 652 653 654 655 656 657 658
    659 667 670 671 672 674 675 676 678 680 682 683 684 685 686 687 689 704
    706 708 709 713 716 717 718 724 727 729 732 733 734 735 736 741 743 745
    746 748 754 757 758 762 763 769 778 779 782 783 784 785 786 787 790 791
    792 793 794 795 796 797 798 799 801 803 810 814 815 819 820 823 825 827
    828 829 830 831 832 833 834 835 836 837 838 839 840 841 842 843 845 846
    847 848 849 862 865 866 867 869 874 881 884 886 887 888 889 891 893 895
    901 910 930 932 933 934 935 937 938 939 942 943 944 945 946 947 975 977
    987)
  "Those exchanges which have moved from the 415 area code to the new 510.")


(defsubst bbdb-convert-415-to-510-internal (phone)
  (cond ((memq (bbdb-phone-exchange phone) bbdb-510-exchanges)
	 (if (= (bbdb-phone-area phone) 510)
	     nil
	   (bbdb-phone-set-area phone 510)))
	((memq (bbdb-phone-exchange phone) bbdb-415-exchanges)
	 (if (= (bbdb-phone-area phone) 415)
	     nil
	   (bbdb-phone-set-area phone 415)))
	(t (error "Exchange %03d is not in the 415 or 510 area codes: %s"
		  (bbdb-phone-exchange phone)
		  (bbdb-phone-string phone)))))


(defun bbdb-convert-415-to-510 ()
  "Convert phone numbers in the BBDB which are in the 415 area code to the
newly-created 510 area code if appropriate."
  (let ((records (bbdb-records))
	phones frobbed change-p)
    (while records
      (setq phones (bbdb-record-phones (car records))
	    change-p nil)
      (while phones
	(if (memq (bbdb-phone-area (car phones)) '(415 510))
	    (setq change-p
		  (or (bbdb-convert-415-to-510-internal (car phones))
		      change-p)))
	(setq phones (cdr phones)))
      (if change-p
	  (progn
	    (setq frobbed (cons (car records) frobbed))
	    (bbdb-change-record (car records) nil)))
      (setq records (cdr records)))
    (bbdb-display-records frobbed)))
