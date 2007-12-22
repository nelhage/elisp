@echo off

rem Written by Yair Friedman (yair@MailAndNews.com)
rem Based upon the gnus make.bat by David Charlap (shamino@writeme.com)
rem
rem There are two possible problems with this batch file.  The emacs.bat batch
rem file may not exist in all distributions.  It is part of the GNU build of
rem Emacs 20.4 (http://www.gnu.org/softare/emacs/windows.ntemacs.html)  If you
rem install BBDB with some other build, you may have to replace calls to
rem %1\emacs.bat with something else.
rem 
rem Also, the emacs.bat file that comes with Emacs does not accept more than 9
rem parameters, so the attempts to compile the .texi files will fail.  To
rem fix that (at least on NT.  I don't know about Win95), the following
rem change should be made to emacs.bat:
rem 
rem     %emacs_dir%\bin\emacs.exe %1 %2 %3 %4 %5 %6 %7 %8 %9
rem 
rem should become
rem 
rem     %emacs_dir%\bin\emacs.exe %*
rem 
rem which will allow the batch file to accept an unlimited number of
rem parameters.

rem For the meaning of these look at the Makefile.
rem Notice that you have to double any backslashes in the path.
set GNUSDIR=E:\\emacs-20.7\\lisp\\gnus
set MHEDIR=
set VMDIR=
set OTHERDIR=
rem give it any value if you want to use rmail with bbdb
set RMAIL=
rem This is where you bbdb lisp is going
set BBDBDIR=D:\\emacs-20.7\\site-lisp\\bbdb-2.2\\lisp

rem Clear PWD so emacs doesn't get confused
set BBDB_PWD_SAVE=%PWD%
set PWD=

if "%1" == "" goto usage

rem Emacs 20.7 no longer includes emacs.bat. Use emacs.exe if the batch file is
rem not present -- this also fixes the problem about too many parameters on Win9x.
set emacs=emacs.exe
if exist %1\bin\emacs.bat set emacs=emacs.bat

set VM=-eval "(progn (if (not (string-match \"%VMDIR%\" \"\")) (setq load-path (cons \"%VMDIR%\" load-path))) (if (load \"vm-version\" t) (cond ((> (string-to-number vm-version) 5.31) (load \"vm\")) (t (load \"vm-vars\") (load \"vm\")))))"
set GNUS=-eval "(if (not (string-match \"%GNUSDIR%\" \"\")) (setq load-path (cons \"%GNUSDIR%\" load-path)))"
set MHE=-eval "(progn (if (not (string-match \"%MHEDIR%\" \"\")) (setq load-path (cons \"%MHEDIR%\" load-path))) (load \"mh-e\"))"
set PUSHPATH=-eval "(setq load-path (delete \"\" (append (list \".\" \"%OTHERDIR%\") load-path)))"

cd lisp

call %1\bin\%emacs% -batch -q -no-site-file -f batch-byte-compile ./bbdb.el
call %1\bin\%emacs% -batch -q -no-site-file %PUSHPATH% -l ./bbdb.elc -f batch-byte-compile bbdb-com.el bbdb-hooks.el bbdb-print.el bbdb-ftp.el bbdb-whois.el bbdb-srv.el bbdb-reportmail.el bbdb-snarf.el bbdb-w3.el bbdb-sc.el bbdb-merge.el bbdb-migrate.el

IF "%RMAIL%" == "" goto afterrmail
call %1\bin\%emacs% -batch -q -no-site-file %PUSHPATH% -l ./bbdb.elc -f batch-byte-compile bbdb-rmail.el
:afterrmail

IF "%GNUSDIR%" == "" goto aftergnus
call %1\bin\%emacs% -batch -q -no-site-file %PUSHPATH% -l ./bbdb.elc %GNUS% -f batch-byte-compile bbdb-gnus.el
:aftergnus

IF "%VMDIR%"=="" goto aftervm
call %1\bin\%emacs% -batch -q -no-site-file %PUSHPATH% -l ./bbdb.elc %VM% -f batch-byte-compile bbdb-vm.el
:aftervm

IF "%MHEDIR%"=="" goto aftermhe
call %1\bin\%emacs% -batch -q -no-site-file %PUSHPATH% -l ./bbdb.elc %MHE% -f batch-byte-compile bbdb-mhe.el
:aftermhe

echo home is where the heart is.

rem echo >bbdb-autoloads.el
rem call %1\bin\%emacs% -batch -q -no-site-file -l autoload -eval "(setq generated-autoload-file \"%BBDBDIR%\\bbdb-autoloads.el\")" -eval "(setq make-backup-files nil)" -eval "(setq autoload-package-name \"bbdb\")" -f batch-update-autoloads %BBDBDIR%
call %1\bin\%emacs% -batch -q -no-site-file -f batch-byte-compile bbdb-autoloads.el

if not "%2%"=="copy" goto info
attrib -r %1\lisp\bbdb\*
copy *.el* %1\lisp\bbdb

:info
set EMACSINFOHACK="(while (re-search-forward \"@\\(end \\)?ifnottex\" nil t) (replace-match \"\"))"
cd ..\texinfo
call %1\bin\%emacs% -batch -q -no-site-file bbdb.texinfo -eval %EMACSINFOHACK% -eval "(setq max-lisp-eval-depth 600)" -f texinfo-every-node-update -f texinfo-format-buffer -f save-buffer
if not "%2" == "copy" goto done
copy bbdb.info %1\info

:done
cd ..
goto end

:usage
echo Usage: make :emacs-dir: [copy]
echo.
echo where: :emacs-dir: is the directory you installed emacs in
echo                    eg. d:\emacs\20.4
echo        copy indicates that the compiled files should be copied to your
echo             emacs lisp, info, and etc directories

rem Restore PWD so whoever called this batch file doesn't get confused
set PWD=%BBDB_PWD_SAVE%
set BBDB_PWD_SAVE=
:end
