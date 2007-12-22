# Generated automatically from Makefile.in by configure.
EMACS=emacs
GNUPG=gpg

GPG_KEYS = remailer-keys/pubring.gpg

all: check

$(GPG_KEYS): rings.txt
	mkdir remailer-keys
	gpg --homedir remailer-keys --allow-secret-key-import --fast-import rings.txt
#	./fix-trust.py full-rings
	rm -f remailer-keys/pubring.gpg~

rings.txt:
	@echo "rings.txt must be manually created"
	@exit 1

# check to make sure the necessary python module is available first. A
# missing module will cause an error deep inside the test loop, and stderr
# isn't communicated upwards very well.
have-py-gnupg.stamp:
	python -c 'import GnuPGInterface'
	touch $@

check: $(GPG_KEYS) have-py-gnupg.stamp
	$(EMACS) -q --no-site-file -batch -l test-remailer.el -f run-all-tests

onetest: $(GPG_KEYS) have-py-gnupg.stamp
	$(EMACS) -q --no-site-file -batch -l test-remailer.el -f run-one-test

clean:
	rm -rf remailer-keys

pack:
	gpg --homedir remailer-keys -a --export-all >rings.txt
	gpg --homedir remailer-keys -a --export-secret-keys >>rings.txt
