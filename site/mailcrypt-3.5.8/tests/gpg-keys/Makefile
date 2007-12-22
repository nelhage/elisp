
# The distribution tarball comes with an importable text version of the
# test keyring. For a normal build, this is unpacked, then the stripped
# down keyring (used by the decryption tests to verify behavior when the user
# tries to decrypt a message without possessing the right key) is generated.

# To build the keyrings from source, use the "new-rings" target. This uses
# a few python scripts to create the keyring, but the process requires some
# manual intervention. After the manual steps are done, you should build
# the "exported" target to contruct the 

unpack: unpack-full-rings exported

rings.txt:
	@echo "rings.txt must be manually created"
	@echo "try this:"
	@echo "make new-rings"
	@echo " (it needs manual help)"
	@echo "make exported"
	@echo "make pack"
	@exit 1


unpack-full-rings: rings.txt
	mkdir full-rings
	chmod go-rwsx full-rings
	gpg --homedir full-rings --allow-secret-key-import --fast-import rings.txt
	./fix-trust.py full-rings
	chmod go-r full-rings/*
	chmod u-w full-rings/pubring.gpg full-rings/secring.gpg
	rm -f full-rings/*~

new-rings:
	./make-rings.py full-rings
	@echo "Please complete the manual steps requested by make-rings.py"
	@echo "Then 'make exported' to create the stripped down keyring."
	@echo "'make pack' to build the packed keyring for distribution."

exported:
	gpg --homedir full-rings --check-trustdb
	mkdir exported
	chmod go-rwsx exported
	touch exported/pubring.gpg exported/secring.gpg
	chmod go-rw exported/*
	gpg --homedir exported --check-trustdb
	chmod go-rw exported/trustdb.gpg
	gpg --homedir full-rings --export owner1 owner2 other trusted untrusted | gpg --homedir exported --import
	gpg --homedir full-rings --export-secret-keys owner1 owner2 |gpg --homedir exported --allow-secret-key-import --fast-import
	chmod go-rw exported/*
	./fix-trust.py exported
	rm -f exported/*~
	chmod u-w exported/pubring.gpg exported/secring.gpg

clean:
	rm -rf full-rings exported
update:
	gpg --homedir full-rings --update-trustdb
sigs:
	gpg --homedir full-rings --list-sigs
trust:
	gpg --homedir full-rings --export-ownertrust
dump:
	gpg --homedir full-rings --with-colons --list-keys

pack:
	gpg --homedir full-rings -a --export-all >rings.txt
	gpg --homedir full-rings -a --export-secret-keys >>rings.txt
	rm -rf full-rings exported
