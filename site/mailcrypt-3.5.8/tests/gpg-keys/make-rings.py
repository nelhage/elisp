#! /usr/bin/python

import popen2, commands, re, os, sys

# create the keyrings. There are two: the full rings are used to create the
# test cases, then the stripped-down set are used when running the tests.

# The full ring contains the following keys (both public and secret):
#  (with the given signatures)

# #1: owner1 <user@test>
# #2: owner2 <user@test>
#      signed by owner1 ("careful checking 3")
# #3: other <other@test>
#      signed by owner1 ("casual checking 2")
# #4: unknown <unknown@test>
# #5: trusted <trusted@test>
#      signed by other ("2")
# #6: untrusted <untrusted@test>
#      signed by trusted ("2")

# The stripped ring contains the following subset, with the given trust values:

# owner1 (public+secret) ultimate  u
# owner2 (public+secret) ultimate  u
# other (public only) marginal     m
# trusted (public only) do not trust n
# untrusted (public only) "don't know" q

# as a result, messages signed by these keys should have the following computed
# trust values:
#  owner1 TRUST_ULTIMATE u
#  owner2 TRUST_ULTIMATE u
#  other  TRUST_FULLY f
#  unknown none -
#  trusted TRUST_MARGINAL m
#  untrusted TRUST_NONE -

# messages signed by owner1 or owner2 should be accepted with TRUST_ULTIMATE,
#  because we have secret keys for them
# messages signed by other should have 

homedir = sys.argv[1]

def make_key(name):
    # this requires a lot of random bytes. look for a gpg option to suck
    # on /dev/urandom instead of the blocking /dev/random
    cmd = "gpg --homedir %s --batch --gen-key 2>&1" % homedir
    (stdout,stdin) = popen2.popen2(cmd)
    stdin.write("Key-Type: DSA\n")
    stdin.write("Key-Length: 1024\n")
    stdin.write("Subkey-Type: ELG-E\n")
    stdin.write("Subkey-Length: 768\n")
    stdin.write("Name-Real: %s\n" % name)
    stdin.write("Name-Email: %s@test\n" % name)
    stdin.write("Passphrase: %s\n" % name)
    stdin.write("%commit\n")
    stdin.write("%echo done\n")
    stdin.close()
    while 1:
        out = stdout.readline()
        print out,
        if out == "gpg: done\n": break
def get_keyid(name):
    # now what keyid did it get?
    cmd = "gpg --homedir %s --with-colons --fingerprint %s" % (homedir,name)
    (s,out) = commands.getstatusoutput(cmd)
    assert(s==0)
    r = re.search(r'^fpr:::::::::(\w+):',out, re.M)
    if r == None:
        print "problem, out '%s'" % out
        return None
    id = r.group(1) # long form
    return id

# gpg claims that signing keys in batch mode is not possible. To work around
# it would probably involve a lot of expect-style futzing.
def sign_key(signer, key, checklevel):
    cmd = "gpg --homedir %s --local-user %s --sign-key %s" % \
          (homedir, signer, key)
    print cmd
    print " check level %d" % checklevel

# this can probably be automated by emitting a new copy of trustdb.gpg
def make_trustdb(trust):
    print "updating trustdb"
    cmd = "gpg --homedir %s --import-ownertrust" % homedir
    (stdout,stdin) = popen2.popen2(cmd)
    for id in trust.keys():
        stdin.write("%s:%d:\n" % (id, trust[id]))
    stdin.close()

    
users = ("owner1", "owner2", "other", "unknown", "trusted", "untrusted")
if 1:
    os.mkdir(homedir)
    os.chmod(homedir, 0700)
    for u in users:
        print "creating key for", u
        make_key(u)
os.chmod(os.path.join(homedir,"pubring.gpg"), 0600)
os.chmod(os.path.join(homedir,"secring.gpg"), 0600)
os.chmod(os.path.join(homedir,"trustdb.gpg"), 0600)

# get keyids
id = {}
for u in users:
    id[u] = get_keyid(u)

# now sign them
sign_key("owner1", "owner2", 3)
sign_key("owner1", "other", 2)
sign_key("other", "trusted", 2)
sign_key("trusted", "untrusted", 2)

# and assign trust values
trust = {
    id["owner1"]: 6,    # u
    id["owner2"]: 6,    # u
    id["other"]: 4,     # m
    id["trusted"]: 3,    # n
    id["untrusted"]: 2, # q
    }

make_trustdb(trust)

# update_trustdb
