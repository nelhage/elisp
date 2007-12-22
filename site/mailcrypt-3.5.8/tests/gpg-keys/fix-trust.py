#! /usr/bin/python

import popen2, commands, re, os, sys

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

homedir = sys.argv[1]

def get_keyid(name):
    # now what keyid did it get?
    cmd = "gpg --homedir %s --with-colons --fingerprint %s" % (homedir,name)
    (s,out) = commands.getstatusoutput(cmd)
    if s != 0:
        return None
    r = re.search(r'^fpr:::::::::(\w+):',out, re.M)
    if r == None:
        print "problem, out '%s'" % out
        return None
    id = r.group(1) # long form
    return id

def make_trustdb(trust):
    print "updating trustdb"
    cmd = "gpg --homedir %s --import-ownertrust" % homedir
    (stdout,stdin) = popen2.popen2(cmd)
    for id in trust.keys():
        stdin.write("%s:%d:\n" % (id, trust[id]))
    stdin.close()

    
users = ("owner1", "owner2", "other", "trusted", "untrusted", "unknown")

# get keyids
id = {}
for u in users:
    id[u] = get_keyid(u)

# and assign trust values
trust = {
    id["owner1"]: 6,    # u
    id["owner2"]: 6,    # u
    id["other"]: 4,     # m
    id["trusted"]: 3,    # n
    id["untrusted"]: 2, # q
    }
if id["unknown"]:
    trust[id["unknown"]] = 6 # u
    
make_trustdb(trust)
os.system("gpg --homedir %s --check-sigs >/dev/null" % homedir)
os.system("gpg --homedir %s --check-trustdb" % homedir)
