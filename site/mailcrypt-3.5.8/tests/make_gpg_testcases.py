#! /usr/bin/python

import os, commands, re, time
import GnuPGInterface

# emit test case files into test-cases/*

# keys:
#  1: owner1 <user@test>
#  2: owner2 <user@test>
#  3: other <other@test>
#  4: unknown key

homedir = "gpg-keys/full-rings"
testcasedir = "gpg-testcases"
users = ("owner1", "owner2", "other", "unknown", "trusted", "untrusted")

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

class Encrypt(GnuPGInterface.GnuPG):
    def __init__(self):
        GnuPGInterface.GnuPG.__init__(self)
        self.setup_my_options()

    def setup_my_options(self):
        self.options.armor = 1
        self.options.meta_interactive = 0
        self.options.extra_args.append('--no-secmem-warning')
        self.options.homedir = homedir

    def encrypt_string(self, string, recipients):
        self.options.recipients = recipients   # a list!
        proc = self.run(['--encrypt'], create_fhs=['stdin', 'stdout'])
        proc.handles['stdin'].write(string)
        proc.handles['stdin'].close()
        output = proc.handles['stdout'].read()
        proc.handles['stdout'].close()
        proc.wait()
        return output

    def signencrypt_string(self, string, signer, passphrase, recipients):
        self.options.recipients = recipients   # a list!
        self.options.default_key = signer
        self.passphrase = passphrase
        proc = self.run(['--sign', '--encrypt'],
                        create_fhs=['stdin', 'stdout'])
        proc.handles['stdin'].write(string)
        proc.handles['stdin'].close()
        output = proc.handles['stdout'].read()
        proc.handles['stdout'].close()
        proc.wait()
        return output

    def sign_string(self, string, signer, passphrase):
        self.options.recipients = []
        self.options.default_key = signer
        self.passphrase = passphrase
        proc = self.run(['--sign'],
                        create_fhs=['stdin', 'stdout'])
        proc.handles['stdin'].write(string)
        proc.handles['stdin'].close()
        output = proc.handles['stdout'].read()
        proc.handles['stdout'].close()
        proc.wait()
        return output

    def sym_string(self, string, passphrase):
        self.options.recipients = []
        self.passphrase = passphrase
        proc = self.run(['--symmetric'],
                        create_fhs=['stdin', 'stdout'])
        proc.handles['stdin'].write(string)
        proc.handles['stdin'].close()
        output = proc.handles['stdout'].read()
        proc.handles['stdout'].close()
        proc.wait()
        return output

    def armor_string(self, string):
        self.options.recipients = []
        proc = self.run(['--enarmor'],
                        create_fhs=['stdin', 'stdout'])
        proc.handles['stdin'].write(string)
        proc.handles['stdin'].close()
        output = proc.handles['stdout'].read()
        proc.handles['stdout'].close()
        proc.wait()
        return output

    def clearsign_string(self, string, signer, passphrase):
        self.options.recipients = []
        self.options.default_key = signer
        self.passphrase = passphrase
        proc = self.run(['--clearsign'],
                        create_fhs=['stdin', 'stdout'])
        proc.handles['stdin'].write(string)
        proc.handles['stdin'].close()
        output = proc.handles['stdout'].read()
        proc.handles['stdout'].close()
        proc.wait()
        return output

def emit_alist(f, d):
    f.write("(\n")
    keys = d.keys()
    keys.sort()
    for k in keys:
        if d[k] == None:
            f.write("(%s . nil)\n" % (k))
        else:
            f.write("(%s . \"%s\")\n" % (k, d[k]))
    f.write(")\n")

encryptor = Encrypt()
def encrypt(to, plaintext):
    return encryptor.encrypt_string(plaintext, [to])
def signencrypt(to, signer, plaintext):
    return encryptor.signencrypt_string(plaintext, signer, signer, [to])
def sign(signer, plaintext):
    return encryptor.sign_string(plaintext, signer, signer)
def sym(passphrase, plaintext):
    return encryptor.sym_string(plaintext, passphrase)
def armor(plaintext):
    # note: this isn't very useful, you must use 'gpg --dearmor' to
    # extract the result, not --decrypt. It also has a different banner.
    return encryptor.armor_string(plaintext)
def clearsign(signer, plaintext):
    return encryptor.clearsign_string(plaintext, signer, signer)

#def encrypt(to, plaintext):
    #cmd = "gpg --armor --homedir %s --recipient %s --batch --encrypt" % (homedir, to)
    #(stdout,stdin) = popen2.popen2(cmd)
    #stdin.write(plaintext)
    #stdin.close()
    #crypttext = stdout.read()
    #return crypttext

class TestCase:
    def __init__(self, filename):
        self.filename = filename
        self.plaintext = self.make_plaintext()
        self.d = {}
        self.d['name'] = self.filename
        self.d['plaintext'] = self.plaintext
        self.d['error'] = None
        self.d['signature_status'] = None

    def make_plaintext(self):
        return "This is a plaintext message\n"
    def date_string(self):
        # mc-gpg.el takes the YYYY-MM-DD date string from the SIG_ID
        # status-fs line and delivers it to the user. This appears to be the
        # GMT date of the signature. Extract the same thing here so we can
        # tell the test harness what to expect. This needs to run on the
        # same day as the gpg invocation used to create the signature, but
        # this whole program only takes a few seconds to execute.
        return time.strftime("%Y-%m-%d", time.gmtime()) # GMT

    def encrypted_fields(self, recip):
        # run this after signed_fields so [signature_status] gets cleared
        self.d['encryption_id'] = "0x%s" % id[recip][-16:]
        self.d['passphrase'] = recip
        if recip == "other" or recip == "unknown":
            self.d['error'] = "This message is not addressed to you"
            self.d['plaintext'] = None
            self.d['signature_status'] = None

    def symencrypted_fields(self):
        self.d['encryption_id'] = "***** CONVENTIONAL *****"
        self.d['passphrase'] = self.passphrase

    def signed_fields(self, signer):
        self.d['signature_status'] = ("Good signature from '%s <%s@test>' " + \
                                 "TRUST_%s made %s") % \
                                 (signer, signer,
                                  trustmap[signer], self.date_string())
        if signer == "unknown":
            self.d['signature_status'] = "cannot check signature " + \
                                         "from keyid %s" % \
                                         id[signer][-16:]
            # comes from 'cannot check signature' warning
    def process(self, testcasedir):
        self.make_crypttext()
        f = open(os.path.join(testcasedir, self.filename), "w")
        emit_alist(f, self.d)
        f.close()
        

class E_Case(TestCase):
    def __init__(self, filename, recip):
        TestCase.__init__(self, filename)
        self.recip = recip
        self.encrypted_fields(recip)
    def make_crypttext(self):
        self.d['crypttext'] = "\n" + encrypt(self.recip, self.plaintext)
        
class ES_Case(TestCase):
    def __init__(self, filename, recip, signer):
        TestCase.__init__(self, filename)
        self.recip = recip
        self.signer = signer
        self.signed_fields(signer)
        self.encrypted_fields(recip)
    def make_crypttext(self):
        self.d['crypttext'] = "\n" + signencrypt(self.recip,
                                                 self.signer,
                                                 self.plaintext)

class S_Case(TestCase):
    def __init__(self, filename, signer):
        TestCase.__init__(self, filename)
        self.signer = signer
        self.signed_fields(signer)
    def make_crypttext(self):
        self.d['crypttext'] = "\n" + sign(self.signer, self.plaintext)

class CS_Case(TestCase):
    def __init__(self, filename, signer):
        TestCase.__init__(self, filename)
        self.signer = signer
        self.signed_fields(signer)
    def make_crypttext(self):
        self.d['crypttext'] = "\n" + clearsign(self.signer, self.plaintext)

class SYM_Case(TestCase):
    def __init__(self, filename, passphrase):
        TestCase.__init__(self, filename)
        self.passphrase = passphrase
        self.symencrypted_fields()
    def make_crypttext(self):
        self.d['crypttext'] = "\n" + sym(self.passphrase, self.plaintext)

def make_cases():
    d = testcasedir
    E_Case("E.e1r", "owner1").process(d)
    E_Case("E.e2r", "owner2").process(d)
    E_Case("E.e3", "other").process(d)
    E_Case("E.e4", "unknown").process(d)
    ES_Case("ES.e1r.s1v", "owner1", "owner1").process(d)
    ES_Case("ES.e1r.s2v", "owner1", "trusted").process(d)
    ES_Case("ES.e1r.s3v", "owner1", "untrusted").process(d)
    ES_Case("ES.e1r.s4", "owner1", "unknown").process(d)
    ES_Case("ES.e3.s1v", "other", "owner1").process(d)
    ES_Case("ES.e4.s1v", "unknown", "owner1").process(d)
    S_Case("S.s1v", "owner1").process(d)
    S_Case("S.s2v", "trusted").process(d)
    S_Case("S.s3v", "untrusted").process(d)
    S_Case("S.s4", "unknown").process(d)
    SYM_Case("SE", "password").process(d)
    CS_Case("CS.s1v", "owner1").process(d)
    CS_Case("CS.s2v", "trusted").process(d)
    CS_Case("CS.s3v", "untrusted").process(d)
    CS_Case("CS.s4", "unknown").process(d)

# get keyids
id = {}
for u in users:
    id[u] = get_keyid(u)

trustmap = {
    'owner1': "ULTIMATE",
    'owner2': "ULTIMATE",
    'other': "FULL",
    'unknown': "NONE",
    'trusted': "MARGINAL",
    'untrusted': "UNDEFINED",
    }
    
if __name__ == "__main__":
    if not os.path.isdir(testcasedir):
        os.mkdir(testcasedir)
    make_cases()
    

# todo:
#  hardcoded date in es()
#  cs(): corrupt the signatures by editing the crypttext after signing
