#! /usr/bin/python

import GnuPGInterface
import string, re, sys

gnupg = GnuPGInterface.GnuPG()
gnupg.options.extra_args += ['--homedir', 'remailer-keys', '--batch']

# The keyring should hold private keys for all the remailers in the chain,
# and each one should have a passphrase equal to the name of the key.

def passphrase(bigname):
    return re.search(r'(\w+@\w+\.\w+)', bigname).group(1)

def unwind(recipient, message):
    """Unwind a message. Takes two parameters: the To: address of the
    message and the message body itself.

    This function will return a list of steps. Each element is a dict
    describing what happened on that particular step. The decryption and
    remailing are separate steps. The dict keys are as follows:

     recipient: the target of that step, as requested by the previous step
     encrypted: 1 if the 'Encrypted: PGP' flag was set, else non-existent
     anon-to: name in the Anon-To: header, if present
     subject: subject specified in a ## header, if present
     message: plaintext of the message (for the last step)

    Typical steps:
     recipient=rem1, encrypted=1  ->recurse BRANCH2
     recipient=rem1, anon-to=rem2  BRANCH3, recurse
     recipient=rem2, encrypted=1  ->recurse BRANCH2
     recipient=rem2, anon-to=user, subject=test  BRANCH3, recurse
     recipient=user, message=plaintext  : BRANCH1, terminate
 
    It throws an exception if anything about the message is incorrect:
     message doesn't start with ::\nEncrypted: PGP
     message isn't encrypted or is encrypted to the wrong key (not TO)
     decrypted message doesn't start with ::\nAnon-To:
    """
    # the encrypted message is handled by unwind1. It will look like:
    # ::\nEncrypted: PGP\n\n-----BEGIN PGP MESSAGE----\n etc

    # once decrypted, the message should look like:
    # ::\nAnon-To: dest@foo.com\n\n##\nSubject: test subject\n\nmessage body

    if 0:
        print "-----"
        print message
        print "-----"

    step = {}
    
    # see if we should end the recursion now
    if not re.search(r'^rem\d@test.test', recipient):
        # BRANCH 1
        # plaintext message
        step['recipient'] = recipient
        step['message'] = message
        return [step]
    message = string.split(message, "\n")
    if message[0] != '::':
        raise 'bad message', 'first line was not ::'
    if message[2] != '':
        raise 'bad message', 'third line was not blank'
    # see if it's encrypted
    if message[1] == 'Encrypted: PGP':
        # yes, decrypt it and recurse
        # BRANCH2
        # step is: recipient=rem1, encrypted=1
        crypttext = message[3:]
        assert(message[3] == '-----BEGIN PGP MESSAGE-----')
        # decrypt here
        gnupg.passphrase = passphrase(recipient) # passphrase == keyname
        p = gnupg.run(['--decrypt'],
                      create_fhs=['stdin', 'stdout'],
                      )
        p.handles['stdin'].write(string.join(crypttext,"\n"))
        p.handles['stdin'].close()
        plaintext = p.handles['stdout'].read()
        p.handles['stdout'].close()
        p.wait()
        step['recipient'] = recipient
        step['encrypted'] = 1
        return [step] + unwind(recipient, plaintext)

    # BRANCH3
    
    # message is now:
    #  ::\nAnon-To: remN@test.set (or dest@foo.com)\n\nmessage body
    # or
    #  ::\nAnon-To: dest@foo.com\n\n##\nSubject: subject\n\nmessage body
    
    r = re.search(r'^Anon-To: (.*)$', message[1])
    if not r:
        print "Bad Message, no Anon-To"
        print message
        raise 'bad message', "no anon-to"
    step['recipient'] = recipient
    step['anon-to'] = r.group(1)

    message = message[3:]
    # now: "message body", or "##\nSubject: sub\n\nmessage body"
    
    if message[0] == '##':
        # there are ## headers included
        r = re.search(r'^Subject: (.*)$', message[1])
        if not r:
            raise 'bad message', "## but no Subject: header"
        step['subject'] = r.group(1)
        if message[2] != '':
            raise 'bad message', "no blank line after ## headers"
        message = message[3:]
    # now: "message body"
    message = string.join(message, "\n")
    return [step] + unwind(step['anon-to'], message)

def insistEquals(one, two):
    if one != two:
        raise "results don't match expected", "'%s' != '%s'" % (one, two)


def test_chain(firsthop, crypttext, plaintext, recipient, chain, subject=None):
    """Verify that the crypttext message does indeed match the plaintext
    message, sent to a given recipient and encrypted to the given remailer
    chain."""

    # chain is a list of remailers with long names: rem1@test.test, etc

    # build up the list of what we expect to see at each step
    expected_chain = []
    for i in range(len(chain)-1):
        expected_chain.append({'recipient': chain[i],
                               'encrypted': 1,
                               })
        expected_chain.append({'recipient': chain[i],
                               'anon-to': chain[i+1],
                               })
    last = chain[len(chain)-1]
    expected_chain.append({'recipient': last,
                           'encrypted': 1,
                           })
    pentultimate = {'recipient': last,
                    'anon-to': recipient,
                    }
    if subject:
        pentultimate['subject'] = subject
    expected_chain.append(pentultimate)
    expected_chain.append({'recipient': recipient,
                           'message': plaintext,
                           })

    # unwind the messge
    insistEquals(firsthop, chain[0])
    results = unwind(firsthop, crypttext)

    # compare against expectations
    insistEquals(len(expected_chain), len(results))
    for i in range(len(results)):
        #print i, results[i], expected_chain[i]
        insistEquals(results[i], expected_chain[i])
    print "TEST CASE PASSED"
    


def test1():
    m1 = open("m1").read()
    chain = unwind("rem3@test.test", m1)
    for link in chain: print link

def test2():
    m1 = open("m1").read()
    chain = ["rem3", "rem2", "rem1", "rem1"]
    test_chain(firsthop="rem3@test.test", crypttext=m1,
               plaintext="test message\n",
               recipient="warner@lothar.com",
               chain=chain,
               subject="test subject")

def main():
    # argv is: ['recipient', 'chain1,chain2,chain3', 'subject']
    # plaintext is always "test message\n"
    # crypttext arrives on stdin
    recipient = sys.argv[1]
    chain = string.split(sys.argv[2], ',')
    subject = sys.argv[3]
    crypttext = sys.stdin.read()
    test_chain(firsthop=chain[0],
               crypttext=crypttext,
               plaintext="test message\n",
               recipient=recipient,
               chain=chain,
               subject=subject)
    
        
if __name__ == '__main__':
    main()


