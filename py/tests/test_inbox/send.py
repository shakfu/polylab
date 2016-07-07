import smtplib

sender = 'sa@me.org'
receivers = ['sa@127.0.0.1']

msg = """From: Sandy Norton <sa@me.org>
To: Sue Banks <sa@127.0.0.1>
Subject: A test no less

This is the message that is being sent.
"""

try:
    smtp = smtplib.SMTP('127.0.0.1', 4467)
    smtp.sendmail(sender, receivers, msg)
    print "successfully sent mail."
except smtplib.SMTPException:
    print "Error: unable to send mail"

