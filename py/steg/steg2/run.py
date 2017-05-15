from stegano import lsb
from cryptography.fernet import Fernet

key = Fernet.generate_key()
print(key)

with open('key.txt', 'wb') as f:
    f.write(key)

with open('msg.txt') as f:
    msg = f.read().encode('utf-8')

cipher = Fernet(key)
cipher_txt = cipher.encrypt(msg)

secret = lsb.hide('image.png', cipher_txt)
secret.save('./image2.png')

encrypted_txt = lsb.reveal('./image2.png')
print(encrypted_txt)
unencrypted_txt = cipher.decrypt(encrypted_txt)
print(unencrypted_txt)
