Generate certificate
```
sudo snap install --classic certbot
sudo ln -s /snap/bin/certbot /usr/bin/certbot
certbot certonly --standalone --debug -d remnrem.net
```
Revew webiste certificate
```
/usr/bin/certbot certificates
/usr/bin/certbot renew --quiet
```
