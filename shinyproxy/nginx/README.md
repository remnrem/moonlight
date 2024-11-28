Generate certificate
```
sudo snap install --classic certbot
sudo ln -s /snap/bin/certbot /usr/bin/certbot
certbot certonly --standalone --debug -d remnrem.net
```
Revew secure certificate for remnrem.net
```
/usr/bin/certbot certificates

systemctl stop nginx
/usr/bin/certbot renew --quiet
systemctl start nginx
```
