## Development

```
stack exec site watch
```


## Deployment

The server block in `/etc/nginx/nginx.conf` should have these lines:

```
gzip on;
gzip_min_length 1000;
gzip_types      application/json application/xml image/svg+xml text/css text/plain;
```

The server block in `/etc/nginx/sites-available/default` should have these lines:

```
root /var/www/html;

location /assets {
        add_header Cache-Control "public, max-age=31536000, immutable";
        try_files $uri $uri/ =404;
}

location / {
    try_files $uri $uri/ =404;
}
```

To deploy static site use `scp`:

```
stack exec site build
scp _site/* root@gaumala.com:/var/www/html/
```
