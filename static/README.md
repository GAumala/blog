## Development

Use distrbox

```
distrobox create --image docker.io/haskell:9 --aditional-packages "pkg-config" -n hakyll-blog
distrobox enter hakyll-blog
```
3. Inside the container

First-time setup:

```bash
stack setup  # Only needed when creating a new container
stack build  # Only needed if compiled binaries aren't already cached
```

To start the live preview server:

```bash
stack exec site -- watch --host 0.0.0.0
```
**Note:** The `--host 0.0.0.0` parameter is required to make the server 
accessible from your host machine.

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

To deploy static site use `rsync`:

```
stack exec site build
rsync -aP _site/ root@aumala.dev:/var/www/html/
```
