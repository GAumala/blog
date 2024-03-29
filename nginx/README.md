# M4 templates for Nginx configuration

These templates generate Nginx configuration files. These are meant to be 
included in the main config file because my server hosts more apps than just my
blog.

The generated `server.conf` is meant to be included in a `server` block and the
generated `http.conf` is meant to be included in an `http` block.

It is assumed that the main configuration file already has SSL set up with 
Let's Encrypt.

It is also assumed that the main configuration file already has enabled gzip 
compression. This is the recommended gzip configuration:

```
gzip on;
gzip_min_length 1000;
gzip_types      application/json application/xml image/svg+xml text/css text/plain;
```

To build the actual config files, run the following script in the server:

``` bash
port="<PORT NUMBER>"
site_path="<PATH TO STATIC PAGES>"
ci_secret="<A RANDOM STRING>"
m4 -DAPI_PORT=$port -DSITE_PATH=$site_path ./server.m4 > ./server.conf
m4 ./http.m4 > ./http.conf
```
