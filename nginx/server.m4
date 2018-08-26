root SITE_PATH;

location /blogapi/like {
        limit_req zone=post_like_limit burst=5 nodelay;
        
        rewrite ^/blogapi/like/(.*)$ /like/$1 break;

        proxy_set_header  X-Real-IP $remote_addr;
        proxy_pass http://localhost:API_PORT;
}

location /blogapi/likes {
        limit_req zone=api_limit burst=5 nodelay;

        rewrite ^/blogapi/likes/(.*)$ /likes/$1 break;

        proxy_pass http://localhost:API_PORT;
        proxy_cache blog;
        proxy_cache_valid 200 10s;
}

location / {
        proxy_cache_valid 200 302 10m;
        proxy_cache_valid 404      1m;
		    try_files $uri $uri/ =404;
}
