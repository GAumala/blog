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

location = /ci/CI_SECRET {
        rewrite ^ /ci-hook break;

        proxy_pass http://localhost:API_PORT;
}

location /assets {
        add_header Cache-Control "public, max-age=31536000, immutable";
        try_files $uri =404;
}

location / {
    try_files $uri =404;
}
