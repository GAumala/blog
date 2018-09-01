limit_req_zone $binary_remote_addr zone=post_like_limit:5m rate=10r/s;
limit_req_zone $binary_remote_addr zone=api_limit:10m rate=100r/s;
proxy_cache_path /tmp/blog_cache keys_zone=blog:5m max_size=10m;
