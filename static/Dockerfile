FROM haskell:9

# Install essential tools and build dependencies
RUN apt-get update && \
    apt-get install -y \
        pkg-config && \
    rm -rf /var/lib/apt/lists/*

# Create and switch to blogger user
RUN useradd -m -s /bin/bash blogger
USER blogger

# Set working directory to user home
WORKDIR /home/blogger/

CMD ["/bin/bash"]
