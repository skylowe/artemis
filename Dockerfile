# ARTEMIS JupyterHub Single-User Container
# Based on rocker/r-ver matching renv.lock R version
FROM rocker/r-ver:4.5.0

LABEL maintainer="skylowe"
LABEL description="ARTEMIS OASDI Projection Model - JupyterHub container"

# ============================================================================
# 1. System dependencies for R packages
# ============================================================================
RUN apt-get update && apt-get install -y --no-install-recommends \
    # Build tools
    build-essential \
    cmake \
    # R package system deps
    libcurl4-openssl-dev \
    libxml2-dev \
    libssl-dev \
    zlib1g-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libjpeg-dev \
    libtiff-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libgit2-dev \
    libglpk-dev \
    # Python
    python3 \
    python3-pip \
    python3-venv \
    python3-dev \
    # Misc
    pandoc \
    curl \
    && rm -rf /var/lib/apt/lists/*

# ============================================================================
# 2. Python / JupyterHub singleuser + jupyter-server-proxy
# ============================================================================
RUN python3 -m pip install --no-cache-dir --break-system-packages \
    jupyterhub==5.2.1 \
    jupyter-server-proxy \
    notebook

# ============================================================================
# 3. Create artemis user and project directory
# ============================================================================
RUN useradd -m -s /bin/bash -u 1000 artemis
RUN mkdir -p /home/artemis/project \
    && mkdir -p /home/artemis/project/data/raw \
    && mkdir -p /home/artemis/persist \
    && chown -R artemis:artemis /home/artemis

WORKDIR /home/artemis/project

# ============================================================================
# 4. Restore R packages (heaviest layer - cached by Docker)
# ============================================================================
COPY --chown=artemis:artemis renv.lock renv.lock
COPY --chown=artemis:artemis renv/activate.R renv/activate.R
COPY --chown=artemis:artemis .Rprofile .Rprofile

# Create a temporary .Rprofile for renv restore that doesn't load .Renviron
RUN echo 'source("renv/activate.R")' > /tmp/.Rprofile.build

# Restore renv library - disable cache symlinks so packages are copied
# directly into the project library (avoids broken symlinks when switching users)
ENV RENV_CONFIG_CACHE_SYMLINKS=FALSE
RUN R_PROFILE_USER=/tmp/.Rprofile.build Rscript -e " \
    renv::consent(provided = TRUE); \
    renv::restore(prompt = FALSE) \
"

# ============================================================================
# 5. Copy ARTEMIS source code
# ============================================================================
COPY --chown=artemis:artemis R/ R/
COPY --chown=artemis:artemis app/ app/
COPY --chown=artemis:artemis config/ config/
COPY --chown=artemis:artemis data/processed/ data/processed/
COPY --chown=artemis:artemis _targets.R _targets.R
COPY --chown=artemis:artemis scripts/ scripts/
COPY --chown=artemis:artemis run_app.R run_app.R

# ============================================================================
# 6. Copy container startup script and jupyter proxy config
# ============================================================================
COPY --chown=artemis:artemis docker/start.sh /usr/local/bin/start.sh
RUN chmod +x /usr/local/bin/start.sh

COPY --chown=artemis:artemis docker/jupyter_shiny_proxy.py /home/artemis/jupyter_shiny_proxy.py

# Install the server-proxy extension so Jupyter discovers it
RUN mkdir -p /usr/local/share/jupyter/jupyter_server_config.d
RUN echo '{"ServerProxy": {"servers": {}}}' > /usr/local/share/jupyter/jupyter_server_config.d/shiny-proxy.json

# Register shiny proxy as a package-level entry point via pip
RUN mkdir -p /opt/shiny-proxy && \
    cp /home/artemis/jupyter_shiny_proxy.py /opt/shiny-proxy/jupyter_shiny_proxy.py && \
    echo "from .jupyter_shiny_proxy import setup_shiny" > /opt/shiny-proxy/__init__.py && \
    printf '[project]\nname = "jupyter-shiny-proxy"\nversion = "0.1.0"\n\n[project.entry-points."jupyter_serverproxy_servers"]\nshiny = "jupyter_shiny_proxy:setup_shiny"\n' > /opt/shiny-proxy/pyproject.toml && \
    cd /opt/shiny-proxy && python3 -m pip install --no-cache-dir --break-system-packages -e .

# ============================================================================
# 7. Set ownership and switch to artemis user
# ============================================================================
RUN chown -R artemis:artemis /home/artemis/project
USER artemis

ENV HOME=/home/artemis
ENV USER=artemis

EXPOSE 8888

CMD ["/usr/local/bin/start.sh"]
